{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.LanguageTag.BCP47.SyntaxAlt (parseLanguageTag, SyntaxError (..)) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.LanguageTag.Internal.BCP47.Syntax
import Text.LanguageTag.Internal.Subtag (Subtag (..), SubtagChar (..), unMaybeSubtag)
import Text.LanguageTag.Subtag

type Pos = Int

data Token = Token
  { tokenPos :: !Pos,
    tokenTag :: !Subtag
  }

data State = State
  { sCon :: !Normal,
    sFirstSubtag :: !MaybeSubtag,
    sAt :: !AtComponent,
    sVarAcc :: [Subtag] -> [Subtag],
    sCurrExt :: !SubtagChar,
    sCurrExtPos :: !Pos,
    sExtAcc :: [Subtag] -> [Subtag],
    sExtsAcc :: [Extension] -> [Extension],
    sPrivPos :: !Pos,
    sPrivAcc :: [Subtag] -> [Subtag]
  }

initState :: State
initState =
  State
    { sCon = Normal nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [] [] [],
      sFirstSubtag = nullSubtag,
      sAt = AtBeginning,
      sVarAcc = id,
      sCurrExt = subtagCharx,
      sCurrExtPos = 0,
      sExtAcc = id,
      sExtsAcc = id,
      sPrivPos = 0,
      sPrivAcc = id
    }

newtype M a = M {unM :: State -> Either SyntaxError (State, a)}

execM :: M a -> State -> Either SyntaxError State
execM m s = fst <$> unM m s

modify :: (State -> State) -> M ()
modify f = M $ \s -> pure (f s, ())

setSat :: AtComponent -> M ()
setSat c = modify $ \s -> s {sAt = c}

setCurrExt :: SubtagChar -> M ()
setCurrExt c = modify $ \s -> s {sCurrExt = c}

setCurrExtPos :: Pos -> M ()
setCurrExtPos pos = modify $ \s -> s {sCurrExtPos = pos}

snocPriv :: Subtag -> M ()
snocPriv s = modify $ \st -> st {sPrivAcc = sPrivAcc st . (s :)}

snocExt :: Subtag -> M ()
snocExt s = modify $ \st -> st {sExtAcc = sExtAcc st . (s :)}

setFirstSubtag :: Subtag -> M ()
setFirstSubtag s = modify $ \st -> st {sFirstSubtag = justSubtag s}

gets :: (State -> a) -> M a
gets f = M $ \s -> pure (s, f s)

instance Functor M where
  fmap f (M a) = M $ \s -> do
    (s', x) <- a s
    pure (s', f x)
  {-# INLINE fmap #-}

instance Applicative M where
  pure a = M $ \s -> pure (s, a)
  {-# INLINE pure #-}
  mf <*> mx = M $ \s -> do
    (s', f) <- unM mf s
    (s'', x) <- unM mx s'
    pure (s'', f x)
  {-# INLINE (<*>) #-}

instance Monad M where
  mx >>= f = M $ \s -> do
    (s', x) <- unM mx s
    unM (f x) s'
  {-# INLINE (>>=) #-}

throw :: SyntaxError -> M a
throw e = M $ \_ -> Left e

-- | Fold over the tokens in the input, stopping at the end of input
-- or when the accumulating function returns @Nothing@.
tokenTraverse_ :: (Token -> M a) -> Text -> M ()
tokenTraverse_ f = go 0
  where
    go !pos t
      | T.null t = pure ()
      | otherwise =
        let (x, xs) = T.span (/= '-') t
         in case parseSubtag x of
              Just st -> do
                let tok = Token pos st
                _ <- f tok
                let pos' = pos + fromIntegral (subtagLength' st) + 1
                go pos' (T.drop 1 xs)
              Nothing -> throw $ SyntaxError pos $ ErrorNotASubtag x
{-# INLINE tokenTraverse_ #-}

data SyntaxError = SyntaxError
  { syntaxErrorPos :: !Int,
    syntaxErrorType :: !SyntaxErrorType
  }
  deriving (Show)

data SyntaxErrorType
  = -- | the input was empty
    ErrorEmptyInput
  | -- | encountered an empty extension section
    ErrorEmptyExtension !SubtagChar
  | -- | encountered an empty private use section
    ErrorEmptyPrivateUse
  | -- | encountered something that is not a subtag
    ErrorNotASubtag !Text
  | -- | encountered a subtag that should not appear where it does
    ErrorBadSubtag !Subtag !AtComponent
  | -- | subtags after an @i-@ did not conform to the grandfathered list
    ErrorBadIrregularI
  deriving (Show)

isDigit :: SubtagChar -> Bool
isDigit c = w >= 48 && w <= 57
  where
    w = unwrapChar c

-- | The section that we are in. In other words, if we see
-- @AtSomething@ then any subtag from a section later than @Something@
-- can appear.
data AtComponent
  = AtBeginning
  | AtPrimlang
  | AtExtlang1
  | AtExtlang2
  | AtExtlang3
  | AtLanguage
  | AtScript
  | AtRegion
  | AtVariant
  | AtExtension
  | AtPrivateUse
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Set a component of the subtag. The input should not be AtPrimlang
-- or AtVariant.
setAt :: AtComponent -> Subtag -> Normal -> Normal
setAt !c !s !n = case c of
  AtExtlang1 -> n {extlang1 = s'}
  AtExtlang2 -> n {extlang2 = s'}
  AtExtlang3 -> n {extlang3 = s'}
  AtScript -> n {script = s'}
  AtRegion -> n {region = s'}
  _ -> n -- bit of a hack
  where
    s' = justSubtag s
{-# INLINE setAt #-}

orElse :: M a -> M a -> M a
orElse (M x) (M y) = M $ \s -> case x s of
  Left _ -> y s
  Right a -> Right a
{-# INLINE orElse #-}

step :: Token -> M ()
step t = do
  c <- gets sAt
  step' c t
{-# INLINE step #-}

step' :: AtComponent -> Token -> M ()
step' c tok = case c of
  AtBeginning
    | len == 1 ->
      guarding (subtagHead s == subtagCharx) >> setSat AtPrivateUse
        >> recordPrimlang
    | len <= 3 ->
      guarding (containsOnlyLetters s) >> setSat AtPrimlang
        >> recordPrimlang
    | otherwise ->
      guarding (containsOnlyLetters s) >> setSat AtLanguage
        >> recordPrimlang
  AtExtension
    | len == 1 -> popExtAcc >> trySingleton
    | otherwise -> snocExt s
  AtPrivateUse -> snocPriv s
  _
    | len == 1 -> trySingleton
    | len == 2 -> guarding (containsOnlyLetters s && c < AtRegion) >> setting AtRegion
    | len == 3 -> tryExtlang `orElse` tryRegion3
    | len == 4 -> tryScript `orElse` tryVariant4
    | otherwise -> settingVar
  where
    s = tokenTag tok
    len = subtagLength s
    err = SyntaxError (tokenPos tok) $ ErrorBadSubtag s c
    guarding b
      | b = pure ()
      | otherwise = throw err
    setting x = do
      setSat x
      modify $ \st -> st {sCon = setAt x s $ sCon st}
    settingVar = do
      setSat AtVariant
      modify $ \st -> st {sVarAcc = sVarAcc st . (s :)}
    recordPrimlang = do
      setFirstSubtag s
      modify $ \st -> st {sCon = (sCon st) {primlang = justSubtag s}}

    tryExtlang = do
      guarding $ containsOnlyLetters s
      case c of
        AtPrimlang -> setting AtExtlang1
        AtExtlang1 -> setting AtExtlang2
        AtExtlang2 -> setting AtExtlang3
        _ -> throw err

    tryRegion3 = guarding (containsOnlyDigits s && c < AtRegion) >> setting AtRegion

    tryScript = do
      guarding $ containsOnlyLetters s
      guarding $ c < AtScript
      setting AtScript

    tryVariant4 = guarding (isDigit $ subtagHead s) >> settingVar

    trySingleton
      | subtagHead s == subtagCharx = setSat AtPrivateUse
      | otherwise = setSat AtExtension >> setCurrExt (subtagHead s) >> setCurrExtPos (tokenPos tok)

    popExtAcc = do
      acc <- gets sExtAcc
      pos <- gets sCurrExtPos
      e <- gets sCurrExt
      let acc' = acc []
      case NE.nonEmpty acc' of
        Nothing -> throw $ SyntaxError pos $ ErrorEmptyExtension e
        Just acc'' ->
          modify
            ( \st ->
                st
                  { sExtAcc = id,
                    sExtsAcc = sExtsAcc st . (Extension e acc'' :)
                  }
            )
{-# INLINE step' #-}

parseNormalTag' :: Text -> Either SyntaxError State
parseNormalTag' t = execM (tokenTraverse_ step t) initState
{-# INLINE parseNormalTag' #-}

-- FIXME: the ext/pu could be a little more efficient, and with the
-- guardAllNull/pu too
parseNormalTag :: Text -> Either SyntaxError Normal
parseNormalTag t = do
  s <- parseNormalTag' t
  let basetag = sCon s
  let vars = sVarAcc s []
  let pendingext = sExtAcc s []
  let pendingpu = sPrivAcc s []
  exts <- case sAt s of
    AtExtension -> case NE.nonEmpty pendingext of
      Nothing -> Left $ SyntaxError (sCurrExtPos s) $ ErrorEmptyExtension (sCurrExt s)
      Just pendingext' -> pure $ sExtsAcc s [Extension (sCurrExt s) pendingext']
    _ -> pure $ sExtsAcc s []
  pu <- case sAt s of
    AtPrivateUse
      | null pendingpu -> Left $ SyntaxError (sPrivPos s) ErrorEmptyPrivateUse
      | otherwise -> pure pendingpu
    _ -> pure pendingpu
  let guardAllNull
        | all (== nullSubtag) [],
          null vars,
          null exts =
          pure ()
        | otherwise = Left $ SyntaxError 0 $ ErrorBadSubtag (unMaybeSubtag $ sFirstSubtag s) AtBeginning
  if primlang basetag == nullSubtag
    then guardAllNull $> basetag {privateUse = pu}
    else pure basetag {variants = vars, extensions = exts, privateUse = pu}
{-# INLINE parseNormalTag #-}

parseLanguageTag :: Text -> Either SyntaxError LanguageTag
parseLanguageTag = fmap NormalTag . parseNormalTag

----------------------------------------------------------------
-- Tag constants
----------------------------------------------------------------

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 120
