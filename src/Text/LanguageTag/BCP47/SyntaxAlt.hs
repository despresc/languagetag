{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.LanguageTag.BCP47.SyntaxAlt where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Functor (($>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.LanguageTag.Internal.BCP47.Syntax
import Text.LanguageTag.Internal.Subtag (Subtag (..), SubtagChar (..))
import Text.LanguageTag.Subtag

data FollowingTagTy
  = TyExtlang
  | TyScript
  | TyRegion
  | TyVariant

-- | The possible single character extensions; all the ASCII
-- alphanumeric characters (case-insensitive) except the letter X.
data ExtensionChar
  = Ext0
  | Ext1
  | Ext2
  | Ext3
  | Ext4
  | Ext5
  | Ext6
  | Ext7
  | Ext8
  | Ext9
  | ExtA
  | ExtB
  | ExtC
  | ExtD
  | ExtE
  | ExtF
  | ExtG
  | ExtH
  | ExtI
  | ExtJ
  | ExtK
  | ExtL
  | ExtM
  | ExtN
  | ExtO
  | ExtP
  | ExtQ
  | ExtR
  | ExtS
  | ExtT
  | ExtU
  | ExtV
  | ExtW
  | ExtY
  | ExtZ
  deriving (Eq, Ord, Show, Enum, Bounded)

{-
-- TODO: move this and extchar to internal syntax if this works out.
data Extension = Extension !ExtensionChar (NE.NonEmpty Subtag)
-}

unsafeToExtensionChar :: SubtagChar -> ExtensionChar
unsafeToExtensionChar = undefined

type SingletonSection = (ExtensionChar, [Subtag])

type Pos = Int

data Token = Token
  { tokenPos :: !Pos,
    tokenTag :: !Subtag
  }

tokenize :: [Text] -> Either SyntaxError [Token]
tokenize = go (id, 0)
  where
    go (!l, !pos) (x : xs) = case parseSubtag x of
      Just st -> go (l . (Token pos st :), pos + fromIntegral (subtagLength' st) + 1) xs
      Nothing -> Left $ SyntaxError pos $ ErrorNotASubtag x
    go (l, _) [] = Right $ l []
{-# INLINE tokenize #-}

initcon :: Subtag -> Normal
initcon !s = Normal s nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [] [] []
{-# INLINE initcon #-}

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
    ErrorBadSubtag !Subtag !Candidate
  | -- | subtags after an @i-@ did not conform to the grandfathered list
    ErrorBadIrregularI
  deriving (Show)

-- TODO: change to return ExtensionChar if we go with that
gatherSingletons ::
  [Token] ->
  ([Token], [(Pos, SubtagChar, [Subtag])], Maybe (Pos, [Subtag]))
gatherSingletons inp = (start, exts, private)
  where
    spanLargeTags = List.span $ (> 1) . subtagLength . tokenTag
    (start, rest) = spanLargeTags inp
    (exts, private) = go id rest

    go !l (t : ts)
      | h == subtagCharx = (l [], Just (tokenPos t, tokenTag <$> ts))
      | otherwise =
        let (ts', ts'') = spanLargeTags ts
         in go (l . ((tokenPos t, h, tokenTag <$> ts') :)) ts''
      where
        h = subtagHead (tokenTag t)
    go l [] = (l [], Nothing)
{-# INLINE gatherSingletons #-}

parseExtensions :: [(Pos, SubtagChar, [Subtag])] -> Either SyntaxError [Extension]
parseExtensions = go id
  where
    go l ((p, e, toks) : ts) = case NE.nonEmpty toks of
      Nothing -> Left $ SyntaxError p $ ErrorEmptyExtension e
      Just toks' -> go (l . (Extension e toks' :)) ts
    go l [] = Right $ l []
{-# INLINE parseExtensions #-}

parsePrivateUse :: Maybe (Pos, [Subtag]) -> Either SyntaxError [Subtag]
parsePrivateUse Nothing = Right []
parsePrivateUse (Just (p, ts))
  | List.null ts = Left $ SyntaxError p ErrorEmptyPrivateUse
  | otherwise = Right ts
{-# INLINE parsePrivateUse #-}

-- TODO: integrate with other errors
data BodyErr = BodyErr Int Candidate

data Candidate
  = AfterBeginning
  | AfterPrimlang
  | AfterExt1
  | AfterExt2
  | AfterExt3
  | AfterLang
  | AfterScript
  | AfterReg
  | AfterVariant
  deriving (Eq, Ord, Show, Enum, Bounded)

isDigit :: SubtagChar -> Bool
isDigit c = w >= 48 && w <= 57
  where
    w = unwrapChar c

-- The position of the thing we most recently parsed
data AtComponent
  = AtPrimlang
  | AtExtlang1
  | AtExtlang2
  | AtExtlang3
  | AtScript
  | AtRegion
  | AtVariant
  deriving (Eq, Ord, Enum)

atToCandidate :: AtComponent -> Candidate
atToCandidate = toEnum . succ . fromEnum
{-# INLINE atToCandidate #-}

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

-- validate a subtag knowing our current position, returning what our
-- new position is
--
-- FIXME: will do for now
getAt :: AtComponent -> Subtag -> Maybe AtComponent
getAt !c !s
  | len == 2 = guard (containsOnlyLetters s) $> AtRegion
  | len == 3 = tryExtlang <|> tryRegion3
  | len == 4 = tryScript <|> tryVariant4
  | otherwise = Just AtVariant
  where
    len = subtagLength s

    tryExtlang = do
      guard $ containsOnlyLetters s
      case c of
        AtPrimlang -> Just AtExtlang1
        AtExtlang1 -> Just AtExtlang2
        AtExtlang2 -> Just AtExtlang3
        AtExtlang3 -> Just AtScript
        _ -> Nothing
    tryRegion3 = guard (containsOnlyDigits s) $> AtRegion
    tryScript = do
      guard $ containsOnlyLetters s
      guard $ c <= AtScript
      Just AtScript
    tryVariant4 = guard (isDigit $ subtagHead s) $> AtVariant
{-# INLINE getAt #-}

-- TODO: un-where this
fillNormal :: AtComponent -> Normal -> [Token] -> Either SyntaxError Normal
fillNormal = go
  where
    go comp !tg (tok : toks) = case getAt comp st of
      Just comp' -> case comp' of
        AtVariant -> grabVariants tg (st :) toks
        _ -> go comp' (setAt comp' st tg) toks
      Nothing ->
        Left $
          SyntaxError (tokenPos tok) $
            ErrorBadSubtag (tokenTag tok) (atToCandidate comp)
      where
        st = tokenTag tok
    go _ !tg [] = Right tg

    grabVariants !tg l (tok : toks)
      | subtagLength st >= 5 || subtagLength st == 4 && isDigit (subtagHead st) =
        grabVariants tg (l . (st :)) toks
      | otherwise = Left $ SyntaxError (tokenPos tok) $ ErrorBadSubtag (tokenTag tok) AfterVariant
      where
        st = tokenTag tok
    grabVariants !tg l [] = Right $ tg {variants = l []}
{-# INLINE fillNormal #-}

parseLanguageTag :: Text -> Either SyntaxError LanguageTag
parseLanguageTag = parseLanguageTagComponents . T.split (== '-')
{-# INLINE parseLanguageTag #-}

parseLanguageTagComponents :: [Text] -> Either SyntaxError LanguageTag
parseLanguageTagComponents xs = do
  toks <- tokenize xs
  case toks of
    (tok : toks') -> handleFirst tok toks'
    [] -> Left $ SyntaxError 0 ErrorEmptyInput
  where
    handleFirst tok toks
      | st == Subtag 15132094747964866577 = parseIrregularI toks
      | st == Subtag 17293822569102704657 = parsePrivateUseTag $ tokenTag <$> toks
      | len == 1 = Left $ SyntaxError 0 $ ErrorBadSubtag st AfterBeginning
      | len <= 3 = handleGrandfathered tok toks
      | otherwise = handleNormal AtPrimlang (initcon st) toks
      where
        st = tokenTag tok
        len = subtagLength st

    parsePrivateUseTag (st : sts) = Right $ PrivateTag (st NE.:| sts)
    parsePrivateUseTag [] = Left $ SyntaxError 0 ErrorEmptyPrivateUse

    -- FIXME: this and handleGrandfathered can obviously be more elegant
    parseIrregularI [x] = fmap IrregularGrandfathered $ case unwrapSubtag $ tokenTag x of
      14102819922971197459 -> Right Iami
      14248104991419006995 -> Right Ibnn
      14526138628724883479 -> Right Idefault
      14680466211245977112 -> Right Ienochian
      15098133032806121491 -> Right Ihak
      15542853518732230679 -> Right Iklingon
      15697226132455686163 -> Right Ilux
      15827749698417983509 -> Right Imingo
      15962927641447628822 -> Right Inavajo
      16275850723642572819 -> Right Ipwn
      16827550474088480787 -> Right Itao
      16827638435018702867 -> Right Itay
      16847869448969781267 -> Right Itsu
      _ -> Left $ SyntaxError 0 ErrorBadIrregularI
    parseIrregularI _ = Left $ SyntaxError 0 ErrorBadIrregularI

    -- handle grandfathered tags not starting with i, if the passed
    -- tag is one, and otherwise pass off the tag to handleNormal.
    handleGrandfathered tok toks = case unwrapSubtag st of
      14679482985414131730 -> case unwrappedToks of
        [14954202562683731986, 16111381376313327635] ->
          Right $
            IrregularGrandfathered EnGBoed
        _ -> handleNormal AtPrimlang (initcon st) toks
      16690181889360658451 -> case unwrappedToks of
        [14237004322024980498, 14828101773117358098] ->
          Right $
            IrregularGrandfathered SgnBEFR
        [14237004322024980498, 15974267878283149330] ->
          Right $
            IrregularGrandfathered SgnBENL
        [14384497209821364242, 14525234698176692242] ->
          Right $
            IrregularGrandfathered SgnCHDE
        _ -> handleNormal AtPrimlang (initcon st) toks
      14108546179528654867 -> case unwrappedToks of
        [15690354374758891542] ->
          Right $
            RegularGrandfathered Artlojban
        _ -> handleNormal AtPrimlang (initcon st) toks
      14382069488147234835 -> case unwrappedToks of
        [14954113284221173783] ->
          Right $
            RegularGrandfathered Celgaulish
        _ -> handleNormal AtPrimlang (initcon st) toks
      15977645578003677202 -> case unwrappedToks of
        [14249204503046782995] ->
          Right $
            RegularGrandfathered Nobok
        [15989872147304546323] ->
          Right $
            RegularGrandfathered Nonyn
        _ -> handleNormal AtPrimlang (initcon st) toks
      17699146535566049298 -> case unwrappedToks of
        [14976579405109788693] ->
          Right $
            RegularGrandfathered Zhguoyu
        [15098140437866610709] ->
          Right $
            RegularGrandfathered Zhhakka
        [15827742560719208467] ->
          Right $
            RegularGrandfathered Zhmin
        [15827742560719208467, 15962850549540323347] ->
          Right $
            RegularGrandfathered Zhminnan
        [17412902894784479253] ->
          Right $
            RegularGrandfathered Zhxiang
        _ -> handleNormal AtPrimlang (initcon st) toks
      _ -> handleNormal AtPrimlang (initcon st) toks
      where
        st = tokenTag tok
        unwrappedToks = unwrapSubtag . tokenTag <$> toks

    handleNormal comp tg toks = do
      let (start, exttoks, privatetoks) = gatherSingletons toks
      tg' <- fillNormal comp tg start
      exts <- parseExtensions exttoks
      pu <- parsePrivateUse privatetoks
      pure $ NormalTag tg' {extensions = exts, privateUse = pu}
{-# INLINE parseLanguageTagComponents #-}

----------------------------------------------------------------
-- Tag constants
----------------------------------------------------------------

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 120
