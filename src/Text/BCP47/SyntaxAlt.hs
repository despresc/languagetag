{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: remove me or integrate me with Syntax

module Text.BCP47.SyntaxAlt where

import qualified Data.ByteString.Internal as BI
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Debug.Trace (traceM)
import Text.BCP47.Internal.SyntaxAlt

{- TODO HERE

Okay, write the rest of the parsing for bench purposes. (N.B. need to
fix original's parsing to output proper case).

Also bench the implementation with just bytestrings and unfoldrN

(maybe a combo of the two, I don't know).

TODO: consider putting bang patterns in some of the M invocations (test)

TODO: test strictness in more places (e.g. the non-strictness of : and
:| may be a problem)

-}

-- TODO: need to improve the documentation for the error types (e.g. errComponent)

-- | The component just before what we're trying to parse
data Component
  = -- | just started
    Cbeginning
  | -- | primary language tag
    Cprimary
  | -- | first language extension
    Clext1
  | -- | second language extension
    Clext2
  | -- | the entire language tag
    Clanguage
  | -- | script tag
    Cscript
  | -- | region tag
    Cregion
  | -- | variant tag
    Cvariant
  | -- | extension tag
    Cextension
  | -- | private use tag
    Cprivateuse
  | -- | tag right after an initial @i-@
    CirregI
  deriving (Eq, Ord, Show, Read)

-- | An error that may occur during parsing
data Err = Err
  { -- | the start of the tag where the error occurred
    errPos :: !Int,
    -- | the section just before the erroneous tag
    errComponent :: !Component,
    -- | the error itself
    errType :: !ErrType
  }
  deriving (Eq, Ord, Show, Read)

-- TODO: come up with a type for the errlength bool?
-- TODO: could distinguish between "extremely bad" (not even a
-- alphanum/dash) and merely inappropriate
data ErrType
  = -- | incorrect tag length for the section (false for too short)
    ErrLength !Bool
  | -- | empty input
    ErrEmpty
  | -- | empty tag
    ErrEmptyTag
  | -- | character was encountered that was incorrect for the section
    ErrBadChar
  | -- | only one single-character tag at the beginning (TODO: better name)
    ErrSingleBegin
  | -- | another tag was expected
    ErrNeededTag
  | -- | expecting a tag separator or the end of input
    ErrTagEnd
  | -- | invalid tag
    ErrBadTag
  deriving (Eq, Ord, Show, Read)

{-
-- | The content of an error message
data ErrMessage
  = -- | input was empty
    ErrEmpty
  | -- | invalid tag and the last thing that was parsed correctly
    ErrBadTag !Text !Component
  | -- | empty private use section
    ErrPrivateEmpty
  | -- | empty extension section
    ErrExtensionEmpty
  | -- | did not have exactly one tag after an @i-@
    ErrIrregINum
  deriving (Eq, Ord, Show, Read)
-}

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

-- | Parse a digit or letter, also lower-casing it if it is a
-- letter.

-- TODO: optimize
-- TODO: bench the more straightforward one too
lowAlphaNum :: Char -> Maybe Word8
lowAlphaNum c
  | c < '0' = Nothing
  | c <= '9' = Just $ BI.c2w c
  | c < 'A' = Nothing
  | c <= 'Z' = Just $ BI.c2w c + 32
  | c >= 'a' && c <= 'z' = Just $ BI.c2w c
  | otherwise = Nothing
{-# INLINE lowAlphaNum #-}

-- | Parse a letter, also lower-casing it.
lowAlpha :: Char -> Maybe Word8
lowAlpha c
  | c < 'A' = Nothing
  | c <= 'Z' = Just $ BI.c2w c + 32
  | c >= 'a' && c <= 'z' = Just $ BI.c2w c
  | otherwise = Nothing
{-# INLINE lowAlpha #-}

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

onChar ::
  Char ->
  r ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  r
onChar c bad f g h
  | c > 'z' = bad
  | c >= 'a' = f $! BI.c2w c
  | c > 'Z' = bad
  | c >= 'A' = g $! BI.c2w c
  | c <= '9' && c >= '0' = h $! BI.c2w c
  | otherwise = bad
{-# INLINE onChar #-}

{-

Either input is empty or it is not.

One case is Just (Char, Text), the other is Nothing

So our stuff is

Maybe (Char, Text) -> Either Err (Maybe (Char, Text), a)

which becomes

-}

newtype M a = M
  { unM ::
      forall b.
      (a -> Text -> Component -> Int -> b) ->
      (Err -> b) ->
      Text ->
      Component ->
      Int ->
      b
  }

runM :: M a -> Text -> Either Err a
runM (M f) t = f good bad t Cbeginning 0
  where
    good a _ _ _ = Right a
    bad = Left
{-# INLINE runM #-}

instance Functor M where
  fmap f (M k) = M $ \ok err ->
    k (ok . f) err
  {-# INLINE fmap #-}

instance Applicative M where
  pure a = M $ \ok _ !t !c !n -> ok a t c n
  {-# INLINE pure #-}

  (M kf) <*> (M kx) = M $ \ok err !t !c !n ->
    let ok' f t' c' n' = kx (ok . f) err t' c' n'
     in kf ok' err t c n
  {-# INLINE (<*>) #-}

instance Monad M where
  M ka >>= mab = M $ \ok err !t !c !n ->
    let ok' b = unM (mab b) ok err
     in ka ok' err t c n
  {-# INLINE (>>=) #-}

throw :: ErrType -> M a
throw e = M $ \_ er _ !clast !pos -> er $ Err pos clast e
{-# INLINE throw #-}

-- | Parse the text of a tag given the first character of the tag
tagPop ::
  Char ->
  M (ShortByteString, Bool, Bool)
tagPop initchar = M $ \ok err inp !clast !pos ->
  let go l idx t sl sd
        | idx == 8 = ok (BS.pack $ l 8 [], sl, sd) t clast pos
        | otherwise = case T.uncons t of
          Just (c, t')
            | c /= '-' -> onChar' go l idx t' c (err $ Err pos clast ErrBadChar)
          _ -> ok (BS.pack $ l idx [], sl, sd) t clast pos
   in onChar' go (const id) 0 inp initchar (err $ Err pos clast ErrBadChar)
  where
    onChar' f l idx t c e =
      onChar
        c
        e
        (\w -> f (\len -> l len . (condDown idx w len :)) (idx + 1) t True False)
        (\w -> f (\len -> l len . (condUp idx w len :)) (idx + 1) t True False)
        (\w -> f (\len -> l len . (condDig idx w len :)) (idx + 1) t False True)
    condUp :: Word8 -> Word8 -> Word8 -> Word8
    condUp i n l
      | l == 2 && i < 2 = n
      | l == 4 && i == 0 = n
      | otherwise = n + 32

    condDown :: Word8 -> Word8 -> Word8 -> Word8
    condDown i n l
      | l == 2 && i < 2 = n - 32
      | l == 4 && i == 0 = n - 32
      | otherwise = n

    condDig :: Word8 -> Word8 -> Word8 -> Word8
    condDig _ n _ = n
{-# INLINE tagPop #-}

-- | Parse the text of a tag given the first character of the tag
tagPopBegin ::
  Char ->
  M (ShortByteString, Bool, Bool)
tagPopBegin initchar = M $ \ok err inp !clast !pos ->
  let go l idx t sl sd
        | idx == 8 = ok (BS.pack $ l 8 [], sl, sd) t clast pos
        | otherwise = case T.uncons t of
          Just (c, t')
            | c /= '-' -> onChar' go l idx t' c (err $ Err pos clast ErrBadChar)
          _ -> ok (BS.pack $ l idx [], sl, sd) t clast pos
   in onChar' go (const id) 0 inp initchar (err $ Err pos clast ErrBadChar)
  where
    onChar' f l idx t c e =
      onChar
        c
        e
        (\w -> f (\len -> l len . (condDown idx w len :)) (idx + 1) t True False)
        (\w -> f (\len -> l len . (condUp idx w len :)) (idx + 1) t True False)
        (\w -> f (\len -> l len . (condDig idx w len :)) (idx + 1) t False True)
    condUp :: Word8 -> Word8 -> Word8 -> Word8
    condUp _ n _ = n + 32


    condDown :: Word8 -> Word8 -> Word8 -> Word8
    condDown _ n _ = n

    condDig :: Word8 -> Word8 -> Word8 -> Word8
    condDig _ n _ = n
{-# INLINE tagPopBegin #-}

-- returns the character immediately after the separator if there was
-- one, and Nothing on end of input.
-- Takes the length of the tag we just parsed (successfully) and what it was
tagSep :: Component -> Int -> M (Maybe Char)
tagSep comp len = M $ \ok err inp !clast !pos -> case T.uncons inp of
  Just (c, t)
    | c == '-' -> case T.uncons t of
      -- TODO: I *think* this is correct for pos?
      Just (c', t') -> ok (Just c') t' comp (pos + 1 + len)
      Nothing -> err $ Err pos clast ErrEmptyTag
    | otherwise -> err $ Err pos clast ErrTagEnd
  Nothing -> ok Nothing inp comp (pos + len)
{-# INLINE tagSep #-}

-- takes the length of the tag we just parsed and what it was, and
-- whether the next tag should be all lower-case
mfinish ::
  Finishing a =>
  Component ->
  Int ->
  a ->
  (a -> ShortByteString -> Bool -> Bool -> M LanguageTag) ->
  M LanguageTag
mfinish !clast !len con pr = do
  mc <- tagSep clast len
  case mc of
    Just c -> tagPop c >>= \(sbs, sl, sd) -> pr con sbs sl sd
    Nothing -> pure $ finish con
{-# INLINE mfinish #-}

isDigit :: Word8 -> Bool
isDigit w = 48 <= w && w <= 57
{-# INLINE isDigit #-}

parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 t = case T.uncons t of
  Just (c, t') -> runM (parseBCP47' c) t'
  Nothing -> Left $ Err 0 Cbeginning ErrEmpty

-- TODO: catch irregulars and grandfathered
parseBCP47' :: Char -> M LanguageTag
parseBCP47' !initchar = tagPopBegin initchar >>= parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- the letter x
    wex = 120

    parsePrimary (sbs, _, sd)
      | sd = throw ErrBadChar
      | BS.length sbs == 1
      , BS.index sbs 0 == wex
      = PrivateTag <$> parsePrivate
      | BS.length sbs >= 4 =
        mfinish
          Cprimary
          (BS.length sbs)
          (initcon sbs BS.empty BS.empty BS.empty)
          tryScript
      | otherwise = mfinish Cprimary (BS.length sbs) (initcon sbs) tryLext1

    tryLext1 con sbs sl sd
      | not sd,
        BS.length sbs == 3 =
        mfinish Clext1 (BS.length sbs) (con sbs) tryLext2
      | otherwise = tryScript (con BS.empty BS.empty BS.empty) sbs sl sd

    tryLext2 con sbs sl sd
      | not sd,
        BS.length sbs == 3 =
        mfinish Clext2 (BS.length sbs) (con sbs) tryLext3
      | otherwise = tryScript (con BS.empty BS.empty) sbs sl sd

    tryLext3 con sbs sl sd
      | not sd,
        BS.length sbs == 3 =
        mfinish Clanguage (BS.length sbs) (con sbs) tryScript
      | otherwise = tryScript (con BS.empty) sbs sl sd

    tryScript con sbs sl sd
      | BS.length sbs == 4,
        not sd =
        mfinish Cscript (BS.length sbs) (con sbs) tryRegion
      | otherwise = tryRegion (con BS.empty) sbs sl sd

    tryRegion con sbs sl sd
      | sl && not sd && BS.length sbs == 2
          || sd && not sl && BS.length sbs == 3 =
        mfinish Cregion (BS.length sbs) (con sbs) tryVariant
      | otherwise = tryVariant (con BS.empty) sbs sl sd

    -- TODO: Technically I can stop falling at the length 4 case if
    -- the other condition doesn't hold, since nothing else should
    -- have length 4.
    tryVariant con sbs sl sd
      | BS.length sbs == 4,
        isDigit $ BS.index sbs 0 =
        mfinish Cvariant (BS.length sbs) (con . (sbs :)) tryVariant
      | BS.length sbs >= 5 =
        mfinish Cvariant (BS.length sbs) (con . (sbs :)) tryVariant
      | otherwise = trySingleton (con []) sbs sl sd

    trySingleton con sbs _ _
      | BS.length sbs /= 1 = throw ErrBadTag
      | BS.index sbs 0 == wex =
        parsePrivateUse (con [])
      | otherwise = parseExtension (\ne -> con . (Extension (BS.index sbs 0) ne :))

    parsePrivateUse con = do
      ms <- tagSep Cprivateuse 1
      case ms of
        Just c ->
          tagPop c >>= \(sbs, sl, sd) ->
            parsePrivateUseTag con sbs sl sd
        Nothing -> throw ErrNeededTag

    parsePrivateUseTag con sbs _ _ =
      mfinish Cprivateuse (BS.length sbs) (con . (sbs :)) parsePrivateUseTag

    parseExtension con = do
      ms <- tagSep Cextension 1
      case ms of
        Just c -> do
          (sbs, _, _) <- tagPop c
          if BS.length sbs >= 2
            then mfinish Cextension (BS.length sbs) (con . (sbs NE.:|)) parseExtensionTag
            else throw ErrNeededTag
        Nothing -> throw ErrNeededTag

    parseExtensionTag con sbs sl sd
      | BS.length sbs == 1 = trySingleton (con []) sbs sl sd
      | otherwise = mfinish Cextension (BS.length sbs) (con . (sbs :)) parseExtensionTag
{-# INLINE parseBCP47' #-}

parsePrivate :: M (NE.NonEmpty ShortByteString)
parsePrivate = do
  ms <- tagSep Cprivateuse 1
  case ms of
    Just c ->
      tagPop c >>= \(sbs, sl, sd) ->
        parsePrivateUseTag (sbs NE.:|) (BS.length sbs)
    Nothing -> throw ErrNeededTag
  where
    parsePrivateUseTag con len = do
      mc <- tagSep Cprivateuse len
      case mc of
        Just c -> tagPop c >>= \(sbs, sl, sd) -> parsePrivateUseTag (con . (sbs :)) (BS.length sbs)
        Nothing -> pure $ con []

{-
-- | Split a tag from the input stream. Note that if a tag is returned
-- then it will be a non-empty 'ShortByteString'. The first bool is
-- whether or not we've seen a letter. The second is for a digit.

-- TODO: reduce duplication
grabTag ::
  forall a.
  Bool ->
  Int ->
  Component ->
  Char ->
  Text ->
  (ShortByteString -> Bool -> Bool -> Char -> Text -> Either Err a) ->
  (ShortByteString -> Bool -> Bool -> Either Err a) ->
  Either Err a
grabTag allLow pos clast initchar rest more end =
  onChar
    initchar
    (Left $ Err pos clast ErrBadChar)
    (\w -> go (\len -> (condDown 0 w len :)) 1 rest True False)
    (\w -> go (\len -> (condUp 0 w len :)) 1 rest True False)
    (\w -> go (\len -> (condDig 0 w len :)) 1 rest False True)
  where
    go :: (Word8 -> [Word8] -> [Word8]) -> Word8 -> Text -> Bool -> Bool -> Either Err a
    go l idx t sl sd
      | idx == 8 = case T.uncons t of
        Just (c, t')
          | c == '-' -> case T.uncons t' of
            Just (c', t'') -> more (BS.pack $ l 8 []) sl sd c' t''
            Nothing -> Left $ Err (pos + 9) clast ErrEmptyTag
          | otherwise -> Left $ Err 0 Cprimary $ ErrLength True
        Nothing -> end (BS.pack $ l 8 []) sl sd
      | otherwise = case T.uncons t of
        Just (c, t')
          | c > 'z' -> Left $ Err pos clast ErrBadChar
          | c >= 'a' -> go (\len -> l len . (condDown idx (BI.c2w c) len :)) (idx + 1) t' True sd
          | c > 'Z' -> Left $ Err pos clast ErrBadChar
          | c >= 'A' -> go (\len -> l len . (condUp idx (BI.c2w c) len :)) (idx + 1) t' True sd
          | c <= '9' && c >= '0' -> go (\len -> l len . (condDig idx (BI.c2w c) len :)) (idx + 1) t' sl True
          | c == '-' -> case T.uncons t' of
            Just (c', t'') -> more (BS.pack $ l idx []) sl sd c' t''
            Nothing -> Left $ Err (pos + 1 + length (l idx [])) clast ErrEmptyTag
          | otherwise -> Left $ Err pos clast ErrBadChar
        Nothing -> end (BS.pack $ l idx []) sl sd

    condUp :: Word8 -> Word8 -> Word8 -> Word8
    condUp i n l
      | allLow = n + 32
      | l == 2 && i < 2 = n
      | l == 4 && i == 0 = n
      | otherwise = n + 32
    condDown :: Word8 -> Word8 -> Word8 -> Word8
    condDown i n l
      | l == 2 && i < 2 = n - 32
      | l == 4 && i == 0 = n - 32
      | otherwise = n
    condDig :: Word8 -> Word8 -> Word8 -> Word8
    condDig _ n _ = n
{-# INLINE grabTag #-}

-- TODO: catch irregulars
parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 x = case T.uncons x of
  Just (c, t) ->
    grabTag
      True
      0
      Cprimary
      c
      t
      ( \sbs _ sd c' t' ->
          if sd
            then Left $ Err 0 Cprimary ErrBadChar
            else parsePrimary sbs c' t'
      )
      ( \sbs _ sd ->
          if sd
            then Left $ Err 0 Cprimary ErrBadChar
            else parseSinglePrimary sbs
      )
  Nothing -> Left $ Err 0 Cprimary $ ErrEmpty
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    parseSinglePrimary sbs
      | BS.length sbs == 1 = undefined -- bad
      | otherwise = Right $! finish $ initcon sbs

    parsePrimary sbs c t
      | BS.length sbs >= 4 =
        tryScript
          Cprimary
          (initcon sbs BS.empty BS.empty BS.empty)
          (BS.length sbs + 1)
          c
          t
      | otherwise = undefined

    tryScript clast con pos c t = undefined
-}
{-

Not quite. What we should do:

- grab a new tag

- take in a failthrough callback for the case where we have more input
  (will take the sbs and bools)

- take in a failthrough callback for the case where we're at the end
  (will again take the sbs and bools)

everything can be made an instance of finish if we relax the output to
Either Err LanguageTag instead of just LanguageTag! Oh, but then we'd
need to pass in the last location. I guess that's not too bad.

what can we pass in?

- the accumulating constructor
- the position and section of the last thing parsed

also need the function that recognizes a particular tag, I suppose.

ideally I'd be able to create some kind of step-to class that fills a
constructor up to a particula component, but maybe that's too exotic.

-}
{-
grabSecTag ::
  Bool ->
  Int ->
  Component ->
  Char ->
  Text ->
  (ShortByteString -> Bool -> Bool -> Maybe a) ->
  (Component -> Text -> Either Err b) ->
  (a -> Text -> Either Err b) ->
  (a -> Either Err b) ->
  Either Err b
grabSecTag allLow pos clast c t recog failed more end
  = grabTag allLow pos clast c t go1 go2
  where
    go1 sbs t' sl sd = case recog sbs sl sd of
      Just a -> more a t'
      Nothing -> failed clast t'
-}

{-
Three cases:

- bad character encountered or tag too long
- candidate tag at the end of input
- candidate tag and there's more input

so need a function that accepts callbacks for all of those

I guess that could be what grabtag is?

then we have three actual cases:

- bad character encountered or tag too long
- good tag for the section being tested, end of input
- good tag for the section being tested, more to go
- bad tag for the section being tested, end of input/more to go

-}

{-

parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 t = case grabTag 8 lowAlpha t of
  More sbs t'
    | BS.null sbs -> Left $ Err 0 $ ErrLength False Cprimary
  BadChar -> Left $ Err 0 $ ErrBadChar Cprimary
  TooLong -> Left $ Err 0 $ ErrLength True Cprimary
  Done -> Left $ Err 0 $ ErrEmpty
-}

{-

type InputStream = [(Text, Int)]

inputSplit :: Text -> InputStream
inputSplit = List.unfoldr go . (,) 0 . T.split (== '-')
  where
    go (!pos, tag : tags) = Just ((tag, pos), (pos + T.length tag + 1, tags))
    go (_, []) = Nothing
{-# INLINE inputSplit #-}

-- TODO: add normalish
parseBCP47' :: InputStream -> Either Err LanguageTag
parseBCP47' ((t, _) : ts)
  | len == 0 = Left $ Err 0 ErrEmpty
  | len == 1 = case T.head t of
    'x' -> PrivateTag <$> parsePrivate 0 ts
    'X' -> PrivateTag <$> parsePrivate 0 ts
    _ -> Left $ Err 0 $ ErrBadTag t Cbeginning
  | otherwise = Left $ Err 0 $ ErrBadTag t Cbeginning
  where
    len = T.length t
parseBCP47' [] = Left $ Err 0 ErrEmpty
{-# INLINE parseBCP47' #-}

-- Takes the start of the private section for errors
parsePrivate :: Int -> InputStream -> Either Err (NonEmpty ShortByteString)
parsePrivate !_ ((x, xpos) : xs) = do
  x' <- parsePrivateTag x xpos
  go (x' NE.:|) xs
  where
    parsePrivateTag t pos = case getPrivate t of
      Just t' -> pure t'
      Nothing -> Left $ Err pos $ ErrBadTag t Cprivateuse
    go l ((t, pos) : ts) = do
      t' <- parsePrivateTag t pos
      go (l . (t' :)) ts
    go l [] = let l' = l [] in forcing l' `seq` (pure l')
    forcing (x NE.:| xs) = x `seq` forcing' xs
    forcing' (x : xs) = x `seq` forcing' xs
    forcing' [] = ()
parsePrivate privatestart [] = Left $ Err privatestart ErrPrivateEmpty
{-# INLINE parsePrivate #-}

-- optimized for private use, of course
privateUnfold :: (Char -> Maybe Word8) -> Text -> Maybe [Word8]
privateUnfold f t
  | T.length t >= 1,
    T.length t <= 8 =
    ending $ T.foldl' go (False, id) t
  | otherwise = Nothing
  where
    ending (b, l)
      | b = Nothing
      | otherwise = Just $ l []
    go (b, l) c
      | b = (b, l)
      | otherwise = case f c of
        Just w -> (b, l . (w :))
        Nothing -> (True, l)
{-# INLINE privateUnfold #-}

-- TODO: use guard
getPrivate :: Text -> Maybe ShortByteString
getPrivate = go . fmap BS.pack . privateUnfold lowAlphaNum
  where
    go Nothing = Nothing
    go (Just sbs)
      | BS.null sbs || BS.length sbs > 8 = Nothing
      | otherwise = Just sbs
{-# INLINE getPrivate #-}

{- TODO HERE:

try a char-by-char approach in this module

-}

-}
