{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: remove me or integrate me with Syntax

module Text.BCP47.SyntaxAlt where

import qualified Data.ByteString.Internal as BI
import Data.Char (isAsciiLower)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.BCP47.Internal.SyntaxAlt

{- TODO HERE

Okay, write the rest of the parsing for bench purposes. (N.B. need to
fix original's parsing to output proper case).

Also bench the implementation with just bytestrings and unfoldrN

(maybe a combo of the two, I don't know).

TODO: test strictness in more places (e.g. the non-strictness of : and
:| may be a problem)

TODO: try parsing strategy that first lexes the input stream into
Subtag tokens (i.e. pop everything first with some kind of foldr).

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
-- TODO: remember that Err 0 Cprimary ErrNeededTag can only occur
-- when the start is "x" or "i".
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
  | -- | an @i-@ tag should be followed by exactly one
    -- tag
    ErrIrregINum
  deriving (Eq, Ord, Show, Read)

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
  | isAsciiLower c = Just $ BI.c2w c
  | otherwise = Nothing
{-# INLINE lowAlphaNum #-}

-- | Parse a letter, also lower-casing it.
lowAlpha :: Char -> Maybe Word8
lowAlpha c
  | c < 'A' = Nothing
  | c <= 'Z' = Just $ BI.c2w c + 32
  | isAsciiLower c = Just $ BI.c2w c
  | otherwise = Nothing
{-# INLINE lowAlpha #-}

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Pop a tag from the input stream

-- TODO: we don't do/allow for input normalization yet!
tagPop ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, Bool, Bool, Text)
tagPop initchar inp clast pos = case popSubtag initchar inp of
  Just (st, sl, sd, t) -> Right (st, sl, sd, t)
  Nothing -> Left $ Err pos clast ErrBadChar
{-# INLINE tagPop #-}

tagPopBegin ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, Bool, Bool, Text)
tagPopBegin = tagPop
{-# INLINE tagPopBegin #-}

tagPopMid ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, Bool, Bool, Text)
tagPopMid = tagPop
{-# INLINE tagPopMid #-}

-- returns the character immediately after the separator if there was
-- one, and Nothing on end of input.
tagSep :: Component -> Int -> Text -> Either Err (Maybe (Char, Text))
tagSep !clast !pos !inp = case T.uncons inp of
  Just (c, t)
    | c == '-' -> case T.uncons t of
      Just (c', t') -> Right $ Just (c', t')
      Nothing -> Left $ Err pos clast ErrTagEnd
    | otherwise -> Left $ Err pos clast ErrTagEnd
  Nothing -> Right Nothing
{-# INLINE tagSep #-}

-- for the middle of the tag
mfinish ::
  Finishing a =>
  Word8 ->
  Component ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> Bool -> Bool -> Component -> Int -> Text -> Either Err LanguageTag) ->
  Either Err LanguageTag
mfinish !len !clast !pos !inp !con !pr = do
  mc <- tagSep clast pos inp
  case mc of
    Just (c, t) ->
      tagPopMid c t clast pos >>= \(sbs, sl, sd, t') ->
        pr con sbs sl sd clast (fromIntegral len + pos + 1) t'
    Nothing -> pure $ finish con
{-# INLINE mfinish #-}

isDigit :: SubtagChar -> Bool
isDigit (SubtagChar w) = w < 10
{-# INLINE isDigit #-}

parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 t = case T.uncons t of
  Just (c, t') -> parseBCP47' c t'
  Nothing -> Left $ Err 0 Cbeginning ErrEmpty

-- TODO: catch irregulars and grandfathered

-- TODO: also test out the normal approach of 'split'ting the input beforehand
parseBCP47' :: Char -> Text -> Either Err LanguageTag
parseBCP47' !initchar !inp = tagPopBegin initchar inp Cbeginning 0 >>= parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- TODO: could be optimized a bit
    parsePrimary (st, _, sd, t)
      | sd = Left $ Err 0 Cbeginning ErrBadChar
      | tagLength st == 1 =
        if subtagHead st == subtagCharx
          then PrivateTag <$> parsePrivate 0 t
          else do
            msep <- tagSep Cprimary 0 t
            case msep of
              Just (c, t') -> parseIrregularI st c t'
              Nothing -> Left $ Err 0 Cprimary ErrNeededTag
      | tagLength st >= 4 =
        mfinish
          (tagLength st)
          Cprimary
          0
          t
          (initcon st nullSubtag nullSubtag nullSubtag)
          tryScript
      | otherwise = mfinish (tagLength st) Cprimary 0 t (initcon st) tryLext1

    tryLext1 !con st sl sd clast pos t
      | not sd,
        tagLength st == 3 =
        mfinish (tagLength st) Clext1 pos t (con $ justSubtag st) tryLext2
      | otherwise = tryScript (con nullSubtag nullSubtag nullSubtag) st sl sd clast pos t

    tryLext2 !con st sl sd clast pos t
      | not sd,
        tagLength st == 3 =
        mfinish (tagLength st) Clext2 pos t (con $ justSubtag st) tryLext3
      | otherwise = tryScript (con nullSubtag nullSubtag) st sl sd clast pos t

    tryLext3 !con st sl sd clast pos t
      | not sd,
        tagLength st == 3 =
        mfinish (tagLength st) Clanguage pos t (con $ justSubtag st) tryScript
      | otherwise = tryScript (con nullSubtag) st sl sd clast pos t

    tryScript !con st sl sd clast pos t
      | tagLength st == 4,
        not sd =
        mfinish (tagLength st) Cscript pos t (con $ justSubtag st) tryRegion
      | otherwise = tryRegion (con nullSubtag) st sl sd clast pos t

    tryRegion !con st sl sd clast pos t
      | tagLength st == 2 =
        if not sd
          then mfinish (tagLength st) Cregion pos t (con $ justSubtag st) tryVariant
          else Left $ Err pos clast ErrBadTag
      | tagLength st == 3 =
        if not sl
          then mfinish (tagLength st) Cregion pos t (con $ justSubtag st) tryVariant
          else Left $ Err pos clast ErrBadTag
      | otherwise = tryVariant (con nullSubtag) st sl sd clast pos t

    tryVariant !con st sl sd clast pos t
      | tagLength st == 4 =
        if isDigit $ subtagHead st
          then mfinish (tagLength st) Cvariant pos t (con . strictCons st) tryVariant
          else Left $ Err pos clast ErrBadTag
      | tagLength st >= 5 =
        mfinish (tagLength st) Cvariant pos t (con . strictCons st) tryVariant
      | otherwise = trySingleton (con []) st sl sd clast pos t

    trySingleton !con st _ _ clast pos t
      | tagLength st /= 1 = Left $ Err pos clast ErrBadTag
      | subtagHead st == subtagCharx =
        parsePrivateUse (con []) pos t
      | otherwise = parseExtension (\ne -> con . strictCons (Extension (subtagHead st) ne)) pos t

    parsePrivateUse !con pos t = do
      ms <- tagSep Cprivateuse pos t
      case ms of
        Just (c, t') -> do
          (st, sl, sd, t'') <- tagPopMid c t' Cprivateuse pos
          parsePrivateUseTag con st sl sd Cprivateuse pos t''
        Nothing -> Left $ Err pos Cprivateuse ErrNeededTag

    parsePrivateUseTag !con st _ _ _ pos t =
      mfinish (tagLength st) Cprivateuse pos t (con . strictCons st) parsePrivateUseTag

    parseExtension ::
      (NE.NonEmpty Subtag -> [Extension] -> [Subtag] -> LanguageTag) ->
      Int ->
      Text ->
      Either Err LanguageTag
    parseExtension !con pos t = do
      ms <- tagSep Cextension pos t
      case ms of
        Just (c, t') -> do
          (st, _, _, t'') <- tagPopMid c t' Cextension pos
          if tagLength st >= 2
            then mfinish (tagLength st) Cextension pos t'' (con . strictNE st) parseExtensionTag
            else Left $ Err pos Cextension ErrNeededTag
        Nothing -> Left $ Err pos Cextension ErrNeededTag

    parseExtensionTag ::
      ([Subtag] -> [Extension] -> [Subtag] -> LanguageTag) ->
      Subtag ->
      Bool ->
      Bool ->
      Component ->
      Int ->
      Text ->
      Either Err LanguageTag
    parseExtensionTag con st sl sd _ pos t
      | tagLength st == 1 = trySingleton (con []) st sl sd Cextension pos t
      | otherwise = mfinish (tagLength st) Cextension pos t (con . strictCons st) parseExtensionTag

    parseIrregularI st c t
      | st /= subtagI = Left $ Err 0 Cbeginning ErrBadChar
      | otherwise = case tagPopMid c t Cbeginning 0 of
        Right (st', _, _, t') -> case T.uncons t' of
          Just _ -> Left $ Err 0 Cprimary ErrIrregINum
          Nothing -> recognizeIrregI st'
        Left e -> Left e

    -- TODO: might want to test to make sure these constants remain
    -- accurate
    recognizeIrregI (Subtag n) = case n of
      10595562548319223811 -> Right $ IrregularGrandfathered Iami
      10888648367819194371 -> Right $ IrregularGrandfathered Ibnn
      11424054330861289479 -> Right $ IrregularGrandfathered Idefault
      11753452397160103944 -> Right $ IrregularGrandfathered Ienochian
      12559272723341115395 -> Right $ IrregularGrandfathered Ihak
      13473417321460531207 -> Right $ IrregularGrandfathered Iklingon
      14036711545832996869 -> Right $ IrregularGrandfathered Imingo
      14289469405371826182 -> Right $ IrregularGrandfathered Inavajo
      14964406030589493251 -> Right $ IrregularGrandfathered Ipwn
      16018318712138366979 -> Right $ IrregularGrandfathered Itao
      16019022399580143619 -> Right $ IrregularGrandfathered Itay
      16099805717896101891 -> Right $ IrregularGrandfathered Itsu
      _ -> Left $ Err 2 CirregI ErrBadTag
{-# INLINE parseBCP47' #-}

parsePrivate :: Int -> Text -> Either Err (NE.NonEmpty Subtag)
parsePrivate initpos inp = do
  ms <- tagSep Cprivateuse initpos inp
  case ms of
    Just (c, t) -> do
      (st, _, _, t') <- tagPopMid c t Cprivateuse initpos
      parsePrivateUseTag (strictNE st) (initpos + fromIntegral (tagLength st) + 1) t'
    Nothing -> Left $ Err initpos Cprivateuse ErrNeededTag
  where
    parsePrivateUseTag !con !pos !t = do
      mc <- tagSep Cprivateuse pos t
      case mc of
        Just (c, t') -> do
          (st, _, _, t'') <- tagPopMid c t' Cprivateuse pos
          parsePrivateUseTag (con . strictCons st) (pos + fromIntegral (tagLength st) + 1) t''
        Nothing -> pure $ con []
{-# INLINE parsePrivate #-}

strictNE :: a -> [a] -> NE.NonEmpty a
strictNE !x !y = x NE.:| y

strictCons :: a -> [a] -> [a]
strictCons !x !y = x : y

----------------------------------------------------------------
-- Grandfathered (sub)tag constants
----------------------------------------------------------------

subtagI :: Subtag
subtagI = Subtag 12682136550675316737
