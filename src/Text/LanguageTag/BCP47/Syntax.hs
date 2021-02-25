{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Text.LanguageTag.BCP47.Syntax
-- Description : BCP47 language tag parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'parseBCP47' function to parse well-formed
-- (but not necessarily valid) BCP47 language tags as of the current
-- 2009 version. A copy of this standard is available at
-- <https://tools.ietf.org/html/bcp47>.
module Text.LanguageTag.BCP47.Syntax
  ( -- * Parsing and rendering tags
    LanguageTag,
    parseBCP47,
    renderLanguageTag,
    renderLanguageTagBuilder,

    -- * Constructing tags directly
    -- $valueconstruction

    -- ** Normal and private use tags
    unsafeNormalTag,
    unsafeFullNormalTag,
    unsafePrivateTag,

    -- ** Grandfathered tags
    -- $grandfathered

    -- *** Irregular grandfathered tags
    -- $irregular
    enGbOed,
    iAmi,
    iBnn,
    iDefault,
    iEnochian,
    iHak,
    iKlingon,
    iLux,
    iMingo,
    iNavajo,
    iPwn,
    iTao,
    iTay,
    iTsu,
    sgnBeFr,
    sgnBeNl,
    sgnChDe,

    -- *** Regular grandfathered tags
    -- $regular
    artLojban,
    celGaulish,
    noBok,
    noNyn,
    zhGuoyu,
    zhHakka,
    zhMin,
    zhMinNan,
    zhXiang,

    -- * Subtags
    Subtag,
    renderSubtag,
    renderSubtagBuilder,
    packSubtagMangled,
    unwrapSubtag,
    wrapSubtag,
    unsafeWrapSubtag,
    unpackSubtag,
    subtagHead,
    unsafeIndexSubtag,
    MaybeSubtag,
    justSubtag,
    nullSubtag,
    fromMaybeSubtag,
    SubtagChar,
    packChar,
    packCharMangled,
    unpackChar,

    -- * Errors
    Err (..),
    Component (..),
    ErrType (..),
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import Text.LanguageTag.Internal.BCP47.Syntax

{- TODO:
- spin out the try* functions into their own functions?
- better errors - might like to say something like: "subtag whatever
  is not one of <stuff>, at <location>", but in a data type of course
- benchmark a more straightforward implementation that does things
  like pre-splitting the input
- maybe benchmark the obvious megaparsec implementation?
-}

-- | The component just before what we're trying to parse.
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

-- TODO: could distinguish between "extremely bad" (not even a
-- alphanum/dash) and merely inappropriate
-- TODO: remember that Err 0 Cprimary ErrNeededTag can only occur
-- when the start is "x" or "i".

-- | The type of error that occurred during parsing
data ErrType
  = -- | empty input
    ErrEmpty
  | -- | character was encountered that was incorrect for the section
    ErrBadChar
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
-- Parsing
----------------------------------------------------------------

-- | Pop a tag from the input stream
tagPopDetail ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, SeenChar, Text)
tagPopDetail initchar inp clast pos = case popSubtagDetail initchar inp of
  Just (x, y, z) -> Right (x, y, z)
  Nothing -> Left $ Err pos clast ErrBadChar

-- | Pop a tag from the input stream when we don't care about the
-- SeenChar.
tagPop ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, Text)
tagPop initchar inp clast pos = case popSubtag initchar inp of
  Just (s, t) -> Right (s, t)
  Nothing -> Left $ Err pos clast ErrBadChar

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

mfinish ::
  Finishing a =>
  Word8 ->
  Component ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> SeenChar -> Component -> Int -> Text -> Either Err LanguageTag) ->
  Either Err LanguageTag
mfinish !len !clast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  mc <- tagSep clast pos' inp
  case mc of
    Just (c, t) ->
      tagPopDetail c t clast pos' >>= \(sbs, sc, t') ->
        pr con sbs sc clast pos' t'
    Nothing -> pure $ finish con

mfinishSimple ::
  Finishing a =>
  Word8 ->
  Component ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> Component -> Int -> Text -> Either Err LanguageTag) ->
  Either Err LanguageTag
mfinishSimple !len !clast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  mc <- tagSep clast pos' inp
  case mc of
    Just (c, t) ->
      tagPop c t clast pos' >>= \(sbs, t') ->
        pr con sbs clast pos' t'
    Nothing -> pure $ finish con

isDigit :: SubtagChar -> Bool
isDigit (SubtagChar w) = w >= 48 && w <= 57

-- | Parse a BCP47 language tag
parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 inp = case T.uncons inp of
  Just (c, t) -> catchIrregulars $ parseBCP47' c t
  Nothing -> Left $ Err 0 Cbeginning ErrEmpty
  where
    -- FIXME: sufficient for the moment
    catchIrregulars (Right a) = Right a
    catchIrregulars (Left e)
      | T.length inp == 9 = testIrregs
      | otherwise = Left e
      where
        testIrregs
          | T.toLower inp == "en-gb-oed" =
            Right $ IrregularGrandfathered EnGBoed
          | T.toLower inp == "sgn-be-fr" =
            Right $ IrregularGrandfathered SgnBEFR
          | T.toLower inp == "sgn-be-nl" =
            Right $ IrregularGrandfathered SgnBENL
          | T.toLower inp == "sgn-ch-de" =
            Right $ IrregularGrandfathered SgnCHDE
          | otherwise = Left e

-- TODO: also test out the normal approach of 'split'ting the input beforehand
parseBCP47' :: Char -> Text -> Either Err LanguageTag
parseBCP47' !initchar !inp = tagPopDetail initchar inp Cbeginning 0 >>= parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- TODO: could be optimized a bit
    parsePrimary (st, OnlyLetter, t)
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
      | otherwise = mfinish (tagLength st) Cprimary 0 t (initcon st) (tryGrandPrimary st)
    parsePrimary _ = Left $ Err 0 Cbeginning ErrBadChar

    tryGrandPrimary st0 con st1 sc clast pos t =
      case (unSubtag st0, unSubtag st1) of
        (14108546179528654851, 15690354374758891526)
          | T.null t -> pure $ RegularGrandfathered Artlojban
        (14382069488147234819, 14954113284221173767)
          | T.null t -> pure $ RegularGrandfathered Celgaulish
        (15977645578003677186, 14249204503046782979)
          | T.null t -> pure $ RegularGrandfathered Nobok
        (15977645578003677186, 15989872147304546307)
          | T.null t -> pure $ RegularGrandfathered Nonyn
        (17699146535566049282, 14976579405109788677)
          | T.null t -> pure $ RegularGrandfathered Zhguoyu
        (17699146535566049282, 15098140437866610693)
          | T.null t -> pure $ RegularGrandfathered Zhhakka
        (17699146535566049282, 15827742560719208451) -> do
          let pos' = pos + fromIntegral (tagLength st1) + 1
          msep <- tagSep clast pos' t
          case msep of
            Nothing -> pure $ RegularGrandfathered Zhmin
            Just (c, t') -> do
              (st2, sc', t'') <- tagPopDetail c t' Clext1 pos'
              case unSubtag st2 of
                15962850549540323331
                  | T.null t'' -> pure $ RegularGrandfathered Zhminnan
                _ -> tryLext2 (con $ justSubtag st1) st2 sc' Clext1 pos' t''
        (17699146535566049282, 17412902894784479237)
          | T.null t -> pure $ RegularGrandfathered Zhxiang
        _ -> tryLext1 con st1 sc clast pos t

    tryLext1 !con st sc clast pos t
      | OnlyLetter <- sc,
        tagLength st == 3 =
        mfinish (tagLength st) Clext1 pos t (con $ justSubtag st) tryLext2
      | otherwise = tryScript (con nullSubtag nullSubtag nullSubtag) st sc clast pos t

    tryLext2 !con st sc clast pos t
      | OnlyLetter <- sc,
        tagLength st == 3 =
        mfinish (tagLength st) Clext2 pos t (con $ justSubtag st) tryLext3
      | otherwise = tryScript (con nullSubtag nullSubtag) st sc clast pos t

    tryLext3 !con st sc clast pos t
      | OnlyLetter <- sc,
        tagLength st == 3 =
        mfinish (tagLength st) Clanguage pos t (con $ justSubtag st) tryScript
      | otherwise = tryScript (con nullSubtag) st sc clast pos t

    tryScript !con st sc clast pos t
      | tagLength st == 4,
        OnlyLetter <- sc =
        mfinish (tagLength st) Cscript pos t (con $ justSubtag st) tryRegion
      | otherwise = tryRegion (con nullSubtag) st sc clast pos t

    tryRegion !con st sc clast pos t
      | tagLength st == 2 = case sc of
        OnlyLetter -> mfinishSimple (tagLength st) Cregion pos t (con $ justSubtag st) tryVariant
        _ -> Left $ Err pos clast ErrBadTag
      | tagLength st == 3 = case sc of
        OnlyDigit -> mfinishSimple (tagLength st) Cregion pos t (con $ justSubtag st) tryVariant
        _ -> Left $ Err pos clast ErrBadTag
      | otherwise = tryVariant (con nullSubtag) st clast pos t

    tryVariant !con st clast pos t
      | tagLength st == 4 =
        if isDigit $ subtagHead st
          then mfinishSimple (tagLength st) Cvariant pos t (con . strictCons st) tryVariant
          else Left $ Err pos clast ErrBadTag
      | tagLength st >= 5 =
        mfinishSimple (tagLength st) Cvariant pos t (con . strictCons st) tryVariant
      | otherwise = trySingleton (con []) st clast pos t

    trySingleton con st clast pos t
      | tagLength st /= 1 = Left $ Err pos clast ErrBadTag
      | subtagHead st == subtagCharx =
        parsePrivateUse (con []) pos t
      | otherwise = parseExtension (\ne -> con . strictCons (Extension (subtagHead st) ne)) pos t

    parsePrivateUse con pos t = do
      let pos' = pos + 2
      ms <- tagSep Cprivateuse pos' t
      case ms of
        Just (c, t') -> do
          (st, sc, t'') <- tagPopDetail c t' Cprivateuse pos'
          parsePrivateUseTag con st sc Cprivateuse pos' t''
        Nothing -> Left $ Err pos Cprivateuse ErrNeededTag

    parsePrivateUseTag con st _ _ pos t =
      mfinish (tagLength st) Cprivateuse pos t (con . strictCons st) parsePrivateUseTag

    parseExtension con pos t = do
      let pos' = pos + 2
      ms <- tagSep Cextension pos' t
      case ms of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cextension pos'
          if tagLength st >= 2
            then
              mfinishSimple
                (tagLength st)
                Cextension
                pos'
                t''
                (con . strictNE st)
                parseExtensionTag
            else Left $ Err pos Cextension ErrNeededTag
        Nothing -> Left $ Err pos Cextension ErrNeededTag

    parseExtensionTag con st _ pos t
      | tagLength st == 1 = trySingleton (con []) st Cextension pos t
      | otherwise = mfinishSimple (tagLength st) Cextension pos t (con . strictCons st) parseExtensionTag

    parseIrregularI st c t
      | st /= subtagI = Left $ Err 0 Cbeginning ErrBadChar
      | otherwise = case tagPop c t Cbeginning 0 of
        Right (st', t') -> case T.uncons t' of
          Just _ -> Left $ Err 0 Cprimary ErrIrregINum
          Nothing -> recognizeIrregI st'
        Left e -> Left e

    -- TODO: might want to test to make sure these constants remain
    -- accurate
    recognizeIrregI (Subtag n) = case n of
      14102819922971197443 -> Right $ IrregularGrandfathered Iami
      14248104991419006979 -> Right $ IrregularGrandfathered Ibnn
      14526138628724883463 -> Right $ IrregularGrandfathered Idefault
      14680466211245977096 -> Right $ IrregularGrandfathered Ienochian
      15098133032806121475 -> Right $ IrregularGrandfathered Ihak
      15542853518732230663 -> Right $ IrregularGrandfathered Iklingon
      15697226132455686147 -> Right $ IrregularGrandfathered Ilux
      15827749698417983493 -> Right $ IrregularGrandfathered Imingo
      15962927641447628806 -> Right $ IrregularGrandfathered Inavajo
      16275850723642572803 -> Right $ IrregularGrandfathered Ipwn
      16827550474088480771 -> Right $ IrregularGrandfathered Itao
      16827638435018702851 -> Right $ IrregularGrandfathered Itay
      16847869448969781251 -> Right $ IrregularGrandfathered Itsu
      _ -> Left $ Err 2 CirregI ErrBadTag

parsePrivate :: Int -> Text -> Either Err (NE.NonEmpty Subtag)
parsePrivate initpos inp = do
  ms <- tagSep Cprivateuse initpos inp
  case ms of
    Just (c, t) -> do
      (st, t') <- tagPop c t Cprivateuse initpos
      parsePrivateUseTag (strictNE st) (initpos + fromIntegral (tagLength st) + 1) t'
    Nothing -> Left $ Err initpos Cprivateuse ErrNeededTag
  where
    parsePrivateUseTag con pos t = do
      mc <- tagSep Cprivateuse pos t
      case mc of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cprivateuse pos
          parsePrivateUseTag (con . strictCons st) (pos + fromIntegral (tagLength st) + 1) t''
        Nothing -> pure $ con []

----------------------------------------------------------------
-- Grandfathered (sub)tag constants
----------------------------------------------------------------

subtagI :: Subtag
subtagI = Subtag 15132094747964866561

----------------------------------------------------------------
-- Renamed exports
----------------------------------------------------------------

-- | Render a subtag as a laxy text builder. The resulting value will
-- be in lower case.
renderSubtagBuilder :: Subtag -> TB.Builder
renderSubtagBuilder = renderSubtagLow

-- | Render a subtag as a strict text value. The resulting value will
-- be in lower case. This should be faster than converting the result
-- of 'renderSubtagBuilder' to strict text.
renderSubtag :: Subtag -> Text
renderSubtag w = T.unfoldrN (fromIntegral len) go (w, 0)
  where
    len = tagLength w
    go (n, idx)
      | idx == len = Nothing
      | otherwise =
        let (c, n') = unsafePopChar n
         in Just (unpackChar c, (n', idx + 1))

-- $valueconstruction
--
-- Other than the 'parseBCP47' function, tags can also be constructed
-- using 'unsafeNormalTag' and 'unsafePrivateTag' if they are known to
-- be well-formed. Constants are also provided for all the
-- grandfathered tags in BCP47, but note that as of 2021-02-23 all of
-- them are deprecated except for 'iDefault' and 'iMingo', and of the
-- deprecated tags, only 'iEnochian' has no replacement tag.

-- $grandfathered
--
-- In a prior standard it was possible to register entire tags, not
-- simply subtags. Of those tags, the ones that could not be
-- represented via registered subtags were explicitly grandfathered
-- into the current standard via the grammar of the tags itself. All
-- of them are valid, but they are all deprecated except for
-- 'iDefault' and 'iMingo'.

-- $regular
--
-- Grandfathered tags that conform to the normal language tag grammar,
-- but have one or more subtags that do not appear in the registry, or
-- appear with different semantics.

-- $irregular
--
-- Grandfathered tags that do not conform to the normal language tag
-- grammar.