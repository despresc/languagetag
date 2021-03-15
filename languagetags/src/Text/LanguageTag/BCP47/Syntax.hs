{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.LanguageTag.BCP47.Syntax
-- Description : Language tag types and parser
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
    BCP47,
    parseBCP47,
    renderBCP47,
    renderBCP47Builder,
    toSubtags,

    -- * Constructing tags directly
    -- $valueconstruction

    -- ** Normal and private use tags
    unsafeNormalTag,
    unsafeFullNormalTag,
    unsafePrivateTag,

    -- ** Grandfathered tags
    -- $grandfathered
    grandfatheredSyntax,
    module Text.LanguageTag.Internal.BCP47.Registry.Grandfathered,

    -- * Errors
    Err (..),
    Component (..),
    ErrType (..),
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Syntax
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..), SubtagChar (..))
import Text.LanguageTag.BCP47.Subtag

{- TODO:
- spin out the try* functions into their own functions?
- benchmark a more straightforward implementation that does things
  like pre-splitting the input
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

instance NFData Component where
  rnf = rwhnf

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

instance NFData Err where
  rnf = rwhnf

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

instance NFData ErrType where
  rnf = rwhnf

----------------------------------------------------------------
-- Rendering to subtags
----------------------------------------------------------------

-- | Convert a 'BCP47' tag into its component subtags

-- TODO: check round-tripping of this function
toSubtags :: BCP47 -> NonEmpty Subtag
toSubtags (NormalTag (Normal p e1 e2 e3 s r vs es ps)) =
  p NE.:| (mapMaybe go [e1, e2, e3, s, r] <> vs <> es' <> ps)
  where
    go = maybeSubtag Nothing Just
    es' = flip concatMap es $ \(Extension c ts) ->
      extensionCharToSubtag c : NE.toList ts
toSubtags (PrivateUse x) = subtagX NE.:| NE.toList x
toSubtags (Grandfathered g) = case g of
  ArtLojban -> Subtag 14108546179528654867 NE.:| [Subtag 15690354374758891542]
  CelGaulish -> Subtag 14382069488147234835 NE.:| [Subtag 14954113284221173783]
  EnGbOed -> Subtag 14679482985414131730 NE.:| [Subtag 14954202562683731986, Subtag 16111381376313327635]
  IAmi -> Subtag 15132094747964866577 NE.:| [Subtag 14102819922971197459]
  IBnn -> Subtag 15132094747964866577 NE.:| [Subtag 14248104991419006995]
  IDefault -> Subtag 15132094747964866577 NE.:| [Subtag 14680466211245977112]
  IEnochian -> Subtag 15132094747964866577 NE.:| [Subtag 14680466211245977112]
  IHak -> Subtag 15132094747964866577 NE.:| [Subtag 15098133032806121491]
  IKlingon -> Subtag 15132094747964866577 NE.:| [Subtag 15542853518732230679]
  ILux -> Subtag 15132094747964866577 NE.:| [Subtag 15697226132455686163]
  IMingo -> Subtag 15132094747964866577 NE.:| [Subtag 15827749698417983509]
  INavajo -> Subtag 15132094747964866577 NE.:| [Subtag 15962927641447628822]
  IPwn -> Subtag 15132094747964866577 NE.:| [Subtag 16275850723642572819]
  ITao -> Subtag 15132094747964866577 NE.:| [Subtag 16827638435018702867]
  ITay -> Subtag 15132094747964866577 NE.:| [Subtag 16827550474088480787]
  ITsu -> Subtag 15132094747964866577 NE.:| [Subtag 16847869448969781267]
  NoBok -> Subtag 15977645578003677202 NE.:| [Subtag 14249204503046782995]
  NoNyn -> Subtag 15977645578003677202 NE.:| [Subtag 15989872147304546323]
  SgnBeFr -> Subtag 16690181889360658451 NE.:| [Subtag 14237004322024980498, Subtag 14828101773117358098]
  SgnBeNl -> Subtag 16690181889360658451 NE.:| [Subtag 14237004322024980498, Subtag 15974267878283149330]
  SgnChDe -> Subtag 16690181889360658451 NE.:| [Subtag 14384497209821364242, Subtag 14525234698176692242]
  ZhGuoyu -> Subtag 17699146535566049298 NE.:| [Subtag 14976579405109788693]
  ZhHakka -> Subtag 17699146535566049298 NE.:| [Subtag 15098140437866610709]
  ZhMin -> Subtag 17699146535566049298 NE.:| [Subtag 15827742560719208467]
  ZhMinNan -> Subtag 17699146535566049298 NE.:| [Subtag 15827742560719208467, Subtag 15962850549540323347]
  ZhXiang -> Subtag 17699146535566049298 NE.:| [Subtag 17412902894784479253]

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Pop a tag from the input stream
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
  (a -> Subtag -> Component -> Int -> Text -> Either Err BCP47) ->
  Either Err BCP47
mfinish !len !clast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  mc <- tagSep clast pos' inp
  case mc of
    Just (c, t) ->
      tagPop c t clast pos' >>= \(sbs, t') ->
        pr con sbs clast pos' t'
    Nothing -> pure $ finish con

isDigit :: SubtagChar -> Bool
isDigit c = w >= 48 && w <= 57
  where
    w = unwrapChar c

-- | Parse a BCP47 language tag
parseBCP47 :: Text -> Either Err BCP47
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
            Right $ Grandfathered EnGbOed
          | T.toLower inp == "sgn-be-fr" =
            Right $ Grandfathered SgnBeFr
          | T.toLower inp == "sgn-be-nl" =
            Right $ Grandfathered SgnBeNl
          | T.toLower inp == "sgn-ch-de" =
            Right $ Grandfathered SgnChDe
          | otherwise = Left e

parseBCP47' :: Char -> Text -> Either Err BCP47
parseBCP47' !initchar !inp = tagPop initchar inp Cbeginning 0 >>= parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- TODO: could be optimized a bit
    parsePrimary (st, t)
      | containsDigit st = Left $ Err 0 Cbeginning ErrBadChar
      | subtagLength st == 1 =
        if subtagHead st == subtagCharx
          then PrivateUse <$> parsePrivate 0 t
          else do
            msep <- tagSep Cprimary 0 t
            case msep of
              Just (c, t') -> parseIrregularI st c t'
              Nothing -> Left $ Err 0 Cprimary ErrNeededTag
      | subtagLength st >= 4 =
        mfinish
          (subtagLength st)
          Cprimary
          0
          t
          (initcon st nullSubtag nullSubtag nullSubtag)
          tryScript
      | otherwise = mfinish (subtagLength st) Cprimary 0 t (initcon st) (tryGrandPrimary st)

    tryGrandPrimary st0 con st1 clast pos t =
      case (unwrapSubtag st0, unwrapSubtag st1) of
        (14108546179528654867, 15690354374758891542)
          | T.null t -> pure $ Grandfathered ArtLojban
        (14382069488147234835, 14954113284221173783)
          | T.null t -> pure $ Grandfathered CelGaulish
        (15977645578003677202, 14249204503046782995)
          | T.null t -> pure $ Grandfathered NoBok
        (15977645578003677202, 15989872147304546323)
          | T.null t -> pure $ Grandfathered NoNyn
        (17699146535566049298, 14976579405109788693)
          | T.null t -> pure $ Grandfathered ZhGuoyu
        (17699146535566049298, 15098140437866610709)
          | T.null t -> pure $ Grandfathered ZhHakka
        (17699146535566049298, 15827742560719208467) -> do
          let pos' = pos + fromIntegral (subtagLength st1) + 1
          msep <- tagSep clast pos' t
          case msep of
            Nothing -> pure $ Grandfathered ZhMin
            Just (c, t') -> do
              (st2, t'') <- tagPop c t' Clext1 pos'
              case unwrapSubtag st2 of
                15962850549540323347
                  | T.null t'' -> pure $ Grandfathered ZhMinNan
                _ -> tryLext2 (con $ justSubtag st1) st2 Clext1 pos' t''
        (17699146535566049298, 17412902894784479253)
          | T.null t -> pure $ Grandfathered ZhXiang
        _ -> tryLext1 con st1 clast pos t

    tryLext1 !con st clast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) Clext1 pos t (con $ justSubtag st) tryLext2
      | otherwise = tryScript (con nullSubtag nullSubtag nullSubtag) st clast pos t

    tryLext2 !con st clast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) Clext2 pos t (con $ justSubtag st) tryLext3
      | otherwise = tryScript (con nullSubtag nullSubtag) st clast pos t

    tryLext3 !con st clast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) Clanguage pos t (con $ justSubtag st) tryScript
      | otherwise = tryScript (con nullSubtag) st clast pos t

    tryScript !con st clast pos t
      | subtagLength st == 4 && containsOnlyLetters st =
        mfinish (subtagLength st) Cscript pos t (con $ justSubtag st) tryRegion
      | otherwise = tryRegion (con nullSubtag) st clast pos t

    tryRegion !con st clast pos t
      | subtagLength st == 2 =
        if containsDigit st
          then Left $ Err pos clast ErrBadTag
          else mfinish (subtagLength st) Cregion pos t (con $ justSubtag st) tryVariant
      | subtagLength st == 3 =
        if containsLetter st
          then Left $ Err pos clast ErrBadTag
          else mfinish (subtagLength st) Cregion pos t (con $ justSubtag st) tryVariant
      | otherwise = tryVariant (con nullSubtag) st clast pos t

    tryVariant !con st clast pos t
      | subtagLength st == 4 =
        if isDigit $ subtagHead st
          then mfinish (subtagLength st) Cvariant pos t (con . (st :)) tryVariant
          else Left $ Err pos clast ErrBadTag
      | subtagLength st >= 5 =
        mfinish (subtagLength st) Cvariant pos t (con . (st :)) tryVariant
      | otherwise = trySingleton (con []) st clast pos t

    trySingleton con st clast pos t
      | subtagLength st /= 1 = Left $ Err pos clast ErrBadTag
      | subtagHead st == subtagCharx =
        parsePrivateUse (con []) pos t
      | otherwise =
        parseExtension
          ( \ne ->
              con
                . ( Extension
                      ( unsafeSubtagCharToExtension $ subtagHead st
                      )
                      ne
                      :
                  )
          )
          pos
          t

    parsePrivateUse con pos t = do
      let pos' = pos + 2
      ms <- tagSep Cprivateuse pos' t
      case ms of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cprivateuse pos'
          parsePrivateUseTag con st Cprivateuse pos' t''
        Nothing -> Left $ Err pos Cprivateuse ErrNeededTag

    parsePrivateUseTag con st _ pos t =
      mfinish (subtagLength st) Cprivateuse pos t (con . (st :)) parsePrivateUseTag

    parseExtension con pos t = do
      let pos' = pos + 2
      ms <- tagSep Cextension pos' t
      case ms of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cextension pos'
          if subtagLength st >= 2
            then
              mfinish
                (subtagLength st)
                Cextension
                pos'
                t''
                (con . (st NE.:|))
                parseExtensionTag
            else Left $ Err pos Cextension ErrNeededTag
        Nothing -> Left $ Err pos Cextension ErrNeededTag

    parseExtensionTag con st _ pos t
      | subtagLength st == 1 = trySingleton (con []) st Cextension pos t
      | otherwise = mfinish (subtagLength st) Cextension pos t (con . (st :)) parseExtensionTag

    parseIrregularI st c t
      | st /= subtagI = Left $ Err 0 Cbeginning ErrBadChar
      | otherwise = case tagPop c t Cbeginning 0 of
        Right (st', t') -> case T.uncons t' of
          Just _ -> Left $ Err 0 Cprimary ErrIrregINum
          Nothing -> recognizeIrregI st'
        Left e -> Left e

    recognizeIrregI n = case unwrapSubtag n of
      14102819922971197459 -> Right $ Grandfathered IAmi
      14248104991419006995 -> Right $ Grandfathered IBnn
      14526138628724883479 -> Right $ Grandfathered IDefault
      14680466211245977112 -> Right $ Grandfathered IEnochian
      15098133032806121491 -> Right $ Grandfathered IHak
      15542853518732230679 -> Right $ Grandfathered IKlingon
      15697226132455686163 -> Right $ Grandfathered ILux
      15827749698417983509 -> Right $ Grandfathered IMingo
      15962927641447628822 -> Right $ Grandfathered INavajo
      16275850723642572819 -> Right $ Grandfathered IPwn
      16827550474088480787 -> Right $ Grandfathered ITao
      16827638435018702867 -> Right $ Grandfathered ITay
      16847869448969781267 -> Right $ Grandfathered ITsu
      _ -> Left $ Err 2 CirregI ErrBadTag

parsePrivate :: Int -> Text -> Either Err (NE.NonEmpty Subtag)
parsePrivate initpos inp = do
  ms <- tagSep Cprivateuse initpos inp
  case ms of
    Just (c, t) -> do
      (st, t') <- tagPop c t Cprivateuse initpos
      parsePrivateUseTag (st NE.:|) (initpos + fromIntegral (subtagLength st) + 1) t'
    Nothing -> Left $ Err initpos Cprivateuse ErrNeededTag
  where
    parsePrivateUseTag con pos t = do
      mc <- tagSep Cprivateuse pos t
      case mc of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cprivateuse pos
          parsePrivateUseTag (con . (st :)) (pos + fromIntegral (subtagLength st) + 1) t''
        Nothing -> pure $ con []

-- $valueconstruction
--
-- Other than the 'parseBCP47' function, tags can also be constructed
-- using 'unsafeNormalTag' and 'unsafePrivateTag' if they are known to
-- be well-formed, as well as 'grandfatheredSyntax'.

-- $grandfathered
--
-- In a prior standard it was possible to register entire tags, not
-- simply subtags. Of those tags, the ones that could not be
-- represented via registered subtags were explicitly grandfathered
-- into the current standard via the grammar of the tags itself. All
-- of them are valid, but most are deprecated; see the documentation
-- for
-- 'Text.LanguageTag.Internal.BCP47.Registry.Grandfathered.Grandfathered'
-- for up-to-date details.

-- | Embed a 'Text.LanguageTag.BCP47.Registry.Grandfathered' language
-- tag in the 'BCP47' type
grandfatheredSyntax :: Grandfathered -> BCP47
grandfatheredSyntax = Grandfathered
{-# INLINE grandfatheredSyntax #-}

-- $regular
--
-- Grandfathered tags that conform to the normal language tag grammar,
-- but have one or more subtags that do not appear in the registry, or
-- appear with different semantics.

-- $irregular
--
-- Grandfathered tags that do not conform to the normal language tag
-- grammar.

----------------------------------------------------------------
-- Tag constants
----------------------------------------------------------------

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 120

subtagI :: Subtag
subtagI = Subtag 15132094747964866577

subtagX :: Subtag
subtagX = Subtag 17293822569102704657

----------------------------------------------------------------
-- Internal convenience class
----------------------------------------------------------------

class Finishing a where
  finish :: a -> BCP47

instance Finishing a => Finishing (MaybeSubtag -> a) where
  finish con = finish $ con nullSubtag

instance Finishing a => Finishing ([b] -> a) where
  finish con = finish $ con []

instance Finishing BCP47 where
  finish = id