{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Language tag types and parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'parseBCP47' function to parse well-formed
-- (but not necessarily valid) BCP47 language tags as of the current
-- 2009 version of the standard, a copy of which is available at
-- <https://tools.ietf.org/html/bcp47>.
--
-- Note that it is, generally speaking, easier to work with valid tags
-- represented by the 'LanguageTag.BCP47.Registry.BCP47' type in
-- "LanguageTag.BCP47.Registry", since the invariants required by
-- the 'BCP47' type in this module are difficult to guarantee
-- statically without the interface becoming very unwieldy. Valid tags
-- can be constructed from these tags with
-- 'LanguageTag.BCP47.Registry.validateBCP47', or with
-- 'LanguageTag.BCP47.Quasi.tag' and the related quasi-quoters in
-- that module. This module is provided for applications that only
-- need to verify well-formedness of tags, without analyzing them
-- further.
module LanguageTag.BCP47.Syntax
  ( -- * Parsing and rendering tags
    BCP47,
    parseBCP47,
    parseBCP47FromSubtags,
    popBCP47Detail,
    popBCP47DetailWith,
    renderBCP47,
    renderBCP47Builder,
    toSubtags,

    -- * Constructing tags directly
    -- $valueconstruction
    grandfatheredSyntax,
    Grandfathered (..),

    -- * Errors
    ParseError (..),
    StepError (..),
    StepErrorType (..),
    AtComponent (..),
    SubtagCategory (..),
    PopError (..),
    subtagCategoryName,
    subtagCategorySyntax,
    atComponentDescription,
    expectedCategories,

    -- * Stepped parsing
    startBCP47,
    stepBCP47,
    finalizeBCP47,

    -- * Tag character predicates
    isTagChar,
    isTagByte,

    -- * Temp partial exports
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import LanguageTag.BCP47.Registry.Grandfathered
import LanguageTag.BCP47.Subtag hiding (ParseError (..), PopError (..))
import qualified LanguageTag.BCP47.Subtag as Sub
import LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import LanguageTag.Internal.BCP47.Syntax

----------------------------------------------------------------
-- Partial parser
----------------------------------------------------------------

-- | An error that may occur when trying to append a 'Subtag' to a
-- 'PartialBCP47' value. The 'startBCP47', 'stepBCP47', and 'finalizeBCP47'
-- functions will, whenever possible, include the 'BCP47' tag that has been
-- parsed so far in the errors that they emit. This is not possible only when
-- the fragment of the tag parsed so far does not constitute a well-formed
-- 'BCP47' tag itself, something that occurs exactly when
--
-- * the first subtag is not one of @i@, @x@, or a primary language subtag
--
-- * an @x@ (indicating a private use tag) or @i@ (indicating a type of
--   grandfathered tag) was the first subtag, but no subsequent subtags were
--   added to the 'PartialBCP47' tag before attempted finalization
data StepError = StepError (Maybe BCP47) AtComponent StepErrorType
  deriving (Eq, Ord, Show)

-- | The types of errors that may occur when trying to append a 'Subtag' to a
-- 'PartialBCP47' value
data StepErrorType
  = -- | an extension section start was immediately followed by another
    -- singleton (@Just extensionChar@) or nothing at all (@Nothing@)
    EmptyExtensionSection ExtensionChar (Maybe ExtensionChar)
  | -- | an empty private use tag or section was encountered
    EmptyPrivateUse
  | -- | the subtag was not well-formed for the position at which it was
    -- encountered
    ImproperSubtag Subtag
  | -- | there was no subtag after an initial @i@ subtag
    EmptyStartI
  | -- | a subtag was encountered after an irregular grandfathered tag
    SubtagAfterIrreg Subtag Grandfathered
  deriving (Eq, Ord, Show)

-- | Parse the start of a BCP47 tag and return a 'PartialBCP47' result. This
-- will fail if the given 'Subtag' cannot start a tag, which happens if it is
-- not
--
-- * a primary language subtag (two to eight ASCII letters),
--
-- * the subtag @x@, for a private use tag, or
--
-- * the subtag @i@, for irregular grandfathered tags beginning with that tag.
startBCP47 :: Subtag -> Either StepError PartialBCP47
startBCP47 st
  | containsDigit st = Left $ StepError Nothing AtBeginning $ ImproperSubtag st
  | st == subtagI = Right PartialStartI
  | st == subtagX = Right PartialStartPrivateUse
  | subtagLength st >= 4 = Right $ PartialPrimaryLong initcon
  | otherwise = Right $ PartialPrimaryShort initcon
  where
    initcon = initNormal st

-- | Add another 'Subtag' to the end of a partially-parsed BCP47 tag, returning
-- a new 'PartialBCP47' if the subtag can actually occur at that position, and
-- otherwise throwing a 'StepError'

-- TODO: some lookups in here implemented as case statements - could be more
-- efficient?
stepBCP47 :: Subtag -> PartialBCP47 -> Either StepError PartialBCP47
stepBCP47 st partbcp47 = case partbcp47 of
  PartialPrimaryShort n
    -- slightly delicate at the start, since all regular grandfathered tags (or
    -- a prefix of them) could show up here and need to be caught
    | isExtlang -> handleExtlangGrandfathered n
    | isScript -> Right $ PartialScript n {script = justSubtag st}
    | isRegion -> Right $ PartialRegion n {region = justSubtag st}
    | isVariant -> handleVariantGrandfathered n
    | otherwise -> handleSingleton AtPrimaryShort n id
  PartialExtlang1 n ->
    (asExt2 n <|> asScr n <|> asReg n <|> asVar n id)
      `andOtherwise` handleSingleton AtExtlang1 n id
  PartialExtlang2 n ->
    (asExt3 n <|> asScr n <|> asReg n <|> asVar n id)
      `andOtherwise` handleSingleton AtExtlang2 n id
  PartialExtlang3 n ->
    (asScr n <|> asReg n <|> asVar n id)
      `andOtherwise` handleSingleton AtExtlang3 n id
  PartialPrimaryLong n ->
    (asScr n <|> asReg n <|> asVar n id)
      `andOtherwise` handleSingleton AtPrimaryLong n id
  PartialScript n ->
    (asReg n <|> asVar n id)
      `andOtherwise` handleSingleton AtScript n id
  PartialRegion n ->
    -- the last grandfathered tags are detected here (the four irregular
    -- grandfathered tags that look like normal tags at the start)
    asVar n id `andOtherwise` handleSingleton AtRegion n id `orElse` detectLateIrregular n
  PartialVariant n f ->
    asVar n f `andOtherwise` handleSingleton AtVariant (n {variants = f []}) id
  PartialStartExtension n exts c
    | isExtensionSubtag -> Right $ PartialExtension n exts c (st :|)
    | otherwise ->
      Left $
        StepError (Just $ NormalTag n {extensions = exts []}) AtStartExtension $
          EmptyExtensionSection
            c
            (Just errC)
    where
      errC = unsafeSubtagCharToExtension $ subtagHead st
  PartialExtension n exts c extsubs
    | isExtensionSubtag -> Right $ PartialExtension n exts c (extsubs . (st :))
    -- Avoiding a second subtag length check
    | st == subtagX -> Right $ PartialStartPrivateUseSection n {extensions = extsEnd []}
    | otherwise -> Right $ PartialStartExtension n extsEnd c'
    where
      extsEnd = exts . (Extension c (extsubs []) :)
      c' = unsafeSubtagCharToExtension $ subtagHead st
  PartialStartPrivateUseSection n -> Right $ PartialPrivateUseSection n (st :)
  PartialPrivateUseSection n f -> Right $ PartialPrivateUseSection n (f . (st :))
  PartialStartI -> case unwrapSubtag st of
    14102819922971197443 -> Right $ PartialGrandfathered IAmi
    14248104991419006979 -> Right $ PartialGrandfathered IBnn
    14526138628724883463 -> Right $ PartialGrandfathered IDefault
    14680466211245977096 -> Right $ PartialGrandfathered IEnochian
    15098133032806121475 -> Right $ PartialGrandfathered IHak
    15542853518732230663 -> Right $ PartialGrandfathered IKlingon
    15697226132455686147 -> Right $ PartialGrandfathered ILux
    15827749698417983493 -> Right $ PartialGrandfathered IMingo
    15962927641447628806 -> Right $ PartialGrandfathered INavajo
    16275850723642572803 -> Right $ PartialGrandfathered IPwn
    16827550474088480771 -> Right $ PartialGrandfathered ITao
    16827638435018702851 -> Right $ PartialGrandfathered ITay
    16847869448969781251 -> Right $ PartialGrandfathered ITsu
    _ -> Left $ StepError Nothing AtStartI $ ImproperSubtag st
  PartialGrandfathered g -> case g of
    -- TODO - might want to make this its own function so I can have an
    -- onRegularGrandfathered :: (Normal -> r) -> r -> Grandfathered -> r or
    -- something like that (useful, e.g., if we want to write a BCP47 -> Maybe
    -- Normal function that ignores private use and irregular grandfathered
    -- tags)

    -- the regular grandfathered tags can all potentially have subtags appended
    -- to them (turning them into normal tags), and in addition the tag "zh-min"
    -- can have the "nan" subtag appended to it (turning it into the ZhMinNan
    -- grandfathered tag). the irregular grandfathered tags, however, cannot
    -- have any subsequent subtags.
    ArtLojban -> stepVariant 14108546179528654851 15690354374758891526
    CelGaulish -> stepVariant 14382069488147234819 14954113284221173767
    EnGbOed -> err
    IAmi -> err
    IBnn -> err
    IDefault -> err
    IEnochian -> err
    IHak -> err
    IKlingon -> err
    ILux -> err
    IMingo -> err
    INavajo -> err
    IPwn -> err
    ITao -> err
    ITay -> err
    ITsu -> err
    NoBok -> stepExt 15977645578003677186 14249204503046782979
    NoNyn -> stepExt 15977645578003677186 15989872147304546307
    SgnBeFr -> err
    SgnBeNl -> err
    SgnChDe -> err
    ZhGuoyu -> stepVariant 17699146535566049282 14976579405109788677
    ZhHakka -> stepVariant 17699146535566049282 15098140437866610693
    ZhMin
      | unwrapSubtag st == 15962850549540323331 -> Right $ PartialGrandfathered ZhMinNan
      | otherwise -> stepExt 17699146535566049282 15827742560719208451
    ZhMinNan ->
      fixErr $
        stepBCP47 st $
          PartialExtlang2 $
            (initNormal (Subtag 17699146535566049282))
              { extlang1 = justSubtag $ Subtag 15827742560719208451,
                extlang2 = justSubtag $ Subtag 15962850549540323331
              }
    ZhXiang -> stepVariant 17699146535566049282 17412902894784479237
    where
      err =
        Left $
          StepError (Just $ GrandfatheredTag g) AtIrregGrandfathered $
            SubtagAfterIrreg st g
      -- if there is an error in stepping then we would like to have a more
      -- accurate tag in the errors
      fixErr x = case x of
        Left (StepError _ loc e) -> Left $ StepError (Just $ GrandfatheredTag g) loc e
        Right a -> Right a
      stepExt prim ext =
        fixErr $
          stepBCP47 st $
            PartialExtlang1 $
              (initNormal (Subtag prim)) {extlang1 = justSubtag $ Subtag ext}
      stepVariant prim var =
        fixErr $ stepBCP47 st $ PartialVariant (initNormal $ Subtag prim) (Subtag var :)
  PartialStartPrivateUse -> Right $ PartialPrivateUse (st :|)
  PartialPrivateUse f -> Right $ PartialPrivateUse (f . (st :))
  where
    -- tests for the subtags in a normal tag (other than the primary language
    -- subtag and the singletons)

    -- TODO: could factor these tests a bit
    isExtlang = containsOnlyLetters st && subtagLength st == 3
    isScript = containsOnlyLetters st && subtagLength st == 4
    isRegion =
      containsOnlyLetters st && subtagLength st == 2
        || containsOnlyDigits st && subtagLength st == 3
    isVariant =
      subtagLength st >= 5
        || subtagLength st == 4 && subtagHeadIsDigit st
    isExtensionSubtag = subtagLength st >= 2

    asExt2 n = guard isExtlang $> PartialExtlang2 n {extlang2 = justSubtag st}
    asExt3 n = guard isExtlang $> PartialExtlang3 n {extlang3 = justSubtag st}
    asScr n = guard isScript $> PartialScript n {script = justSubtag st}
    asReg n = guard isRegion $> PartialRegion n {region = justSubtag st}
    asVar n f = guard isVariant $> PartialVariant n (f . (st :))

    andOtherwise (Just x) _ = Right x
    andOtherwise Nothing e = e

    -- handle a singleton given an accumulated list of extension sections

    handleSingleton loc n acc
      | subtagLength st /= 1 =
        Left $
          StepError (Just $ NormalTag nEnd) loc $
            ImproperSubtag st
      | st == subtagX = Right $ PartialStartPrivateUseSection nEnd
      | otherwise =
        Right $
          PartialStartExtension
            n
            acc
            (unsafeSubtagCharToExtension $ subtagHead st)
      where
        nEnd = n {extensions = acc []}

    -- recognizing the regular grandfathered tags

    recognizeOn stGot stExpect res fallback
      | unwrapSubtag stGot == stExpect = Right $ PartialGrandfathered res
      | otherwise = Right fallback
    handleExtlangGrandfathered n = case unwrapSubtag st of
      14249204503046782979 ->
        recognizeOn (primlang n) 15977645578003677186 NoBok n'
      15989872147304546307 ->
        recognizeOn (primlang n) 15977645578003677186 NoNyn n'
      15827742560719208451 ->
        recognizeOn (primlang n) 17699146535566049282 ZhMin n'
      _ -> Right n'
      where
        n' = PartialExtlang1 n {extlang1 = justSubtag st}
    handleVariantGrandfathered n = case unwrapSubtag st of
      15690354374758891526 ->
        recognizeOn (primlang n) 14108546179528654851 ArtLojban n'
      14954113284221173767 ->
        recognizeOn (primlang n) 14382069488147234819 CelGaulish n'
      14976579405109788677 ->
        recognizeOn (primlang n) 17699146535566049282 ZhGuoyu n'
      15098140437866610693 ->
        recognizeOn (primlang n) 17699146535566049282 ZhHakka n'
      17412902894784479237 ->
        recognizeOn (primlang n) 17699146535566049282 ZhXiang n'
      _ -> Right n'
      where
        n' = PartialVariant n (st :)

    -- recognizing the four irregular grandfathered tags not starting with 'i'

    orElse (Right x) _ = Right x
    orElse (Left _) x = x
    -- doesn't test for emptiness of the unimportant subtags - that's done in
    -- detectLateIrregular
    isLateIrregular n lang reg st' =
      primlang n == Subtag lang
        && region n == justSubtag (Subtag reg)
        && st == Subtag st'
    -- we don't need to check for the emptiness of variants, extensions, or the
    -- private use section, since this function is only relevant exactly at
    -- AtRegion
    detectLateIrregular n
      | extlang1 n /= nullSubtag
          || extlang2 n /= nullSubtag
          || extlang3 n /= nullSubtag
          || script n /= nullSubtag =
        err
      | isLateIrregular n 14679482985414131714 14954202562683731970 16111381376313327619 =
        Right $ PartialGrandfathered EnGbOed
      | isLateIrregular n 16690181889360658435 14237004322024980482 14828101773117358082 =
        Right $ PartialGrandfathered SgnBeFr
      | isLateIrregular n 16690181889360658435 14237004322024980482 15974267878283149314 =
        Right $ PartialGrandfathered SgnBeNl
      | isLateIrregular n 16690181889360658435 14384497209821364226 14525234698176692226 =
        Right $ PartialGrandfathered SgnChDe
      | otherwise = err
      where
        err = Left $ StepError (Just $ NormalTag n) AtRegion $ ImproperSubtag st

-- | Attempt to interpret a partially-parsed BCP47 tag as a full tag. This
-- function fails when the standard requires that at least one more 'Subtag' be
-- present in a tag, which happens when the 'PartialBCP47' result has reached
-- one of:
--
-- * the start of an extension section;
--
-- * the start of the private use section or of a private use tag;
--
-- * an initial @i@ subtag, for irregular grandfathered tags beginning with that
--   subtag.
finalizeBCP47 :: PartialBCP47 -> Either StepError BCP47
finalizeBCP47 (PartialPrimaryShort n) = Right $ NormalTag n
finalizeBCP47 (PartialExtlang1 n) = Right $ NormalTag n
finalizeBCP47 (PartialExtlang2 n) = Right $ NormalTag n
finalizeBCP47 (PartialExtlang3 n) = Right $ NormalTag n
finalizeBCP47 (PartialPrimaryLong n) = Right $ NormalTag n
finalizeBCP47 (PartialScript n) = Right $ NormalTag n
finalizeBCP47 (PartialRegion n) = Right $ NormalTag n
finalizeBCP47 (PartialVariant n f) = Right $ NormalTag $ n {variants = f []}
finalizeBCP47 (PartialStartExtension n exts c) =
  Left $
    StepError (Just $ NormalTag n') AtStartExtension $
      EmptyExtensionSection c Nothing
  where
    n' = n {extensions = exts []}
finalizeBCP47 (PartialExtension n exts c exttags) =
  Right $ NormalTag $ n {extensions = exts $ [Extension c $ exttags []]}
finalizeBCP47 (PartialStartPrivateUseSection n) =
  Left $ StepError (Just $ NormalTag n) AtStartPrivateUse EmptyPrivateUse
finalizeBCP47 (PartialPrivateUseSection n f) =
  Right $ NormalTag $ n {privateUse = f []}
finalizeBCP47 PartialStartI = Left $ StepError Nothing AtStartI EmptyStartI
finalizeBCP47 (PartialGrandfathered g) = Right $ GrandfatheredTag g
finalizeBCP47 PartialStartPrivateUse =
  Left $ StepError Nothing AtStartPrivateUse EmptyPrivateUse
finalizeBCP47 (PartialPrivateUse f) = Right $ PrivateUse $ f []

-- | Helper function that (unsafely!) constructs a 'Normal' tag with the given
-- 'Subtag' as its primary language subtag
initNormal :: Subtag -> Normal
initNormal st = Normal st nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [] [] []

-- | An error that may occur when parsing a complete 'BCP47' tag. These errors
-- also include the offset of the start of the subtag at which the error
-- occurred, the portion of the tag parsed before the error (if possible), and
-- where in parsing the error occurred. The offset is the number of characters
-- that need to be dropped from the initial input in order to reach the position
-- of the error.
data PopError
  = -- | an error occurred while attempting to parse a subtag
    PopErrorSubtag Int (Maybe BCP47) AtComponent Sub.PopError
  | -- | an error not related to subtag parsing
    PopErrorStep Int (Maybe BCP47) AtComponent StepErrorType
  deriving (Eq, Ord, Show)

-- TODO: test the below! especially test that the stuff returned by the stream is

-- | Parse a 'BCP47' tag at the beginning of the stream, stopping at the end of
-- input or the first invalid character encountered. Also returns the total
-- length (according to the passed popping functions) of input that was consumed
-- and whatever input was not consumed. This function has the property that if
-- it parses a 'BCP47' tag successfully and the input is composed only of subtag
-- characters and dashes, then it will have consumed its entire input (i.e., the
-- returned unconsumed input will be empty).
--
-- Along with the 'PopError', this function will return any unconsumed input;
-- the error will have occurred exactly at the start of that returned value, so
-- in the case of a 'PopErrorStep' that will be the start of the subtag
-- currently being processed, and in the case of a 'PopErrorSubtag' that will be
-- somewhere in the ill-formed subtag.
popBCP47DetailWith ::
  -- | function to pop a 'SubtagChar' from @s@
  (s -> Maybe (SubtagChar, s)) ->
  -- | function to pop a dash separator from @s@
  (s -> Maybe s) ->
  -- | the input
  s ->
  -- | the tag, length of input, unconsumed input
  Either (PopError, s) (BCP47, Int, s)
popBCP47DetailWith popChar popSep initinp =
  popSubtag' 0 Nothing AtBeginning initinp >>= startParse
  where
    collapseLeft (Left _) = Nothing
    collapseLeft (Right a) = Just a
    popSubtag' !pos bcpForErr loc t = case popSubtagWith popChar t of
      Left (e, t') -> Left (PopErrorSubtag pos bcpForErr loc e, t')
      Right (st, t') -> Right (st, t, t')
    -- we pass in the input stream corresponding to both the start and right
    -- after the end of the subtag
    startParse (st, startt, endt) = case startBCP47 st of
      Left (StepError mtag loc e) -> Left (PopErrorStep 0 mtag loc e, startt)
      Right a -> step (subtagLength' st) endt a
    step !startpos t acc
      | Just t' <- popSep t = do
        let bcp47ForErr = collapseLeft $ finalizeBCP47 acc
        let startpos' = startpos + 1
        (st, startt, endt) <- popSubtag' startpos' bcp47ForErr (whereInParsing acc) t'
        case stepBCP47 st acc of
          Left (StepError mtag loc e) -> Left (PopErrorStep startpos' mtag loc e, startt)
          Right acc' -> step (startpos' + subtagLength' st) endt acc'
      | otherwise = finalize startpos t acc
    finalize pos t tag = case finalizeBCP47 tag of
      Left (StepError mtag loc e) -> Left (PopErrorStep pos mtag loc e, t)
      Right a -> Right (a, pos, t)

-- | Parse a 'BCP47' tag at the beginning of the text stream. See also the
-- documentation for 'popBCP47DetailWith'.
popBCP47Detail :: Text -> Either (PopError, Text) (BCP47, Int, Text)
popBCP47Detail = popBCP47DetailWith popChar popSep
  where
    popChar t = do
      (c, t') <- T.uncons t
      w <- packChar c
      pure (w, t')
    popSep t
      | Just ('-', t') <- T.uncons t =
        Just t'
      | otherwise = Nothing

-- | Tests whether or not the given 'Char' can occur in a well-formed tag. This
-- is true exactly when the character is a dash @\'-\'@ or an ASCII alphanumeric
-- character.
isTagChar :: Char -> Bool
isTagChar c = isSubtagChar c || c == '-'

-- | Tests whether or not the give byte can occur in a well-formed, UTF-8
-- encoded tag. This is true exactly for the bytes @[45] <> [48..57] <> [65..90]
-- <> [97..122]@. See also 'isTagChar'.
isTagByte :: Word8 -> Bool
isTagByte w = isSubtagByte w || w == 45

-- | Parse a 'BCP47' tag from a non-empty list of subtags

-- TODO: can be made more efficient
parseBCP47FromSubtags :: NonEmpty Subtag -> Either PopError BCP47
parseBCP47FromSubtags =
  go . popBCP47Detail . T.intercalate "-" . NE.toList . fmap renderSubtagLower
  where
    go (Left (x, _)) = Left x
    go (Right (x, _, _)) = Right x

-- | An error that may occur while parsing a 'BCP47' tag taking up the entire
-- input stream
data ParseError c
  = ParseErrorPop PopError
  | ParseErrorInvalidChar Int (Maybe BCP47) c
  deriving (Eq, Ord, Show)

-- | Parse a 'BCP47' tag that takes up the entire input stream.
parseBCP47 :: Text -> Either (ParseError Char) BCP47
parseBCP47 = parseBCP47With T.uncons packChar popSep
  where
    popSep t = do
      (c, t') <- T.uncons t
      guard $ c == '-'
      pure t'

-- | Parse a 'BCP47' tag that takes up the entire input stream using
-- 'popBCP47DetailWith'
parseBCP47With ::
  -- | pop a character from the input stream
  (s -> Maybe (c, s)) ->
  -- | parse that character
  (c -> Maybe SubtagChar) ->
  -- | pop a dash separator from the input stream
  (s -> Maybe s) ->
  -- | the input stream
  s ->
  Either (ParseError c) BCP47
parseBCP47With unc toChar popSep t = case popBCP47DetailWith popChar popSep t of
  Left (e, t')
    | Just (off, mbcp) <- getDetail e,
      Just (c, _) <- unc t' ->
      Left $ ParseErrorInvalidChar off mbcp c
  Left (e, _) -> Left $ ParseErrorPop e
  Right (bcp, len, t') -> case unc t' of
    Nothing -> Right bcp
    Just (c, _) -> Left $ ParseErrorInvalidChar len (Just bcp) c
  where
    popChar s = do
      (c, s') <- unc s
      w <- toChar c
      pure (w, s')
    -- inelegant, but we want to catch all of the "empty <something>" errors
    -- that don't come at the very end of the stream, since these should all
    -- become invalid character errors
    getDetail (PopErrorSubtag x y _ Sub.PopEmptySubtag) = Just (x, y)
    getDetail (PopErrorStep x y _ EmptyExtensionSection {}) = Just (x, y)
    getDetail (PopErrorStep x y _ EmptyPrivateUse) = Just (x, y)
    getDetail (PopErrorStep x y _ EmptyStartI) = Just (x, y)
    getDetail (PopErrorSubtag _ _ _ Sub.PopSubtagTooLong {}) = Nothing
    getDetail (PopErrorStep _ _ _ ImproperSubtag {}) = Nothing
    getDetail (PopErrorStep _ _ _ SubtagAfterIrreg {}) = Nothing

-- $valueconstruction
--
-- Other than the 'parseBCP47' function, tags can also be constructed
-- directly using 'grandfatheredSyntax', since all of the
-- 'Grandfathered' tags are automatically well-formed. These tags are
-- the result of a prior standard allowing the registration of entire
-- tags, not simply subtags; of those tags, the ones that could not be
-- represented via registered subtags were explicitly grandfathered
-- into the current standard via the grammar of the tags itself. All
-- of them are valid, but most are deprecated.
--
-- The 'LanguageTag.BCP47.Quasi.syntag' quasi-quoter is also
-- available to construct compile-time-checked tags.

----------------------------------------------------------------
-- Rendering to subtags
----------------------------------------------------------------

-- | Convert a 'BCP47' tag into its component subtags
toSubtags :: BCP47 -> NonEmpty Subtag
toSubtags (NormalTag (Normal p e1 e2 e3 s r vs es ps)) =
  p :| (mapMaybe go [e1, e2, e3, s, r] <> vs <> es' <> ps')
  where
    go = maybeSubtag Nothing Just
    es' = flip concatMap es $ \(Extension c ts) ->
      extensionCharToSubtag c : NE.toList ts
    ps'
      | null ps = []
      | otherwise = subtagX : ps
toSubtags (PrivateUse x) = subtagX :| NE.toList x
toSubtags (GrandfatheredTag g) = grandfatheredToSubtags g

----------------------------------------------------------------
-- Tag constants
----------------------------------------------------------------

subtagI :: Subtag
subtagI = Subtag 15132094747964866561

----------------------------------------------------------------
-- Error handling
----------------------------------------------------------------

-- | The possible syntactic categories that a 'Subtag' within a
-- 'BCP47' tag may fall under.
data SubtagCategory
  = -- | primary language subtag
    PrimaryLanguage
  | -- | extended language subtag
    ExtendedLanguage
  | -- | script subtag
    Script
  | -- | region subtag
    Region
  | -- | variant subtag
    Variant
  | -- | singleton subtag (start of an extension or private use
    -- section)
    Singleton
  | -- | extension section subtag
    ExtensionSubtag
  | -- | singleton subtag @x@ or @X@
    PrivateUseSingleton
  | -- | private use section subtag
    PrivateUseSubtag
  | -- | singleton subtag @i@ or @I@ at the beginning of a tag
    GrandfatheredIStart
  | -- | one of the subtags that can follow an initial @i@ in a
    -- 'Grandfathered' tag
    GrandfatheredIFollower
  | -- | no more subtags - this is only explicitly expected after a complete
    -- irregular grandfathered tag and is otherwise left implicit
    EndOfTag
  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData SubtagCategory where
  rnf = rwhnf

-- | A description of what was just parsed for each 'AtComponent'
--
-- >>> atComponentDescription AtPrimaryShort
-- "short primary language subtag"
-- >>> atComponentDescription AtPrivateUse
-- "private use subtag"
atComponentDescription :: AtComponent -> Text
atComponentDescription AtBeginning = "beginning of the tag"
atComponentDescription AtPrimaryShort = "short primary language subtag"
atComponentDescription AtExtlang1 = "first extended language subtag"
atComponentDescription AtExtlang2 = "second extended language subtag"
atComponentDescription AtExtlang3 = "third extended language subtag"
atComponentDescription AtPrimaryLong = "long primary language subtag"
atComponentDescription AtScript = "script subtag"
atComponentDescription AtRegion = "region subtag"
atComponentDescription AtVariant = "variant subtag"
atComponentDescription AtStartExtension = "extension singleton subtag"
atComponentDescription AtExtension = "extension subtag"
atComponentDescription AtStartPrivateUse = "start of private use section"
atComponentDescription AtPrivateUse = "private use subtag"
atComponentDescription AtStartI = "initial \"i\" subtag"
atComponentDescription AtIrregGrandfathered = "end of irregular grandfathered tag"

-- | The categories of input that are expected at any position within a tag, for
-- use with 'ImproperSubtag' errors
--
-- >>> expectedCategories AtPrimaryShort
-- ExtendedLanguage :| [Script, Region, Variant, Singleton]
-- >>> expectedCategories AtPrivateUse
-- PrivateUseSubtag :| []
-- >>> expectedCategories AtIrregGrandfathered
-- EndOfTag :| []
expectedCategories :: AtComponent -> NonEmpty SubtagCategory
expectedCategories AtBeginning =
  PrimaryLanguage :| [GrandfatheredIStart, PrivateUseSingleton]
expectedCategories AtPrimaryShort =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories AtExtlang1 =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories AtExtlang2 =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories AtExtlang3 =
  Script :| [Region, Variant, Singleton]
expectedCategories AtPrimaryLong =
  Script :| [Region, Variant, Singleton]
expectedCategories AtScript =
  Region :| [Variant, Singleton]
expectedCategories AtRegion =
  Variant :| [Singleton]
expectedCategories AtVariant =
  Variant :| [Singleton]
expectedCategories AtStartExtension =
  ExtensionSubtag :| []
expectedCategories AtExtension =
  ExtensionSubtag :| [Singleton]
expectedCategories AtStartPrivateUse = PrivateUseSubtag :| []
expectedCategories AtPrivateUse = PrivateUseSubtag :| []
expectedCategories AtStartI = GrandfatheredIFollower :| []
expectedCategories AtIrregGrandfathered = EndOfTag :| []

-- | The names of each 'SubtagCategory', for use as an expectation.
--
-- >>> subtagCategoryName PrimaryLanguage
-- "primary language subtag"
-- >>> subtagCategoryName PrivateUseSingleton
-- "start of private use section"
subtagCategoryName :: SubtagCategory -> Text
subtagCategoryName PrimaryLanguage = "primary language subtag"
subtagCategoryName ExtendedLanguage = "extended language subtag"
subtagCategoryName Script = "script subtag"
subtagCategoryName Region = "region subtag"
subtagCategoryName Variant = "variant subtag"
subtagCategoryName Singleton = "start of extension or private use section"
subtagCategoryName ExtensionSubtag = "extension subtag"
subtagCategoryName PrivateUseSingleton = "start of private use section"
subtagCategoryName PrivateUseSubtag = "private use subtag"
subtagCategoryName GrandfatheredIStart = "start of irregular grandfathered tag"
subtagCategoryName GrandfatheredIFollower = "end of irregular grandfathered tag"
subtagCategoryName EndOfTag = "end of tag"

-- | A brief description of the syntax of each 'SubtagCategory'.
--
-- >>> subtagCategorySyntax PrimaryLanguage
-- "two to eight letters"
-- >>> subtagCategorySyntax PrivateUseSingleton
-- "x or X"
subtagCategorySyntax :: SubtagCategory -> Text
subtagCategorySyntax PrimaryLanguage = "two to eight letters"
subtagCategorySyntax ExtendedLanguage = "three letters"
subtagCategorySyntax Script = "four letters"
subtagCategorySyntax Region = "two letters or three digits"
subtagCategorySyntax Variant = "four to eight letters or characters (starting with a digit if four)"
subtagCategorySyntax Singleton = "one letter or digit"
subtagCategorySyntax ExtensionSubtag = "two to eight letters or digits"
subtagCategorySyntax PrivateUseSingleton = "x or X"
subtagCategorySyntax PrivateUseSubtag = "any subtag"
subtagCategorySyntax GrandfatheredIStart = "i or I"
subtagCategorySyntax GrandfatheredIFollower =
  "ami, bnn, default, enochian, hak, klingon, lux, mingo, navajo, pwn, tao, tay, or tsu"
subtagCategorySyntax EndOfTag = "end of tag"
