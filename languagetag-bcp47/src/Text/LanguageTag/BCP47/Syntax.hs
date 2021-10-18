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
-- represented by the 'Text.LanguageTag.BCP47.Registry.BCP47' type in
-- "Text.LanguageTag.BCP47.Registry", since the invariants required by
-- the 'BCP47' type in this module are difficult to guarantee
-- statically without the interface becoming very unwieldy. Valid tags
-- can be constructed from these tags with
-- 'Text.LanguageTag.BCP47.Registry.validateBCP47', or with
-- 'Text.LanguageTag.BCP47.Quasi.tag' and the related quasi-quoters in
-- that module. This module is provided for applications that only
-- need to verify well-formedness of tags, without analyzing them
-- further.
module Text.LanguageTag.BCP47.Syntax
  ( -- * Parsing and rendering tags
    BCP47,
    popBCP47Len,
    popBCP47LenWith,
    renderBCP47,
    renderBCP47Builder,
    toSubtags,

    -- * Constructing tags directly
    -- $valueconstruction
    grandfatheredSyntax,
    Grandfathered (..),

    -- * Errors

    --    SyntaxError (..),
    --    Pos,
    --    AtComponent (..),
    SubtagCategory (..),
    --    atComponentDescription,
    --    expectedCategories,
    subtagCategoryName,
    subtagCategorySyntax,

    -- * Temp partial exports
    StepError (..),
    StepErrorType (..),
    startBCP47,
    stepBCP47,
    finalizeBCP47,
    SyntaxError (..),
    isTagChar,
    AtComponent (..),
    parseBCP47FromSubtags,
    parseBCP47,
    CompleteSyntaxError (..),
    atComponentDescription',
    expectedCategories',
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
import Text.LanguageTag.BCP47.Registry.Grandfathered
import Text.LanguageTag.BCP47.Subtag hiding (PopError (..))
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import Text.LanguageTag.Internal.BCP47.Syntax

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
    ErrEmptyExtensionSection ExtensionChar (Maybe ExtensionChar)
  | -- | an empty private use tag or section was encountered
    ErrEmptyPrivateUse
  | -- | the subtag was not well-formed for the position at which it was
    -- encountered
    ErrImproperSubtag Subtag
  | -- | there was no subtag after an initial @i@ subtag
    ErrEmptyStartI
  | -- | a subtag was encountered after an irregular grandfathered tag
    ErrSubtagAfterIrreg Subtag Grandfathered
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
  | containsDigit st = Left $ StepError Nothing AtBeginning $ ErrImproperSubtag st
  | st == subtagI = Right PartialStartI
  | st == subtagX = Right PartialStartPrivateUse
  | subtagLength st >= 4 = Right $ PartialPrimaryLong initcon
  | otherwise = Right $ PartialPrimaryShort initcon
  where
    initcon = initNormal st

-- | Add another 'Subtag' to the end of a partially-parsed BCP47 tag, returning
-- a new 'PartialBCP47' if the subtag can actually occur at that position, and
-- otherwise throwing a 'StepError'
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
          ErrEmptyExtensionSection
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
    14102819922971197459 -> Right $ PartialGrandfathered IAmi
    14248104991419006995 -> Right $ PartialGrandfathered IBnn
    14526138628724883479 -> Right $ PartialGrandfathered IDefault
    14680466211245977112 -> Right $ PartialGrandfathered IEnochian
    15098133032806121491 -> Right $ PartialGrandfathered IHak
    15542853518732230679 -> Right $ PartialGrandfathered IKlingon
    15697226132455686163 -> Right $ PartialGrandfathered ILux
    15827749698417983509 -> Right $ PartialGrandfathered IMingo
    15962927641447628822 -> Right $ PartialGrandfathered INavajo
    16275850723642572819 -> Right $ PartialGrandfathered IPwn
    16827550474088480787 -> Right $ PartialGrandfathered ITao
    16827638435018702867 -> Right $ PartialGrandfathered ITay
    16847869448969781267 -> Right $ PartialGrandfathered ITsu
    _ -> Left $ StepError Nothing AtStartI $ ErrImproperSubtag st
  PartialGrandfathered g -> case g of
    -- the regular grandfathered tags can all potentially have subtags appended
    -- to them (turning them into normal tags), and in addition the tag "zh-min"
    -- can have the "nan" subtag appended to it (turning it into the ZhMinNan
    -- grandfathered tag). the irregular grandfathered tags, however, cannot
    -- have any subsequent subtags.
    ArtLojban -> stepVariant 14108546179528654867 15690354374758891542
    CelGaulish -> stepVariant 14382069488147234835 14954113284221173783
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
    NoBok -> stepExt 15977645578003677202 14249204503046782995
    NoNyn -> stepExt 15977645578003677202 15989872147304546323
    SgnBeFr -> err
    SgnBeNl -> err
    SgnChDe -> err
    ZhGuoyu -> stepVariant 17699146535566049298 14976579405109788693
    ZhHakka -> stepVariant 17699146535566049298 15098140437866610709
    ZhMin
      | unwrapSubtag st == 15962850549540323347 -> Right $ PartialGrandfathered ZhMinNan
      | otherwise -> stepExt 17699146535566049298 15827742560719208467
    ZhMinNan ->
      fixErr $
        stepBCP47 st $
          PartialExtlang2 $
            (initNormal (Subtag 17699146535566049298))
              { extlang1 = justSubtag $ Subtag 15827742560719208467,
                extlang2 = justSubtag $ Subtag 15962850549540323347
              }
    ZhXiang -> stepVariant 17699146535566049298 17412902894784479253
    where
      err =
        Left $
          StepError (Just $ GrandfatheredTag g) AtIrregGrandfathered $
            ErrSubtagAfterIrreg st g
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
            ErrImproperSubtag st
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
      14249204503046782995 ->
        recognizeOn (primlang n) 15977645578003677202 NoBok n'
      15989872147304546323 ->
        recognizeOn (primlang n) 15977645578003677202 NoNyn n'
      15827742560719208467 ->
        recognizeOn (primlang n) 17699146535566049298 ZhMin n'
      _ -> Right n'
      where
        n' = PartialExtlang1 n {extlang1 = justSubtag st}
    handleVariantGrandfathered n = case unwrapSubtag st of
      15690354374758891542 ->
        recognizeOn (primlang n) 14108546179528654867 ArtLojban n'
      14954113284221173783 ->
        recognizeOn (primlang n) 14382069488147234835 CelGaulish n'
      14976579405109788693 ->
        recognizeOn (primlang n) 17699146535566049298 ZhGuoyu n'
      15098140437866610709 ->
        recognizeOn (primlang n) 17699146535566049298 ZhHakka n'
      17412902894784479253 ->
        recognizeOn (primlang n) 17699146535566049298 ZhXiang n'
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
      | isLateIrregular n 14679482985414131730 14954202562683731986 16111381376313327635 =
        Right $ PartialGrandfathered EnGbOed
      | isLateIrregular n 16690181889360658451 14237004322024980498 14828101773117358098 =
        Right $ PartialGrandfathered SgnBeFr
      | isLateIrregular n 16690181889360658451 14237004322024980498 15974267878283149330 =
        Right $ PartialGrandfathered SgnBeNl
      | isLateIrregular n 16690181889360658451 14384497209821364242 14525234698176692242 =
        Right $ PartialGrandfathered SgnChDe
      | otherwise = err
      where
        err = Left $ StepError (Just $ NormalTag n) AtRegion $ ErrImproperSubtag st

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
      ErrEmptyExtensionSection c Nothing
  where
    n' = n {extensions = exts []}
finalizeBCP47 (PartialExtension n exts c exttags) =
  Right $ NormalTag $ n {extensions = exts $ [Extension c $ exttags []]}
finalizeBCP47 (PartialStartPrivateUseSection n) =
  Left $ StepError (Just $ NormalTag n) AtStartPrivateUse ErrEmptyPrivateUse
finalizeBCP47 (PartialPrivateUseSection n f) =
  Right $ NormalTag $ n {privateUse = f []}
finalizeBCP47 PartialStartI = Left $ StepError Nothing AtStartI ErrEmptyStartI
finalizeBCP47 (PartialGrandfathered g) = Right $ GrandfatheredTag g
finalizeBCP47 PartialStartPrivateUse =
  Left $ StepError Nothing AtStartPrivateUse ErrEmptyPrivateUse
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
data SyntaxError
  = -- | an error occurred while attempting to parse a subtag
    SyntaxErrorPop Int (Maybe BCP47) AtComponent Sub.PopError
  | -- | an error not related to subtag parsing
    SyntaxErrorStep Int (Maybe BCP47) AtComponent StepErrorType
  deriving (Eq, Ord, Show)

-- | Parse a 'BCP47' tag at the beginning of the stream, stopping at the end of
-- input or the first invalid character encountered. Also returns the total
-- length (according to the passed popping functions) of input that was consumed
-- and whatever input was not consumed. This function has the property that if
-- it parses a 'BCP47' tag successfully and the input is composed only of subtag
-- characters and dashes, then it will have consumed its entire input (i.e., the
-- returned unconsumed input will be empty).
popBCP47LenWith ::
  -- | function to pop a 'SubtagChar' from @s@
  (s -> Maybe (SubtagChar, s)) ->
  -- | function to pop a dash separator from @s@
  (s -> Maybe s) ->
  -- | the input
  s ->
  -- | the tag, length of input, unconsumed input
  Either SyntaxError (BCP47, Int, s)
popBCP47LenWith popChar popSep initinp =
  popSubtag' 0 Nothing AtBeginning initinp >>= startParse
  where
    collapseLeft (Left _) = Nothing
    collapseLeft (Right a) = Just a
    popSubtag' !pos bcpForErr loc t = case popSubtagWith popChar t of
      Left e -> Left $ SyntaxErrorPop pos bcpForErr loc e
      Right a -> Right a
    startParse (st, t) = case startBCP47 st of
      Left (StepError mtag loc e) -> Left $ SyntaxErrorStep 0 mtag loc e
      Right a -> step (subtagLength' st) t a
    step !startpos t acc
      | Just t' <- popSep t = do
        let bcp47ForErr = collapseLeft $ finalizeBCP47 acc
        let startpos' = startpos + 1
        (st, t'') <- popSubtag' startpos' bcp47ForErr (whereInParsing acc) t'
        case stepBCP47 st acc of
          Left (StepError mtag loc e) -> Left $ SyntaxErrorStep startpos' mtag loc e
          Right acc' -> step (startpos' + subtagLength' st) t'' acc'
      | otherwise = finalize startpos t acc
    finalize pos t tag = case finalizeBCP47 tag of
      Left (StepError mtag loc e) -> Left $ SyntaxErrorStep pos mtag loc e
      Right a -> Right (a, pos, t)

-- | Parse a 'BCP47' tag at the beginning of the text stream. See also the
-- documentation for 'popBCP47LenWith'.
popBCP47Len :: Text -> Either SyntaxError (BCP47, Int, Text)
popBCP47Len = popBCP47LenWith popChar popSep
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

-- TODO: can be made more efficient
parseBCP47FromSubtags :: NonEmpty Subtag -> Either SyntaxError BCP47
parseBCP47FromSubtags =
  fmap go . popBCP47Len . T.intercalate "-" . NE.toList . fmap renderSubtagLower
  where
    go (x, _, _) = x

-- TODO: document, maybe factor out the (Int, Maybe BCP47, AtComponent) into
-- its own "Located" type or something
data CompleteSyntaxError c
  = CompleteSyntaxError SyntaxError
  | InvalidCharacter Int (Maybe BCP47) AtComponent c
  deriving (Eq, Ord, Show)

-- TODO: test this
-- TODO: write a finalizeBCP47 that returns the category of the last thing
-- parsed, then use that to fix error locations
parseBCP47 :: Text -> Either (CompleteSyntaxError Char) BCP47
parseBCP47 t = case popBCP47Len t of
  Left e
    | Just (off, mbcp, loc) <- getDetail e,
      Just (c, _) <- T.uncons $ T.drop off t ->
      Left $ InvalidCharacter off mbcp loc c
  Left e -> Left (CompleteSyntaxError e)
  Right (bcp, len, t') -> case T.uncons t' of
    Nothing -> Right bcp
    Just (c, _) -> Left $ InvalidCharacter len (Just bcp) AtBeginning c
  where
    -- inelegant, but we want to catch all of the "empty <something>" errors
    -- that don't come at the very end of the stream, since these should all
    -- become invalid character errors
    getDetail (SyntaxErrorPop x y z Sub.PopEmptySubtag) = Just (x, y, z)
    getDetail (SyntaxErrorStep x y z ErrEmptyExtensionSection {}) = Just (x, y, z)
    getDetail (SyntaxErrorStep x y z ErrEmptyPrivateUse) = Just (x, y, z)
    getDetail (SyntaxErrorStep x y z ErrEmptyStartI) = Just (x, y, z)
    getDetail (SyntaxErrorPop _ _ _ Sub.PopSubtagTooLong {}) = Nothing
    getDetail (SyntaxErrorStep _ _ _ ErrImproperSubtag {}) = Nothing
    getDetail (SyntaxErrorStep _ _ _ ErrSubtagAfterIrreg {}) = Nothing

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
-- The 'Text.LanguageTag.BCP47.Quasi.syntag' quasi-quoter is also
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
subtagI = Subtag 15132094747964866577

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

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

-- TODO: salvage documentation from the various block comments, then delete

{-
-- | The parser's current location in the tag, named after the most
-- recent component that was successfully parsed, more or less. What
-- it really indicates is what subtags the parser expects to occur
-- next: being 'AtPrimaryShort', for instance, means that we just
-- parsed a short primary language subtag and so the next tag
-- component we expect to see is anything from an extended language
-- subtag to the start of the private use section or end of input.
--
-- The positions corresponding to a singleton subtag (the start of an
-- extension or private use section) are not represented because only
-- an 'EmptySingleton' error can happen at those positions.
data AtComponent
  = -- | start of input
    AtBeginning
  | -- | primary language subtag of length at least four
    AtPrimaryLong
  | -- | primary language subtag of length at most three
    AtPrimaryShort
  | -- | first extended language subtag
    AtExtlang1
  | -- | second extended language subtag
    AtExtlang2
  | -- | third extended language subtag
    AtExtlang3
  | -- | script subtag
    AtScript
  | -- | region subtag
    AtRegion
  | -- | variant subtag
    AtVariant
  | -- | extension subtag
    AtExtension
  | -- | private use subtag
    AtPrivateUse
  | -- | subtag right after an initial @i-@
    AtIrregI
  deriving (Eq, Ord, Show)

instance NFData AtComponent where
  rnf = rwhnf

-- | A possible syntax error that may occur. The 'Pos' in the errors
-- indicates where the error occurred, which is:
--
-- * for 'UnparsableSubtag', the start of the ill-formed 'Subtag',
--   and, if applicable, the position of the invalid character inside
--   that subtag
--
-- * for 'BadSubtag', the start of the inappropriate 'Subtag'
--
-- * for 'EmptySubtag', the position of the second @\'-\'@
--
-- * for 'EmptySingleton', the start of the empty extension or private
--   use section that was encountered
--
-- The errors also include the initial segment of the tag that was
-- parsed before the error occurred whenever possible.
data SyntaxError
  = -- | encountered text that could not be parsed as a 'Subtag' (too
    -- long, or encountered a bad 'Char')
    UnparsableSubtag Pos AtComponent (Maybe (Pos, Char)) (Maybe BCP47)
  | -- | encountered a 'Subtag' that was ill-formed for its location
    BadSubtag Pos AtComponent Subtag (Maybe BCP47)
  | -- | input was empty
    EmptyInput
  | -- | an empty subtag was found
    EmptySubtag Pos AtComponent (Maybe BCP47)
  | -- | a trailing @\'-\'@ terminator was found after a well-formed
    -- tag
    TrailingTerminator BCP47
  | -- | an empty extension (@Just extchar@) or private use
    -- section (@Nothing@) was encountered
    EmptySingleton Pos (Maybe ExtensionChar) (Maybe BCP47)
  | -- | an irregular grandfathered tag was encountered that had more
    -- input after it
    IrregNum Grandfathered
  | -- | no tag was found after an initial @i-@
    EmptyIrregI
  deriving (Eq, Ord, Show)

instance NFData SyntaxError where
  rnf (UnparsableSubtag x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w
  rnf (BadSubtag x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w
  rnf (TrailingTerminator x) = rnf x
  rnf EmptyInput = ()
  rnf (EmptySubtag x y z) = rnf x `seq` rnf y `seq` rnf z
  rnf (EmptySingleton x y z) = rnf x `seq` rnf y `seq` rnf z
  rnf EmptyIrregI = ()
  rnf (IrregNum x) = rnf x

-- | A position in the input 'Text' stream
type Pos = Int
-}

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

{-
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
atComponentDescription AtExtension = "extension subtag"
atComponentDescription AtPrivateUse = "private use subtag"
atComponentDescription AtIrregI = "grandfathered \"i\" subtag"
-}

-- TODO: document
atComponentDescription' :: AtComponent -> Text
atComponentDescription' AtBeginning = "beginning of the tag"
atComponentDescription' AtPrimaryShort = "short primary language subtag"
atComponentDescription' AtExtlang1 = "first extended language subtag"
atComponentDescription' AtExtlang2 = "second extended language subtag"
atComponentDescription' AtExtlang3 = "third extended language subtag"
atComponentDescription' AtPrimaryLong = "long primary language subtag"
atComponentDescription' AtScript = "script subtag"
atComponentDescription' AtRegion = "region subtag"
atComponentDescription' AtVariant = "variant subtag"
atComponentDescription' AtStartExtension = "extension singleton subtag"
atComponentDescription' AtExtension = "extension subtag"
atComponentDescription' AtStartPrivateUse = "start of private use section"
atComponentDescription' AtPrivateUse = "private use subtag"
atComponentDescription' AtStartI = "initial \"i\" subtag"
atComponentDescription' AtIrregGrandfathered = "end of irregular grandfathered tag"

{-
-- | The categories of subtag that are expected at any position within
-- a tag, for use with 'BadSubtag' errors
--
-- >>> expectedCategories AtPrimaryShort
-- ExtendedLanguage :| [Script, Region, Variant, Singleton]
-- >>> expectedCategories AtPrivateUse
-- PrivateUseSubtag :| []
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
expectedCategories AtExtension =
  ExtensionSubtag :| [Singleton]
expectedCategories AtPrivateUse = PrivateUseSubtag :| []
expectedCategories AtIrregI = GrandfatheredIFollower :| []
-}

-- TODO: document
expectedCategories' :: AtComponent -> NonEmpty SubtagCategory
expectedCategories' AtBeginning =
  PrimaryLanguage :| [GrandfatheredIStart, PrivateUseSingleton]
expectedCategories' AtPrimaryShort =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories' AtExtlang1 =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories' AtExtlang2 =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories' AtExtlang3 =
  Script :| [Region, Variant, Singleton]
expectedCategories' AtPrimaryLong =
  Script :| [Region, Variant, Singleton]
expectedCategories' AtScript =
  Region :| [Variant, Singleton]
expectedCategories' AtRegion =
  Variant :| [Singleton]
expectedCategories' AtVariant =
  Variant :| [Singleton]
expectedCategories' AtStartExtension =
  ExtensionSubtag :| []
expectedCategories' AtExtension =
  ExtensionSubtag :| [Singleton]
expectedCategories' AtStartPrivateUse = PrivateUseSubtag :| []
expectedCategories' AtPrivateUse = PrivateUseSubtag :| []
expectedCategories' AtStartI = GrandfatheredIFollower :| []
expectedCategories' AtIrregGrandfathered = EndOfTag :| []

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
