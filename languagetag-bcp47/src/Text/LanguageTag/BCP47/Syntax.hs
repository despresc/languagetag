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
    parseBCP47,
    renderBCP47,
    renderBCP47Builder,
    toSubtags,
    parseBCP47FromSubtags,

    -- * Constructing tags directly
    -- $valueconstruction
    grandfatheredSyntax,
    Grandfathered (..),

    -- * Errors
    SyntaxError (..),
    Pos,
    AtComponent (..),
    SubtagCategory (..),
    atComponentDescription,
    expectedCategories,
    subtagCategoryName,
    subtagCategorySyntax,

    -- * Temp partial exports
    StepError (..),
    startBCP47,
    stepBCP47,
    finalizeBCP47,
    SyntaxError' (..),
    popBCP47Len,
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.LanguageTag.BCP47.Registry.Grandfathered
import Text.LanguageTag.BCP47.Subtag hiding (PopSubtagError (..))
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..), SubtagChar (..))
import Text.LanguageTag.Internal.BCP47.Syntax

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

-- | Parse a 'BCP47' tag from a list of subtags

-- Could be more efficient, of course
parseBCP47FromSubtags :: NonEmpty Subtag -> Either SyntaxError BCP47
parseBCP47FromSubtags = parseBCP47 . T.intercalate "-" . NE.toList . fmap renderSubtagLower

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Attempt to pop a subtag from the text stream, either returning
-- the given default value if the stream was empty, or passing the
-- subtag and remaining stream to the given continuation. Also handles
-- trailing terminator errors properly.
tagPop ::
  Text ->
  AtComponent ->
  Int ->
  Either SyntaxError BCP47 ->
  (Subtag -> Text -> Either SyntaxError BCP47) ->
  Either SyntaxError BCP47
tagPop inp atlast pos end more = case popSubtagText inp of
  -- TODO: this is temporarily written a little weirdly because of compatibility
  -- issues with the new parseSubtagText
  Left Sub.PopEmptySubtag -> case T.uncons inp of
    Just (c, _)
      | c == '-' -> Left $ EmptySubtag pos atlast mcon
      | otherwise -> Left $ UnparsableSubtag pos atlast (Just (pos, c)) mcon
    Nothing -> end
  Left Sub.PopSubtagTooLong {} -> Left $ UnparsableSubtag pos atlast Nothing mcon
  Right (st, t) -> case T.uncons t of
    Just (c, t')
      | c == '-' && T.null t' -> case more st "" of
        Left e -> Left e
        Right a -> Left $ TrailingTerminator a
      | c == '-' -> more st t'
      | otherwise -> Left $ UnparsableSubtag pos atlast (Just (pos + subtagLength' st, c)) mcon
    Nothing -> more st t
  where
    collapseLeft (Left _) = Nothing
    collapseLeft (Right a) = Just a
    mcon = collapseLeft end
{-# INLINE tagPop #-}

-- | Given the length, component type, and position of the previous
-- tag, either successfully pop a subtag from the stream and continue
-- parsing with the passed constructor, or return the finished
-- constructor, or return a subtag-parsing-related error.
mfinish ::
  Finishing a =>
  Word8 ->
  AtComponent ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> AtComponent -> Int -> Text -> Either SyntaxError BCP47) ->
  Either SyntaxError BCP47
mfinish !len !atlast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  let con' = finish con
  tagPop inp atlast pos' (Right con') $ \st t -> pr con st atlast pos' t
{-# INLINE mfinish #-}

isDigit :: SubtagChar -> Bool
isDigit c = w >= 48 && w <= 57
  where
    w = unwrapChar c

-- | Parse a BCP47 language tag
parseBCP47 :: Text -> Either SyntaxError BCP47
parseBCP47 inp =
  tagPop inp AtBeginning 0 (Left EmptyInput) $ \st t -> catchIrregulars $ parseBCP47' st t
  where
    -- FIXME: sufficient for the moment, but might want to be more
    -- efficient
    catchIrregulars (Right a) = Right a
    catchIrregulars (Left e) = testIrregs (T.toLower inp)
      where
        throwGrand t x
          | T.null t = Right $ GrandfatheredTag x
          | Just ('-', t') <- T.uncons t =
            if T.null t'
              then Left $ TrailingTerminator $ GrandfatheredTag x
              else Left $ IrregNum x
          | otherwise = Left e
        testIrregs t
          | Just x <- T.stripPrefix "en-gb-oed" t =
            throwGrand x EnGbOed
          | Just x <- T.stripPrefix "sgn-be-fr" t =
            throwGrand x SgnBeFr
          | Just x <- T.stripPrefix "sgn-be-nl" t =
            throwGrand x SgnBeNl
          | Just x <- T.stripPrefix "sgn-ch-de" t =
            throwGrand x SgnChDe
          | otherwise = Left e

parseBCP47' :: Subtag -> Text -> Either SyntaxError BCP47
parseBCP47' = parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- TODO: could be optimized a bit
    parsePrimary !st !t
      | containsDigit st = Left $ BadSubtag 0 AtBeginning st Nothing
      | subtagLength st == 1 =
        if subtagHead st == subtagCharx
          then parsePrivateTag t
          else parseIrregularI st t
      | subtagLength st >= 4 =
        mfinish
          (subtagLength st)
          AtPrimaryLong
          0
          t
          (initcon st nullSubtag nullSubtag nullSubtag)
          tryScript
      | otherwise = mfinish (subtagLength st) AtPrimaryShort 0 t (initcon st) (tryGrandPrimary st)

    tryGrandPrimary st0 con st1 atlast pos t =
      case (unwrapSubtag st0, unwrapSubtag st1) of
        (14108546179528654867, 15690354374758891542)
          | T.null t -> pure $ GrandfatheredTag ArtLojban
        (14382069488147234835, 14954113284221173783)
          | T.null t -> pure $ GrandfatheredTag CelGaulish
        (15977645578003677202, 14249204503046782995)
          | T.null t -> pure $ GrandfatheredTag NoBok
        (15977645578003677202, 15989872147304546323)
          | T.null t -> pure $ GrandfatheredTag NoNyn
        (17699146535566049298, 14976579405109788693)
          | T.null t -> pure $ GrandfatheredTag ZhGuoyu
        (17699146535566049298, 15098140437866610709)
          | T.null t -> pure $ GrandfatheredTag ZhHakka
        (17699146535566049298, 15827742560719208467) -> do
          let pos' = pos + fromIntegral (subtagLength st1) + 1
          tagPop t AtExtl1 pos' (Right $ GrandfatheredTag ZhMin) $ \st2 t' ->
            case unwrapSubtag st2 of
              15962850549540323347
                | T.null t' -> pure $ GrandfatheredTag ZhMinNan
              _ -> tryLext2 (con $ justSubtag st1) st2 AtExtl1 pos' t'
        (17699146535566049298, 17412902894784479253)
          | T.null t -> pure $ GrandfatheredTag ZhXiang
        _ -> tryLext1 con st1 atlast pos t

    tryLext1 !con st atlast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) AtExtl1 pos t (con $ justSubtag st) tryLext2
      | otherwise = tryScript (con nullSubtag nullSubtag nullSubtag) st atlast pos t

    tryLext2 !con st atlast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) AtExtl2 pos t (con $ justSubtag st) tryLext3
      | otherwise = tryScript (con nullSubtag nullSubtag) st atlast pos t

    tryLext3 !con st atlast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) AtExtl3 pos t (con $ justSubtag st) tryScript
      | otherwise = tryScript (con nullSubtag) st atlast pos t

    tryScript !con st atlast pos t
      | subtagLength st == 4 && containsOnlyLetters st =
        mfinish (subtagLength st) AtScript pos t (con $ justSubtag st) tryRegion
      | otherwise = tryRegion (con nullSubtag) st atlast pos t

    tryRegion !con st atlast pos t
      | subtagLength st == 2 =
        if containsDigit st
          then Left $ BadSubtag pos atlast st (Just $ finish con)
          else mfinish (subtagLength st) AtRegion pos t (con $ justSubtag st) tryVariant
      | subtagLength st == 3 =
        if containsLetter st
          then Left $ BadSubtag pos atlast st (Just $ finish con)
          else mfinish (subtagLength st) AtRegion pos t (con $ justSubtag st) tryVariant
      | otherwise = tryVariant (con nullSubtag) st atlast pos t

    tryVariant !con st atlast pos t
      | subtagLength st == 4 =
        if isDigit $ subtagHead st
          then mfinish (subtagLength st) AtVariant pos t (con . (st :)) tryVariant
          else Left $ BadSubtag pos atlast st (Just $ finish con)
      | subtagLength st >= 5 =
        mfinish (subtagLength st) AtVariant pos t (con . (st :)) tryVariant
      | otherwise = trySingleton (con []) st atlast pos t

    trySingleton con st atlast pos t
      | subtagLength st /= 1 = Left $ BadSubtag pos atlast st (Just $ finish con)
      | subtagHead st == subtagCharx =
        parsePrivateUse (con []) pos t
      | otherwise =
        let c = unsafeSubtagCharToExtension $ subtagHead st
         in parseExtension c con pos t

    parsePrivateUse con pos t = do
      let pos' = pos + 2
      let emptyerr = Left $ EmptySingleton pos Nothing $ Just $ finish con
      tagPop t AtPrivateUse pos emptyerr $ \st t' ->
        parsePrivateUseTag con st AtPrivateUse pos' t'

    parsePrivateUseTag con st _ pos t =
      mfinish (subtagLength st) AtPrivateUse pos t (con . (st :)) parsePrivateUseTag

    parseExtension c con pos t = tagPop t AtExtension pos' emptyerr go
      where
        emptyerr = Left $ EmptySingleton pos (Just c) $ Just $ finish con
        pos' = pos + 2
        con' ne = con . (Extension c ne :)
        go st t'
          | subtagLength st >= 2 =
            mfinish (subtagLength st) AtExtension pos' t' (con' . (st :|)) parseExtensionTag
          | otherwise = emptyerr

    parseExtensionTag con st _ pos t
      | subtagLength st == 1 = trySingleton (con []) st AtExtension pos t
      | otherwise = mfinish (subtagLength st) AtExtension pos t (con . (st :)) parseExtensionTag

    parseIrregularI st t
      | st /= subtagI = Left $ BadSubtag 0 AtBeginning st Nothing
      | otherwise = tagPop t AtIrregI 2 (Left EmptyIrregI) $ \st' t' ->
        case recognizeIrregI st' of
          Just g
            | T.null t' -> Right $ GrandfatheredTag g
            | otherwise -> Left $ IrregNum g
          Nothing -> Left $ BadSubtag 2 AtIrregI st' Nothing

    recognizeIrregI n = case unwrapSubtag n of
      14102819922971197459 -> Just IAmi
      14248104991419006995 -> Just IBnn
      14526138628724883479 -> Just IDefault
      14680466211245977112 -> Just IEnochian
      15098133032806121491 -> Just IHak
      15542853518732230679 -> Just IKlingon
      15697226132455686163 -> Just ILux
      15827749698417983509 -> Just IMingo
      15962927641447628822 -> Just INavajo
      16275850723642572819 -> Just IPwn
      16827550474088480787 -> Just ITao
      16827638435018702867 -> Just ITay
      16847869448969781267 -> Just ITsu
      _ -> Nothing

-- | Parse an entire private use tag
parsePrivateTag :: Text -> Either SyntaxError BCP47
parsePrivateTag inp = do
  tagPop inp AtPrivateUse 2 (Left $ EmptySingleton 0 Nothing Nothing) $ \st t ->
    parsePrivateUseTag (PrivateUse . (st :|)) (2 + fromIntegral (subtagLength st) + 1) t
  where
    parsePrivateUseTag con pos t =
      tagPop t AtPrivateUse pos (Right $ con []) $ \st t' ->
        parsePrivateUseTag (con . (st :)) (pos + fromIntegral (subtagLength st) + 1) t'

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
-- Tag constants
----------------------------------------------------------------

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 120

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
    AtExtl1
  | -- | second extended language subtag
    AtExtl2
  | -- | third extended language subtag
    AtExtl3
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
atComponentDescription AtExtl1 = "first extended language subtag"
atComponentDescription AtExtl2 = "second extended language subtag"
atComponentDescription AtExtl3 = "third extended language subtag"
atComponentDescription AtPrimaryLong = "long primary language subtag"
atComponentDescription AtScript = "script subtag"
atComponentDescription AtRegion = "region subtag"
atComponentDescription AtVariant = "variant subtag"
atComponentDescription AtExtension = "extension subtag"
atComponentDescription AtPrivateUse = "private use subtag"
atComponentDescription AtIrregI = "grandfathered \"i\" subtag"

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
expectedCategories AtExtl1 =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories AtExtl2 =
  ExtendedLanguage :| [Script, Region, Variant, Singleton]
expectedCategories AtExtl3 =
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
data StepError = StepError (Maybe BCP47) StepErrorType
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
    ErrImproperSubtag Subtag AtComponent
  | -- TODO review ones below
    ErrStartI
  | ErrAfterStartI
  | ErrStartContainsDigit
  | ErrTooManyAfterIrreg
  deriving (Eq, Ord, Show)

-- TODO: consider changing AtExtl1 et al to AtExtlang1

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
  | containsDigit st = Left $ StepError Nothing ErrStartContainsDigit
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
  PartialExtlang1 n
    | isExtlang -> Right $ PartialExtlang2 n {extlang2 = justSubtag st}
    | isScript -> Right $ PartialScript n {script = justSubtag st}
    | isRegion -> Right $ PartialRegion n {region = justSubtag st}
    | isVariant -> Right $ PartialVariant n (st :)
    | otherwise -> handleSingleton AtExtl1 n id
  PartialExtlang2 n
    | isExtlang -> Right $ PartialExtlang3 n {extlang3 = justSubtag st}
    | isScript -> Right $ PartialScript n {script = justSubtag st}
    | isRegion -> Right $ PartialRegion n {region = justSubtag st}
    | isVariant -> Right $ PartialVariant n (st :)
    | otherwise -> handleSingleton AtExtl2 n id
  PartialExtlang3 n
    | isScript -> Right $ PartialScript n {script = justSubtag st}
    | isRegion -> Right $ PartialRegion n {region = justSubtag st}
    | isVariant -> Right $ PartialVariant n (st :)
    | otherwise -> handleSingleton AtExtl3 n id
  PartialPrimaryLong n
    | isScript -> Right $ PartialScript n {script = justSubtag st}
    | isRegion -> Right $ PartialRegion n {region = justSubtag st}
    | isVariant -> Right $ PartialVariant n (st :)
    | otherwise -> handleSingleton AtPrimaryLong n id
  PartialScript n
    | isRegion -> Right $ PartialRegion n {region = justSubtag st}
    | isVariant -> Right $ PartialVariant n (st :)
    | otherwise -> handleSingleton AtScript n id
  PartialRegion n
    -- the last grandfathered tags are detected here (the four irregular
    -- grandfathered tags that look like normal tags at the start)
    | isVariant -> Right $ PartialVariant n (st :)
    | otherwise -> handleSingleton AtRegion n id `orElse` detectLateIrregular n
  PartialVariant n f
    | isVariant -> Right $ PartialVariant n (f . (st :))
    | otherwise -> handleSingleton AtVariant (n {variants = f []}) id
  PartialStartExtension n exts c
    | isExtensionSubtag -> Right $ PartialExtension n exts c (st :|)
    | otherwise ->
      Left $
        StepError (Just $ NormalTag n {extensions = exts []}) $
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
    -- TODO: I think this should be a bad subtag error?
    _ -> Left $ StepError Nothing ErrAfterStartI
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
      err = Left $ StepError (Just $ GrandfatheredTag g) ErrTooManyAfterIrreg
      fixErr x = case x of
        Left (StepError _ e) -> Left $ StepError (Just $ GrandfatheredTag g) e
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

    isExtlang = containsOnlyLetters st && subtagLength st == 3
    isScript = subtagLength st == 4 && containsOnlyLetters st
    isRegion =
      subtagLength st == 2 && containsOnlyLetters st
        || subtagLength st == 3 && containsOnlyDigits st
    isVariant =
      subtagLength st >= 5
        || subtagLength st == 4 && not (isDigit $ subtagHead st)
    isExtensionSubtag = subtagLength st >= 2

    -- handle a singleton given an accumulated list of extension sections

    handleSingleton loc n acc
      | subtagLength st /= 1 =
        Left $
          StepError (Just $ NormalTag nEnd) $
            ErrImproperSubtag st loc
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
        err = Left $ StepError (Just $ NormalTag n) $ ErrImproperSubtag st AtRegion

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
  Left $ StepError (Just $ NormalTag n') $ ErrEmptyExtensionSection c Nothing
  where
    n' = n {extensions = exts []}
finalizeBCP47 (PartialExtension n exts c exttags) =
  Right $ NormalTag $ n {extensions = exts $ [Extension c $ exttags []]}
finalizeBCP47 (PartialStartPrivateUseSection _) =
  Left $ StepError Nothing ErrEmptyPrivateUse
finalizeBCP47 (PartialPrivateUseSection n f) =
  Right $ NormalTag $ n {privateUse = f []}
finalizeBCP47 PartialStartI = Left $ StepError Nothing ErrStartI
finalizeBCP47 (PartialGrandfathered g) = Right $ GrandfatheredTag g
finalizeBCP47 PartialStartPrivateUse = Left $ StepError Nothing ErrEmptyPrivateUse
finalizeBCP47 (PartialPrivateUse f) = Right $ PrivateUse $ f []

-- | Helper function that (unsafely!) constructs a 'Normal' tag with the given
-- 'Subtag' as its primary language subtag
initNormal :: Subtag -> Normal
initNormal st = Normal st nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [] [] []

-- | An error that may occur when parsing a complete 'BCP47' tag
data SyntaxError'
  = -- | offset of the error in the input, the step error itself
    SyntaxErrorStep Int StepError
  | -- | offset of the error in the input, the tag parsed so far (if possible),
    -- the subtag error itself
    SyntaxErrorPop Int (Maybe BCP47) Sub.PopSubtagError
  deriving (Eq, Ord, Show)

-- | Parse a 'BCP47' tag at the beginning of the text stream, stopping at the
-- end of input or the first invalid character encountered. Also returns the
-- total length (according to the passed popping functions) of input that was
-- consumed and whatever input was not consumed.
popBCP47LenWith ::
  -- | function to pop a 'SubtagChar' from @s@
  (s -> Maybe (SubtagChar, s)) ->
  -- | function to pop a dash separator from @s@
  (s -> Maybe s) ->
  -- | the input
  s ->
  Either SyntaxError' (BCP47, Int, s)
popBCP47LenWith popChar popSep initinp = popSubtag' 0 Nothing initinp >>= startParse
  where
    collapseLeft (Left _) = Nothing
    collapseLeft (Right a) = Just a
    popSubtag' !pos bcpForErr t = case popSubtagWith popChar t of
      Left e -> Left $ SyntaxErrorPop pos bcpForErr e
      Right a -> Right a
    startParse (st, t) = case startBCP47 st of
      Left e -> Left $ SyntaxErrorStep 0 e
      Right a -> step (subtagLength' st) t a
    step !startpos t acc
      | Just t' <- popSep t = do
        let bcp47ForErr = collapseLeft $ finalizeBCP47 acc
        let startpos' = startpos + 1
        (st, t'') <- popSubtag' startpos' bcp47ForErr t'
        case stepBCP47 st acc of
          Left e -> Left $ SyntaxErrorStep startpos' e
          Right acc' -> step (startpos' + subtagLength' st) t'' acc'
      | otherwise = finalize startpos t acc
    finalize pos t tag = case finalizeBCP47 tag of
      Left e -> Left $ SyntaxErrorStep pos e
      Right a -> Right (a, pos, t)

-- | Parse a 'BCP47' tag at the beginning of the text stream, stopping at the
-- end of input or the first invalid character encountered. Also returns the
-- total length of input that was consumed and whatever input was not consumed.
popBCP47Len :: Text -> Either SyntaxError' (BCP47, Int, Text)
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
