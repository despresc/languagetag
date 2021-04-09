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
import Text.LanguageTag.BCP47.Subtag hiding (SyntaxError (..))
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
tagPop inp atlast pos end more = case popSubtag inp of
  Right (st, t) -> more st t
  Left (Sub.TrailingTerminator st) -> case more st "" of
    Left e -> Left e
    Right a -> Left $ TrailingTerminator a
  Left Sub.EmptyInput -> end
  Left Sub.EmptySubtag -> Left $ EmptySubtag pos atlast mcon
  Left Sub.TagTooLong -> Left $ UnparsableSubtag pos atlast Nothing mcon
  Left (Sub.InvalidChar n c) -> Left $ UnparsableSubtag pos atlast (Just (pos + n, c)) mcon
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
