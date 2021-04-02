{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Subtag record types
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: this is an internal module and may change or disappear
-- without regard to the PVP.
module Text.LanguageTag.Internal.BCP47.Registry.Types
  ( BCP47 (..),
    Normal (..),
    ExtensionSubtag (..),
    toExtensionSubtag,
    fromExtensionSubtag,
    LanguageRecord (..),
    ExtlangRecord (..),
    ScriptRecord (..),
    RegionRecord (..),
    VariantRecord (..),
    RangeRecord (..),
    Scope (..),
    Deprecation (..),
    unsafeBinSearchIndexOn,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bits (shiftR)
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.LanguageTag.BCP47.Subtag (Subtag, subtagLength)
import Text.LanguageTag.Internal.BCP47.Registry.Extlang
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Registry.Language
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.Script
import Text.LanguageTag.Internal.BCP47.Registry.Variant
import Text.LanguageTag.Internal.BCP47.Syntax (ExtensionChar (..))

-- | A valid (not merely well-formed) BCP47 language tag. These fall
-- into three categories:
--
-- * 'Normal' language tags consist of a primary langauge subtag and
--   zero or more additional subtags that qualify the meaning of the
--   tag. Most tags that you will encounter are normal tags.
--
-- * 'PrivateUseTag' language tags start with @x-@ and are followed by
--   one or more private use subtags. These tags are totally
--   uninterpreted; their meaning is specified by private agreement,
--   and the only condition on their validity is that the subtags be
--   syntactically well-formed.
--
-- * 'Grandfathered' language tags are tags that were grandfathered in
--   their entirety into the current standard. These tags might not be
--   well-formed according to the normal tag grammar, and if they are,
--   then one or more of their subtags will be unregistered. Most of
--   these are deprecated; see the 'Grandfathered' documentation for
--   up-to-date details.
--
-- Note that there is also a fourth type of tag, the "redundant" tags,
-- that are valid 'Normal' tags but for historical reasons are also
-- registered in their entirety. These are represented as 'Normal'
-- tags here; the fact that they are so registered only affects how
-- they are canonicalized, since they may be deprecated in a way that
-- differs from the deprecation of their component subtags.
data BCP47
  = NormalTag Normal
  | PrivateUseTag (NonEmpty Subtag)
  | GrandfatheredTag Grandfathered
  deriving (Eq, Ord)

instance Hashable BCP47 where
  hashWithSalt s (NormalTag t) =
    s
      `hashWithSalt` (0 :: Int)
      `hashWithSalt` t
  hashWithSalt s (PrivateUseTag t) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` t
  hashWithSalt s (GrandfatheredTag t) =
    s
      `hashWithSalt` (2 :: Int)
      `hashWithSalt` t

instance NFData BCP47 where
  rnf (NormalTag x) = rnf x
  rnf (PrivateUseTag x) = rnf x
  rnf (GrandfatheredTag _) = ()

-- | A normal language tag, as opposed to a private use or
-- grandfathered tag. Note that validating the subtags in an extension
-- section of a tag is not required by the standard, and we do not
-- attempt to do so.
data Normal = Normal
  { language :: Language,
    extlang :: Maybe Extlang,
    script :: Maybe Script,
    region :: Maybe Region,
    variants :: Set Variant,
    extensions :: Map ExtensionChar (NonEmpty ExtensionSubtag),
    privateUse :: [Subtag]
  }
  deriving (Eq, Ord)

instance Hashable Normal where
  hashWithSalt sl (Normal l e s r v es p) =
    sl `hashWithSalt` l `hashWithSalt` e
      `hashWithSalt` s
      `hashWithSalt` r
      `hashWithSalt` S.toList v
      `hashWithSalt` M.toList es
      `hashWithSalt` p

instance NFData Normal where
  rnf (Normal l e s r v es p) =
    rnf l `seq` rnf e `seq` rnf s `seq` rnf r `seq` rnf v `seq` rnf es `seq` rnf p

-- | An extension subtag is a 'Subtag' that is at least two characters
-- long
newtype ExtensionSubtag = ExtensionSubtag Subtag
  deriving (Eq, Ord, Show, Hashable)

-- | Convert a subtag to an extension subtag, failing if it has length
-- one
toExtensionSubtag :: Subtag -> Maybe ExtensionSubtag
toExtensionSubtag s
  | subtagLength s >= 2 = Just $ ExtensionSubtag s
  | otherwise = Nothing

-- | Convert an extension subtag to a subtag
fromExtensionSubtag :: ExtensionSubtag -> Subtag
fromExtensionSubtag (ExtensionSubtag t) = t

instance NFData ExtensionSubtag where
  rnf a = seq a ()

----------------------------------------------------------------
-- Language records
----------------------------------------------------------------

-- | A primary language subtag record. Note that the descriptions are
-- not guaranteed by the registry to be in any particular language or
-- script. They also happen to be the same descriptions that appear as
-- semicolon-separated lists next to the constructors of the
-- 'Language' type.
data LanguageRecord = LanguageRecord
  { languageDescription :: NonEmpty Text,
    languageDeprecation :: Deprecation Language,
    languageScriptSuppression :: Maybe Script,
    languageMacrolanguage :: Maybe Language,
    languageScope :: Maybe Scope
  }
  deriving (Eq, Ord)

-- | An extended language subtag record. In these records, a preferred
-- value always appears and is always equal to the subtag, so the
-- 'extlangDeprecation' is a simple 'Bool' ('True' being "is
-- deprecated"). Note also that these records are the only exception
-- to the general rule that a preferred value (whether tag or subtag)
-- will never be deprecated: these will have a deprecated
-- 'extlangPreferredValue' exactly when 'extlangDeprecation' is
-- 'True'.
--
-- The descriptions are not guaranteed by the registry to be in any
-- particular language or script. They also happen to be the same
-- descriptions that appear as semicolon-separated lists next to the
-- constructors of the 'Extlang' type.
data ExtlangRecord = ExtlangRecord
  { extlangDescription :: NonEmpty Text,
    extlangDeprecation :: Bool,
    extlangPreferredValue :: Language,
    extlangPrefix :: Language,
    extlangScriptSuppression :: Maybe Script,
    extlangMacrolanguage :: Maybe Language,
    extlangScope :: Maybe Scope
  }
  deriving (Eq, Ord)

-- | A variant subtag record. Note that the descriptions are not
-- guaranteed by the registry to be in any particular language or
-- script. They also happen to be the same descriptions that appear as
-- semicolon-separated lists next to the constructors of the 'Variant'
-- type.
data VariantRecord = VariantRecord
  { variantDescription :: NonEmpty Text,
    variantDeprecation :: Deprecation Variant,
    variantPrefixes :: [Normal]
  }
  deriving (Eq, Ord)

-- | A script subtag record. Note that the descriptions are not
-- guaranteed by the registry to be in any particular language or
-- script. They also happen to be the same descriptions that appear as
-- semicolon-separated lists next to the constructors of the 'Script'
-- type.
data ScriptRecord = ScriptRecord
  { scriptDescription :: NonEmpty Text,
    scriptDeprecation :: Deprecation Script
  }
  deriving (Eq, Ord)

-- | A region subtag record. Note that for deprecated region records,
--  the associated preferred value may not have exactly the same
--  meaning as the deprecated subtag. Note also that the descriptions
--  are not guaranteed by the registry to be in any particular
--  language or script. They also happen to be the same descriptions
--  that appear as semicolon-separated lists next to the constructors
--  of the 'Region' type.
data RegionRecord = RegionRecord
  { regionDescription :: NonEmpty Text,
    regionDeprecation :: Deprecation Region
  }
  deriving (Eq, Ord)

-- | A grandfathered or redundant subtag record. These records are
-- distinguished from the others in that they define entire tags, and
-- that the preferred values associated to their deprecation are an
-- "extended language range", which is an entire tag that is strongly
-- recommended as the replacement for the tag.
--
-- The descriptions are not guaranteed by the registry to be in any
-- particular language or script. They also happen to be the same
-- descriptions that appear as semicolon-separated lists next to the
-- constructors of the 'Text.LanguageTag.BCP47.Registry.Grandfathered'
-- and 'Text.LanguageTag.BCP47.Registry.Redundant' tag types.
data RangeRecord = RangeRecord
  { rangeDescription :: NonEmpty Text,
    rangeDeprecation :: Deprecation Normal
  }
  deriving (Eq, Ord)

-- | The scope of a language or extended language. If this is not
-- present in a record then the language is an "individual language",
-- i.e., what one would normally consider to be a language.
data Scope
  = -- | a cluster of closely related languages that are sometimes
    -- considered to be a single language
    Macrolanguage
  | -- | a collection of languages usually related by history or
    -- geography; a looser relationship than a 'Macrolanguage'
    Collection
  | -- | a subtag identifying something not particularly associated
    -- with a concrete language
    Special
  | -- | a subtag reserved for private use
    PrivateUseScope
  deriving (Eq, Ord, Show)

-- | The deprecation status of a subtag
data Deprecation a
  = -- | not deprecated
    NotDeprecated
  | -- | deprecated but without a preferred value
    DeprecatedSimple
  | -- | deprecated with a preferred value
    DeprecatedPreferred a
  deriving (Eq, Ord, Show)

-- | Search for an element in a vector with the given key using the
-- given projection function and return its index. The vector must be
-- sorted with respect to the projection (otherwise the function will
-- not work properly) and must be non-empty (otherwise the function
-- will segfault).
unsafeBinSearchIndexOn :: Ord b => (a -> b) -> b -> Vector a -> Maybe Int
unsafeBinSearchIndexOn proj b v = go 0 (V.length v)
  where
    -- n.b. we are searching for indices between low and high,
    -- inclusive of low and exclusive of high, and since low < high at
    -- the start, then low <= idx < high for the entire run. The idx
    -- == low check is necessary to catch the case where the element
    -- is not present (otherwise it could be omitted).
    go low high
      | idx == low = if b == proj a then Just idx else Nothing
      | otherwise = case compare b (proj a) of
        Prelude.LT -> go low idx
        EQ -> Just idx
        Prelude.GT -> go idx high
      where
        idx = (low + high) `shiftR` 1
        a = V.unsafeIndex v idx
{-# INLINE unsafeBinSearchIndexOn #-}
