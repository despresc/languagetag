{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: qualify exports

-- |
-- Module      : Text.LanguageTag.Internal.BCP47.Registry.Types
-- Description : Subtag record types
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning: this is an internal module and may change or disappear
-- without regard to the PVP.
module Text.LanguageTag.Internal.BCP47.Registry.Types where

import Control.DeepSeq (NFData (..))
import Data.Bits (shiftR)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.LanguageTag.Internal.BCP47.Registry.Extlang
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Registry.Language
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.Script
import Text.LanguageTag.Internal.BCP47.Registry.Variant
import Text.LanguageTag.Internal.BCP47.Syntax (ExtensionChar (..))
import Text.LanguageTag.Subtag (Subtag, subtagLength)

-- | A valid (not merely well-formed) BCP47 language tag. These fall
-- into three categories:
--
-- * 'Normal' language tags consist of a primary langauge subtag and
--   zero or more additional subtags that qualify the meaning of the
--   tag. Most tags that you will encounter are normal tags.
--
-- * 'PrivateUse' language tags start with @x-@ and are followed by
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
-- registered in their entirety. These are represented as normal tags;
-- the fact that they are so registered will only influence
-- canonicalization, and only when the redundant tag has been
-- deprecated.
data BCP47Tag
  = NormalTag Normal
  | PrivateUse (NonEmpty Subtag)
  | GrandfatheredTag Grandfathered

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

-- | An extension subtag is a 'Subtag' that is at least two characters
-- long
newtype ExtensionSubtag = ExtensionSubtag Subtag
  deriving (Eq, Ord, Show, Hashable)

-- | Convert a subtag to an extension subtag
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

-- | A primary language subtag record
data LanguageRecord = LanguageRecord
  { languageDescription :: NonEmpty Text,
    languageDeprecation :: Deprecation Language,
    languageScriptSuppression :: Maybe Script,
    languageMacrolanguage :: Maybe Language,
    languageScope :: Maybe Scope
  }

-- | An extended language subtag record. In these records, a preferred
-- value always appears and is always equal to the subtag, so the
-- 'extlangDeprecation' is a simple 'Bool' ('True' being "is
-- deprecated").
data ExtlangRecord = ExtlangRecord
  { extlangDescription :: NonEmpty Text,
    extlangDeprecation :: Bool,
    extlangPreferredValue :: Language,
    extlangPrefix :: Language,
    extlangScriptSuppression :: Maybe Script,
    extlangMacrolanguage :: Maybe Language,
    extlangScope :: Maybe Scope
  }

-- | A variant subtag record
data VariantRecord = VariantRecord
  { variantDescription :: NonEmpty Text,
    variantDeprecation :: Deprecation Variant,
    variantPrefixes :: [Normal]
  }

-- | A script subtag record
data ScriptRecord = ScriptRecord
  { scriptDescription :: NonEmpty Text,
    scriptDeprecation :: Deprecation Script
  }

-- | A region subtag record. Note that for deprecated region records,
--  the associated preferred value may not represent the same meaning
--  as the deprecated subtag.
data RegionRecord = RegionRecord
  { regionDescription :: NonEmpty Text,
    regionDeprecation :: Deprecation Region
  }

-- | A grandfathered or redundant subtag record. These records are
-- distinguished from the others in that they define entire tags, and
-- that the preferred values associated to their deprecation are an
-- "extended language range", which is an entire tag that is strongly
-- recommended as the replacement for the tag.
data RangeRecord = RangeRecord
  { rangeDescription :: NonEmpty Text,
    rangeDeprecation :: Deprecation Normal
  }

-- | The scope of a language or extended language
data Scope
  = Macrolanguage
  | Collection
  | Special
  | PrivateUseScope
  deriving (Show)

-- | The deprecation status of a subtag
data Deprecation a
  = NotDeprecated
  | DeprecatedSimple
  | DeprecatedPreferred a
  deriving (Show)

-- | Search for an element in a vector with the given key using the
-- given projection function and return its index. The vector must be
-- sorted with respect to the projection and must be non-empty.
binSearchIndexOn :: Ord b => (a -> b) -> b -> Vector a -> Maybe Int
binSearchIndexOn proj b v = go 0 (V.length v)
  where
    -- n.b. we are searching for indices between low and high,
    -- inclusive of low and exclusive of high, and since low < high at
    -- the start, then low <= idx < high for the entire run.
    go low high
      | idx == low = if b == proj a then Just idx else Nothing
      | otherwise = case compare b (proj a) of
        Prelude.LT -> go low idx
        EQ -> Just idx
        Prelude.GT -> go idx high
      where
        idx = (low + high) `shiftR` 1
        a = V.unsafeIndex v idx
{-# INLINE binSearchIndexOn #-}

-- | Search for an element in a vector with the given key using the
-- given projection function and return its index. The vector must be
-- sorted with respect to the projection and must contain an element
-- with the given key.
binSearchIndexOnPresent :: Ord b => (a -> b) -> b -> Vector a -> Maybe Int
binSearchIndexOnPresent proj b v = go 0 (V.length v)
  where
    go low high = case compare b (proj a) of
      Prelude.LT -> go low idx
      EQ -> Just idx
      Prelude.GT -> go idx high
      where
        idx = (low + high) `shiftR` 1
        a = V.unsafeIndex v idx
{-# INLINE binSearchIndexOnPresent #-}
