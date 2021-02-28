{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Text.LanguageTag.BCP47.Validate
-- Description : BCP47 language tag parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'validateBCP47' function to validate a
-- syntactically well-formed 'LanguageTag', transforming it into a
-- 'BCP47Tag' value. This module also re-exports the (very large)
-- module "Text.LanguageTag.BCP47.RegisteredSubtags", which contains
-- the data types representing all of the subtags in the IANA
-- registry, the current version of which is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>. See
-- 'bcp47RegistryDate' for the version of the registry that this
-- library uses.
module Text.LanguageTag.BCP47.Validate
  ( -- * Valid tags
    BCP47Tag (..),
    Normal (..),
    ExtensionSubtag,
    toExtensionSubtag,
    fromExtensionSubtag,
    ExtensionChar (..),
    toExtensionChar,
    fromExtensionChar,

    -- ** Valid tag parsers

    {-    parseLanguage,
        parseExtlang,
        parseScript,
        parseRegion,
        parseVariant, -}

    -- * Registry records
    -- $therecords
    LanguageRecord (..),
    ExtlangRecord (..),
    ScriptRecord (..),
    RegionRecord (..),
    VariantRecord (..),
    RangeRecord (..),
    Scope (..),
    Deprecation (..),

    -- * The registered subtags
    -- $thetags
    bcp47RegistryDate,

    -- ** Language subtags
    Language (..),

    -- ** Extended language subtags
    Extlang (..),

    -- ** Script subtags
    Script (..),

    -- ** Region subtags
    Region (..),

    -- ** Variant subtags
    Variant (..),

    -- ** Grandfathered subtags
    Grandfathered (..),

    -- ** Redundant subtags
    Redundant (..),
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..), hashUsing)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Text.LanguageTag.BCP47.Syntax (Subtag, subtagLength)
import Text.LanguageTag.Internal.BCP47.Extlang
import Text.LanguageTag.Internal.BCP47.Grandfathered
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Internal.BCP47.Redundant
import Text.LanguageTag.Internal.BCP47.Region
import Text.LanguageTag.Internal.BCP47.RegistryDate
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.Variant

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
--   then one or more of their subtags will be unregistered. As of
--   2021-02-23, all of these but 'IMingo' and 'IDefault' have been
--   deprecated (but note that the 'Grandfathered' documentation may
--   have more up-to-date information).
--
-- Note that there is also third type of tag, the "redundant" tags,
-- that are valid 'Normal' tags but for historical reasons are also
-- registered in their entirety. These are represented as normal tags;
-- the fact that they are so registered will only influence
-- canonicalization, and only when the redundant tag has been
-- deprecated.
data BCP47Tag
  = NormalTag Normal
  | PrivateUse (NonEmpty Subtag)
  | GrandfatheredTag Grandfathered

{-

The rules:

- primary subtags - be registered

- extended language subtags - be registered. recommended that the
  related primary language subtag be used instead. must only have one
  extended language subtag (zh-min-nan excluded, I suppose)

- script subtags - should be omitted if it adds no value or if the
  primary language subtag has a relevant suppress-script field.

- variants - must not be repeated. variants sharing a prefix should
  not be used in the same tag (variants that can be used in
  combination should have multiple prefix fields indicating that
  fact).

- extension subtags - a singleton must appear at most once (except in
  private use), must have been registered. should canonicalize by
  ordering the singletions by ascii order.

- grandfathered/redundant - must match exactly, of course.

valid tag:

- well-formed
- either grandfathered or all the primary language, extended language,
  script, region, and variant subtags appear in the IANA subtag
  registry.
- no duplicate variant subtags
- no duplicate singleton (extension) subtags

canonicalizing:

- extension sequences are ordered by singleton subtag (ascii
  case-insensitive)
- redundant or grandfathered tags are replaced by their preferred
  value
- subtags are replaced by their preferred value (notably this means
  that there will be no extlangs in canonical form)

(so maybe we define a separate canonical tag type? and the canonical
form -> extlang form transformation can be defined as a CanonicalTag
-> ValidTag function).

-}

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
    privateuse :: [Subtag]
  }

-- | The possible single character extensions; all the ASCII
-- alphanumeric characters (case-insensitive) except the letter X.
data ExtensionChar
  = Ext0
  | Ext1
  | Ext2
  | Ext3
  | Ext4
  | Ext5
  | Ext6
  | Ext7
  | Ext8
  | Ext9
  | ExtA
  | ExtB
  | ExtC
  | ExtD
  | ExtE
  | ExtF
  | ExtG
  | ExtH
  | ExtI
  | ExtJ
  | ExtK
  | ExtL
  | ExtM
  | ExtN
  | ExtO
  | ExtP
  | ExtQ
  | ExtR
  | ExtS
  | ExtT
  | ExtU
  | ExtV
  | ExtW
  | ExtY
  | ExtZ
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Convert an ASCII alphanumeric character other than X to an 'ExtensionChar'
toExtensionChar :: Char -> Maybe ExtensionChar
toExtensionChar c = case c of
  '0' -> Just Ext0
  '1' -> Just Ext1
  '2' -> Just Ext2
  '3' -> Just Ext3
  '4' -> Just Ext4
  '5' -> Just Ext5
  '6' -> Just Ext6
  '7' -> Just Ext7
  '8' -> Just Ext8
  '9' -> Just Ext9
  'a' -> Just ExtA
  'b' -> Just ExtB
  'c' -> Just ExtC
  'd' -> Just ExtD
  'e' -> Just ExtE
  'f' -> Just ExtF
  'g' -> Just ExtG
  'h' -> Just ExtH
  'i' -> Just ExtI
  'j' -> Just ExtJ
  'k' -> Just ExtK
  'l' -> Just ExtL
  'm' -> Just ExtM
  'n' -> Just ExtN
  'o' -> Just ExtO
  'p' -> Just ExtP
  'q' -> Just ExtQ
  'r' -> Just ExtR
  's' -> Just ExtS
  't' -> Just ExtT
  'u' -> Just ExtU
  'v' -> Just ExtV
  'w' -> Just ExtW
  'y' -> Just ExtY
  'z' -> Just ExtZ
  'A' -> Just ExtA
  'B' -> Just ExtB
  'C' -> Just ExtC
  'D' -> Just ExtD
  'E' -> Just ExtE
  'F' -> Just ExtF
  'G' -> Just ExtG
  'H' -> Just ExtH
  'I' -> Just ExtI
  'J' -> Just ExtJ
  'K' -> Just ExtK
  'L' -> Just ExtL
  'M' -> Just ExtM
  'N' -> Just ExtN
  'O' -> Just ExtO
  'P' -> Just ExtP
  'Q' -> Just ExtQ
  'R' -> Just ExtR
  'S' -> Just ExtS
  'T' -> Just ExtT
  'U' -> Just ExtU
  'V' -> Just ExtV
  'W' -> Just ExtW
  'Y' -> Just ExtY
  'Z' -> Just ExtZ
  _ -> Nothing

-- | Convert an 'ExtensionChar' to a lower case 'Char'.
fromExtensionChar :: ExtensionChar -> Char
fromExtensionChar c = case c of
  Ext0 -> '0'
  Ext1 -> '1'
  Ext2 -> '2'
  Ext3 -> '3'
  Ext4 -> '4'
  Ext5 -> '5'
  Ext6 -> '6'
  Ext7 -> '7'
  Ext8 -> '8'
  Ext9 -> '9'
  ExtA -> 'a'
  ExtB -> 'b'
  ExtC -> 'c'
  ExtD -> 'd'
  ExtE -> 'e'
  ExtF -> 'f'
  ExtG -> 'g'
  ExtH -> 'h'
  ExtI -> 'i'
  ExtJ -> 'j'
  ExtK -> 'k'
  ExtL -> 'l'
  ExtM -> 'm'
  ExtN -> 'n'
  ExtO -> 'o'
  ExtP -> 'p'
  ExtQ -> 'q'
  ExtR -> 'r'
  ExtS -> 's'
  ExtT -> 't'
  ExtU -> 'u'
  ExtV -> 'v'
  ExtW -> 'w'
  ExtY -> 'y'
  ExtZ -> 'z'

instance Hashable ExtensionChar where
  hashWithSalt = hashUsing fromEnum

instance NFData ExtensionChar where
  rnf a = seq a ()

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

-- $thetags
--
-- Below are all of the subtags that are registered with the
-- IANA. Check 'bcp47RegistryDate' for the version of the registry
-- that this library uses.
--
-- The names of the subtag data constructors are roughly the same as
-- the subtags themselves; all of them are title case except for the
-- 'Region' tags, which are all upper case. Additionally, the
-- 'Extlang' constructors have an @Ext@ prefix, any tag that starts
-- with a digit is prefixed with the first letter of their type, and
-- the redundant and grandfathered tags have been converted from kebab
-- case to camel case. These modifications were necessary to arrive at
-- constructor names that are valid and free of name collisions. Some
-- examples:
--
-- @
--  -- for the language subtag en
-- 'En' :: 'Language'
-- -- for the extended language subtag sgg
-- 'ExtSgg' :: 'Extlang'
-- -- for the script subtag moon
-- 'Moon' :: 'Script'
-- -- for the region subtag AU
-- 'AU' :: 'Region'
-- -- for the region subtag 419
-- 'R419' :: 'Region'
-- -- for the variant subtag gascon
-- 'Gascon' :: 'Variant'
-- -- for the variant subtag 1606nict
-- 'V1606nict' :: 'Variant'
-- -- for the grandfathered subtag i-enochian
-- 'IEnochian' :: 'Grandfathered'
-- -- for the redundant subtag en-scouse
-- 'EnScouse' :: 'Redundant'
-- @

----------------------------------------------------------------
-- Language records
----------------------------------------------------------------

-- $therecords
--
-- The subtag registry contains records for language, extended
-- language, script, and region subtags, and grandfathered and
-- redundants tags. Some general notes on these records:
--
-- * Each record contains at least one (non-normative) description of
--   what the subtag represents. The registry does not guarantee that
--   this description will be in any particular language, and these
--   descriptions may be added, changed, or removed as the registry
--   updates.
--
-- * Each record may contain a deprecation notice, indicating that the
--   associated tag or subtag should not be used. This value may be
--   added, changed, or removed as the registry updates.
--
-- * If deprecated, a record may contain a preferred value that is
--   recommended for use instead of the deprected tag. Note that for
--   regions, this preferred value may not have exactly the same
--   meaning as the old tag. This value may be added, removed or
--   modified as the registry updates, and a change in this value does
--   not imply that the affected subtag needs to be retagged. Tags
--   that appear as preferred values will never have a preferred value
--   themselves.
--
-- * Extlang and variant records may have a prefix (more than one in
--   the case of variant records) that is recommended as a prefix to
--   the record's subtag. If a record does not have a prefix field,
--   one will not be added as the registry updates, and changes to a
--   prefix field must only widen the range of possible prefixes.
--
-- * Language an extlang records have macrolanguage and scope fields,
--   which are informational, and indicate an encompassing language
--   and a classification of the language, respectively. These may be
--   added, removed, or changed as the registry updates.

-- | A primary language subtag record
data LanguageRecord = LanguageRecord
  { langDescription :: NonEmpty Text,
    langDeprecation :: Deprecation Language,
    langScriptSuppression :: Maybe Script,
    langMacrolanguage :: Maybe Language,
    langScope :: Maybe Scope
  }

-- | An extended language subtag record. In these records, a preferred
-- value always appears and is always equal to the subtag, so the
-- 'extlangDeprecation' is a simple 'Bool' ('True' being "is
-- deprecated").
data ExtlangRecord = ExtlangRecord
  { extlangDescription :: NonEmpty Text,
    extlangDeprecation :: Bool,
    extlangPrefix :: BCP47Tag,
    extlangScriptSuppression :: Maybe Script,
    extlangMacrolanguage :: Maybe Language,
    extlangScope :: Maybe Scope
  }

-- | A variant subtag record
data VariantRecord = VariantRecord
  { variantDescription :: NonEmpty Text,
    variantDeprecation :: Deprecation Variant,
    variantPrefixes :: [BCP47Tag]
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
    rangeDeprecation :: Deprecation BCP47Tag
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
