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
    lookupLanguageDetails,
    lookupSubtagLanguage,
    ExtlangRecord (..),
    lookupExtlangDetails,
    lookupSubtagExtlang,
    ScriptRecord (..),
    lookupScriptDetails,
    lookupSubtagScript,
    RegionRecord (..),
    lookupRegionDetails,
    lookupSubtagRegion,
    VariantRecord (..),
    lookupVariantDetails,
    lookupSubtagVariant,
    RangeRecord (..),
    lookupGrandfatheredDetails,
    lookupSubtagGrandfathered,
    lookupRedundantDetails,
    lookupSubtagRedundant,
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
import Text.LanguageTag.Internal.BCP47.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Grandfathered
import Text.LanguageTag.Internal.BCP47.GrandfatheredRecords
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Internal.BCP47.LanguageRecords
import Text.LanguageTag.Internal.BCP47.Redundant
import Text.LanguageTag.Internal.BCP47.RedundantRecords
import Text.LanguageTag.Internal.BCP47.Region
import Text.LanguageTag.Internal.BCP47.RegionRecords
import Text.LanguageTag.Internal.BCP47.RegistryDate
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Validate
import Text.LanguageTag.Internal.BCP47.Variant
import Text.LanguageTag.Internal.BCP47.VariantRecords

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
