-- |
-- Description : Registered BCP47 tags, subtags and their records
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module re-exports all of the registered tag and subtag types,
-- their associated record lookup functions, as well as the
-- 'bcp47RegistryDate' that this the library uses. The current version
-- of the IANA registry, not necessarily the one used in the library,
-- is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Normal (..),
    toSyntaxTag,
    toSubtags,

    -- * Rendering
    renderBCP47,
    renderBCP47Builder,

    -- * Extension subtags and characters
    ExtensionSubtag,
    toExtensionSubtag,
    fromExtensionSubtag,
    Syn.ExtensionChar (..),
    Syn.charToExtensionChar,
    Syn.extensionCharToChar,
    Syn.extensionCharToSubtag,

    -- * Registry types and re-exports

    -- ** Notes on the registry and types
    -- $theregistry

    -- ** Types and re-exports
    bcp47RegistryDate,
    Scope (..),
    Deprecation (..),
    module Text.LanguageTag.BCP47.Registry.Language,
    module Text.LanguageTag.BCP47.Registry.Extlang,
    module Text.LanguageTag.BCP47.Registry.Script,
    module Text.LanguageTag.BCP47.Registry.Region,
    module Text.LanguageTag.BCP47.Registry.Variant,
    module Text.LanguageTag.BCP47.Registry.Grandfathered,
    module Text.LanguageTag.BCP47.Registry.Redundant,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import Text.LanguageTag.BCP47.Registry.Extlang
import Text.LanguageTag.BCP47.Registry.Grandfathered
import Text.LanguageTag.BCP47.Registry.Language
import Text.LanguageTag.BCP47.Registry.Redundant
import Text.LanguageTag.BCP47.Registry.Region
import Text.LanguageTag.BCP47.Registry.Script
import Text.LanguageTag.BCP47.Registry.Variant
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Registry
import Text.LanguageTag.Internal.BCP47.Registry.Date
import Text.LanguageTag.Internal.BCP47.Registry.Types
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'BCP47' tag to strict text
renderBCP47 :: BCP47 -> Text
renderBCP47 = Syn.renderBCP47 . toSyntaxTag

-- | Render a 'BCP47' tag to a lazy text builder
renderBCP47Builder :: BCP47 -> TB.Builder
renderBCP47Builder = Syn.renderBCP47Builder . toSyntaxTag

-- $theregistry
--
-- The names of the subtag data constructors are roughly the same as
-- the subtags themselves; all of them are camel case except for the
-- 'Region' tags, which are all upper case. Additionally, the
-- 'Extlang' constructors and the constructors corresponding to
-- subtags that start with a digit are prefixed with the first three
-- letters of their types. These modifications were necessary to
-- arrive at constructor names that are valid and free of name
-- collisions.
--
-- The registry itself contains records for language, extended
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
--   not imply that the affected subtag needs to be retagged.
--
-- * Extlang and variant records may have a prefix (more than one in
--   the case of variant records) that is recommended as a prefix to
--   the record's subtag. If a record does not have a prefix field,
--   one will not be added as the registry updates, and changes to a
--   prefix field must only widen the range of possible prefixes.
--
-- * Language and extlang records have macrolanguage and scope fields,
--   which are informational and indicate an encompassing language and
--   a classification of the language, respectively. These may be
--   added, removed, or changed as the registry updates.
