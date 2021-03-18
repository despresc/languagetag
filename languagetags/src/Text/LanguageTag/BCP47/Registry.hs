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
    renderBCP47,
    renderBCP47Builder,
    toSyntaxTag,
    toSubtags,
    Normal (..),
    Syn.ExtensionChar (..),
    Syn.charToExtensionChar,
    Syn.extensionCharToChar,
    Syn.extensionCharToSubtag,
    ExtensionSubtag,
    toExtensionSubtag,
    fromExtensionSubtag,
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
