-- |
-- Description : Language subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Language' type, which enumerates all of
-- the language subtags in the IANA registry, the current version of
-- which (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module LanguageTag.BCP47.Registry.Language
  ( -- * Language subtags
    Language (..),

    -- * Rendering and conversion
    renderLanguage,
    renderLanguageBuilder,
    languageToSubtag,

    -- * Records and lookup
    LanguageRecord (..),
    lookupLanguageRecord,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified LanguageTag.BCP47.Subtag as Sub
import LanguageTag.Internal.BCP47.Registry.Language
import LanguageTag.Internal.BCP47.Registry.LanguageRecords
import LanguageTag.Internal.BCP47.Registry.Orphans ()
import LanguageTag.Internal.BCP47.Registry.Types

-- | Render a 'Language' subtag to a strict text value
renderLanguage :: Language -> Text
renderLanguage = Sub.renderSubtagLower . languageToSubtag

-- | Render a 'Language' subtag to a lazy text builder
renderLanguageBuilder :: Language -> TB.Builder
renderLanguageBuilder = Sub.renderSubtagBuilderLower . languageToSubtag
