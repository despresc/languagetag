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
module Text.LanguageTag.BCP47.Registry.Language
  ( -- * Language subtags
    Language (..),
    languageToSubtag,

    -- * Language subtag records
    LanguageRecord (..),
    lookupLanguageRecord,
    languageDetails,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Language
import Text.LanguageTag.Internal.BCP47.Registry.LanguageRecords
import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Types
