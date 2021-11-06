-- |
-- Description : Grandfathered subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Grandfathered' type, which enumerates all
-- of the grandfathered tags in the IANA registry, the current version
-- of which (not necessarily the one used in the library) is available
-- at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module LanguageTag.BCP47.Registry.Grandfathered
  ( -- * Grandfathered tags
    Grandfathered (..),
    renderGrandfathered,
    renderGrandfatheredBuilder,
    grandfatheredSyntax,
    Syn.grandfatheredToSubtags,

    -- * Grandfathered tag records
    RangeRecord (..),
    lookupGrandfatheredRecord,
  )
where

import LanguageTag.BCP47.LegacyTag
import LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords
import LanguageTag.Internal.BCP47.Registry.Types
import qualified LanguageTag.Internal.BCP47.Syntax as Syn
