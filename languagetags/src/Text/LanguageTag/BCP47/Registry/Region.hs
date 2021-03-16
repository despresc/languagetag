-- |
-- Description : Region subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Region' type, which enumerates all of the
-- region subtags in the IANA registry, the current version of which
-- (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Registry.Region
  ( -- * Region subtags
    Region (..),
    regionToSubtag,

    -- * Region subtag records
    RegionRecord (..),
    lookupRegionRecord,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.RegionRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
