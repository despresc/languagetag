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

    -- * Rendering and conversion
    renderRegion,
    renderRegionBuilder,
    regionToSubtag,

    -- * Records and lookup
    RegionRecord (..),
    lookupRegionRecord,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.RegionRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types

-- | Render a 'Region' subtag to a strict text value
renderRegion :: Region -> Text
renderRegion = Sub.renderSubtagLower . regionToSubtag

-- | Render a 'Region' subtag to a lazy text builder
renderRegionBuilder :: Region -> TB.Builder
renderRegionBuilder = Sub.renderSubtagBuilderLower . regionToSubtag
