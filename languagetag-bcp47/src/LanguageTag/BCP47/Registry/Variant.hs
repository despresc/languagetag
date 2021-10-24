-- |
-- Description : Variant subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Variant' type, which enumerates all of the
-- variant subtags in the IANA registry, the current version of which
-- (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module LanguageTag.BCP47.Registry.Variant
  ( -- * Variant subtags
    Variant (..),

    -- * Rendering and conversion
    renderVariant,
    renderVariantBuilder,
    variantToSubtag,

    -- * Records and lookup
    VariantRecord (..),
    lookupVariantRecord,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified LanguageTag.BCP47.Subtag as Sub
import LanguageTag.Internal.BCP47.Registry.Orphans ()
import LanguageTag.Internal.BCP47.Registry.Types
import LanguageTag.Internal.BCP47.Registry.Variant
import LanguageTag.Internal.BCP47.Registry.VariantRecords

-- | Render a 'Variant' subtag to a strict text value
renderVariant :: Variant -> Text
renderVariant = Sub.renderSubtagLower . variantToSubtag

-- | Render a 'Variant' subtag to a lazy text builder
renderVariantBuilder :: Variant -> TB.Builder
renderVariantBuilder = Sub.renderSubtagBuilderLower . variantToSubtag
