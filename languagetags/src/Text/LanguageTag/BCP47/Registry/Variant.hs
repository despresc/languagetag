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
module Text.LanguageTag.BCP47.Registry.Variant
  ( -- * Variant subtags
    Variant (..),
    variantToSubtag,

    -- * Variant subtag records
    VariantRecord (..),
    lookupVariantRecord,
    variantDetails,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Text.LanguageTag.Internal.BCP47.Registry.Variant
import Text.LanguageTag.Internal.BCP47.Registry.VariantRecords
