-- |
-- Module      : Text.LanguageTag.BCP47.Registry.Grandfathered
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
module Text.LanguageTag.BCP47.Registry.Grandfathered
  ( -- * Grandfathered tags
    Grandfathered (..),
    grandfatheredToTag,

    -- * Grandfathered tag records
    RangeRecord (..),
    lookupGrandfatheredRecord,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'Grandfathered' tag to a 'Syn.LanguageTag'
grandfatheredToTag :: Grandfathered -> Syn.LanguageTag
grandfatheredToTag = Syn.Grandfathered
{-# INLINE grandfatheredToTag #-}
