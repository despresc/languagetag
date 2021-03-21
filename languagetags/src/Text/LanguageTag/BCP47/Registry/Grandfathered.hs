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
module Text.LanguageTag.BCP47.Registry.Grandfathered
  ( -- * Grandfathered tags
    Grandfathered (..),
    renderGrandfathered,
    renderGrandfatheredBuilder,
    grandfatheredSyntax,

    -- * Grandfathered tag records
    RangeRecord (..),
    lookupGrandfatheredRecord,
    grandfatheredDetails,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'Grandfathered' tag to a 'Syn.BCP47' tag
grandfatheredSyntax :: Grandfathered -> Syn.BCP47
grandfatheredSyntax = Syn.Grandfathered
{-# INLINE grandfatheredSyntax #-}

-- | Render a 'Grandfathered' tag to a strict text value
renderGrandfathered :: Grandfathered -> Text
renderGrandfathered = Syn.renderBCP47 . grandfatheredSyntax

-- | Render a 'Grandfathered' tag to a lazy text builder
renderGrandfatheredBuilder :: Grandfathered -> TB.Builder
renderGrandfatheredBuilder = Syn.renderBCP47Builder . grandfatheredSyntax
