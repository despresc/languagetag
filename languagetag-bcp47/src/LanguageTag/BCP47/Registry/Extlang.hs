-- |
-- Description : Extended language subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Extlang' type, which enumerates all of the
-- extended language subtags in the IANA registry, the current version
-- of which (not necessarily the one used in the library) is available
-- at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module LanguageTag.BCP47.Registry.Extlang
  ( -- * Extended language subtags
    Extlang (..),

    -- * Rendering and conversion
    renderExtlang,
    renderExtlangBuilder,
    extlangToSubtag,

    -- * Records and lookup
    ExtlangRecord (..),
    lookupExtlangRecord,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified LanguageTag.BCP47.Subtag as Sub
import LanguageTag.Internal.BCP47.Registry.Extlang
import LanguageTag.Internal.BCP47.Registry.ExtlangRecords
import LanguageTag.Internal.BCP47.Registry.Orphans ()
import LanguageTag.Internal.BCP47.Registry.Types

-- | Render an 'Extlang' subtag to a strict text value
renderExtlang :: Extlang -> Text
renderExtlang = Sub.renderSubtagLower . extlangToSubtag

-- | Render an 'Extlang' subtag to a lazy text builder
renderExtlangBuilder :: Extlang -> TB.Builder
renderExtlangBuilder = Sub.renderSubtagBuilderLower . extlangToSubtag
