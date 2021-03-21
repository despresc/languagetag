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
module Text.LanguageTag.BCP47.Registry.Extlang
  ( -- * Extended language subtags
    Extlang (..),
    renderExtlang,
    renderExtlangBuilder,
    extlangToSubtag,

    -- * Extended language subtag records
    ExtlangRecord (..),
    lookupExtlangRecord,
    extlangDetails,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import Text.LanguageTag.Internal.BCP47.Registry.Extlang
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Types

-- | Render an 'Extlang' subtag to a strict text value
renderExtlang :: Extlang -> Text
renderExtlang = Sub.renderSubtagLower . extlangToSubtag

-- | Render an 'Extlang' subtag to a lazy text builder
renderExtlangBuilder :: Extlang -> TB.Builder
renderExtlangBuilder = Sub.renderSubtagBuilderLower . extlangToSubtag
