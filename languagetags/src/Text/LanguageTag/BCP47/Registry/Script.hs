-- |
-- Description : Script subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Script' type, which enumerates all of the
-- script subtags in the IANA registry, the current version of which
-- (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Registry.Script
  ( -- * Script subtags
    Script (..),

    -- * Rendering and conversion
    renderScript,
    renderScriptBuilder,
    scriptToSubtag,

    -- * Records and lookup
    ScriptRecord (..),
    lookupScriptRecord,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Script
import Text.LanguageTag.Internal.BCP47.Registry.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types

-- | Render a 'Script' subtag to a strict text value
renderScript :: Script -> Text
renderScript = Sub.renderSubtagLower . scriptToSubtag

-- | Render a 'Script' subtag to a lazy text builder
renderScriptBuilder :: Script -> TB.Builder
renderScriptBuilder = Sub.renderSubtagBuilderLower . scriptToSubtag
