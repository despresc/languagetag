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
    scriptToSubtag,

    -- * Script subtag records
    ScriptRecord (..),
    lookupScriptRecord,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Script
import Text.LanguageTag.Internal.BCP47.Registry.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
