-- |
-- Module      : Text.LanguageTag.BCP47.Registry.Extlang
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
    extlangToSubtag,

    -- * Extended language subtag records
    ExtlangRecord (..),
    lookupExtlangRecord,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Extlang
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Types
