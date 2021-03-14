-- |
-- Module      : Text.LanguageTag.BCP47.Registry.Redundant
-- Description : Redundant subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Redundant' type, which enumerates all of
-- the redundant tags in the IANA registry, the current version of
-- which (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Registry.Redundant
  ( -- * Redundant tags
    Redundant (..),
    redundantToTag,

    -- * Redundant tag records
    RangeRecord (..),
    lookupRedundantRecord,
  )
where

import Text.LanguageTag.Internal.BCP47.Registry.Redundant
import Text.LanguageTag.Internal.BCP47.Registry.RedundantRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
