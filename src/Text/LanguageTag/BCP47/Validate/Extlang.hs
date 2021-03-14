-- |
-- Module      : Text.LanguageTag.BCP47.Validate.Extlang
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
module Text.LanguageTag.BCP47.Validate.Extlang
  ( Extlang (..),
  )
where

import Text.LanguageTag.Internal.BCP47.Validate.Extlang
import Text.LanguageTag.Internal.BCP47.Validate.Orphans ()
