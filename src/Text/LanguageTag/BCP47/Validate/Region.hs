-- |
-- Module      : Text.LanguageTag.BCP47.Validate.Region
-- Description : Region subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Region' type, which enumerates all of the
-- region subtags in the IANA registry, the current version of which
-- (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Validate.Region
  ( Region (..),
  )
where

import Text.LanguageTag.Internal.BCP47.Validate.Region
import Text.LanguageTag.Internal.BCP47.Validate.Orphans ()
