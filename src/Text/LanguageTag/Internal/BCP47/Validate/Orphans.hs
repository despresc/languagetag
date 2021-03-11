{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Text.LanguageTag.Internal.BCP47.Validate.Orphans
-- Description : Orphan instances for the subtag types
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module contains all of the orphan 'Show' instances for the
-- different registered subtag types. Also see
-- "Text.LanguageTag.Internal.BCP47.Validate.DataConShow".
module Text.LanguageTag.Internal.BCP47.Validate.Orphans where

import Text.LanguageTag.Internal.BCP47.Validate.DataConShow
import Text.LanguageTag.Internal.BCP47.Validate.Extlang
import Text.LanguageTag.Internal.BCP47.Validate.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Validate.Language
import Text.LanguageTag.Internal.BCP47.Validate.LanguageRecords
import Text.LanguageTag.Internal.BCP47.Validate.Region
import Text.LanguageTag.Internal.BCP47.Validate.RegionRecords
import Text.LanguageTag.Internal.BCP47.Validate.Script
import Text.LanguageTag.Internal.BCP47.Validate.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Validate.Types
import Text.LanguageTag.Internal.BCP47.Validate.Variant
import Text.LanguageTag.Internal.BCP47.Validate.VariantRecords

instance Show Language where
  show = languageConShow . languageToSubtag

instance Show Extlang where
  show = extlangConShow . extlangToSubtag

instance Show Region where
  show = regionConShow . regionToSubtag

instance Show Script where
  show = scriptConShow . scriptToSubtag

instance Show Variant where
  show = variantConShow . variantToSubtag

deriving instance Show LanguageRecord

deriving instance Show ExtlangRecord

deriving instance Show ScriptRecord

deriving instance Show RegionRecord

deriving instance Show VariantRecord

deriving instance Show RangeRecord

-- FIXME: temporary show instances
deriving instance Show Normal

deriving instance Show BCP47Tag
