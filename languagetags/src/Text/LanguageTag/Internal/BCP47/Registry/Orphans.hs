{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Text.LanguageTag.Internal.BCP47.Registry.Orphans
-- Description : Orphan instances for the subtag types
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module contains all of the orphan 'Show' instances for the
-- different registered subtag types. Also see
-- "Text.LanguageTag.Internal.BCP47.Registry.DataConShow".
module Text.LanguageTag.Internal.BCP47.Registry.Orphans where

import Text.LanguageTag.Internal.BCP47.Registry.DataConShow
import Text.LanguageTag.Internal.BCP47.Registry.Extlang
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Registry.Language
import Text.LanguageTag.Internal.BCP47.Registry.LanguageRecords
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.RegionRecords
import Text.LanguageTag.Internal.BCP47.Registry.Script
import Text.LanguageTag.Internal.BCP47.Registry.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Text.LanguageTag.Internal.BCP47.Registry.Variant
import Text.LanguageTag.Internal.BCP47.Registry.VariantRecords

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
