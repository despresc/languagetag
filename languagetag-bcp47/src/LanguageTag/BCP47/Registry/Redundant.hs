-- |
-- Description : Redundant subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Redundant' type, which enumerates all of
-- the redundant tags in the IANA registry, the current version of
-- which (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module LanguageTag.BCP47.Registry.Redundant
  ( -- * Redundant tags
    Redundant (..),

    -- * Recognition
    recognizeRedundantNormal,
    recognizeRedundantTag,

    -- * Rendering and conversion
    renderRedundant,
    renderRedundantBuilder,
    redundantToSyntaxTag,
    redundantToNormalTag,
    redundantToValidTag,

    -- * Records and lookup
    RangeRecord (..),
    lookupRedundantRecord,

    -- * Redundant tag trie
    redundantTrie,
  )
where

import Data.Maybe (catMaybes)
import qualified Data.Set as S
import LanguageTag.BCP47.LegacyTag
import LanguageTag.Internal.BCP47.Registry.ExtlangRecords (extlangToSubtag)
import LanguageTag.Internal.BCP47.Registry.LanguageRecords (languageToSubtag)
import LanguageTag.Internal.BCP47.Registry.RedundantRecords
import qualified LanguageTag.Internal.BCP47.Registry.RedundantRecords as Internal
import LanguageTag.Internal.BCP47.Registry.RegionRecords (regionToSubtag)
import LanguageTag.Internal.BCP47.Registry.ScriptRecords (scriptToSubtag)
import LanguageTag.Internal.BCP47.Registry.Types
import LanguageTag.Internal.BCP47.Registry.VariantRecords (variantToSubtag)

-- | Convert a 'Redundant' tag to a valid 'Normal' tag
redundantToNormalTag :: Redundant -> Normal
redundantToNormalTag = Internal.redundantToValidNormal

-- | Convert a 'Redundant' tag to a valid 'BCP47' tag
redundantToValidTag :: Redundant -> BCP47
redundantToValidTag = NormalTag . redundantToNormalTag

-- | Determine whether or not a valid 'BCP47' tag represents a
-- 'Redundant' tag
recognizeRedundantNormal :: Normal -> Maybe Redundant
recognizeRedundantNormal n
  | null $ extensions n,
    null $ privateUse n =
    recognizeRedundant $ l' : (catMaybes [e', s', r'] <> vs')
  | otherwise = Nothing
  where
    l' = languageToSubtag $ language n
    e' = extlangToSubtag <$> extlang n
    s' = scriptToSubtag <$> script n
    r' = regionToSubtag <$> region n
    -- not the correct presentation in general, but none of the
    -- redundant tags have more than one variant
    vs' = fmap variantToSubtag $ S.toAscList $ variants n

-- | Determine whether or not a valid 'BCP47' tag represents a
-- 'Redundant' tag
recognizeRedundantTag :: BCP47 -> Maybe Redundant
recognizeRedundantTag (NormalTag n) = recognizeRedundantNormal n
recognizeRedundantTag _ = Nothing
