-- |
-- Description : Testing grandfathered tag registry data
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Registry.GrandfatheredSpec (spec) where

import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Text.LanguageTag.BCP47.Canonicalization (lintBCP47)
import Text.LanguageTag.BCP47.Registry (BCP47 (..))
import Text.LanguageTag.BCP47.Registry.Grandfathered
  ( Grandfathered,
    RangeRecord (..),
    lookupGrandfatheredRecord,
  )
import Text.LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords (grandfatheredDetails)

spec :: Spec
spec = do
  let grandfatheredtags = [minBound .. maxBound :: Grandfathered]
  describe "lookupGrandfatheredRecord" $ do
    it "has values that lint cleanly to themselves" $ do
      let depNonCanon x =
            maybePreferred (rangeDeprecation $ lookupGrandfatheredRecord x)
              >>= badLint
          badLint t = case lintBCP47 (NormalTag t) of
            (w, NormalTag t')
              | w == mempty && t == t' -> Nothing
            _ -> Just t
      depNonCanon `shouldNotMatch` grandfatheredtags
  describe "grandfatheredDetails" $ do
    it "has the same number of entries as the Grandfathered type has constructors" $ do
      V.length grandfatheredDetails `shouldBe` fromEnum (maxBound :: Grandfathered) + 1
