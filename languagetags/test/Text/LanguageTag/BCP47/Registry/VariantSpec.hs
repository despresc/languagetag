-- |
-- Description : Testing variant subtag registry data and validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Registry.VariantSpec (spec) where

import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Text.LanguageTag.BCP47.Canonicalization
  ( canonicalizeNormal,
    canonicalizeVariant,
  )
import Text.LanguageTag.BCP47.Registry.Variant
  ( Variant,
    VariantRecord (..),
    lookupVariantRecord,
    variantToSubtag,
  )
import Text.LanguageTag.BCP47.Validation (validateVariant)
import Text.LanguageTag.Internal.BCP47.Registry.VariantRecords (variantDetails)

spec :: Spec
spec = do
  let varianttags = [minBound .. maxBound :: Variant]
  describe "variantToSubtag" $ do
    it "composes on the right with validateVariant correctly" $ do
      badRound (validateVariant . variantToSubtag) `shouldNotMatch` varianttags
  describe "lookupVariantRecord" $ do
    it "has a canonical preferred value, if applicable" $ do
      let depNonCanon x =
            maybePreferred (variantDeprecation $ lookupVariantRecord x)
              >>= badRound' canonicalizeVariant
      depNonCanon `shouldNotMatch` varianttags
    it "has canonical prefixes, if applicable" $ do
      let depNonCanon x = case badPrefs of
            (a : _) -> Just a
            _ -> Nothing
            where
              badPrefs =
                mapMaybe (badRound' canonicalizeNormal) $
                  variantPrefixes $ lookupVariantRecord x
      depNonCanon `shouldNotMatch` varianttags
  describe "variantDetails" $ do
    it "has the same number of entries as the Variant type has constructors" $ do
      V.length variantDetails `shouldBe` fromEnum (maxBound :: Variant) + 1
    it "isSorted" $ do
      badSortOnPair fst variantDetails `shouldBe` Nothing
