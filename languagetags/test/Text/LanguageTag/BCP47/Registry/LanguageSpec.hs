-- |
-- Description : Testing language subtag registry data and validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Registry.LanguageSpec (spec) where

import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Text.LanguageTag.BCP47.Canonicalization (canonicalizeLanguage)
import Text.LanguageTag.BCP47.Registry
  ( Scope (..),
  )
import Text.LanguageTag.BCP47.Registry.Language
  ( Language,
    LanguageRecord (..),
    languageToSubtag,
    lookupLanguageRecord,
  )
import Text.LanguageTag.BCP47.Validation (validateLanguage)
import Text.LanguageTag.Internal.BCP47.Registry.LanguageRecords (languageDetails)

spec :: Spec
spec = do
  let langtags = [minBound .. maxBound :: Language]
  describe "languageToSubtag" $ do
    it "composes on the right with validateLanguage correctly" $ do
      badRound (validateLanguage . languageToSubtag) `shouldNotMatch` langtags
  describe "lookupLanguageRecord" $ do
    it "has a canonical preferred value, if applicable" $ do
      let depNonCanon x =
            maybePreferred (languageDeprecation $ lookupLanguageRecord x)
              >>= badRound' canonicalizeLanguage
      depNonCanon `shouldNotMatch` langtags
    it "has a macrolanguage macrolanguage, if applicable" $ do
      let getMacroScope x =
            languageScope . lookupLanguageRecord
              <$> languageMacrolanguage (lookupLanguageRecord x)
      let badMacro x = case getMacroScope x of
            Just (Just Macrolanguage) -> Nothing
            Just y -> Just (x, y)
            Nothing -> Nothing
      badMacro `shouldNotMatch` langtags
  describe "languageDetails" $ do
    it "has the same number of entries as the Language type has constructors" $ do
      V.length languageDetails `shouldBe` fromEnum (maxBound :: Language) + 1
    it "isSorted" $ do
      badSortOnPair fst languageDetails `shouldBe` Nothing
