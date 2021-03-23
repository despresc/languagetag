-- |
-- Description : Testing extlang subtag registry data and validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Registry.ExtlangSpec (spec) where

import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Text.LanguageTag.BCP47.Registry
  ( Deprecation (..),
    Scope (..),
    languageDeprecation,
    languageScope,
    lookupLanguageRecord,
  )
import Text.LanguageTag.BCP47.Registry.Extlang
  ( Extlang,
    ExtlangRecord (..),
    extlangToSubtag,
    lookupExtlangRecord,
  )
import Text.LanguageTag.BCP47.Validation (validateExtlang)
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords (extlangDetails)

spec :: Spec
spec = do
  let extlangtags = [minBound .. maxBound :: Extlang]
  describe "extlangToSubtag" $ do
    it "composes on the right with validateExtlang correctly" $ do
      badRound (validateExtlang . extlangToSubtag) `shouldNotMatch` extlangtags
  describe "lookupExtlangRecord" $ do
    it "has the correct preferred language" $ do
      -- sort of hacky, of course
      let badPreferred x = drop 3 (show x) /= show (extlangPreferredValue $ lookupExtlangRecord x)
      badPreferred `shouldNotFind` extlangtags
    it "has a macrolanguage macrolanguage, if applicable" $ do
      let getMacroScope x =
            languageScope . lookupLanguageRecord
              <$> extlangMacrolanguage (lookupExtlangRecord x)
      let badMacro x = case getMacroScope x of
            Just (Just Macrolanguage) -> Nothing
            Just y -> Just (x, y)
            Nothing -> Nothing
      badMacro `shouldNotMatch` extlangtags
    it "has a deprecated preferred value exactly when it is deprecated" $ do
      let badPref x = case languageDeprecation $ lookupLanguageRecord pref of
            NotDeprecated -> isdep
            _ -> not isdep
            where
              recd = lookupExtlangRecord x
              pref = extlangPreferredValue recd
              isdep = extlangDeprecation recd
      badPref `shouldNotFind` extlangtags
    it "has a deprecated preferred value that itself has no preferred value, if applicable" $ do
      let badPref x = case languageDeprecation $ lookupLanguageRecord pref of
            DeprecatedPreferred _ -> True
            _ -> False
            where
              recd = lookupExtlangRecord x
              pref = extlangPreferredValue recd
      badPref `shouldNotFind` extlangtags
  describe "extlangDetails" $ do
    it "has the same number of entries as the Extlang type has constructors" $ do
      V.length extlangDetails `shouldBe` fromEnum (maxBound :: Extlang) + 1
    it "isSorted" $ do
      badSortOnPair fst extlangDetails `shouldBe` Nothing
