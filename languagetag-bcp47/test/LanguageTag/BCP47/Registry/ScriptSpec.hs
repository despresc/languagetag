-- |
-- Description : Testing script subtag registry data and validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module LanguageTag.BCP47.Registry.ScriptSpec (spec) where

import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import LanguageTag.BCP47.Registry
  ( Deprecation (..),
  )
import LanguageTag.BCP47.Registry.Script
  ( Script,
    ScriptRecord (..),
    lookupScriptRecord,
    scriptToSubtag,
  )
import LanguageTag.BCP47.Validation (validateScript)
import LanguageTag.Internal.BCP47.Registry.ScriptRecords (scriptDetails)

spec :: Spec
spec = do
  let scripttags = [minBound .. maxBound :: Script]
  let canonScript x = case scriptDeprecation $ lookupScriptRecord x of
        NotDeprecated -> Just x
        _ -> Nothing
  describe "scriptToSubtag" $ do
    it "composes on the right with validateScript correctly" $ do
      badRound (validateScript . scriptToSubtag) `shouldNotMatch` scripttags
  describe "lookupScriptRecord" $ do
    it "has a canonical preferred value, if applicable" $ do
      let depNonCanon x =
            maybePreferred (scriptDeprecation $ lookupScriptRecord x)
              >>= badRound canonScript
      depNonCanon `shouldNotMatch` scripttags
  describe "scriptDetails" $ do
    it "has the same number of entries as the Script type has constructors" $ do
      V.length scriptDetails `shouldBe` fromEnum (maxBound :: Script) + 1
    it "isSorted" $ do
      badSortOnPair fst scriptDetails `shouldBe` Nothing
