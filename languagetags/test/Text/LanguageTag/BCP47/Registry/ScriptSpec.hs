-- |
-- Description : Testing script subtag registry data and validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Registry.ScriptSpec (spec) where

import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Text.LanguageTag.BCP47.Canonicalization (canonicalizeScript)
import Text.LanguageTag.BCP47.Registry.Script
  ( Script,
    ScriptRecord (..),
    lookupScriptRecord,
    scriptToSubtag,
  )
import Text.LanguageTag.BCP47.Validation (validateScript)
import Text.LanguageTag.Internal.BCP47.Registry.ScriptRecords (scriptDetails)

spec :: Spec
spec = do
  let scripttags = [minBound .. maxBound :: Script]
  describe "scriptToSubtag" $ do
    it "composes on the right with validateScript correctly" $ do
      badRound (validateScript . scriptToSubtag) `shouldNotMatch` scripttags
  describe "lookupScriptRecord" $ do
    it "has a canonical preferred value, if applicable" $ do
      let depNonCanon x =
            maybePreferred (scriptDeprecation $ lookupScriptRecord x)
              >>= badRound' (snd . canonicalizeScript)
      depNonCanon `shouldNotMatch` scripttags
  describe "scriptDetails" $ do
    it "has the same number of entries as the Script type has constructors" $ do
      V.length scriptDetails `shouldBe` fromEnum (maxBound :: Script) + 1
    it "isSorted" $ do
      badSortOnPair fst scriptDetails `shouldBe` Nothing
