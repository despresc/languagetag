-- |
-- Description : Testing redundant tag registry data and validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Registry.RedundantSpec (spec) where

import Data.Either (fromRight)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( forAllShrink,
    (===),
  )
import Text.LanguageTag.BCP47.Canonicalization (canonicalizeNormal)
import Text.LanguageTag.BCP47.Registry.Redundant
  ( RangeRecord (..),
    Redundant,
    lookupRedundantRecord,
    recognizeRedundantNormal,
    recognizeRedundantTag,
    redundantToNormalTag,
    redundantToValidTag,
    redundantTrie,
  )
import qualified Text.LanguageTag.BCP47.Subtag.Trie as Trie
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.BCP47.Validation (validateBCP47)
import Text.LanguageTag.Internal.BCP47.Registry.RedundantRecords (redundantDetails)

spec :: Spec
spec = do
  let redundanttags = [minBound .. maxBound :: Redundant]
  describe "redundantTrie" $ do
    let errSyn = fromRight (error "ill-formed redundant trie tag") . Syn.parseBCP47FromSubtags
    let errVal = fromRight (error "invalid redundant trie tag") . validateBCP47
    let rendpath (sts, r) = (errVal $ errSyn $ NE.fromList sts, r)
    let redpaths = List.sortOn snd $ rendpath <$> Trie.fromTrie redundantTrie
    it "has paths that validate to their nodes" $ do
      List.find (\(x, y) -> recognizeRedundantTag x /= Just y) redpaths `shouldBe` Nothing
    it "has exactly the list of redundant tags as nodes" $ do
      findFirstDiff (snd <$> redpaths) [minBound .. maxBound] `shouldBe` Nothing
  describe "recognizeRedundantNormal" $ do
    it "recognizes all of the redundant tags correctly" $ do
      let rendred = redundantToNormalTag <$> [minBound .. maxBound]
      badRound (fmap redundantToNormalTag . recognizeRedundantNormal) `shouldNotMatch` rendred
    prop "recognizes redundant tags like a simple find would" $
      forAllShrink genValidTag shrinkValidTag $ \tg -> do
        recognizeRedundantTag tg === List.find ((== tg) . redundantToValidTag) redundanttags
  describe "lookupRedundantRecord" $ do
    it "has only canonical preferred values" $ do
      let depNonCanon x =
            maybePreferred (rangeDeprecation $ lookupRedundantRecord x)
              >>= badRound' (snd . canonicalizeNormal)
      depNonCanon `shouldNotMatch` redundanttags
  describe "redundantDetails" $ do
    it "has the same number of entries as the Redundant type has constructors" $ do
      V.length redundantDetails `shouldBe` fromEnum (maxBound :: Redundant) + 1
    it "isSorted" $ do
      badSortOnPair (\(x, _, _) -> x) redundantDetails `shouldBe` Nothing
