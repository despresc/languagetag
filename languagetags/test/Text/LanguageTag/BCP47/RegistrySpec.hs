module Text.LanguageTag.BCP47.RegistrySpec (spec) where

import Data.Either (fromRight)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( arbitrary,
    chooseInt,
    forAll,
    forAllShrink,
    getNonEmpty,
    shrink,
    (===),
  )
import Text.LanguageTag.BCP47.Registry
  ( recognizeRedundantNormal,
    redundantToNormalTag,
    redundantToValidTag,
    toSyntaxTag,
  )
import Text.LanguageTag.BCP47.Registry.Redundant (recognizeRedundantTag, redundantTrie)
import qualified Text.LanguageTag.BCP47.Subtag.Trie as Trie
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.BCP47.Validation (validateBCP47)
import Text.LanguageTag.Internal.BCP47.Registry.Types (unsafeBinSearchIndexOn)

spec :: Spec
spec = do
  describe "binSearchIndexOn" $ do
    -- this one only tests vectors of strictly increasing elements,
    -- but that should be enough, given the other match finding test
    prop "behaves like Vector findIndex" $
      forAllShrink
        (V.uniq . V.fromList . List.sort . getNonEmpty <$> arbitrary)
        (filter (not . V.null) . fmap (V.uniq . V.fromList . List.sort) . shrink . V.toList)
        $ \v ->
          forAll (chooseInt (0, V.length v - 1)) $ \idx ->
            forAll (chooseInt (-1, 1)) $ \didx ->
              let el = V.unsafeIndex v idx + fromEnum didx
               in unsafeBinSearchIndexOn id el v === V.findIndex (== el) v
    prop "actually finds matches" $
      forAllShrink
        (V.fromList . List.sort . getNonEmpty <$> arbitrary)
        (filter (not . V.null) . fmap (V.fromList . List.sort) . shrink . V.toList)
        $ \v ->
          forAll (chooseInt (0, V.length v - 1)) $ \idx ->
            let el = V.unsafeIndex v idx :: Int
             in (V.unsafeIndex v <$> unsafeBinSearchIndexOn id el v) === Just el
  describe "toSyntaxTag" $ do
    prop "composes with validateBCP47 correctly on the right" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        validateBCP47 (toSyntaxTag tg) === Right tg
  describe "redundantTrie" $ do
    let errSyn = fromRight (error "ill-formed redundant trie tag") . Syn.parseBCP47FromSubtags
    let errVal = fromRight (error "invalid redundant trie tag") . validateBCP47
    let rendpath (sts, r) = (errVal $ errSyn $ NE.fromList sts, r)
    let redpaths = List.sortOn snd $ rendpath <$> Trie.fromTrie redundantTrie
    it "has paths that validate to their nodes" $ do
      List.find (\(x, y) -> recognizeRedundantTag x /= Just y) redpaths `shouldBe` Nothing
    it "has exactly the list of redundant tags as nodes" $ do
      findFirstDiff (snd <$> redpaths) [minBound .. maxBound] `shouldBe` Nothing
  describe "recognizeRedundant" $ do
    it "recognizes all of the redundant tags correctly" $ do
      let rendred = redundantToNormalTag <$> [minBound .. maxBound]
      let unrecog x = case recognizeRedundantNormal x of
            Just a
              | redundantToNormalTag a == x -> Nothing
            _ -> Just x
      mapMaybe unrecog rendred `shouldBe` []
    prop "recognizes redundant tags like a simple find would" $
      forAllShrink genValidTag shrinkValidTag $ \tg -> do
        let rendred = [minBound .. maxBound]
        recognizeRedundantTag tg === List.find ((== tg) . redundantToValidTag) rendred
