{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module LanguageTag.BCP47.TrieSpec (spec) where

import qualified Data.List as List
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( Gen,
    Property,
    Testable,
    arbitrary,
    chooseInt,
    forAll,
    forAllBlind,
    forAllShrink,
    liftArbitrary,
    oneof,
    shrink,
    vectorOf,
    (.&&.),
    (===),
  )
import LanguageTag.BCP47.Subtag (Subtag)
import LanguageTag.BCP47.Subtag.Trie (Trie)
import qualified LanguageTag.BCP47.Subtag.Trie as Trie

genIntTrie :: Gen (Trie Int)
genIntTrie = liftGenTrie arbitrary

shrinkIntTrie :: Trie Int -> [Trie Int]
shrinkIntTrie = liftShrinkTrie shrink

forAll' :: Testable prop => (Trie Int -> prop) -> Property
forAll' = forAllShrink genIntTrie shrinkIntTrie

forAllEntry :: Testable prop => (([Subtag], Int) -> prop) -> Property
forAllEntry = forAllShrink genE shrinkE
  where
    genE = (,) <$> (liftArbitrary genSubtag :: Gen [Subtag]) <*> (arbitrary :: Gen Int)
    shrinkE (x, y) =
      [(x', y) | x' <- shrinkListWith shrinkSubtag x]
        <> [(x, y') | y' <- shrink (y :: Int)]

forAllPath :: Testable prop => ([Subtag] -> prop) -> Property
forAllPath = forAllShrink (liftArbitrary genSubtag) (shrinkListWith shrinkSubtag)

forAllSemi :: Testable prop => (Trie [Int] -> prop) -> Property
forAllSemi = forAllShrink (liftGenTrie arbitrary) (liftShrinkTrie shrink)

addNodes :: Trie a -> [(Maybe a, [Subtag])] -> Trie a
addNodes !x ((mn, p) : ps) = case mn of
  Nothing -> addNodes x ps
  Just n -> addNodes (Trie.insert p n x) ps
addNodes x [] = x

-- Perhaps not the cleanest. This exists because sometimes we want to
-- make sure that particular branches in a trie are relatively
-- populated.
perturbPath ::
  (Show a, Testable prop) =>
  Gen a ->
  (a -> [a]) ->
  [Subtag] ->
  Trie a ->
  (Trie a -> prop) ->
  Property
perturbPath gen shr pth t = flip forAllShrink (liftShrinkTrie shr) $
  do
    let len = length pth
    extras <- vectorOf 4 genSubtag
    pathNodes <- vectorOf (len + 4) $ oneof [Just <$> gen, pure Nothing]
    pure $ addNodes t $ drop 1 $ zip pathNodes $ List.inits $ pth <> extras

spec :: Spec
spec = do
  let genP = (,) <$> (liftArbitrary genSubtag :: Gen [Subtag]) <*> (arbitrary :: Gen Int)
  let shrinkP (x, y) =
        [(x', y) | x' <- shrinkListWith shrinkSubtag x]
          <> [(x, y') | y' <- shrink (y :: Int)]
  describe "toTrie" $ do
    prop "is the left inverse of fromTrie" $
      forAll' $ \t ->
        Trie.toTrie (Trie.fromTrie t) === t
    prop "is sort of the right inverse of fromTrie" $
      forAll' $ \t ->
        let t' = Trie.fromTrie t
         in Trie.fromTrie (Trie.toTrie t') === t'
  describe "insert" $ do
    prop "inserts in the correct place according to lookup" $
      forAll' $ \t ->
        forAllShrink genP shrinkP $ \(p, a) ->
          Trie.lookup p (Trie.insert p a t) === Just a
    prop "disturbs nothing else according to delete" $
      forAll' $ \t ->
        forAllShrink genP shrinkP $ \(p, a) ->
          Trie.delete p (Trie.insert p a t) === Trie.delete p t
  describe "singleton" $ do
    prop "composes with lookup correctly" $
      forAllShrink genP shrinkP $ \(p, a) ->
        Trie.lookup p (Trie.singleton p a) === Just a
    prop "composes with delete correctly" $
      forAllShrink genP shrinkP $ \(p, a) ->
        Trie.delete p (Trie.singleton p a) === Trie.TrieNil
  describe "(<>)" $ do
    prop "is associative" $
      forAllSemi $ \t1 ->
        forAllSemi $ \t2 ->
          forAllSemi $ \t3 ->
            t1 <> (t2 <> t3) === (t1 <> t2) <> t3
    prop "has mempty as left identity" $
      forAllSemi $ \t ->
        mempty <> t === t
    prop "has mempty as right identity" $
      forAllSemi $ \t ->
        t <> mempty === t
  describe "lookupLax" $ do
    prop "composes with insert correctly" $
      forAll' $ \t ->
        forAllEntry $ \(p, a) ->
          perturbPath arbitrary shrink p t $ \t' ->
            let t'' = Trie.insert p a t'
             in Trie.lookupLax p t'' === Just (a, [])
    prop "composes with insert correctly after pruning" $
      forAll' $ \t ->
        forAllEntry $ \(p, a) ->
          perturbPath arbitrary shrink p t $ \t' ->
            forAllPath $ \p' ->
              Trie.lookupLax (p <> p') (Trie.prunePast p $ Trie.insert p a t')
                === Just (a, p')
    -- tests that the accumulator in lookupLax actually does what it's
    -- intended to do, by ensuring that the path in the lookup ends at
    -- a Nothing in a branch that looks like:
    --- Just a ---> Nothing ---> Just a'. again, not the cleanest.
    prop "composes with insert correctly on a near-miss" $
      forAll' $ \t ->
        forAllEntry $ \(p, a) ->
          forAll ((,) <$> genSubtag <*> genSubtag) $ \(x, y) ->
            forAllBlind arbitrary $ \a' ->
              let pAt = p <> [x]
                  pBefore = p
                  pAfter = p <> [x, y]
               in perturbPath arbitrary shrink pAfter t $ \t' ->
                    let t'' = Trie.insert pBefore a $ Trie.insert pAfter a' $ Trie.delete pAt t'
                     in Trie.lookupLax pAt t'' === Just (a, [x])
  -- A small test of the manual Foldable instance, and I suppose of
  -- the semigroup instance too
  describe "length" $ do
    prop "adds on disjoint tries" $ do
      let disSplit t = do
            let t' = Trie.fromTrie t
            let l = length t'
            let q = l `div` 4
            s <- chooseInt (q, l - q)
            let (x, y) = splitAt s t'
            pure (Trie.toTrie x, Trie.toTrie y)
      forAllSemi $ \t ->
        forAll (disSplit t) $ \(u, v) ->
          length u + length v === length (u <> v) .&&. length (u <> v) === length t
    prop "is sub-additive on all tries" $ do
      forAllSemi $ \t ->
        forAllSemi $ \u ->
          length (t <> u) <= length t + length u
