{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Text.LanguageTag.BCP47.Subtag.Trie
-- Description : Tries indexed by subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Subtag.Trie
  ( Trie,
    nullTrie,
    trie,
    singletonTrie,
    leafTrie,
    pathTrie,
    trieStep,
    stepBranch,
    stepPath,
    stepLeaf,
    adjustTriePath,
    lookupTrie,
    lookupTrieLax,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import Text.LanguageTag.Internal.BCP47.Subtag.Trie

-- | Construct a trie with a possibly-empty root node and the given
-- children. In case of duplicates 'Subtag' entries in the list, the
-- rightmost is kept.
trie :: Maybe a -> [(Subtag, Trie a)] -> Trie a
trie a = Trie a . HM.fromList

-- | The empty 'Trie'
nullTrie :: Trie a
nullTrie = Trie Nothing mempty

-- | Create a trie with a single path with the given value at the
-- terminal node
singletonTrie :: a -> [Subtag] -> Trie a
singletonTrie x (st : sts) = Trie Nothing $ HM.singleton st $ singletonTrie x sts
singletonTrie x [] = Trie (Just x) mempty

singletonTrie' :: Maybe a -> [Subtag] -> Trie a
singletonTrie' x (st : sts) = Trie Nothing $ HM.singleton st $ singletonTrie' x sts
singletonTrie' x [] = Trie x mempty

-- | Create a trie with only the given node and no children
leafTrie :: a -> Trie a
leafTrie x = Trie (Just x) mempty

-- | Modify the node at the end of the given path using the given
-- function
adjustTriePath :: [Subtag] -> (Maybe a -> Maybe a) -> Trie a -> Trie a
adjustTriePath (st : sts) f (Trie x m) = Trie x $ HM.alter go st m
  where
    go Nothing = Just $ singletonTrie' (f Nothing) sts
    go (Just t) = Just $ adjustTriePath sts f t
adjustTriePath [] f (Trie x m) = Trie (f x) m

-- | Construct a trie by gathering paths with common indices together,
-- preferring the rightmost path if there are duplicates

-- FIXME: may have more efficient implementation
pathTrie :: [([Subtag], a)] -> Trie a
pathTrie = List.foldl' go nullTrie
  where
    go t (sts, a) = adjustTriePath sts (const $ Just a) t

-- | Construct a trie from its children, preferring the rightmost
-- child if there are duplicates
trieStep :: Maybe a -> [TrieStep a] -> Trie a
trieStep x = trie x . fmap go
  where
    go (TrieStep k v) = (k, v)

-- | Construct a child 'Trie' from its label, node content, and
-- children
stepBranch :: Subtag -> Maybe a -> [TrieStep a] -> TrieStep a
stepBranch k v = TrieStep k . trieStep v

-- | Construct a child 'Trie' with the the given value as its node
-- content
stepLeaf :: Subtag -> a -> TrieStep a
stepLeaf k = TrieStep k . leafTrie

-- | Construct a child 'Trie' that consists of a single path with the
-- given value at its end
stepPath :: Subtag -> [Subtag] -> a -> TrieStep a
stepPath k ks = TrieStep k . flip singletonTrie ks

-- | Find the entry in a 'Trie' corresponding to the given list
-- of subtags exactly
lookupTrie :: [Subtag] -> Trie a -> Maybe a
lookupTrie (x : xs) (Trie _ m) = HM.lookup x m >>= lookupTrie xs
lookupTrie [] (Trie a _) = a

-- | Find the entry in a 'Trie' that best matches the given list
-- of subtags. This is "lookup" in the sense of BCP47: in effect,
-- subtags are successively dropped from the end of the list until an
-- entry is found.
lookupTrieLax :: [Subtag] -> Trie a -> Maybe a
lookupTrieLax l (Trie ma m) = go ma l m
  where
    go !mnode (x : xs) mp = case HM.lookup x mp of
      Nothing -> mnode
      Just (Trie mnode' mp') -> go (mnode' <|> mnode) xs mp'
    go !mnode [] _ = mnode
