{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Description : Tries indexed by subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports a strict 'Trie' type with paths indexed with
-- 'Subtag' values. Most relevant for language tags analysis are the
-- 'toTrie' function for constructing a 'Trie' and 'lookup' and
-- 'lookupLax' for querying them. Tries can also be constructed with
-- 'stepTrie', 'leaf', 'branch', and 'path' if that is more
-- convenient.
module Text.LanguageTag.BCP47.Subtag.Trie
  ( Trie (..),
    Step (..),

    -- * Lookup and query
    lookup,
    lookupLax,
    lookupSub,

    -- * Direct construction
    empty,
    root,
    singleton,
    stepTrie,
    leaf,
    path,
    branch,

    -- * Modification
    insert,
    delete,
    prune,
    prunePast,
    raise,
    alterSub,
    modCreate,

    -- * Conversion to and from non-empty tries
    nonEmpty,
    possiblyEmpty,

    -- * Converse to and from lists
    toTrie,
    toStep,
    fromTrie,
    fromStep,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Text.LanguageTag.BCP47.Subtag.NonEmptyTrie as NET
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import qualified Text.LanguageTag.Internal.BCP47.Subtag.NonEmptyTrie as NET
import Text.LanguageTag.Internal.BCP47.Subtag.Trie
import Prelude hiding (lookup, null)

-- | Attempt to convert a possibly-empty 'Trie' to a non-empty
-- 'NET.Trie', failing if the input is empty. This is the inverse of
-- 'possiblyEmpty'.
nonEmpty :: Trie a -> Maybe (NET.Trie a)
nonEmpty (Trie n) = Just n
nonEmpty TrieNil = Nothing

-- | Convert 'Just' a non-empty 'NET.Trie' to its corresponding
-- 'Trie', and convert 'Nothing' to an empty 'Trie'. This is the
-- inverse of 'nonEmpty'.
possiblyEmpty :: Maybe (NET.Trie a) -> Trie a
possiblyEmpty (Just t) = Trie t
possiblyEmpty Nothing = TrieNil

-- | Lift an operation on non-empty tries that may fail to return a
-- non-empty trie to an operation on possibly-empty tries that returns
-- the empty trie on failure
joinNonEmpty :: (NET.Trie a -> Maybe (NET.Trie a)) -> Trie a -> Trie a
joinNonEmpty f (Trie n) = maybe TrieNil Trie $ f n
joinNonEmpty _ TrieNil = TrieNil

-- | Construct a 'Trie' by gathering paths with common indices
-- together, preferring the rightmost path if there are duplicates
toTrie :: [([Subtag], a)] -> Trie a
toTrie = List.foldl' go empty
  where
    go t (sts, a) = insert sts a t

-- | Construct a 'Step' by gathering paths with common indices
-- together, preferring the rightmost path if there are duplicates
toStep :: Subtag -> [([Subtag], a)] -> Step a
toStep s = Step s . toTrie

-- | The empty 'Trie'
empty :: Trie a
empty = TrieNil

-- | Insert a node into a 'Trie' at the end of the given 'Subtag' path
insert :: [Subtag] -> a -> Trie a -> Trie a
insert l a = Trie . insertToNonEmpty l a

-- | Modify, create, or delete the sub-'Trie' at the given path
alterSub :: (Trie a -> Trie a) -> [Subtag] -> Trie a -> Trie a
alterSub f l (Trie t) = possiblyEmpty $ NET.alterSub (nonEmpty . f . possiblyEmpty) l t
alterSub f l TrieNil = case f TrieNil of
  Trie t -> Trie $ NET.raise l t
  TrieNil -> TrieNil

-- | Transform the sub-'Trie' at the given path into a non-empty
-- 'NET.Trie', or create a 'NET.Trie' there, returning the non-empty
-- result. (Note that the output trie types are different from the
-- input trie types).
modCreate :: (Trie a -> NET.Trie a) -> [Subtag] -> Trie a -> NET.Trie a
modCreate f l (Trie n) = NET.modCreate go l n
  where
    go Nothing = f TrieNil
    go (Just t) = f $ Trie t
modCreate f l TrieNil = NET.raise l (f TrieNil)

-- | Insert a node into a 'Trie' at the end of the given 'Subtag'
-- path, returning a non-empty 'NET.Trie'
insertToNonEmpty :: [Subtag] -> a -> Trie a -> NET.Trie a
insertToNonEmpty l a (Trie x) = NET.insert l a x
insertToNonEmpty l a TrieNil = NET.singleton l a

-- | Construct a 'Step' with the given element at its end and with no
-- children
leaf :: Subtag -> a -> Step a
leaf s = Step s . root

-- | Construct a 'Trie' with the given element as its root node and
-- with no children
root :: a -> Trie a
root = Trie . NET.root

-- | Construct a 'Step' from its children, preferring the rightmost
-- child if there are duplicates
branch :: Subtag -> Maybe a -> [Step a] -> Step a
branch s x = Step s . stepTrie x

-- | Construct a 'Step' consisting of the given path with the given
-- element at its end
path :: Subtag -> [Subtag] -> a -> Step a
path x xs = Step x . singleton xs

-- | Construct a 'Trie' from its children, preferring the rightmost
-- child if there are duplicates
stepTrie :: Maybe a -> [Step a] -> Trie a
stepTrie x =
  maybe TrieNil (Trie . NET.stepTrie x) . NE.nonEmpty
    . mapMaybe (\(Step k v) -> NET.Step k <$> nonEmpty v)

-- | Find the entry in a 'Trie' corresponding to the given list
-- of subtags exactly
lookup :: [Subtag] -> Trie a -> Maybe a
lookup l (Trie x) = NET.lookup l x
lookup _ TrieNil = Nothing

-- | Find the entry in a 'Trie' that best matches the given list of
-- subtags. This is "lookup" in the sense of BCP47: in effect, subtags
-- are successively dropped from the end of the list until an entry is
-- found. The entries that were dropped in order to find a match are
-- also returned.
lookupLax :: [Subtag] -> Trie a -> Maybe (a, [Subtag])
lookupLax l (Trie n) = NET.lookupLax l n
lookupLax _ TrieNil = Nothing

-- | Return the sub-'Trie' rooted at the given node
lookupSub :: [Subtag] -> Trie a -> Trie a
lookupSub l (Trie n) = possiblyEmpty $ NET.lookupSub l n
lookupSub _ TrieNil = TrieNil

-- | Construct a 'Trie' with the given element at the end of the given
-- path
singleton :: [Subtag] -> a -> Trie a
singleton l = Trie . NET.singleton l

-- | Construct a 'Trie' with exactly the given sub-'Trie' at the end
-- of the path
raise :: [Subtag] -> Trie a -> Trie a
raise l (Trie t) = Trie $ NET.raise l t
raise _ TrieNil = TrieNil

-- | Delete the given node if it exists
delete :: [Subtag] -> Trie a -> Trie a
delete = joinNonEmpty . NET.delete

-- | Delete the given node and all of its children, if it exists
prune :: [Subtag] -> Trie a -> Trie a
prune = alterSub $ const TrieNil

-- | Delete all of the children of the given node
prunePast :: [Subtag] -> Trie a -> Trie a
prunePast = alterSub go
  where
    go (Trie (NET.Branch (NET.Node a) _ _)) = Trie $ NET.Leaf a
    go (Trie (NET.Leaf a)) = Trie $ NET.Leaf a
    go _ = TrieNil
