{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Description : Non-empty tries indexed by subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports a strict, non-empty 'Trie' type with paths
-- indexed with 'Subtag' values. Most relevant for language tags
-- analysis are the 'toTrie' function for constructing a 'Trie' and
-- 'lookup' and 'lookupLax' for querying them. Tries can also be
-- constructed with 'stepTrie', 'leaf', 'branch', and 'path' if that
-- is more convenient.
module Text.LanguageTag.BCP47.Subtag.NonEmptyTrie
  ( Trie,
    Step (..),

    -- * Lookup
    lookup,
    lookupLax,
    lookupSub,

    -- * Direct construction
    root,
    singleton,
    stepTrie,
    leaf,
    path,
    branch,

    -- * Update and creation
    insert,
    raise,
    modCreate,

    -- * Update and deletion
    delete,
    prune,
    prunePast,
    update,
    alterSub,

    -- * Conversions to and from non-empty lists
    toTrie,
    toStep,
    fromTrie,
    fromStep,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import Text.LanguageTag.Internal.BCP47.Subtag.NonEmptyTrie
import Prelude hiding (lookup, null)

mnode :: Node a -> Maybe a
mnode (Node a) = Just a
mnode Nil = Nothing

-- | Construct a 'Trie' by gathering paths with common indices
-- together, preferring the rightmost path if there are duplicates

-- FIXME: may have more efficient implementation?
toTrie :: NonEmpty ([Subtag], a) -> Trie a
toTrie ((sts, a) :| xs) = foldl' go (singleton sts a) xs
  where
    go t (sts', a') = insert sts' a' t

-- | Construct a 'Step' by gathering paths with common indices
-- together, preferring the rightmost path if there are duplicates
toStep :: Subtag -> NonEmpty ([Subtag], a) -> Step a
toStep s = Step s . toTrie

-- | Construct a trie from its children, preferring the rightmost
-- child if there are duplicates
stepTrie :: Maybe a -> NonEmpty (Step a) -> Trie a
stepTrie ma (Step b c :| stps) = Branch n (Step smallk smallv) children
  where
    n = maybe Nil Node ma
    (smallk, smallv, children) = foldl' go (b, c, mempty) stps
    go (!s, !t, !m) (Step s' t') = case compare s s' of
      LT -> (s, t, M.insert s' t' m)
      EQ -> (s', t', m)
      GT -> (s', t', M.insert s t m)

-- | Construct a 'Step' with the given element at its end and with no
-- further children
leaf :: Subtag -> a -> Step a
leaf s = Step s . root

-- | Construct a 'Step' consisting of the given path with the given
-- element at its end
path :: NonEmpty Subtag -> a -> Step a
path (x :| xs) = Step x . singleton xs

-- | Construct a trie from its children, preferring the rightmost
-- child if there are duplicates
branch :: Subtag -> Maybe a -> NonEmpty (Step a) -> Step a
branch s x = Step s . stepTrie x

-- | Construct a 'Trie' with the given element at its root and with no
-- further children
root :: a -> Trie a
root = Leaf

-- | Find the entry in a 'Trie' corresponding to the given list
-- of subtags exactly
lookup :: [Subtag] -> Trie a -> Maybe a
lookup l t = case lookupSub l t of
  Just (Branch (Node a) _ _) -> Just a
  Just (Leaf a) -> Just a
  _ -> Nothing

-- | Return the sub-'Trie' rooted at the given node, if it exists
lookupSub :: [Subtag] -> Trie a -> Maybe (Trie a)
lookupSub (x : xs) (Branch _ (Step s t) m) = case compare x s of
  LT -> Nothing
  EQ -> lookupSub xs t
  GT -> M.lookup x m >>= lookupSub xs
lookupSub (_ : _) (Leaf _) = Nothing
lookupSub [] t = Just t

-- | Find the entry in a 'Trie' that best matches the given list
-- of subtags. This is "lookup" in the sense of BCP47: in effect,
-- subtags are successively dropped from the end of the list until an
-- entry is found.
lookupLax :: [Subtag] -> Trie a -> Maybe a
lookupLax = go Nothing
  where
    go mn (x : xs) (Branch n (Step s t) m) =
      let mn' = mn <|> mnode n
       in case compare x s of
            LT -> mn'
            EQ -> go mn' xs t
            GT -> case M.lookup x m of
              Nothing -> mn'
              Just t' -> go mn' xs t'
    go mn [] (Branch n _ _) = mn <|> mnode n
    go _ _ (Leaf a) = Just a
