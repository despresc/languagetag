{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Internal subtag trie definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the possibly-empty 'Trie' type and basic
-- functions to create and manipulate them. Note: everything in this
-- module is re-exported by "Text.LanguageTag.BCP47.Subtag.Trie".
module Text.LanguageTag.Internal.BCP47.Subtag.Trie
  ( Trie (..),
    Step (..),
    fromTrie,
    fromStep,
  )
where

import Data.Functor.Classes
  ( Show1 (..),
    showsPrec1,
    showsUnaryWith,
  )
import qualified Data.List.NonEmpty as NE
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag)
import qualified Text.LanguageTag.Internal.BCP47.Subtag.NonEmptyTrie as NET

-- | A strict trie indexed by 'Subtag' values
data Trie a
  = Trie !(NET.Trie a)
  | TrieNil
  deriving (Eq, Ord, Functor)

instance Foldable Trie where
  foldr f b (Trie t) = foldr f b t
  foldr _ b TrieNil = b
  null (Trie _) = False
  null _ = True

-- | Merge two 'Trie's together, also merging overlapping nodes
instance Semigroup a => Semigroup (Trie a) where
  Trie x <> Trie y = Trie $ x <> y
  Trie x <> TrieNil = Trie x
  TrieNil <> Trie y = Trie y
  TrieNil <> TrieNil = TrieNil

instance Semigroup a => Monoid (Trie a) where
  mempty = TrieNil

-- | A step in a trie path; a child of a trie node.
data Step a
  = Step !Subtag !(Trie a)
  deriving (Eq, Ord, Foldable, Show)

-- | Transform a 'Trie' into its constituent paths.
fromTrie :: Trie a -> [([Subtag], a)]
fromTrie (Trie x) = NE.toList $ NET.fromTrie x
fromTrie TrieNil = []

-- | Transform a 'Step' into its label and constituent paths.
fromStep :: Step a -> (Subtag, [([Subtag], a)])
fromStep (Step s t) = (s, fromTrie t)

instance Show a => Show (Trie a) where
  showsPrec = showsPrec1

instance Show1 Trie where
  liftShowsPrec sp sl d m =
    showsUnaryWith (liftShowsPrec sp' sl') "fromTrie" d (fromTrie m)
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl
