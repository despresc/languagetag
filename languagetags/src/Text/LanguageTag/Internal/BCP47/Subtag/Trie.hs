{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- |
-- Description : Internal subtag trie definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning: this is an internal module and may change or disappear
-- without regard to the PVP.
module Text.LanguageTag.Internal.BCP47.Subtag.Trie where

import Data.HashMap.Strict (HashMap)
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag)

-- | A trie indexed by 'Subtag'. Note that the 'Foldable' methods do
-- not fold over the paths in the 'Trie' in a consistent order.
data Trie a = Trie !(Maybe a) !(HashMap Subtag (Trie a))
  deriving (Functor, Foldable)

-- | A step in a trie path; a child of a trie node. Equivalent to
-- @('Subtag', 'Trie' a)@. Note that the 'Foldable' methods do not
-- fold over the paths in the 'TrieStep' in a consistent order.
data TrieStep a = TrieStep !Subtag !(Trie a)
  deriving (Functor, Foldable)
