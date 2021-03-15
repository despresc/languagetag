{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      : Text.LanguageTag.Internal.BCP47.Subtag.Trie
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

-- | A trie indexed by 'Subtag'
data Trie a = Trie (Maybe a) !(HashMap Subtag (Trie a))
  deriving (Functor)

-- | A step in a trie path
data TrieStep a = TrieStep !Subtag !(Trie a)
  deriving (Functor)
