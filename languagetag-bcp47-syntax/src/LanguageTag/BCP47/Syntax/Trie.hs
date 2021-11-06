{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Description : BCP47 tag interface to subtag tries
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides functions to create tries out of
-- 'BCP47'-indexed paths, and to look up entries in those tries.
module LanguageTag.BCP47.Syntax.Trie where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import LanguageTag.BCP47.Subtag (Subtag)
import qualified LanguageTag.BCP47.Subtag.Trie as Trie
import LanguageTag.BCP47.Syntax (BCP47, toSubtagsNE)
import LanguageTag.Internal.BCP47.Subtag.Trie (Trie (..))

-- TODO: replace toSubtagsNE with toSubtags once BCP47 gets its IsSubtags
-- instance

-- | Convert a language-tag-indexed list to a 'Trie' with the given
-- root node value and paths
tagTrie :: Maybe a -> [(BCP47, a)] -> Trie a
tagTrie r ls = Trie.toTrie $ r' <> fmap (first $ toList . toSubtagsNE) ls
  where
    r' = ([],) <$> toList r

-- | Find an entry in a 'Trie' corresponding to the given 'BCP47' tag
-- exactly, like 'LanguageTag.BCP47.Subtag.lookupTrie'
lookupBCP47Trie :: BCP47 -> Trie a -> Maybe a
lookupBCP47Trie = Trie.lookup . toList . toSubtagsNE

-- | Find an entry in a 'Trie' that best matches the given 'BCP47'
-- tag, like 'LanguageTag.BCP47.Subtag.lookupTrieLax'. Note that
-- this does not perform any canonicalization or consider other
-- information like macrolanguage relationships that might improve
-- matching. This function also returns the subtags that were dropped
-- from the tag in order to find the match.
lookupBCP47TrieLax :: BCP47 -> Trie a -> Maybe (a, [Subtag])
lookupBCP47TrieLax = Trie.lookupLax . toList . toSubtagsNE
