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
module Text.LanguageTag.BCP47.Syntax.Trie where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import qualified Text.LanguageTag.BCP47.Subtag.Trie as Trie
import Text.LanguageTag.BCP47.Syntax (BCP47, toSubtags)
import Text.LanguageTag.Internal.BCP47.Subtag.Trie (Trie (..))

-- | Convert a language-tag-indexed list to a 'Trie' with the given
-- root node value and paths
tagTrie :: Maybe a -> [(BCP47, a)] -> Trie a
tagTrie r ls = Trie.toTrie $ r' <> fmap (first $ toList . toSubtags) ls
  where
    r' = ([],) <$> toList r

-- | Find an entry in a 'Trie' corresponding to the given 'BCP47' tag
-- exactly, like 'Text.LanguageTag.BCP47.Subtag.lookupTrie'
lookupBCP47Trie :: BCP47 -> Trie a -> Maybe a
lookupBCP47Trie = Trie.lookup . toList . toSubtags

-- | Find an entry in a 'Trie' that best matches the given 'BCP47'
-- tag, like 'Text.LanguageTag.BCP47.Subtag.lookupTrieLax'. Note that
-- this does not perform any canonicalization or consider other
-- information like macrolanguage relationships that might improve
-- matching.
lookupBCP47TrieLax :: BCP47 -> Trie a -> Maybe a
lookupBCP47TrieLax = Trie.lookupLax . toList . toSubtags
