{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.LanguageTag.BCP47.Syntax.Trie
-- Description : Subtag trie interface with BCP47 tags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.Syntax.Trie where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Text.LanguageTag.BCP47.Syntax (BCP47, toSubtags)
import Text.LanguageTag.Internal.Subtag.Trie (Trie (..))
import Text.LanguageTag.Subtag.Trie (lookupTrie, lookupTrieLax, pathTrie)

-- | Convert a language tag indexed list to a 'Trie' with the given
-- root node value
tagTrie :: Maybe a -> [(BCP47, a)] -> Trie a
tagTrie r ls = pathTrie $ r' <> fmap (first $ toList . toSubtags) ls
  where
    r' = ([],) <$> toList r

-- | Find an entry in a 'Trie' corresponding to the given 'BCP47' tag
-- exactly, like 'lookupTrie'

-- TODO: this and Lax might be more efficiently implemented without the toSubtags
lookupBCP47Trie :: BCP47 -> Trie a -> Maybe a
lookupBCP47Trie = lookupTrie . toList . toSubtags

-- | Find an entry in a 'Trie' that best matches the given list of
-- subtags, like 'lookupTrieLax'
lookupBCP47TrieLax :: BCP47 -> Trie a -> Maybe a
lookupBCP47TrieLax = lookupTrieLax . toList . toSubtags
