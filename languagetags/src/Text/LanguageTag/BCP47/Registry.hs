{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Registered BCP47 tags, subtags and their records
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module re-exports all of the registered tag and subtag types,
-- their associated record lookup functions, as well as the
-- 'bcp47RegistryDate' that this the library uses. The current version
-- of the IANA registry, not necessarily the one used in the library,
-- is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Normal (..),
    toSyntaxTag,
    toSubtags,

    -- * Rendering
    renderBCP47,
    renderBCP47Builder,

    -- * Extension subtags and characters
    ExtensionSubtag,
    toExtensionSubtag,
    fromExtensionSubtag,
    Syn.ExtensionChar (..),
    Syn.charToExtensionChar,
    Syn.extensionCharToChar,
    Syn.extensionCharToSubtag,

    -- * Registry types and re-exports

    -- ** Notes on the registry and types
    -- $theregistry

    -- ** Types and re-exports
    bcp47RegistryDate,
    Scope (..),
    Deprecation (..),
    module Text.LanguageTag.BCP47.Registry.Language,
    module Text.LanguageTag.BCP47.Registry.Extlang,
    module Text.LanguageTag.BCP47.Registry.Script,
    module Text.LanguageTag.BCP47.Registry.Region,
    module Text.LanguageTag.BCP47.Registry.Variant,
    module Text.LanguageTag.BCP47.Registry.Grandfathered,
    module Text.LanguageTag.BCP47.Registry.Redundant,
  )
where

import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Text.LanguageTag.BCP47.Registry.Extlang
import Text.LanguageTag.BCP47.Registry.Grandfathered
import Text.LanguageTag.BCP47.Registry.Language
import Text.LanguageTag.BCP47.Registry.Redundant
import Text.LanguageTag.BCP47.Registry.Region
import Text.LanguageTag.BCP47.Registry.Script
import Text.LanguageTag.BCP47.Registry.Variant
import Text.LanguageTag.BCP47.Subtag (renderSubtagBuilderLower)
import Text.LanguageTag.Internal.BCP47.Canonicalization
import Text.LanguageTag.Internal.BCP47.Registry
import Text.LanguageTag.Internal.BCP47.Registry.Date
import Text.LanguageTag.Internal.BCP47.Registry.Types
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'BCP47' tag to strict text, using 'renderBCP47Builder'
-- internally
renderBCP47 :: BCP47 -> Text
renderBCP47 = TL.toStrict . TB.toLazyText . renderBCP47Builder

-- | Render a 'BCP47' tag to a lazy text builder. Note that this
-- function is not equivalent to @'Syn.renderBCP47' . 'toSyntaxTag'@,
-- because the variants in the tag may need to be rendered
-- non-alphabetically to conform to the standard's recommendations. If
-- you would like to know exactly how this is done, and how this
-- relates to the standard, consult the documentation for the internal
-- function 'Internal.categorizeVariants'. (Of the presentation
-- options given in that function, this rendering function happens to
-- choose to concatenate all of the variant chains and then remove
-- duplicates).
renderBCP47Builder :: BCP47 -> TB.Builder
renderBCP47Builder (NormalTag n) =
  mconcat $
    List.intersperse "-" $
      l' : (e' <> s' <> r' <> firstVariants' <> midVariants <> endVariants <> es' <> pu')
  where
    l' = renderLanguageBuilder $ language n
    e' = toList $ renderExtlangBuilder <$> extlang n
    s' = toList $ renderScriptBuilder <$> script n
    r' = toList $ renderRegionBuilder <$> region n
    (chains, havePrefs, noPrefs) = categorizeVariants n
    ordNub' _ [] = []
    ordNub' s (x : xs)
      | x `S.member` s = ordNub' s xs
      | otherwise = x : ordNub' (S.insert x s) xs
    firstVariants = ordNub' mempty $ mconcat $ toList <$> listVariantChains chains
    firstVariants' = fmap renderVariantBuilder firstVariants
    midVariants = renderVariantBuilder <$> S.toAscList noPrefs
    endVariants =
      fmap renderVariantBuilder $
        S.toAscList $ havePrefs S.\\ S.fromList firstVariants
    es' = concatMap renderExt $ M.toAscList $ extensions n
    renderExt (c, subs) =
      TB.singleton (Syn.extensionCharToChar c) :
      toList (renderSubtagBuilderLower . fromExtensionSubtag <$> subs)
    pu'
      | null $ privateUse n = []
      | otherwise = "x" : (renderSubtagBuilderLower <$> privateUse n)
renderBCP47Builder (PrivateUseTag sts) =
  mconcat $ List.intersperse "-" $ toList $ renderSubtagBuilderLower <$> sts
renderBCP47Builder (GrandfatheredTag t) =
  renderGrandfatheredBuilder t

-- $theregistry
--
-- The names of the subtag data constructors are roughly the same as
-- the subtags themselves; all of them are camel case except for the
-- 'Region' tags, which are all upper case. Additionally, the
-- 'Extlang' constructors and the constructors corresponding to
-- subtags that start with a digit are prefixed with the first three
-- letters of their types. These modifications were necessary to
-- arrive at constructor names that are valid and free of name
-- collisions.
--
-- The registry itself contains records for language, extended
-- language, script, and region subtags, and grandfathered and
-- redundants tags. Some general notes on these records:
--
-- * Each record contains at least one (non-normative) description of
--   what the subtag represents. The registry does not guarantee that
--   this description will be in any particular language, and these
--   descriptions may be added, changed, or removed as the registry
--   updates.
--
-- * Each record may contain a deprecation notice, indicating that the
--   associated tag or subtag should not be used. This value may be
--   added, changed, or removed as the registry updates.
--
-- * If deprecated, a record may contain a preferred value that is
--   recommended for use instead of the deprected tag. Note that for
--   regions, this preferred value may not have exactly the same
--   meaning as the old tag. This value may be added, removed or
--   modified as the registry updates, and a change in this value does
--   not imply that the affected subtag needs to be retagged.
--
-- * Extlang and variant records may have a prefix (more than one in
--   the case of variant records) that is recommended as a prefix to
--   the record's subtag. If a record does not have a prefix field,
--   one will not be added as the registry updates, and changes to a
--   prefix field must only widen the range of possible prefixes.
--
-- * Language and extlang records have macrolanguage and scope fields,
--   which are informational and indicate an encompassing language and
--   a classification of the language, respectively. These may be
--   added, removed, or changed as the registry updates.
