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
    orderNormalVariants,

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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
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
import Text.LanguageTag.BCP47.Subtag
  ( Subtag,
    justSubtag,
    nullSubtag,
    renderSubtagBuilderLower,
  )
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Canonicalization
import Text.LanguageTag.Internal.BCP47.Registry.Date
import Text.LanguageTag.Internal.BCP47.Registry.Types
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'BCP47' tag to a strict text value. Note that this
-- function is not equivalent to @'Syn.renderBCP47' . 'toSyntaxTag'@,
-- because the variants in the tag may need to be rendered
-- non-alphabetically to conform to the standard's
-- recommendations. The order chosen here is as follows:
--
-- 1. The variants in the tag with at least one satisfied prefix (a
--    prefix in their registry record that match the rest of the tag)
--    come before the others. Within this group, if @x@ and @y@ are two
--    variants and @x@ is mentioned in one of @y@'s satisfied prefixes,
--    then @x@ comes before @y@. Beyond that, any groups of
--    ambiguously-ordered variants are alphabetized.
--
-- 2. The variants with no prefixes in their registry records (the
--    "generalist" variants) come next. These are alphabetized.
--
-- 3. The variants with prefixes that are unsatisfied come last. These
--    are again alphabetized.
--
-- For tags following the standard's guidelines regarding variants,
-- group 3 will not exist, and group 1 will consist of a single chain
-- of variants, with the variant at the end having a prefix that
-- matches the preceding tag (including the rest of the
-- chain). Additionally, most tags that are encountered have no
-- variants at all, and most of the remainder have only one variant,
-- so this (still necessary) treatment of the variants will not affect
-- the tag's presentation. (The documentation for the internal
-- function 'categorizeVariants' goes into more detail about analyzing
-- the variants of tags).
--
-- A (nonsensical) example of rendering a 'Normal' tag with all of the
-- possible tag parts present:
--
-- >>> :set -XQuasiQuotes
-- >>> import Text.LanguageTag.BCP47.Quasi
-- >>> renderBCP47 [validtag|sl-cmn-hant-us-njiva-rozaj-biske-1994-alalc97-1606nict-b-and-a-tag-x-more|]
-- "sl-cmn-Hant-US-rozaj-biske-njiva-1994-alalc97-1606nict-a-tag-b-and-x-more"
renderBCP47 :: BCP47 -> Text
renderBCP47 = TL.toStrict . TB.toLazyText . renderBCP47Builder

-- | Render a 'BCP47' tag to a lazy text builder. This function is
-- used to generate the text for 'renderBCP47', so the remarks in its
-- documentation apply to this function as well.
renderBCP47Builder :: BCP47 -> TB.Builder
renderBCP47Builder (NormalTag n) =
  mconcat $
    List.intersperse "-" $
      l' : (e' <> s' <> r' <> vs' <> es' <> pu')
  where
    l' = renderLanguageBuilder $ language n
    e' = toList $ renderExtlangBuilder <$> extlang n
    s' = toList $ renderScriptBuilder <$> script n
    r' = toList $ renderRegionBuilder <$> region n
    vs' = renderVariantBuilder <$> orderNormalVariants n
    es' = concatMap renderExt $ M.toAscList $ extensions n
    renderExt (c, subs) =
      TB.singleton (Syn.extensionCharToChar c) :
      toList (renderSubtagBuilderLower . fromExtensionSubtag <$> subs)
    pu'
      | null $ privateUse n = []
      | otherwise = "x" : (renderSubtagBuilderLower <$> privateUse n)
renderBCP47Builder (PrivateUseTag sts) =
  mconcat $ List.intersperse "-" $ "x" : toList (renderSubtagBuilderLower <$> sts)
renderBCP47Builder (GrandfatheredTag t) =
  renderGrandfatheredBuilder t

-- | Order the variants in a 'Normal' tag according to the standard's
-- recommendations. This cannot be implemented as a newtype-defined
-- custom 'Ord' instance for 'Variant' because the ordering in the
-- standard is context-sensitive. This function is used to implement
-- 'renderBCP47' and 'toSubtags'.
orderNormalVariants :: Normal -> [Variant]
orderNormalVariants n = firstVariants <> midVariants <> endVariants
  where
    (chains, havePrefs, noPrefs) = categorizeVariants n
    firstVariants = enumerateChainVariants chains
    midVariants = S.toAscList noPrefs
    endVariants = S.toAscList $ havePrefs S.\\ S.fromList firstVariants

-- | Convert a 'BCP47' tag to its component subtags, in the same order
-- as they would appear in 'renderBCP47'. This is not equal to
-- @'Syn.toSubtags' . 'toSyntaxTag'@ for reasons described in the
-- documentation for 'renderBCP47'.
toSubtags :: BCP47 -> NonEmpty Subtag
toSubtags (NormalTag n) = l' :| (catMaybes [e', s', r'] <> vs' <> es' <> pu')
  where
    l' = languageToSubtag $ language n
    e' = extlangToSubtag <$> extlang n
    s' = scriptToSubtag <$> script n
    r' = regionToSubtag <$> region n
    vs' = variantToSubtag <$> orderNormalVariants n
    es' = concatMap fromExt $ M.toAscList $ extensions n
    pu'
      | null $ privateUse n = []
      | otherwise = Syn.subtagX : privateUse n
    fromExt (e, es) = Syn.extensionCharToSubtag e : toList (fromExtensionSubtag <$> es)
toSubtags (PrivateUseTag x) = NE.cons Syn.subtagX x
toSubtags (GrandfatheredTag x) = grandfatheredToSubtags x

-- | Convert a 'BCP47' tag back to a merely well-formed 'Syn.BCP47'
-- tag
toSyntaxTag :: BCP47 -> Syn.BCP47
toSyntaxTag (NormalTag n) =
  Syn.NormalTag $
    Syn.Normal
      { Syn.primlang = languageToSubtag $ language n,
        Syn.extlang1 = mto extlangToSubtag extlang n,
        Syn.extlang2 = nullSubtag,
        Syn.extlang3 = nullSubtag,
        Syn.script = mto scriptToSubtag script n,
        Syn.region = mto regionToSubtag region n,
        Syn.variants = S.toList $ S.map variantToSubtag $ variants n,
        Syn.extensions = fmap toExt $ M.toList $ extensions n,
        Syn.privateUse = privateUse n
      }
  where
    mto f p x = maybe nullSubtag (justSubtag . f) $ p x
    toExt (c, x) = Syn.Extension c $ fromExtensionSubtag <$> x
toSyntaxTag (PrivateUseTag x) = Syn.PrivateUse x
toSyntaxTag (GrandfatheredTag x) = Syn.GrandfatheredTag x

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
--   this description will be in any particular language or script,
--   and these descriptions may be added, changed, or removed as the
--   registry updates.
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
--   not imply that the affected subtag needs to be retagged. (If you
--   do want to keep around tags with deprecated subtags, you will
--   want to avoid using
--   'Text.LanguageTag.BCP47.Canonicalization.canonicalizeBCP47' and
--   its derivatives).
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
