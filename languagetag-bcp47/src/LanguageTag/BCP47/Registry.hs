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
module LanguageTag.BCP47.Registry
  ( BCP47 (..),
    simpleTag,
    Normal (..),
    simpleNormal,
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
    module LanguageTag.BCP47.Registry.Language,
    module LanguageTag.BCP47.Registry.Extlang,
    module LanguageTag.BCP47.Registry.Script,
    module LanguageTag.BCP47.Registry.Region,
    module LanguageTag.BCP47.Registry.Variant,
    module LanguageTag.BCP47.Registry.Grandfathered,
    module LanguageTag.BCP47.Registry.Redundant,
  )
where

import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import LanguageTag.BCP47.Canonicalization
import LanguageTag.BCP47.Registry.Extlang
import LanguageTag.BCP47.Registry.Grandfathered
import LanguageTag.BCP47.Registry.Language
import LanguageTag.BCP47.Registry.Redundant
import LanguageTag.BCP47.Registry.Region
import LanguageTag.BCP47.Registry.Script
import LanguageTag.BCP47.Registry.Variant
import LanguageTag.BCP47.Subtag
  ( Subtag,
    justSubtag,
    nullSubtag,
    renderSubtagBuilderLower,
  )
import qualified LanguageTag.BCP47.Syntax as Syn
import LanguageTag.Internal.BCP47.Registry.Date
import LanguageTag.Internal.BCP47.Registry.Types
import qualified LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'BCP47' tag to a strict text value. Note that this
-- function is not equivalent to @'Syn.renderBCP47' . 'toSyntaxTag'@,
-- because the variants in the tag may need to be rendered
-- non-alphabetically to conform to the standard's
-- recommendations. See the documentation of 'orderNormalVariants',
-- 'categorizeVariants', and their related types and functions for
-- more information about the task of ordering variants in a tag.
--
-- A (nonsensical) example of rendering a 'Normal' tag with all of the
-- possible tag parts present:
--
-- >>> :set -XQuasiQuotes
-- >>> import LanguageTag.BCP47.Quasi
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

-- | Create a language tag with only the given primary language subtag
simpleTag :: Language -> BCP47
simpleTag = NormalTag . simpleNormal

-- | Create a 'Normal' language tag with only the given primary
-- language subtag
simpleNormal :: Language -> Normal
simpleNormal l =
  Normal
    { language = l,
      extlang = Nothing,
      script = Nothing,
      region = Nothing,
      variants = mempty,
      extensions = mempty,
      privateUse = mempty
    }

-- | Convert a 'BCP47' tag to its component subtags, in the same order
-- as they would appear in 'renderBCP47'. This is not equal to
-- 'toSyntaxTag' followed by 'Syn.toSubtags' for reasons described in
-- the documentation of 'renderBCP47'.
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

-- | Convert a valid 'BCP47' tag to a merely well-formed 'Syn.BCP47'
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
        Syn.variants = variantToSubtag <$> orderNormalVariants n,
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
-- redundant tags. Some general notes on these records:
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
--   recommended for use instead of the deprected tag or subtag. Note
--   that for regions, this preferred value may not have exactly the
--   same meaning as the old subtag. This value may be added, removed
--   or modified as the registry updates, and a change in this value
--   does not imply that the affected subtag needs to be retagged. (If
--   you do want to keep around tags with deprecated subtags, you will
--   want to avoid using
--   'LanguageTag.BCP47.Canonicalization.canonicalizeBCP47' and
--   its derivatives).
--
-- * Variant records may have one or more prefixes that are
--   recommended as a prefix to the record's subtag; these should
--   match (in the BCP47 sense) the portion of the tag before the
--   variant in question. If a record does not have a prefix field,
--   one will not be added as the registry updates, and changes to a
--   prefix field will only widen the range of possible
--   prefixes. Extended language records also have prefix entries in
--   their records, but these records differ from those of variants in
--   two ways: an extended language record must have exactly one
--   prefix consisting of a primary language subtag and nothing else,
--   and it is even more strongly recommended that extended language
--   subtags be used only with tags matching their prefix.
--
-- * Language and extended language records have macrolanguage and
--   scope fields, which are informational and indicate an
--   encompassing language and a classification of the language,
--   respectively. These may be added, removed, or changed as the
--   registry updates.
