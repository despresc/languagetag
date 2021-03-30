{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Description : Language tag canonicalization
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: this is an internal module and may change or disappear
-- without regard to the PVP.
--
-- This module defines a simple internal linting monad and various
-- canonicalization-related functions.
module Text.LanguageTag.Internal.BCP47.Canonicalization where

import Control.Monad (ap, when)
import Data.Foldable (foldl', traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry.Grandfathered (lookupGrandfatheredRecord)
import Text.LanguageTag.BCP47.Registry.Language (languageToSubtag, lookupLanguageRecord)
import Text.LanguageTag.BCP47.Registry.Redundant
  ( lookupRedundantRecord,
    recognizeRedundantNormal,
  )
import Text.LanguageTag.BCP47.Registry.Region (lookupRegionRecord)
import Text.LanguageTag.BCP47.Registry.Script (lookupScriptRecord)
import Text.LanguageTag.BCP47.Registry.Variant (lookupVariantRecord)
import Text.LanguageTag.Internal.BCP47.Registry.Extlang (Extlang)
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords
  ( lookupExtlangRecord,
    validateExtlang,
  )
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered (Grandfathered)
import Text.LanguageTag.Internal.BCP47.Registry.Language (Language)
import Text.LanguageTag.Internal.BCP47.Registry.Orphans ()
import Text.LanguageTag.Internal.BCP47.Registry.Redundant (Redundant)
import Text.LanguageTag.Internal.BCP47.Registry.Region (Region)
import Text.LanguageTag.Internal.BCP47.Registry.Script (Script)
import Text.LanguageTag.Internal.BCP47.Registry.Types
  ( BCP47 (..),
    Deprecation (..),
    ExtlangRecord (..),
    LanguageRecord (..),
    Normal (..),
    RangeRecord (..),
    RegionRecord (..),
    ScriptRecord (..),
    VariantRecord (..),
  )
import Text.LanguageTag.Internal.BCP47.Registry.Variant (Variant)

-- | Warnings that may be noted when processing a valid tag. These are
-- either automatically 'fixed', or are 'notFixed'.
data LintWarnings = LintWarnings
  { fixed :: [LintWarning],
    notFixed :: [LintWarning]
  }
  deriving (Eq, Ord, Show)

-- | A possible warning that may be raised during linting. Note that a
-- 'PrefixCollision' warning means that multiple variants with
-- satisfied prefixes were used in the tag, but they couldn't be
-- arranged in a list such that the variant at the end of the list had
-- a matching prefix that included all of the previous variants.
data LintWarning
  = -- | used a deprecated tag or subtag
    UsedDeprecated DeprecatedComponent
  | -- | used a superfluous script subtag
    SuperfluousScript Language Script
  | -- | the prefix of a subtag wasn't satisfied
    PrefixMismatch PrefixedComponent
  | -- | distinct variant chains were used in the same tag
    PrefixCollision (NonEmpty Variant) (NonEmpty (NonEmpty Variant))
  | -- | the tag used an 'Extlang' instead of its corresponding 'Language'
    UsedExtlang Extlang
  deriving (Eq, Ord, Show)

-- | The deprecated component that was used
data DeprecatedComponent
  = DeprecatedLanguage Language
  | DeprecatedExtlang Extlang
  | DeprecatedScript Script
  | DeprecatedRegion Region
  | DeprecatedVariant Variant
  | DeprecatedRedundant Redundant
  | DeprecatedGrandfathered Grandfathered
  deriving (Eq, Ord, Show)

-- | The component with the unsatisfied prefix that was used
data PrefixedComponent
  = PrefixedExtlang Extlang
  | PrefixedVariant Variant
  deriving (Eq, Ord, Show)

-- | Difference list
type DL a = [a] -> [a]

-- | Linting state
data LintS = LintS
  { sFixed :: !(DL LintWarning),
    sNotFixed :: !(DL LintWarning)
  }

-- | A simple internal monad for collecting linting warnings
newtype LintM a = LintM
  { unLintM :: LintS -> (LintS, a)
  }
  deriving (Functor)

-- | Register the given fixed warning
warnFixed :: LintWarning -> LintM ()
warnFixed w = LintM $ \x@LintS {sFixed = f} -> (x {sFixed = (w :) . f}, ())

-- | Register the given not fixed warning
warnNotFixed :: LintWarning -> LintM ()
warnNotFixed w = LintM $ \x@LintS {sNotFixed = f} -> (x {sNotFixed = (w :) . f}, ())

instance Applicative LintM where
  pure a = LintM (,a)
  (<*>) = ap

instance Monad LintM where
  LintM ma >>= mf = LintM $ \l ->
    let (l', a) = ma l
     in unLintM (mf a) l'

-- | Run a 'LintM' action
runLintM :: LintM a -> (LintWarnings, a)
runLintM (LintM act) = case act (LintS id id) of
  (LintS {sFixed = f, sNotFixed = n}, a) ->
    (LintWarnings {fixed = f [], notFixed = n []}, a)

----------------------------------------------------------------
-- Canonicalization
----------------------------------------------------------------

-- | Canonicalize a 'BCP47' tag
canonicalizeBCP47 :: BCP47 -> LintM BCP47
canonicalizeBCP47 (NormalTag t) = NormalTag <$> canonicalizeNormal t
canonicalizeBCP47 (PrivateUseTag t) = pure $ PrivateUseTag t
canonicalizeBCP47 (GrandfatheredTag t) = canonicalizeGrandfathered t

-- | Canonicalize a 'Normal' tag
canonicalizeNormal :: Normal -> LintM Normal
canonicalizeNormal n = case recognizeRedundantNormal n of
  Nothing -> newN
  Just r -> case rangeDeprecation $ lookupRedundantRecord r of
    DeprecatedPreferred x ->
      warnFixed (UsedDeprecated $ DeprecatedRedundant r)
        $> x
    DeprecatedSimple ->
      warnNotFixed (UsedDeprecated $ DeprecatedRedundant r) >> newN
    NotDeprecated -> newN
  where
    newN = do
      n' <- canonicalizeExtlang n
      l <- canonicalizeLanguage $ language n'
      s <- traverse canonicalizeScript $ script n'
      r <- traverse canonicalizeRegion $ region n'
      v <- fmap S.fromList . traverse canonicalizeVariant $ S.toList $ variants n'
      pure
        n'
          { language = l,
            script = s,
            region = r,
            variants = v
          }

-- | Replace a deprecated 'Grandfathered' tag with its preferred
-- equivalent, if possible
canonicalizeGrandfathered :: Grandfathered -> LintM BCP47
canonicalizeGrandfathered t =
  case rangeDeprecation $ lookupGrandfatheredRecord t of
    DeprecatedPreferred x ->
      warnFixed (UsedDeprecated $ DeprecatedGrandfathered t)
        $> NormalTag x
    DeprecatedSimple ->
      warnNotFixed (UsedDeprecated $ DeprecatedGrandfathered t)
        $> GrandfatheredTag t
    NotDeprecated -> pure $ GrandfatheredTag t

-- | Replace deprecated primary and extended language pairs with their
-- preferred primary language equivalent, if possible
canonicalizeExtlang :: Normal -> LintM Normal
canonicalizeExtlang n = case extlang n of
  Just el -> do
    let pref = extlangPrefix $ lookupExtlangRecord el
    if language n == pref
      then
        warnFixed (UsedExtlang el)
          $> n
            { language = extlangPreferredValue $ lookupExtlangRecord el,
              extlang = Nothing
            }
      else do
        warnNotFixed $ PrefixMismatch $ PrefixedExtlang el
        when (extlangDeprecation $ lookupExtlangRecord el) $
          warnNotFixed $
            UsedDeprecated $ DeprecatedExtlang el
        pure n
  Nothing -> pure n

-- | Replace deprecated languages with their non-deprecated
-- equivalents, if possible
canonicalizeLanguage :: Language -> LintM Language
canonicalizeLanguage l = case languageDeprecation $ lookupLanguageRecord l of
  NotDeprecated -> pure l
  DeprecatedPreferred l' -> warnFixed (UsedDeprecated $ DeprecatedLanguage l) $> l'
  DeprecatedSimple -> warnNotFixed (UsedDeprecated $ DeprecatedLanguage l) $> l

-- | Replace deprecated scripts with their non-deprecated
-- equivalents, if possible
canonicalizeScript :: Script -> LintM Script
canonicalizeScript s = case scriptDeprecation $ lookupScriptRecord s of
  NotDeprecated -> pure s
  DeprecatedPreferred s' -> warnFixed (UsedDeprecated $ DeprecatedScript s) $> s'
  DeprecatedSimple -> warnNotFixed (UsedDeprecated $ DeprecatedScript s) $> s

-- | Replace deprecated regions with their non-deprecated
-- equivalents, if possible
canonicalizeRegion :: Region -> LintM Region
canonicalizeRegion r = case regionDeprecation $ lookupRegionRecord r of
  NotDeprecated -> pure r
  DeprecatedPreferred r' -> warnFixed (UsedDeprecated $ DeprecatedRegion r) $> r'
  DeprecatedSimple -> warnNotFixed (UsedDeprecated $ DeprecatedRegion r) $> r

-- | Replace deprecated variants with their non-deprecated
-- equivalents, if possible
canonicalizeVariant :: Variant -> LintM Variant
canonicalizeVariant v = case variantDeprecation $ lookupVariantRecord v of
  NotDeprecated -> pure v
  DeprecatedPreferred v' -> warnFixed (UsedDeprecated $ DeprecatedVariant v) $> v'
  DeprecatedSimple -> warnNotFixed (UsedDeprecated $ DeprecatedVariant v) $> v

----------------------------------------------------------------
-- Script suppression
----------------------------------------------------------------

-- | Suppress a superfluous script subtag. Note that this only
-- suppresses a script that is in the language subtag's
-- @Suppress-Script@ field. It does not look at the extended language
-- subtag's field, since this function is meant to be used on
-- canonicalized tags, and those types of tags only have an extlang
-- when there is a prefix mismatch (in which case the script
-- suppression doesn't really apply).
suppressLanguageScript :: BCP47 -> LintM BCP47
suppressLanguageScript (NormalTag n) = NormalTag <$> suppressLanguageScriptNormal n
suppressLanguageScript x = pure x

-- | Suppress the script subtag of a tag if it appears in the
-- 'languageScriptSuppression' of the tag's 'Language'
suppressLanguageScriptNormal :: Normal -> LintM Normal
suppressLanguageScriptNormal n
  | ssup == script n,
    Just sup <- ssup =
    warnFixed (SuperfluousScript (language n) sup) $> n {script = Nothing}
  | otherwise = pure n
  where
    ssup = languageScriptSuppression $ lookupLanguageRecord $ language n

----------------------------------------------------------------
-- Variant prefix checking
----------------------------------------------------------------

-- | A tree with 'Variant' nodes. This type is intended to represent
-- the chains of variants with satisfied prefixes that can occur in a
-- tag, organized by grouping together the common prefixes of the
-- chains.
newtype VariantChains = VariantChains [(Variant, VariantChains)]
  deriving (Eq, Ord, Show)

-- | Enumerate the nodes in a 'VariantChains' tree with a depth-first
-- traversal
depthFirstEnumeration :: VariantChains -> [Variant]
depthFirstEnumeration (VariantChains chains) = foldr getVars [] chains
  where
    getVars (v, subchains) l = v : (depthFirstEnumeration subchains <> l)

-- | List all of the paths in a 'VariantChains' value
listVariantChains :: VariantChains -> [NonEmpty Variant]
listVariantChains (VariantChains t) = foldr go [] t
  where
    go (v, VariantChains vcs) l
      | null vcs = (v NE.:| []) : l
      | otherwise =
        (NE.cons v <$> listVariantChains (VariantChains vcs)) <> l

-- | Enumerate the variants in a 'VariantChains' tree. This function
-- produces a subsequence of the depth-first traversal of the
-- 'VariantChains' without any repeated variants by performing that
-- traversal and keeping the /final/ appearance of an element. When
-- applied to a 'VariantChains' from 'categorizeVariants', this has
-- the effect of ensuring that if @x@ and @y@ are variants in the
-- 'VariantChains' and @x@ comes before @y@ in one of the
-- 'listVariantChains' chains, then @x@ will come before @y@ in the
-- result of 'enumerateChainVariants'. (This doesn't hold in general,
-- since in an arbitrary 'VariantChains' there could be variants @x@
-- and @y@ appearing in different orders in different
-- 'listVariantChains' chains, making the property impossible to
-- satisfy).
enumerateChainVariants :: VariantChains -> [Variant]
enumerateChainVariants (VariantChains x) = fst $ unfoldChains x [] mempty
  where
    -- This definition has the effect of a double-reversal, since we
    -- need to go through the chains back-to-front. Could be avoided
    -- with a Sequence, but the VariantChains occurring in practice
    -- will be very small, so it shouldn't be an issue.
    unfoldChains chains vlist usedVariants = foldl' go (vlist, usedVariants) (reverse chains)
    go (!vlist, !usedVariants) (v, VariantChains subchains)
      | v `S.member` usedVariants' = (vlist', usedVariants')
      | otherwise = (v : vlist', S.insert v usedVariants')
      where
        (vlist', usedVariants') = unfoldChains subchains vlist usedVariants

-- | Get the set of all the variants appearing in the input
-- 'VariantChains'
variantsInChains :: VariantChains -> Set Variant
variantsInChains = S.fromList . depthFirstEnumeration

-- | Given a set of variants in a tag, return a list of those variants
-- together with the variant parts of their prefixes filtered
-- according to whether or not all of the variants in the set are in
-- the input set
annotateVariants :: Set Variant -> [(Variant, [Set Variant])]
annotateVariants s = go <$> S.toList s
  where
    go v =
      ( v,
        filter (`S.isSubsetOf` s) $
          fmap variants $ variantPrefixes $ lookupVariantRecord v
      )

-- | Take a set of variants and organize them into maximal
-- 'VariantChains', so that every path in the result consists entirely
-- of variants with satisfied prefixes (the variant parts of the
-- prefixes, anyway) and every path is maximal with respect to this
-- property
growVariantChains :: Set Variant -> VariantChains
growVariantChains = growVariantChains' . annotateVariants
  where
    growVariantChains' vars = go $ foldl' distributeToRoot id roots
      where
        go x = VariantChains $ x []
        (roots, compatible) = foldr gatherVar ([], []) vars
        gatherVar (v, prefs) (r, c) = case prefs of
          [x]
            | S.null x -> (v : r, c)
          [] -> (r, c)
          _ -> (r, (v, prefs) : c)
        filterRoot vr prefs = flip mapMaybe prefs $ \s ->
          if vr `S.member` s
            then Just $ S.delete vr s
            else Nothing
        distributeToRoot ::
          ([(Variant, VariantChains)] -> [(Variant, VariantChains)]) ->
          Variant ->
          ([(Variant, VariantChains)] -> [(Variant, VariantChains)])
        distributeToRoot l vr =
          let subelems = foldl' (getCompatible vr) id compatible
              chains = growVariantChains' $ subelems []
           in l . ((vr, chains) :)
        getCompatible root l (v, prefs) = case filterRoot root prefs of
          [] -> l
          prefs' -> l . ((v, prefs') :)

-- | @'splitPrefixedVariants' x@ returns a partition @(y, z)@ of @x@,
-- where @y@ is the subset of variants in @x@ that have at least one
-- prefix in the registry, and @z@ is the subset of variants that have
-- no prefixes in the registry.
splitPrefixedVariants :: Set Variant -> (Set Variant, Set Variant)
splitPrefixedVariants = S.partition $ not . null . variantPrefixes . lookupVariantRecord

-- | The 'categorizeVariants' function serves the niche role of
-- analyzing the variants of a BCP47 tag. Most tags have no variants
-- at all, and the remainder that follow the standard's
-- recommendations will normally have at most three variants (two
-- variants with satisfied prefixes and one variant without a prefix),
-- though tags with more than one variant are very rare. This function
-- deals with the tags that have at least two variants with prefixes
-- in the registry: the result of @'categorizeVariants' n@ is the
-- triple @(chains, prefs, noprefs)@, where
--
-- * @chains@ is the list of maximal chains of variants from
--   @'variants' n@ according to their prefix fields in the registry,
--   organized into a tree by common prefixes (with children sorted
--   alphabetically);
--
-- * @prefs@ is the set of variants in @'variants' n@ that have at
--   least one prefix in the registry (the entries of @chains@ will be
--   elements of this set); and
--
-- * @noprefs@ is the set of variants in @'variants' n@ that have no
--   prefixes in the registry.
--
-- In a tag following the standard's recommendations, @chains@ will
-- have exactly one path using all of the elements of @prefs@ once if
-- @prefs@ is non-empty, and will otherwise be empty.
--
-- As an example, take the valid (but nonsensical) tag
-- @sl-rozaj-biske-njiva-1994-alalc97-1606nict@. The variant @alalc97@
-- has no prefix in the registry, so it would appear in the @noprefs@
-- set. The variants @rozaj@, @biske@, @njiva@, @1994@, and @1606nict@
-- all have prefixes in the registry. The variant @1606nict@ has at
-- least one prefix, and none of its prefixes contain @sl@ (as of
-- writing this, though it is extremely unlikely that @sl@ will ever
-- be added to its prefixes), so it would not appear in the any of the
-- chains. The other variants with prefixes can be arranged in chains
-- so that their prefixes are satisfied: the maximal chains are
-- @rojaz-biske-1994@ and @rojaz-njiva-1994@, and so the @chains@
-- output would be
--
-- @
-- 'VariantChains'
--   [('Text.LanguageTag.BCP47.Registry.Variant.Rozaj',
--     'VariantChains'
--       [('Text.LanguageTag.BCP47.Registry.Variant.Biske', 'VariantChains' [('Text.LanguageTag.BCP47.Registry.Variant.Var1994', 'VariantChains' [])]),
--        ('Text.LanguageTag.BCP47.Registry.Variant.Njiva', 'VariantChains' [('Text.LanguageTag.BCP47.Registry.Variant.Var1994', 'VariantChains' [])])
--       ])]
-- @
--
-- This function exists in part to satisfy the standard's recommended
-- presentation of variants in a tag, given in [item 6, page
-- 57](https://tools.ietf.org/html/bcp47#page-1-57) and
-- elsewhere. Since the standard is not totally precise about the
-- ordering of variants that don't satisfy its recommendations, this
-- library uses the ordering:
--
-- @
-- 'concat' [ 'enumerateChainVariants' chains
--        , 'S.toAscList' noprefs
--        , 'S.toAscList' $ prefs 'S.\\' 'variantsInChains' chains ]
-- @
--
-- This should result in the best matching behaviour for tags with
-- variants that clash with each other or have unsatisfied
-- prefixes. Thus
--
-- > "sl-rozaj-biske-njiva-1994-alalc97-1606nict"
--
-- would be the ordering chosen by
-- 'Text.LanguageTag.BCP47.Registry.renderBCP47' for the example tag
-- above.
--
-- (You may notice that @sl-rozaj@ comes up a lot when discussing
-- variant subtags. That is because the only non-deprecated variant
-- subtags with prefixes that can appear in a chain with other variant
-- subtags with prefixes are @rozaj@ and its five sub-variants.)
categorizeVariants :: Normal -> (VariantChains, Set Variant, Set Variant)
categorizeVariants n = (growVariantChains matchInit, hasPrefs, noPrefs)
  where
    (hasPrefs, noPrefs) = splitPrefixedVariants $ variants n
    matchInit =
      S.filter
        (any matchCut . variantPrefixes . lookupVariantRecord)
        hasPrefs
    matchCut Normal {language = l, script = s, region = r} =
      l == language n
        && (isNothing s || s == script n)
        && (isNothing r || r == region n)

-- | Remove the variants with unsatisfied prefixes from the tag. Also
-- warn about any prefix collisions that are encountered. This only
-- affects 'Normal' tags. Note that applying this function to a tag
-- may invalidate prior warnings (e.g., if a tag uses a 'Variant' that
-- is both deprecated without a preferred value and also has an
-- unsatisfied prefix).
removeMismatchedVariants :: BCP47 -> LintM BCP47
removeMismatchedVariants (NormalTag t) = NormalTag <$> removeMismatchedVariantsNormal t
removeMismatchedVariants x = pure x

-- | Remove the variants with unsatisfied prefixes from the tag. Also
-- warn about any prefix collisions that are encountered.
removeMismatchedVariantsNormal :: Normal -> LintM Normal
removeMismatchedVariantsNormal n = do
  warnUnsatisfiedPrefs
  warnManyChains
  pure n {variants = satisfiedPrefs <> noPrefs}
  where
    (chains, havePrefs, noPrefs) = categorizeVariants n
    warnManyChains = case listVariantChains chains of
      (x : y : ys) -> warnNotFixed $ PrefixCollision x (y NE.:| ys)
      _ -> pure ()
    satisfiedPrefs = variantsInChains chains
    unsatisfiedPrefs = havePrefs S.\\ satisfiedPrefs
    warnUnsatisfiedPrefs =
      traverse_ (warnFixed . PrefixMismatch . PrefixedVariant) $
        S.toList unsatisfiedPrefs

-- | Issue unsatisfied prefix and prefix collision 'notFixed' warnings
-- about the variants
issueVariantPrefixWarningsNormal :: Normal -> LintM ()
issueVariantPrefixWarningsNormal n = do
  warnUnsatisfiedPrefs
  warnManyChains
  where
    (chains, havePrefs, _) = categorizeVariants n
    warnManyChains = case listVariantChains chains of
      (x : y : ys) -> warnNotFixed $ PrefixCollision x (y NE.:| ys)
      _ -> pure ()
    satisfiedPrefs = variantsInChains chains
    unsatisfiedPrefs = havePrefs S.\\ satisfiedPrefs
    warnUnsatisfiedPrefs =
      traverse_ (warnNotFixed . PrefixMismatch . PrefixedVariant) $
        S.toList unsatisfiedPrefs

-- | Issue 'PrefixMismatch' and 'PrefixCollision' warnings if
-- appropriate
issueVariantPrefixWarnings :: BCP47 -> LintM ()
issueVariantPrefixWarnings (NormalTag n) = issueVariantPrefixWarningsNormal n
issueVariantPrefixWarnings _ = pure ()

-- | Order the variants in a 'Normal' tag according to the standard's
-- recommendations. This cannot be implemented as a newtype-defined
-- custom 'Ord' instance for 'Variant' because the ordering in the
-- standard is context-sensitive. This function is used to implement
-- 'Text.LanguageTag.BCP47.Registry.renderBCP47' and
-- 'Text.LanguageTag.BCP47.Registry.toSubtags'.
orderNormalVariants :: Normal -> [Variant]
orderNormalVariants n = firstVariants <> midVariants <> endVariants
  where
    (chains, havePrefs, noPrefs) = categorizeVariants n
    firstVariants = enumerateChainVariants chains
    midVariants = S.toAscList noPrefs
    endVariants = S.toAscList $ havePrefs S.\\ S.fromList firstVariants

----------------------------------------------------------------
-- Extlang form
----------------------------------------------------------------

-- | Transform a language tag into "extlang form", a variant of the
-- canonical form in which extlang subtags are preserved or added
-- wherever possible, so that, e.g., the tags @cmn@ and @zh-cmn@ will
-- both be transformed into the tag @zh-cmn@. This form is otherwise
-- identical to canonical form. It can be useful notably in
-- environments that process a lot of materials with the primary
-- language subtags @zh@ and @ar@, since these will often still use,
-- e.g., @zh-HK@ or @zh@ for Cantonese documents instead of
-- @yue@. Note that applications can canonicalize tags and still have
-- good matching behaviour by considering things like the
-- macrolanguage relationships between subtags, so that a request for
-- @zh@ materials will return, say, @cmn@ or @yue@ documents as
-- expected.
--
-- Note that the standard permits 'Normal' tags to have an extended
-- language subtag whose prefix conflicts with the tag's primary
-- language subtag. In these cases no replacement is performed and,
-- e.g., the tag @en-cmn@ will remain unchanged by this function.
extlangFormBCP47 :: BCP47 -> LintM BCP47
extlangFormBCP47 x = go <$> canonicalizeBCP47 x
  where
    go (NormalTag y) = NormalTag $ normalToExtlangForm' y
    go y = y

-- | Convert a canonical 'Normal' tag to extlang form
normalToExtlangForm' :: Normal -> Normal
normalToExtlangForm' n = case validateExtlang $ languageToSubtag $ language n of
  Just x
    | Nothing <- extlang n ->
      n
        { language = extlangPrefix $ lookupExtlangRecord x,
          extlang = Just x
        }
  _ -> n
