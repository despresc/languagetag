{-# LANGUAGE BangPatterns #-}

-- |
-- Description : Language tag canonicalization
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'canonicalizeBCP47' function to
-- canonicalize 'BCP47' tags according to the [relevant
-- section](https://tools.ietf.org/html/bcp47#section-4.5) of the
-- BCP47 specification. It also provides various helper functions used
-- in that function; these detect and replace tags and subtags with
-- their preferred values. The 'lintBCP47' function and related
-- functions are also exported; these check that a given tag follows
-- various other recommendations in the standard.
module Text.LanguageTag.BCP47.Canonicalization
  ( -- * General linting
    lintBCP47,
    LintWarnings (..),
    DeprecatedComponent (..),

    -- * Canonicalization
    canonicalizeBCP47,
    canonicalizeNormal,
    canonicalizeGrandfathered,
    extlangFormBCP47,
    CanonicalWarnings (..),
    ExtlangWarning (..),

    -- * Script suppression
    suppressScript,
    SuperfluousScriptWarning (..),

    -- * Variant prefix checking and ordering
    getVariantWarnings,
    VariantWarnings (..),
    PrefixCollision (..),
    orderNormalVariants,
    VariantChains (..),
    categorizeVariants,
    depthFirstEnumeration,
    listVariantChains,
    enumerateChainVariants,
    variantsInChains,
  )
where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
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

-- | This function, in order:
--
-- * canonicalizes its input with 'canonicalizeBCP47',
--
-- * suppresses the script when appropriate with 'suppressScript', then
--
-- * warns about variant subtags with prefixes in the registry that
--   aren't satisfied by the rest of the tag or with prefixes that
--   collide with the prefixes of other variants with
--   'getVariantWarnings'.
--
-- After linting, a tag will satisfy many of recommendations scattered
-- throughout the standard that can be checked (and sometimes fixed)
-- automatically in a reasonable way. Variants and extended language
-- subtags with unsatisfied prefixes, or variants with colliding
-- prefixes, on the other hand, can't be fixed as reasonably, and so
-- this function only warns about those problems.
lintBCP47 :: BCP47 -> (LintWarnings, BCP47)
lintBCP47 b = (LintWarnings cw sw vw, b'')
  where
    (cw, b') = canonicalizeBCP47 b
    (sw, b'') = suppressScript b'
    vw = getVariantWarnings b''

-- | The possible warnings that can be issued from 'lintBCP47'
data LintWarnings = LintWarnings
  { canonicalWarnings :: !CanonicalWarnings,
    scriptWarning :: !SuperfluousScriptWarning,
    variantWarnings :: !VariantWarnings
  }
  deriving (Eq, Ord, Show)

instance Semigroup LintWarnings where
  LintWarnings a b c <> LintWarnings x y z =
    LintWarnings (a <> x) (b <> y) (c <> z)

instance Monoid LintWarnings where
  mempty = LintWarnings mempty mempty mempty

-- | A deprecated subtag was used in the tag, or the tag itself was
-- deprecated
data DeprecatedComponent
  = DeprecatedLanguage !Language
  | DeprecatedExtlang !Extlang
  | DeprecatedScript !Script
  | DeprecatedRegion !Region
  | DeprecatedVariant !Variant
  | DeprecatedRedundant !Redundant
  | DeprecatedGrandfathered !Grandfathered
  deriving (Eq, Ord, Show)

-- | Warnings that may be issued during canonicalization
data CanonicalWarnings = CanonicalWarnings
  { deprecatedComponents :: !(Set DeprecatedComponent),
    extlangWarning :: !ExtlangWarning
  }
  deriving (Eq, Ord, Show)

singleDeprecation :: DeprecatedComponent -> CanonicalWarnings
singleDeprecation c = mempty {deprecatedComponents = S.singleton c}

warnExtlang :: ExtlangWarning -> CanonicalWarnings
warnExtlang = CanonicalWarnings mempty

-- | prefers the rightmost 'extlangWarning' and otherwise merges
instance Semigroup CanonicalWarnings where
  CanonicalWarnings x y <> CanonicalWarnings a b =
    CanonicalWarnings (x <> a) (y <> b)

instance Monoid CanonicalWarnings where
  mempty = CanonicalWarnings mempty mempty

-- | Warnings that may be issued when canonicalizing the extended
-- language subtag
data ExtlangWarning
  = -- | the prefix of the extended language subtag didn't match the tag
    ExtlangPrefixMismatch !Extlang
  | -- | used a primary and extended language pair instead of the
    -- preferred primary language replacement
    UsedExtlang !Extlang
  | -- | no warning issued
    NoExtlangWarning
  deriving (Eq, Ord, Show)

instance Semigroup ExtlangWarning where
  x <> NoExtlangWarning = x
  _ <> x = x

instance Monoid ExtlangWarning where
  mempty = NoExtlangWarning

-- | Canonicalize a 'BCP47' tag according to the standard by replacing
-- the language and extended language with their single language
-- subtag equivalents and replacing any deprecated tags (grandfathered
-- or redundant) or subtags with their non-deprecated preferred
-- values, whenever possible. This function will not do anything to a
-- 'PrivateUseTag'. See [section
-- 4.5](https://tools.ietf.org/html/bcp47#section-4.5) of the standard
-- for details. Also see 'extlangFormBCP47' for an alternate canonical
-- form for language tags.
canonicalizeBCP47 :: BCP47 -> (CanonicalWarnings, BCP47)
canonicalizeBCP47 (NormalTag t) = NormalTag <$> canonicalizeNormal t
canonicalizeBCP47 (PrivateUseTag t) = (mempty, PrivateUseTag t)
canonicalizeBCP47 (GrandfatheredTag t) = canonicalizeGrandfathered t

addDeprecated :: DeprecatedComponent -> CanonicalWarnings -> CanonicalWarnings
addDeprecated c w = w {deprecatedComponents = S.insert c $ deprecatedComponents w}

-- | Canonicalize a 'Normal' tag
canonicalizeNormal :: Normal -> (CanonicalWarnings, Normal)
canonicalizeNormal n = case recognizeRedundantNormal n of
  Nothing -> (w'', n'')
  Just red -> case rangeDeprecation $ lookupRedundantRecord red of
    DeprecatedPreferred x -> (addDeprecated (DeprecatedRedundant red) w'', x)
    DeprecatedSimple -> (addDeprecated (DeprecatedRedundant red) w'', n'')
    NotDeprecated -> (w'', n'')
  where
    getWarning _ _ Nothing = (Nothing, Nothing)
    getWarning f g (Just a) = case f a of
      NotDeprecated -> (Nothing, Just a)
      DeprecatedSimple -> (Just $ g a, Just a)
      DeprecatedPreferred a' -> (Just $ g a, Just a')
    addDep mc = maybe id addDeprecated mc
    (lw, n') = canonicalizeLanguage n
    (ds, s) = getWarning (scriptDeprecation . lookupScriptRecord) DeprecatedScript $ script n'
    (dr, r) = getWarning (regionDeprecation . lookupRegionRecord) DeprecatedRegion $ region n'
    (dvs, vs) = canonicalizeVariants $ variants n'
    dvs' = S.mapMonotonic DeprecatedVariant dvs
    n'' = n' {script = s, region = r, variants = vs}
    w'' = addDep dr $ addDep ds $ lw <> mempty {deprecatedComponents = dvs'}

-- | Canonicalize the entire language section (primary language and
-- extended language) of a tag by replacing it or its subtags with
-- their preferred values, if possible
canonicalizeLanguage :: Normal -> (CanonicalWarnings, Normal)
canonicalizeLanguage n@Normal {language = l, extlang = me} =
  (lw <> ew, n'')
  where
    (lw, n') = case languageDeprecation $ lookupLanguageRecord l of
      NotDeprecated -> (mempty, n)
      DeprecatedPreferred x ->
        (singleDeprecation $ DeprecatedLanguage l, n {language = x})
      DeprecatedSimple ->
        (singleDeprecation $ DeprecatedLanguage l, n)
    (ew, n'') = case me of
      Nothing -> (mempty, n')
      Just e
        | language n' == extlangPrefix recd ->
          ( edepWarning <> warnExtlang (UsedExtlang e),
            n' {language = extlangPreferredValue recd, extlang = Nothing}
          )
        | otherwise ->
          (edepWarning <> warnExtlang (ExtlangPrefixMismatch e), n')
        where
          recd = lookupExtlangRecord e
          edepWarning =
            if extlangDeprecation recd
              then singleDeprecation $ DeprecatedExtlang e
              else mempty

-- | Return the variants in the set that are deprecated, and the set
-- after replacing the variants with their preferred values
canonicalizeVariants :: Set Variant -> (Set Variant, Set Variant)
canonicalizeVariants = S.foldl' go (mempty, mempty)
  where
    go (dep, acc) v = case variantDeprecation $ lookupVariantRecord v of
      NotDeprecated -> (dep, S.insert v acc)
      DeprecatedSimple -> (S.insert v dep, S.insert v acc)
      DeprecatedPreferred v' -> (S.insert v dep, S.insert v' acc)

-- | Canonicalize a 'Grandfathered' tag by replacing it with its
-- preferred 'Normal' tag, if it exists
canonicalizeGrandfathered :: Grandfathered -> (CanonicalWarnings, BCP47)
canonicalizeGrandfathered t =
  case rangeDeprecation $ lookupGrandfatheredRecord t of
    DeprecatedPreferred x ->
      (singleDeprecation $ DeprecatedGrandfathered t, NormalTag x)
    DeprecatedSimple ->
      (singleDeprecation $ DeprecatedGrandfathered t, GrandfatheredTag t)
    NotDeprecated ->
      (mempty, GrandfatheredTag t)

-- | Transform a language tag into "extlang form", a variant of the
-- canonical form in which extlang subtags are preserved or added
-- wherever possible, so that, e.g., the tags @cmn@ and @zh-cmn@ will
-- both be transformed into the tag @zh-cmn@ (the latter of which is,
-- incidentally, a deprecated redundant tag, as of this writing). This
-- form is otherwise identical to canonical form, and applications
-- should treat the two forms as equivalent; this is most easily
-- accomplished by picking a particular form and uniformly converting
-- all tags to it.
--
-- Extlang form may be useful in matching or lookup, so that, for
-- example, a query for @zh@ materials will return things tagged with
-- @cmn@ or @yue@, since those tags will have been transformed into
-- @zh-cmn@ and @zh-yue@, respectively. Note that applications can
-- operate on canonicalized tags and still have good matching
-- behaviour by considering things like the macrolanguage
-- relationships between subtags; in the previous example, both @cmn@
-- and @yue@ have the macrolanguage @zh@, so an application that is
-- aware of that fact will correctly return @cmn@ and @yue@ material
-- when faced with a query for @zh@-tagged documents.
--
-- The standard permits 'Normal' tags to have an extended language
-- subtag whose prefix conflicts with the tag's primary language
-- subtag. In these cases no replacement is performed and, e.g., the
-- tag @en-cmn@ will remain unchanged by this function. This will
-- still be noted in the returned 'CanonicalWarnings'.
extlangFormBCP47 :: BCP47 -> (CanonicalWarnings, BCP47)
extlangFormBCP47 x = go $ canonicalizeBCP47 x
  where
    removeUsed (UsedExtlang _) = NoExtlangWarning
    removeUsed a = a
    go (w, NormalTag y) = case validateExtlang $ languageToSubtag $ language y of
      Just y'
        | Nothing <- extlang y ->
          ( w {extlangWarning = removeUsed $ extlangWarning w},
            NormalTag $
              y
                { language = extlangPrefix $ lookupExtlangRecord y',
                  extlang = Just y'
                }
          )
      _ -> (w, NormalTag y)
    go (w, t) = (w, t)

----------------------------------------------------------------
-- Superfluous script suppression
----------------------------------------------------------------

-- | A warning that records the use of a superfluous script in a tag
data SuperfluousScriptWarning
  = SuperfluousLanguageScript !Script !Language
  | SuperfluousExtlangScript !Script !Extlang
  | NoSuperfluousScript
  deriving (Eq, Ord, Show)

instance Semigroup SuperfluousScriptWarning where
  x <> NoSuperfluousScript = x
  _ <> x = x

instance Monoid SuperfluousScriptWarning where
  mempty = NoSuperfluousScript

-- | Suppress the script subtag of a tag if it appears in the
-- @Suppress-Script@ field of the tag's primary language subtag. See
-- [section 3.1.9](https://tools.ietf.org/html/bcp47#section-3.1.9) of
-- the standard for details.
--
-- It is a little unclear what should happen if an extended language
-- subtag with an unsatisfied prefix and a script suppression is
-- present; one might argue that no suppression should take place,
-- since the extended language subtag does not make sense without its
-- one @Prefix@. This function, on the other hand, suppresses the
-- script associated to an extended language subtag
-- unconditionally. There are (as of this writing) no extended
-- language subtags in the registry with a @Suppress-Script@, so this
-- will, for now, not be an issue either way.
suppressScript :: BCP47 -> (SuperfluousScriptWarning, BCP47)
suppressScript t@(NormalTag n@Normal {language = l, extlang = me, script = Just s})
  | languageScriptSuppression (lookupLanguageRecord l) == Just s =
    (SuperfluousLanguageScript s l, NormalTag n {script = Nothing})
  | Just e <- me,
    extlangScriptSuppression (lookupExtlangRecord e) == Just s =
    (SuperfluousExtlangScript s e, NormalTag n {script = Nothing})
  | otherwise = (mempty, t)
suppressScript x = (mempty, x)

----------------------------------------------------------------
-- Variant prefix checking
----------------------------------------------------------------

-- | Warnings that may be issued when checking the prefixes of
-- variants in a tag
data VariantWarnings = VariantWarnings
  { variantPrefixCollisions :: !PrefixCollision,
    variantPrefixMismatches :: !(Set Variant)
  }
  deriving (Eq, Ord, Show)

instance Semigroup VariantWarnings where
  VariantWarnings x a <> VariantWarnings y b = VariantWarnings (x <> y) (a <> b)

instance Monoid VariantWarnings where
  mempty = VariantWarnings NoPrefixCollision mempty

-- | Multiple variants with satisfied prefixes were used in the tag,
-- but they couldn't be arranged in a single chain (see
-- 'VariantChains'). For example, in the tag @de-1901-1996@ the
-- variants @1901@ and @1996@ collide: both (as of this writing) have
-- only the prefix @de@ in both of their records, so both @de-1901@
-- and @de-1996@ are valid, but neither has a prefix that also
-- includes the other. The lists in this warning are the maximal lists
-- of variants that do not collide with each other. In the example
-- above, they would be
--
-- @
-- 'PrefixCollision' ('Text.LanguageTag.BCP47.Registry.Variant.Var1901' ':|' []) (('Text.LanguageTag.BCP47.Registry.Variant.Var1996' ':|' []) ':|' [])
-- @
data PrefixCollision
  = PrefixCollision !(NonEmpty Variant) !(NonEmpty (NonEmpty Variant))
  | -- | no prefix collisions were found, or this was not checked
    NoPrefixCollision
  deriving (Eq, Ord, Show)

instance Semigroup PrefixCollision where
  x <> NoPrefixCollision = x
  _ <> x = x

instance Monoid PrefixCollision where
  mempty = NoPrefixCollision

-- | A tree with 'Variant' nodes. This type is intended to represent
-- the maximal chains of variants with satisfied prefixes that can
-- occur in a tag, organized by grouping together the common prefixes
-- of the chains. Here, a "chain" of variants is a list of variants
-- with prefixes such that the last variant has a prefix in the
-- registry that includes all of the other variants in the chain and
-- matches the entire tag before the variant subtag section.
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
      | null vcs = (v :| []) : l
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
splitPrefixedVariants =
  S.partition $ not . null . variantPrefixes . lookupVariantRecord

-- | The 'categorizeVariants' function analyzes the variants of a
-- BCP47 tag, categorizing them by whether or not they have prefixes
-- in the registry, and by how their prefixes compare to the rest of
-- the tag. The result of @'categorizeVariants' n@ is the triple
-- @(chains, prefs, noprefs)@, where
--
-- * @chains@ is the list of maximal chains of variants from
--   @'variants' n@ according to their prefix fields in the registry,
--   organized into a tree by common prefixes (with siblings sorted
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
-- all have prefixes in the registry, so these would be the content of
-- @prefs@. None of the prefixes in the registry entry for @1606nict@
-- contain @sl@ (as of writing this, though it is extremely unlikely
-- that @sl@ will ever be added to its prefixes), so it would not
-- appear in the any of the chains. The other variants with prefixes
-- can be arranged in chains so that their prefixes are satisfied: the
-- maximal chains are @rozaj-biske-1994@ and @rozaj-njiva-1994@, and
-- so the @chains@ output would be
--
-- @
-- 'VariantChains'
--   [('Text.LanguageTag.BCP47.Registry.Variant.Rozaj',
--     'VariantChains'
--       [('Text.LanguageTag.BCP47.Registry.Variant.Biske', 'VariantChains' [('Text.LanguageTag.BCP47.Registry.Variant.Var1994', 'VariantChains' [])]),
--        ('Text.LanguageTag.BCP47.Registry.Variant.Njiva', 'VariantChains' [('Text.LanguageTag.BCP47.Registry.Variant.Var1994', 'VariantChains' [])])
--       ])]
-- @
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

-- | Order the variants in a 'Normal' tag according to the standard's
-- recommendations in [item 6, page
-- 57](https://tools.ietf.org/html/bcp47#page-1-57). This cannot be
-- implemented as an 'Ord' instance for a newtype over 'Variant'
-- because the ordering in the standard is context-sensitive. Since
-- the standard is not totally precise about the ordering of variants
-- that don't satisfy its recommendations, this function uses the
-- ordering
--
-- @
-- 'concat' [ 'enumerateChainVariants' chains
--        , 'S.toAscList' noprefs
--        , 'S.toAscList' $ prefs 'S.\\' 'variantsInChains' chains ]
-- @
--
-- where @(chains, prefs, noprefs)@ is the output of
-- 'categorizeVariants' on the given 'Normal' tag. This order should
-- result in the best matching behaviour for tags with variants that
-- clash with each other or have unsatisfied prefixes. This function,
-- and so this order, is used to implement
-- 'Text.LanguageTag.BCP47.Registry.renderBCP47' and
-- 'Text.LanguageTag.BCP47.Registry.toSubtags'.
--
-- As an example, the tag @sl-rozaj-biske-njiva-1994-alalc97-1606nict@
-- used in the documentation of 'categorizeVariants' has its variants
-- ordered exactly as they would be by this function.
orderNormalVariants :: Normal -> [Variant]
orderNormalVariants n = firstVariants <> midVariants <> endVariants
  where
    (chains, havePrefs, noPrefs) = categorizeVariants n
    firstVariants = enumerateChainVariants chains
    midVariants = S.toAscList noPrefs
    endVariants = S.toAscList $ havePrefs S.\\ S.fromList firstVariants

-- | Issue unsatisfied prefix and prefix collision warnings about the
-- variants, using 'categorizeVariants'
getVariantWarnings :: BCP47 -> VariantWarnings
getVariantWarnings (NormalTag n) =
  VariantWarnings
    { variantPrefixCollisions = collision,
      variantPrefixMismatches = unsatisfiedPrefs
    }
  where
    (chains, havePrefs, _) = categorizeVariants n
    collision = case listVariantChains chains of
      (x : y : ys) -> PrefixCollision x (y NE.:| ys)
      _ -> NoPrefixCollision
    satisfiedPrefs = variantsInChains chains
    unsatisfiedPrefs = havePrefs S.\\ satisfiedPrefs
getVariantWarnings _ = VariantWarnings mempty mempty
