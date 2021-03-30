-- |
-- Description : Language tag canonicalization
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'canonicalizeBCP47' function to
-- canonicalize 'BCP47' tags according to the [relevant
-- section](https://tools.ietf.org/html/bcp47#section-4.5) of the
-- BCP47 specification. It also exports various helper functions used
-- in that function; these detect and replace tags and subtags with
-- their preferred values.
--
-- This module also provides the 'lintBCP47' function and other types
-- to
module Text.LanguageTag.BCP47.Canonicalization
  ( lintBCP47,
    LintWarnings (..),
    LintWarning (..),
    DeprecatedComponent (..),
    Internal.PrefixedComponent (..),

    -- * Canonicalization
    canonicalizeBCP47,
    canonicalizeNormal,
    canonicalizeLanguage,
    canonicalizeExtlang,
    canonicalizeScript,
    canonicalizeRegion,
    canonicalizeVariant,

    -- * Extlang form
    extlangFormBCP47,

    -- * Other normalization
    suppressLanguageScript,
    suppressLanguageScriptNormal,
    restoreLanguageScript,
    restoreLanguageScriptNormal,
    Internal.issueVariantPrefixWarnings,
    removeMismatchedVariants,
  )
where

import Text.LanguageTag.BCP47.Registry
import Text.LanguageTag.Internal.BCP47.Canonicalization
  ( DeprecatedComponent (..),
    LintWarning (..),
    LintWarnings (..),
    runLintM,
  )
import qualified Text.LanguageTag.Internal.BCP47.Canonicalization as Internal

-- | This function canonicalizes its input with 'canonicalizeBCP47',
--  suppresses the script when appropriate with
--  'suppressLanguageScript', then warns about (but does not remove)
--  variant subtags with prefixes in the registry that aren't
--  satisfied by the rest of the tag or with prefixes that collide
--  with the prefixes of other variants with
--  'Internal.issueVariantPrefixWarnings'. A tag returned by this
--  function (especially with no 'notFixed' warnings) will satisfy
--  many of recommendations scattered throughout the standard that can
--  be checked (and sometimes fixed) automatically in a reasonable
--  way.

-- TODO: really ought to give this function some configuration (e.g.,
-- whether or not to fix certain things that can be automatically
-- fixed)
lintBCP47 :: BCP47 -> (LintWarnings, BCP47)
lintBCP47 t = runLintM $ do
  t' <- Internal.canonicalizeBCP47 t
  t'' <- Internal.suppressLanguageScript t'
  Internal.issueVariantPrefixWarnings t''
  pure t''

----------------------------------------------------------------
-- Canonicalization
----------------------------------------------------------------

-- | Canonicalize a 'BCP47' tag by replacing the language and extended
-- language with their single language subtag equivalents and
-- replacing any deprecated tags or subtags with their non-deprecated
-- preferred values, whenever possible. See also 'canonicalizeExtlang'
-- and 'extlangFormBCP47'.
canonicalizeBCP47 :: BCP47 -> (LintWarnings, BCP47)
canonicalizeBCP47 = runLintM . Internal.canonicalizeBCP47

-- | Canonicalize a 'Normal' tag by replacing correct language and
-- extended language pairs with their single language subtag
-- equivalents, and replacing any deprecated subtags with their
-- non-deprecated preferred values, if possible
canonicalizeNormal :: Normal -> (LintWarnings, Normal)
canonicalizeNormal = runLintM . Internal.canonicalizeNormal

-- | Canonicalize a 'Language' subtag by replacing it with its
-- preferred value, if applicable
canonicalizeLanguage :: Language -> (LintWarnings, Language)
canonicalizeLanguage = runLintM . Internal.canonicalizeLanguage

-- | Canonicalize the language and extended language in a tag by
-- replacing the pair with their preferred single language subtag
-- replacement, if applicable. This process will, e.g., transform the
-- tag @zh-cmn-hant-x-more@ into tag @cmn-hant-x-more@. This function
-- will return an (unfixed) 'Internal.PrefixedComponent' warning if
-- there is a mismatch between the extended language subtag's prefix
-- and the primary language subtag.
canonicalizeExtlang :: Normal -> (LintWarnings, Normal)
canonicalizeExtlang = runLintM . Internal.canonicalizeExtlang

-- | Canonicalize a 'Script' subtag by replacing it with its
-- preferred value, if applicable
canonicalizeScript :: Script -> (LintWarnings, Script)
canonicalizeScript = runLintM . Internal.canonicalizeScript

-- | Canonicalize a 'Region' subtag by replacing it with its preferred
-- value, if applicable
canonicalizeRegion :: Region -> (LintWarnings, Region)
canonicalizeRegion = runLintM . Internal.canonicalizeRegion

-- | Canonicalize a 'Variant' subtag by replacing it with its
-- preferred value, if applicable
canonicalizeVariant :: Variant -> (LintWarnings, Variant)
canonicalizeVariant = runLintM . Internal.canonicalizeVariant

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
suppressLanguageScript :: BCP47 -> (LintWarnings, BCP47)
suppressLanguageScript = runLintM . Internal.suppressLanguageScript

-- | Suppress the script subtag of a tag if it appears in the
-- 'languageScriptSuppression' of the tag's 'Language'
suppressLanguageScriptNormal :: Normal -> (LintWarnings, Normal)
suppressLanguageScriptNormal = runLintM . Internal.suppressLanguageScriptNormal

-- | Add the 'languageScriptSuppression' back to a tag, if it does not
-- conflict with an existing script subtag. This only affects 'Normal'
-- tags.
restoreLanguageScript :: BCP47 -> BCP47
restoreLanguageScript (NormalTag n) = NormalTag $ restoreLanguageScriptNormal n
restoreLanguageScript x = x

-- | Add the 'languageScriptSuppression' back to a tag, if it does not
-- conflict with an existing script subtag
restoreLanguageScriptNormal :: Normal -> Normal
restoreLanguageScriptNormal n
  | Nothing <- script n =
    n {script = ssup}
  | otherwise = n
  where
    ssup = languageScriptSuppression $ lookupLanguageRecord $ language n

-- | Remove the variants from a tag with prefixes that are not
-- satisfied by the rest of the tag. Also warn about any prefix
-- collisions (multiple variants with satisfied prefixes that cannot
-- be put into a single chain together) in the tag. If you would like
-- to know exactly how this is done, and how this relates to the
-- standard, consult the documentation for the internal function
-- 'Internal.categorizeVariants'.
removeMismatchedVariants :: BCP47 -> (LintWarnings, BCP47)
removeMismatchedVariants = runLintM . Internal.removeMismatchedVariants

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
-- e.g., the tag @en-cmn@ will remain unchanged by this function. This
-- will still be noted in the returned 'LintWarnings'.
extlangFormBCP47 :: BCP47 -> (LintWarnings, BCP47)
extlangFormBCP47 = runLintM . Internal.extlangFormBCP47
