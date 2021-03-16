-- |
-- Description : Language tag canonicalization
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'canonicalizeBCP47' function to
-- canonicalize 'BCP47' tags according to the relevant section of the
-- specification, available at
-- <https://tools.ietf.org/html/bcp47#section-4.5>. It also exports
-- various helper functions used in that function; these detect and
-- replace tags and subtags with their preferred values.
module Text.LanguageTag.BCP47.Canonicalization
  ( -- * Canonicalization
    canonicalizeBCP47,
    canonicalizeLanguage,
    canonicalizeExtlang,
    canonicalizeRegion,
    canonicalizeScript,
    canonicalizeVariant,

    -- * Extlang form
    extlangFormBCP47,
    normalToExtlangForm,
  )
where

import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry
import Text.LanguageTag.BCP47.Validation (validateExtlang)

-- | Canonicalize a 'BCP47' tag. This involves replacing deprecated
-- tags and subtags with their preferred values, if they exist, and
-- replacing tags with an appropriate primary language subtag and
-- extended language subtag with the guaranteed-to-exist preferred
-- primary language subtag, if applicable. See also
-- 'extlangFormBCP47'.
canonicalizeBCP47 :: BCP47 -> BCP47
canonicalizeBCP47 (NormalTag n) = NormalTag $ canonicalizeNormal n
canonicalizeBCP47 (GrandfatheredTag t) = canonicalizeGrandfathered t
canonicalizeBCP47 x@(PrivateUseTag _) = x

-- | Canonicalize a 'Normal' tag
canonicalizeNormal :: Normal -> Normal
canonicalizeNormal n =
  n'
    { language = canonicalizeLanguage $ language n',
      script = canonicalizeScript <$> script n,
      region = canonicalizeRegion <$> region n,
      variants = S.map canonicalizeVariant $ variants n
    }
  where
    n' = canonicalizeExtlang n

-- | Canonicalize a 'Grandfathered' tag by replacing it with its
-- preferred value, if it is deprecated
canonicalizeGrandfathered :: Grandfathered -> BCP47
canonicalizeGrandfathered t =
  case rangeDeprecation $ lookupGrandfatheredRecord t of
    DeprecatedPreferred x -> NormalTag x
    _ -> GrandfatheredTag t

-- | Replace the primary language subtag and extended language subtag
-- with the extended language subtag's preferred primary language
-- subtag and remove the extended language subtag, so that, e.g., the
-- tag @zh-cmn@ will be transformed by this function into the tag
-- @cmn@.
--
-- Note that the standard permits 'Normal' tags to have an extended
-- language subtag whose prefix conflicts with the tag's primary
-- language subtag. In these cases no replacement is performed and,
-- e.g., the tag @en-cmn@ will remain unchanged by this function.
canonicalizeExtlang :: Normal -> Normal
canonicalizeExtlang n = case extlang n of
  Just el
    | language n == extlangPrefix (lookupExtlangRecord el) ->
      n
        { language = extlangPreferredValue $ lookupExtlangRecord el,
          extlang = Nothing
        }
  _ -> n

-- | Replace a language with its preferred value, if it is deprecated
canonicalizeLanguage :: Language -> Language
canonicalizeLanguage l = case languageDeprecation $ lookupLanguageRecord l of
  DeprecatedPreferred l' -> l'
  _ -> l

-- | Replace a script with its preferred value, if it is deprecated
canonicalizeScript :: Script -> Script
canonicalizeScript l = case scriptDeprecation $ lookupScriptRecord l of
  DeprecatedPreferred l' -> l'
  _ -> l

-- | Replace a region with its preferred value, if it is deprecated
canonicalizeRegion :: Region -> Region
canonicalizeRegion l = case regionDeprecation $ lookupRegionRecord l of
  DeprecatedPreferred l' -> l'
  _ -> l

-- | Replace a variant with its preferred value, if it is deprecated
canonicalizeVariant :: Variant -> Variant
canonicalizeVariant l = case variantDeprecation $ lookupVariantRecord l of
  DeprecatedPreferred l' -> l'
  _ -> l


-- | Transform a language tag into "extlang form", a variant of the
-- canonical form in which extlang subtags are preserved or added
-- wherever possible, so that, e.g., the tags @cmn@ and @zh-cmn@ will
-- both be transformed into the tag @zh-cmn@. This form is otherwise
-- identical to canonical form. It can be useful notably in
-- environments that process a lot of materials with the primary
-- language subtags @zh@ and @ar@, since these will often still use,
-- e.g., @zh-HK@ or @zh@ for Cantonese documents instead of
-- @yue@. Note that applications can canonicalize tags and still have
-- good matching behaviour by considering the macrolanguage
-- relationships between subtags, so that a request for @zh@ materials
-- will return, say, @cmn@ or @yue@ documents as expected.
extlangFormBCP47 :: BCP47 -> BCP47
extlangFormBCP47 (NormalTag n) = NormalTag $ normalToExtlangForm n
extlangFormBCP47 x = canonicalizeBCP47 x

-- | Convert a 'Normal' tag to extlang form
normalToExtlangForm :: Normal -> Normal
normalToExtlangForm n = case validateExtlang $ languageToSubtag $ language n' of
  Nothing -> n'
  Just x ->
    n'
      { language = extlangPrefix $ lookupExtlangRecord x,
        extlang = Just x
      }
  where
    n' = canonicalizeNormal n
