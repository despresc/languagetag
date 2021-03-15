-- |
-- Module      : Text.LanguageTag.BCP47.Canonicalization
-- Description : Language tag validation
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
module Text.LanguageTag.BCP47.Canonicalization where

import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry

-- | Canonicalize a 'BCP47' tag. Since the variant subtags and
-- extensions of a 'BCP47' tag are already sorted, this function only
-- replaces deprecated tags and subtags with their preferred values,
-- if they exist, and replaces tags with a primary language subtag and
-- extended language subtag with the guaranteed-to-exist preferred
-- primary language subtag.
canonicalizeBCP47 :: BCP47 -> BCP47
canonicalizeBCP47 (NormalTag n) =
  NormalTag
    n'
      { script = maybeDep script scriptDeprecation lookupScriptRecord,
        region = maybeDep region regionDeprecation lookupRegionRecord,
        variants = S.map (dep variantDeprecation lookupVariantRecord) $ variants n
      }
  where
    n' = case extlang n of
      Just el
        | language n == extlangPrefix (lookupExtlangRecord el) ->
          n
            { language = extlangPreferredValue $ lookupExtlangRecord el,
              extlang = Nothing
            }
      _ ->
        n
          { language = dep languageDeprecation lookupLanguageRecord $ language n
          }
    dep proj look x = case proj $ look x of
      DeprecatedPreferred x' -> x'
      _ -> x
    maybeDep proj1 proj2 look = dep proj2 look <$> proj1 n
canonicalizeBCP47 (GrandfatheredTag t) = case rangeDeprecation $ lookupGrandfatheredRecord t of
  DeprecatedPreferred x -> NormalTag x
  _ -> GrandfatheredTag t
canonicalizeBCP47 x@(PrivateUse _) = x
