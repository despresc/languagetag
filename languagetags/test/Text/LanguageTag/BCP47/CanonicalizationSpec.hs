{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Testing canonicalization
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.BCP47.CanonicalizationSpec (spec) where

import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( forAllShrink,
    (===),
  )
import Text.LanguageTag.BCP47.Canonicalization
  ( canonicalizeBCP47,
    extlangFormBCP47,
  )
import Text.LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Deprecation (..),
    RangeRecord (..),
    Redundant (..),
    lookupRedundantRecord,
    redundantToValidTag,
  )

-- TODO: more unit testing of canonicalization (not just the redundant
-- tags), and testing of categorizeVariants, lintBCP47, and the
-- warnings that are returned from them. For example, test:
--
-- - that categorizeVariants uses each of the variants of the original
--   tag (and no others, not that it's likely to introduce new
--   variants)
--
-- - that only non-fixed warnings are returned after running lintBCP47
--   twice, and perhaps also that they're the same as the last time
--   (up to reordering?)
--
-- - that enumerateChainVariants has the following property when
--   called on a VariantChains value produced by categorizeVariants:
--   if x and y are variants in the input and x comes before y in one
--   of the listVariantChains, then x comes before y in the resulting
--   enumeration.
--
-- - that enumerateChainVariants behaves the same as reverse . ordNub
--   . reverse . depthFirstTraversal (in which case we could replace
--   the implementation of enumerateVariantChains with that function)

redundantTags :: [(Redundant, Text)]
redundantTags =
  [ (AzArab, "az-Arab"),
    (AzCyrl, "az-Cyrl"),
    (AzLatn, "az-Latn"),
    (BeLatn, "be-Latn"),
    (BsCyrl, "bs-Cyrl"),
    (BsLatn, "bs-Latn"),
    (De1901, "de-1901"),
    (De1996, "de-1996"),
    (DeAt1901, "de-AT-1901"),
    (DeAt1996, "de-AT-1996"),
    (DeCh1901, "de-CH-1901"),
    (DeCh1996, "de-CH-1996"),
    (DeDe1901, "de-DE-1901"),
    (DeDe1996, "de-DE-1996"),
    (EnBoont, "en-boont"),
    (EnScouse, "en-scouse"),
    (Es419, "es-419"),
    (IuCans, "iu-Cans"),
    (IuLatn, "iu-Latn"),
    (MnCyrl, "mn-Cyrl"),
    (MnMong, "mn-Mong"),
    (SgnBr, "sgn-BR"),
    (SgnCo, "sgn-CO"),
    (SgnDe, "sgn-DE"),
    (SgnDk, "sgn-DK"),
    (SgnEs, "sgn-ES"),
    (SgnFr, "sgn-FR"),
    (SgnGb, "sgn-GB"),
    (SgnGr, "sgn-GR"),
    (SgnIe, "sgn-IE"),
    (SgnIt, "sgn-IT"),
    (SgnJp, "sgn-JP"),
    (SgnMx, "sgn-MX"),
    (SgnNi, "sgn-NI"),
    (SgnNl, "sgn-NL"),
    (SgnNo, "sgn-NO"),
    (SgnPt, "sgn-PT"),
    (SgnSe, "sgn-SE"),
    (SgnUs, "sgn-US"),
    (SgnZa, "sgn-ZA"),
    (SlNedis, "sl-nedis"),
    (SlRozaj, "sl-rozaj"),
    (SrCyrl, "sr-Cyrl"),
    (SrLatn, "sr-Latn"),
    (TgArab, "tg-Arab"),
    (TgCyrl, "tg-Cyrl"),
    (UzCyrl, "uz-Cyrl"),
    (UzLatn, "uz-Latn"),
    (YiLatn, "yi-Latn"),
    (ZhHans, "zh-Hans"),
    (ZhHansCn, "zh-Hans-CN"),
    (ZhHansHk, "zh-Hans-HK"),
    (ZhHansMo, "zh-Hans-MO"),
    (ZhHansSg, "zh-Hans-SG"),
    (ZhHansTw, "zh-Hans-TW"),
    (ZhHant, "zh-Hant"),
    (ZhHantCn, "zh-Hant-CN"),
    (ZhHantHk, "zh-Hant-HK"),
    (ZhHantMo, "zh-Hant-MO"),
    (ZhHantSg, "zh-Hant-SG"),
    (ZhHantTw, "zh-Hant-TW"),
    (ZhCmn, "zh-cmn"),
    (ZhCmnHans, "zh-cmn-Hans"),
    (ZhCmnHant, "zh-cmn-Hant"),
    (ZhGan, "zh-gan"),
    (ZhWuu, "zh-wuu"),
    (ZhYue, "zh-yue")
  ]

spec :: Spec
spec = do
  -- could consider generating canonical tags and then testing
  -- canonicalizeBCP47 on those, but I think that might be more
  -- trouble than it's worth
  describe "canonicalizeBCP47" $ do
    prop "should be idempotent" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let tg' = snd (canonicalizeBCP47 tg)
         in snd (canonicalizeBCP47 tg') === tg'
    prop "composes with extlangFormBCP47 on the left" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let tg' = snd $ canonicalizeBCP47 tg
         in snd (canonicalizeBCP47 (snd $ extlangFormBCP47 tg')) === tg'
    prop "composes with extlangFormBCP47 on the right" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let tg' = snd $ extlangFormBCP47 tg
         in snd (extlangFormBCP47 $ snd $ canonicalizeBCP47 tg') === tg'
    describe "correctly returns the preferred value of the deprecated redundant tag" $ do
      let depPrefer (r, t) = case rangeDeprecation $ lookupRedundantRecord r of
            DeprecatedPreferred n -> Just (NormalTag n, redundantToValidTag r, t)
            _ -> Nothing
      let redundantTags' = mapMaybe depPrefer redundantTags
      let test (canon, r, l) =
            it (T.unpack l) $
              snd (canonicalizeBCP47 r) `shouldBe` canon
      traverse_ test redundantTags'
  describe "extlangFormBCP47" $ do
    prop "should be idempotent" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let tg' = snd $ extlangFormBCP47 tg
         in snd (extlangFormBCP47 tg') === tg'
