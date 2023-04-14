{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Description : Testing canonicalization
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module LanguageTag.BCP47.CanonicalizationSpec (spec) where

import Control.Applicative ((<|>))
import Data.Foldable (toList, traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( forAllShrink,
    (===),
  )
import LanguageTag.BCP47.Canonicalization
  ( CanonicalWarnings (..),
    DeprecatedComponent (..),
    ExtlangWarning (..),
    LintWarnings (..),
    PrefixCollision (..),
    SuperfluousScriptWarning (..),
    VariantWarnings (..),
    canonicalizeBCP47,
    categorizeVariants,
    depthFirstEnumeration,
    enumerateChainVariants,
    extlangFormBCP47,
    lintBCP47,
    listVariantChains,
  )
import LanguageTag.BCP47.Quasi (validtag)
import LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Deprecation (..),
    Extlang (..),
    Language (..),
    Normal (..),
    RangeRecord (..),
    Redundant (..),
    Script (..),
    Variant (..),
    lookupRedundantRecord,
    orderNormalVariants,
    redundantToValidTag,
    simpleTag,
  )

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

lintExamples :: [(BCP47, (BCP47, LintWarnings))]
lintExamples =
  [ ( [validtag|de-1996-1901|],
      ( [validtag|de-1996-1901|],
        LintWarnings
          { canonicalWarnings = mempty,
            scriptWarning = mempty,
            variantWarnings =
              VariantWarnings
                { variantPrefixCollisions = PrefixCollision (Var1901 :| []) ((Var1996 :| []) :| []),
                  variantPrefixMismatches = mempty
                }
          }
      )
    ),
    ( [validtag|zh-cmn|],
      ( [validtag|cmn|],
        LintWarnings
          { canonicalWarnings =
              CanonicalWarnings
                { deprecatedComponents = S.singleton $ DeprecatedRedundant ZhCmn,
                  extlangWarning = UsedExtlang ExtCmn
                },
            scriptWarning = mempty,
            variantWarnings = mempty
          }
      )
    ),
    ( [validtag|en-cmn|],
      ( [validtag|en-cmn|],
        LintWarnings
          { canonicalWarnings =
              CanonicalWarnings
                { deprecatedComponents = mempty,
                  extlangWarning = ExtlangPrefixMismatch ExtCmn
                },
            scriptWarning = mempty,
            variantWarnings = mempty
          }
      )
    ),
    ( [validtag|en-Latn|],
      ( [validtag|en|],
        LintWarnings
          { canonicalWarnings = mempty,
            scriptWarning = SuperfluousLanguageScript Latn En,
            variantWarnings = mempty
          }
      )
    ),
    ( [validtag|en-1606nict|],
      ( [validtag|en-1606nict|],
        LintWarnings
          { canonicalWarnings = mempty,
            scriptWarning = mempty,
            variantWarnings = VariantWarnings mempty $ S.singleton Var1606nict
          }
      )
    )
  ]

-- | A list of @(inittag, finaltag)@ examples
canonicalizationExamples :: [(BCP47, BCP47)]
canonicalizationExamples =
  [ ( [validtag|art-lojban|],
      simpleTag Jbo
    ),
    ( [validtag|zh-min-nan|],
      simpleTag Nan
    ),
    ( [validtag|zh-cmn|],
      simpleTag Cmn
    ),
    ( [validtag|zh-yue|],
      simpleTag Yue
    ),
    -- the ajp extlang is the sole extlang (as of writing) whose preferred value
    -- is itself deprecated with a preferred value, so we test that
    -- canonicalization gets to the correct value
    ( [validtag|ar-ajp|],
      simpleTag Apc
    )
  ]

-- | The relative order of two elements @x@ and @y@ in a list of paths
-- of elements
data PathOrder
  = -- | there is a path where @x@ comes before @y@
    Before
  | -- | there is a path where @y@ comes before @x@
    After
  | -- | both 'Before' and 'After' hold
    Both
  | -- | neither 'Before' nor 'After' hold
    Incomparable

-- Get the path order of two _distinct_ elements
getPathOrder :: Eq a => [[a]] -> a -> a -> PathOrder
getPathOrder ls x y = summary (False, False) $ mapMaybe go ls
  where
    go l = do
      xn <- List.findIndex (== x) l
      yn <- List.findIndex (== y) l
      pure $ xn < yn
    summary (_, !b) (True : xs) = summary (True, b) xs
    summary (!a, _) (False : xs) = summary (a, True) xs
    summary (a, b) []
      | a && b = Both
      | a = Before
      | b = After
      | otherwise = Incomparable

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
    it "canonicalizes the examples correctly" $ do
      let badPair (x, y)
            | y == x' = Nothing
            | otherwise = Just (x, y, x')
            where
              (_, x') = canonicalizeBCP47 x
      badPair `shouldNotMatch` canonicalizationExamples
  describe "extlangFormBCP47" $ do
    prop "should be idempotent" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let tg' = snd $ extlangFormBCP47 tg
         in snd (extlangFormBCP47 tg') === tg'
  describe "enumerateChainVariants" $ do
    prop "behaves like a DFT and reverse-nub" $
      forAllShrink genValidNormal shrinkValidNormal $ \tg ->
        let (chains, _, _) = categorizeVariants tg
            ordNub' _ [] = []
            ordNub' s (x : xs)
              | x `S.member` s = ordNub' s xs
              | otherwise = x : ordNub' (S.insert x s) xs
         in enumerateChainVariants chains
              === reverse
                ( ordNub' mempty $
                    reverse $
                      depthFirstEnumeration chains
                )
    prop "has the correct variant ordering" $
      forAllShrink genValidNormal shrinkValidNormal $ \tg ->
        -- This is the property in the documentation for
        -- enumerateChainVariants (and renderBCP47) that if x appears
        -- before y in one of the paths, then x will appear before y
        -- in the enumeration. This is also implicitly a test that
        -- Both will never appear as a PathOrder for a pair of
        -- distinct elements
        let (chains, _, _) = categorizeVariants tg
            enumeration = enumerateChainVariants chains
            paths = toList <$> listVariantChains chains
            vs = S.toList $ variants tg
            pairs = [(x, y) | x <- vs, y <- vs, x < y]
            getNotBefore l x y
              | Just n <- List.findIndex (== x) l,
                Just m <- List.findIndex (== y) l,
                n < m =
                Nothing
              | otherwise = Just (x, y)
            badPair x y = case getPathOrder paths x y of
              Before -> getNotBefore enumeration x y
              After -> getNotBefore enumeration y x
              Both -> getNotBefore enumeration x y <|> getNotBefore enumeration y x
              Incomparable -> Nothing
         in uncurry badPair `shouldNotMatch` pairs
  describe "orderNormalVariants" $ do
    prop "lists the variants of a normal tag exactly once" $
      forAllShrink genValidNormal shrinkValidNormal $ \tg ->
        List.sort (orderNormalVariants tg) === S.toAscList (variants tg)
  describe "lintBCP47" $ do
    prop "is idempotent" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let (_, tg') = lintBCP47 tg
            (_, tg'') = lintBCP47 tg'
         in tg'' === tg'
    prop "gives the same warnings after the second linting" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let (_, tg') = lintBCP47 tg
            (w, tg'') = lintBCP47 tg'
            (w', _) = lintBCP47 tg''
         in w' === w
    it "gives the correct warnings for the examples" $ do
      let badPair (x, y)
            | y == y' = Nothing
            | otherwise = Just (x, y, y')
            where
              y' = (\(a, b) -> (b, a)) $ lintBCP47 x
      badPair `shouldNotMatch` lintExamples
