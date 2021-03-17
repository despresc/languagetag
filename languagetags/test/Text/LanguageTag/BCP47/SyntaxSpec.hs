{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.BCP47.SyntaxSpec (spec) where

import qualified Data.Char as Char
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( forAll,
    forAllShrink,
    shrink,
    (===),
  )
import Text.LanguageTag.BCP47.Registry (Grandfathered (..))
import qualified Text.LanguageTag.BCP47.Syntax as Syn

{- TODO:

correctness (quickcheck, unit when appropriate) of:

- unsafeNormalTag and the rest
- charToExtensionChar and such
- toSubtags (e.g. T.toLower . renderBCP47 should be intercalate "-"
  . fmap renderSubtagLower . toSubtags)
- errors during parsing (positions, values)
- tag-ish case insensitivity in parseBCP47
-}

irregularTags :: [(Text, Grandfathered)]
irregularTags =
  [ ("en-GB-oed", EnGbOed),
    ("i-ami", IAmi),
    ("i-bnn", IBnn),
    ("i-default", IDefault),
    ("i-enochian", IEnochian),
    ("i-hak", IHak),
    ("i-klingon", IKlingon),
    ("i-lux", ILux),
    ("i-mingo", IMingo),
    ("i-navajo", INavajo),
    ("i-pwn", IPwn),
    ("i-tao", ITao),
    ("i-tay", ITay),
    ("i-tsu", ITsu),
    ("sgn-BE-FR", SgnBeFr),
    ("sgn-BE-NL", SgnBeNl),
    ("sgn-CH-DE", SgnChDe)
  ]

regularTags :: [(Text, Grandfathered)]
regularTags =
  [ ("art-lojban", ArtLojban),
    ("cel-gaulish", CelGaulish),
    ("no-bok", NoBok),
    ("no-nyn", NoNyn),
    ("zh-guoyu", ZhGuoyu),
    ("zh-hakka", ZhHakka),
    ("zh-min", ZhMin),
    ("zh-min-nan", ZhMinNan),
    ("zh-xiang", ZhXiang)
  ]

spec :: Spec
spec = do
  describe "parseBCP47" $ do
    it "parses any well-formed tag" $
      forAllShrink genTagText shrinkTagText $ isRight . Syn.parseBCP47
    it "is case-sensitive on any well-formed tag" $
      forAllShrink genTagText shrinkTagText $ \t ->
        Syn.parseBCP47 t === Syn.parseBCP47 (T.toLower t)
    describe "parses the irregular grandfathered tag" $ do
      let test (l, t) =
            it (T.unpack l) $
              Syn.parseBCP47 l `shouldBe` Right (Syn.grandfatheredSyntax t)
      traverse_ test irregularTags
    describe "parses the regular grandfathered tag" $ do
      let test (l, t) =
            it (T.unpack l) $
              Syn.parseBCP47 l `shouldBe` Right (Syn.grandfatheredSyntax t)
      traverse_ test regularTags
  describe "renderBCP47" $ do
    prop "composes with parseBCP47 on the right correctly" $
      forAllShrink genSynTag shrinkSynTag $ \st ->
        Syn.parseBCP47 (Syn.renderBCP47 st) === Right st
    prop "composes with parseBCP47 on the left correctly" $
      forAllShrink genTagText shrinkTagText $ \t ->
        (T.toLower . Syn.renderBCP47 <$> Syn.parseBCP47 t) === Right (T.toLower t)
