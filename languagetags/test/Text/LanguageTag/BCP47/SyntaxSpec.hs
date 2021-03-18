{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.BCP47.SyntaxSpec (spec) where

import qualified Data.Char as Char
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( forAll,
    forAllShrink,
    suchThat,
    (===),
  )
import Text.LanguageTag.BCP47.Registry (Grandfathered (..))
import Text.LanguageTag.BCP47.Subtag (renderSubtagLower, unpackCharLower)
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Subtag (SubtagChar (..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

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

syntaxFailures :: [(Text, Syn.SyntaxError)]
syntaxFailures =
  [ ("", Syn.SyntaxError 0 Syn.Cbeginning Syn.SyntaxErrorEmpty),
    ("i-nonsense", Syn.SyntaxError 2 Syn.CirregI Syn.SyntaxErrorBadSubtag),
    ("i-bnn-more", Syn.SyntaxError 2 Syn.CirregI Syn.SyntaxErrorIrregNum),
    ("cmn*", Syn.SyntaxError 0 Syn.Cbeginning Syn.SyntaxErrorBadSubtag),
    ("cmn-more*", Syn.SyntaxError 4 Syn.Cprimary Syn.SyntaxErrorBadSubtag),
    ("cmn-lotsoftag*", Syn.SyntaxError 4 Syn.Cprimary Syn.SyntaxErrorBadSubtag),
    ("en-GB-oxendict-x", Syn.SyntaxError 15 Syn.Cprivateuse Syn.SyntaxErrorNeededSubtag),
    ("zh-419-a", Syn.SyntaxError 7 Syn.Cextension Syn.SyntaxErrorNeededSubtag)
  ]

spec :: Spec
spec = do
  describe "parseBCP47" $ do
    it "parses any well-formed tag" $
      forAllShrink genTagText shrinkTagText $ isRight . Syn.parseBCP47
    it "is case-sensitive on any well-formed tag" $
      forAllShrink genTagText shrinkTagText $ \t ->
        Syn.parseBCP47 t === Syn.parseBCP47 (T.toLower t)
    it "is case-sensitive on near-well-formed tags" $
      forAllShrink genTagishText shrinkTagishText $ \t ->
        Syn.parseBCP47 t === Syn.parseBCP47 (T.toLower t)
    describe "fails correctly on" $ do
      let test (l, e) =
            it (T.unpack l) $
              Syn.parseBCP47 l `shouldBe` Left e
      traverse_ test syntaxFailures
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
  describe "charToExtensionChar" $ do
    prop "composes with extensionCharToChar on the right correctly" $
      forAll (genSubtagChar `suchThat` (not . (`elem` ['x', 'X']))) $ \c ->
        (Char.toLower . Syn.extensionCharToChar <$> Syn.charToExtensionChar c)
          === Just (Char.toLower c)
    prop "composes with extensionCharToChar on the left correctly" $
      forAll genExtensionChar' $ \c ->
        (Syn.charToExtensionChar $ Syn.extensionCharToChar c) === Just c
  describe "extensionCharToSubtag" $ do
    prop "returns the correct singleton subtag" $
      forAll genExtensionChar' $ \c ->
        let c' = T.singleton $ Syn.extensionCharToChar c
            c'' = renderSubtagLower $ Syn.extensionCharToSubtag c
         in c'' === c'
  describe "unsafeSubtagCharToExtension" $ do
    prop "returns the correct ExtensionChar" $
      forAll (genSubtagSubtagChar `suchThat` (/= SubtagChar 120)) $ \c ->
        let c' = Syn.unsafeSubtagCharToExtension c
         in Syn.extensionCharToChar c' === unpackCharLower c
  describe "toSubtags" $ do
    prop "should compare correctly with renderBCP47" $
      forAllShrink genSynTag shrinkSynTag $ \st ->
        T.intercalate "-" (NE.toList $ fmap renderSubtagLower $ Syn.toSubtags st)
          === T.toLower (Syn.renderBCP47 st)
