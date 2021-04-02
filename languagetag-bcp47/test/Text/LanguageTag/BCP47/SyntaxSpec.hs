{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Text.LanguageTag.BCP47.Quasi
import Text.LanguageTag.BCP47.Registry (ExtensionChar (..), Grandfathered (..))
import Text.LanguageTag.BCP47.Subtag
  ( isSubtagChar,
    renderSubtagLower,
    unpackCharLower,
  )
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Subtag (SubtagChar (..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

{-
TODO:

- add more ill-formed subtag parsing property/unit tests now that the
  error type is richer
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

syntaxFailures :: [(Text, Syn.SyntaxError)]
syntaxFailures =
  [ ("", Syn.EmptyInput),
    ("i-nonsense", Syn.BadSubtag 2 Syn.AtIrregI [subtag|nonsense|] Nothing),
    ("i-bnn-more", Syn.IrregNum IBnn),
    ("cmn*", Syn.UnparsableSubtag 0 Syn.AtBeginning (Just (3, '*')) Nothing),
    ("en-GB-oed-", Syn.TrailingTerminator [syntag|en-gb-oed|]),
    ("zh-min-nan*", Syn.UnparsableSubtag 7 Syn.AtExtl1 (Just (10, '*')) (Just [syntag|zh-min|])),
    ("cmn-more*", Syn.UnparsableSubtag 4 Syn.AtPrimaryShort (Just (8, '*')) (Just [syntag|cmn|])),
    ("cmnabcd--", Syn.EmptySubtag 8 Syn.AtPrimaryLong (Just [syntag|cmnabcd|])),
    ("cmn-lotsoftag*", Syn.UnparsableSubtag 4 Syn.AtPrimaryShort Nothing (Just [syntag|cmn|])),
    ("en-GB-oxendict-x", Syn.EmptySingleton 15 Nothing (Just [syntag|en-GB-oxendict|])),
    ("zh-419-a", Syn.EmptySingleton 7 (Just ExtA) (Just [syntag|zh-419|]))
  ]

spec :: Spec
spec = do
  -- see SubtagSpec
  let limitedDownCase c
        | isSubtagChar c' && not (isSubtagChar c) = c
        | otherwise = c'
        where
          c' = Char.toLower c
  let mapDownCase = T.map limitedDownCase
  describe "parseBCP47" $ do
    prop "parses any well-formed tag" $
      forAllShrink genTagText shrinkTagText $ isRight . Syn.parseBCP47
    prop "is case-sensitive on any well-formed tag" $
      forAllShrink genTagText shrinkTagText $ \t ->
        Syn.parseBCP47 t === Syn.parseBCP47 (T.toLower t)
    prop "is case-sensitive on near-well-formed tags" $ do
      let fixErr (Left (Syn.UnparsableSubtag p a (Just (n, c)) t)) =
            Left $ Syn.UnparsableSubtag p a (Just (n, limitedDownCase c)) t
          fixErr x = x
      forAllShrink genTagishText shrinkTagishText $ \t ->
        fixErr (Syn.parseBCP47 t) === Syn.parseBCP47 (mapDownCase t)
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
    prop "does not parse tags with strict irregular grandfathered prefixes" $
      forAllShrink genTagishText shrinkTagishText $ \t ->
        (isRight . Syn.parseBCP47 . (<> ("-" <> t)) . fst) `shouldNotFind` irregularTags
    prop "fails to parse trailing terminator tags properly" $ do
      let fixBad (Left (Syn.TrailingTerminator x)) = Just x
          fixBad _ = Nothing
      forAllShrink genTagText shrinkTagText $ \t ->
        fixBad (Syn.parseBCP47 $ t <> "-") === collapseLeft (Syn.parseBCP47 t)
    prop "parses tags with strict regular grandfathered prefixes correctly" $
      forAllShrink genTagishText shrinkTagishText $ \t ->
        let badParse (a, _) = case Syn.parseBCP47 $ a <> "-" <> t of
              Right (Syn.NormalTag _) -> False
              Right _ -> True
              Left _ -> False
         in badParse `shouldNotFind` regularTags
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
        Syn.charToExtensionChar (Syn.extensionCharToChar c) === Just c
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
        T.intercalate "-" (NE.toList $ renderSubtagLower <$> Syn.toSubtags st)
          === T.toLower (Syn.renderBCP47 st)
  describe "parseBCP47FromSubtags" $ do
    prop "composes with toSubtags correctly on the left" $
      forAllShrink genSynTag shrinkSynTag $ \st ->
        Syn.parseBCP47FromSubtags (Syn.toSubtags st) === Right st
