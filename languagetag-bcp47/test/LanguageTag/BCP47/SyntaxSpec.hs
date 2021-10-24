{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LanguageTag.BCP47.SyntaxSpec (spec) where

import qualified Data.Char as Char
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
import LanguageTag.BCP47.Quasi
import LanguageTag.BCP47.Registry (ExtensionChar (..), Grandfathered (..))
import LanguageTag.BCP47.Subtag
  ( renderSubtagLower,
    unpackCharLower,
  )
import qualified LanguageTag.BCP47.Subtag as Sub
import qualified LanguageTag.BCP47.Syntax as Syn
import LanguageTag.Internal.BCP47.Subtag (SubtagChar (..))
import qualified LanguageTag.Internal.BCP47.Syntax as Syn

{-
TODO:

- add more ill-formed subtag parsing property/unit tests now that the
  error type is richer

- add a few unit tests (e.g. of the tags in quasi), which will need to be
  recorded as the types themselves (not as quasi-quoted values, which would be
  cheating)

- add some unit tests of the popping behaviour (i.e. that it parses the correct
  thing and stops where we expect)

- test that for an input that's entirely subtag characters and dashes, if
  popSubtagLen successfully parses something then the returned unconsumed input
  will be empty.

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

syntaxFailures' :: [(Text, (Syn.PopError, Text))]
syntaxFailures' =
  [ ( "",
      ( Syn.PopErrorSubtag 0 Nothing Syn.AtBeginning Sub.PopEmptySubtag,
        ""
      )
    ),
    ( "i-nonsense",
      ( Syn.PopErrorStep 2 Nothing Syn.AtStartI $
          Syn.ImproperSubtag [subtag|nonsense|],
        "nonsense"
      )
    ),
    ( "i-bnn-more",
      ( Syn.PopErrorStep 6 (Just (Syn.GrandfatheredTag IBnn)) Syn.AtIrregGrandfathered $
          Syn.SubtagAfterIrreg [subtag|more|] IBnn,
        "more"
      )
    ),
    ( "cmnabcd--",
      ( Syn.PopErrorSubtag 8 (Just [syntag|cmnabcd|]) Syn.AtPrimaryLong Sub.PopEmptySubtag,
        "-"
      )
    ),
    ( "cmn-lotsoftag*",
      ( Syn.PopErrorSubtag 4 (Just [syntag|cmn|]) Syn.AtPrimaryShort $
          Sub.PopSubtagTooLong [subtag|lotsofta|],
        "g*"
      )
    ),
    ( "en-GB-oxendict-x",
      ( Syn.PopErrorStep
          16
          (Just [syntag|en-GB-oxendict|])
          Syn.AtStartPrivateUse
          Syn.EmptyPrivateUse,
        ""
      )
    ),
    ( "zh-419-a",
      ( Syn.PopErrorStep 8 (Just [syntag|zh-419|]) Syn.AtStartExtension $
          Syn.EmptyExtensionSection ExtA Nothing,
        ""
      )
    )
  ]

spec :: Spec
spec = do
  describe "popBCP47Len" $ do
    prop "parses any well-formed tag completely" $ do
      -- we are interested in cases where we successfully parse a tag but do not
      -- consume all input
      let select (Right (_, _, t)) | not (T.null t) = Just t
          select _ = Nothing
      forAllShrink genTagText shrinkTagText $ \t ->
        select (Syn.popBCP47Detail t) === Nothing
    prop "parses initial well-formed tags completely" $ do
      -- we are interested in cases where we successfully parse a tag but do not
      -- consume all of the initial tag characters
      let select t (Right (_, _, t'))
            | rest /= t' = Just (rest, t')
            where
              rest = T.dropWhile Syn.isTagChar t
          select _ _ = Nothing
      forAllShrink genPopTagText shrinkPopTagText $ \t ->
        select t (Syn.popBCP47Detail t) === Nothing
    prop "is case-insensitive on initial well-formed tags" $ do
      let fixUp (Right (x, y, t)) = Right (x, y, T.toLower t)
          fixUp (Left e) = Left e
      forAllShrink genPopTagText shrinkPopTagText $ \t ->
        fixUp (Syn.popBCP47Detail t) === Syn.popBCP47Detail (T.toLower t)
    describe "fails correctly on" $ do
      let test (l, e) =
            it (T.unpack l) $
              Syn.popBCP47Detail l `shouldBe` Left e
      traverse_ test syntaxFailures'
    describe "parses the grandfathered tag" $ do
      let select (Right (x, _, t)) | T.null t = Right x
          select x = Left x
      let test (l, t) =
            it (T.unpack l) $
              select (Syn.popBCP47Detail l) `shouldBe` Right (Syn.grandfatheredSyntax t)
      traverse_ test $ irregularTags <> regularTags
    prop "parses tags with strict regular grandfathered prefixes correctly" $
      forAllShrink genTagishText shrinkTagishText $ \t ->
        let badParse (a, _) = case Syn.popBCP47Detail $ a <> "-" <> t of
              Right (Syn.NormalTag _, _, _) -> False
              Right _ -> True
              Left _ -> False
         in badParse `shouldNotFind` regularTags
  describe "parseBCP47FromSubtags" $ do
    prop "composes with toSubtags correctly on the left" $
      forAllShrink genSynTag shrinkSynTag $ \st ->
        Syn.parseBCP47FromSubtags (Syn.toSubtags st) === Right st
  describe "renderBCP47" $ do
    prop "composes with parseBCP47 on the right correctly" $
      forAllShrink genSynTag shrinkSynTag $ \st -> do
        let select (x, _, _) = x
        fmap select (Syn.popBCP47Detail $ Syn.renderBCP47 st) === Right st
    prop "composes with parseBCP47 on the left correctly" $
      forAllShrink genTagText shrinkTagText $ \t -> do
        let select (x, _, _) = x
        (T.toLower . Syn.renderBCP47 . select <$> Syn.popBCP47Detail t) === Right (T.toLower t)
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
