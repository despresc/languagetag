{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.LanguageTag.BCP47.RegistrySpec (spec) where

import Data.Either (fromRight)
import Data.FileEmbed (embedFile)
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( arbitrary,
    chooseInt,
    forAll,
    forAllShrink,
    getNonEmpty,
    shrink,
    (===),
  )
import Text.LanguageTag.BCP47.Quasi (validtag)
import Text.LanguageTag.BCP47.Registry
  ( BCP47,
    recognizeRedundantNormal,
    redundantToNormalTag,
    redundantToValidTag,
    renderBCP47,
    renderExtlang,
    renderGrandfathered,
    renderLanguage,
    renderRedundant,
    renderRegion,
    renderScript,
    renderVariant,
    toSubtags,
    toSyntaxTag,
  )
import Text.LanguageTag.BCP47.Registry.Redundant (recognizeRedundantTag, redundantTrie)
import Text.LanguageTag.BCP47.Subtag (parseSubtagText, renderSubtagLower)
import qualified Text.LanguageTag.BCP47.Subtag.Trie as Trie
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.BCP47.Validation
  ( validateBCP47,
    validateExtlang,
    validateLanguage,
    validateRegion,
    validateScript,
    validateVariant,
  )
import Text.LanguageTag.Internal.BCP47.Registry.Types (unsafeBinSearchIndexOn)
import qualified Text.LanguageTag.Internal.BCP47.Syntax as SynI

----------------------------------------------------------------
-- The raw registry itself
----------------------------------------------------------------

-- | The text of the local registry, included so that tests don't get confused
-- when run in CI
registryText :: Text
registryText = T.decodeUtf8 $ $(embedFile "data/registry")

----------------------------------------------------------------
-- Registry parsing
----------------------------------------------------------------

-- | Strip off the beginning of a tag or subtag entry, throwing an error if it fails
stripTagOrSubtag :: Text -> Text
stripTagOrSubtag t
  | Just t' <- T.stripPrefix "Subtag: " t =
    t'
  | Just t' <- T.stripPrefix "Tag: " t =
    t'
  | otherwise = error $ "couldn't strip line: " <> T.unpack (T.take 10 $ T.drop 1 t)

-- | Get a list of all @(entrytype, entry)@ pairs from the registry
getRegistryEntries :: Text -> [(Text, Text)]
getRegistryEntries = go . zip [(2 :: Int) ..] . drop 1 . T.lines
  where
    go ((_, "%%") : tty : tentry : tentries)
      | Just ty <- T.stripPrefix "Type: " $ snd tty,
        entry <- stripTagOrSubtag $ snd tentry =
        (ty, entry) : go tentries
    go ((n, "%%") : _) = error $ "bad record starting on line " <> show n
    go (_ : xs) = go xs
    go [] = []

-- | Returns @Just@ some message if parsing, validation, then rendering doesn't
-- return the original. Skips the few tag ranges that exist in the registry.
anInvalidRegistryEntry :: (Text, Text) -> Maybe Text
anInvalidRegistryEntry (ty, entry) = case ty of
  "language" -> case comparing (toM parseSubtagText) validateLanguage renderLanguage of
    Just _
      | entry == "qaa..qtz" -> Nothing
    x -> x
  "extlang" -> comparing (toM parseSubtagText) validateExtlang renderExtlang
  "script" -> case comparing (toM parseSubtagText) validateScript renderScript of
    Just _
      | entry == "Qaaa..Qabx" -> Nothing
    x -> x
  "region" -> case comparing (toM parseSubtagText) validateRegion renderRegion of
    Just _
      | entry == "QM..QZ" || entry == "XA..XZ" -> Nothing
    x -> x
  "variant" -> comparing (toM parseSubtagText) validateVariant renderVariant
  "grandfathered" -> comparing pGrand Just renderGrandfathered
  "redundant" -> comparing (toM Syn.parseBCP47) validateRedundant renderRedundant
  _ -> error $ "unknown type in entry " <> show (ty, entry)
  where
    toM f = either (const Nothing) Just . f
    pGrand t = case Syn.parseBCP47 t of
      Left _ -> Nothing
      Right (SynI.GrandfatheredTag g) -> Just g
      Right _ -> Nothing
    validateRedundant tag = do
      tag' <- toM validateBCP47 tag
      recognizeRedundantTag tag'
    comparing parse validate render = case parse entry of
      Nothing -> Just "<no-parse>"
      Just a -> case validate a of
        Nothing -> Just "<no-validate>"
        Just a'
          | render a' /= entry -> Just $ render a'
          | otherwise -> Nothing

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- TODO: unit testing of validation (good and bad)

-- TODO: add to me - should also indicate exactly which example fails
-- in the test instead of using shouldNotFind.
renderingExamples :: [(BCP47, Text)]
renderingExamples =
  [ ( [validtag|sl-cmn-hant-us-njiva-rozaj-biske-1994-alalc97-1606nict-b-and-a-tag-x-more|],
      "sl-cmn-Hant-US-rozaj-biske-njiva-1994-alalc97-1606nict-a-tag-b-and-x-more"
    ),
    ( [validtag|oc-vivaraup-aranes-nicard-grmistr|],
      "oc-aranes-nicard-vivaraup-grmistr"
    ),
    ( [validtag|oc-grmistr-vivaraup-aranes-nicard|],
      "oc-aranes-nicard-vivaraup-grmistr"
    ),
    ( [validtag|oc-lemosin-aranes-lengadoc-grclass|],
      "oc-aranes-lemosin-lengadoc-grclass"
    ),
    ( [validtag|oc-grclass-lemosin-aranes-lengadoc|],
      "oc-aranes-lemosin-lengadoc-grclass"
    ),
    ( [validtag|oc-cisaup-provenc-grital|],
      "oc-cisaup-provenc-grital"
    ),
    ( [validtag|oc-provenc-grital-cisaup|],
      "oc-cisaup-provenc-grital"
    )
  ]

spec :: Spec
spec = do
  describe "valid language subtags" $
    it "are at most three characters long" $ do
      -- if this one fails then we'll need to change the representation of the
      -- Normal tag type (and potentially the validated language type) to rule
      -- out the possibility of representing a <longlang>-<extlang> tag.
      -- genValidNormal would also need to be changed to cope.
      let badlang l = T.length (renderLanguage l) > 3
      badlang `shouldNotFind` [minBound .. maxBound]
  describe "binSearchIndexOn" $ do
    -- this one only tests vectors of strictly increasing elements,
    -- but that should be enough, given the other match finding test
    prop "behaves like Vector findIndex" $
      forAllShrink
        (V.uniq . V.fromList . List.sort . getNonEmpty <$> arbitrary)
        (filter (not . V.null) . fmap (V.uniq . V.fromList . List.sort) . shrink . V.toList)
        $ \v ->
          forAll (chooseInt (0, V.length v - 1)) $ \idx ->
            forAll (chooseInt (-1, 1)) $ \didx ->
              let el = V.unsafeIndex v idx + fromEnum didx
               in unsafeBinSearchIndexOn id el v === V.findIndex (== el) v
    prop "actually finds matches" $
      forAllShrink
        (V.fromList . List.sort . getNonEmpty <$> arbitrary)
        (filter (not . V.null) . fmap (V.fromList . List.sort) . shrink . V.toList)
        $ \v ->
          forAll (chooseInt (0, V.length v - 1)) $ \idx ->
            let el = V.unsafeIndex v idx :: Int
             in (V.unsafeIndex v <$> unsafeBinSearchIndexOn id el v) === Just el
  describe "toSyntaxTag" $ do
    prop "composes with validateBCP47 correctly on the right" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        validateBCP47 (toSyntaxTag tg) === Right tg
  describe "renderBCP47" $ do
    prop "equals the toSubtags implementation up to case" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        T.toLower (renderBCP47 tg)
          === T.intercalate
            "-"
            ( fmap renderSubtagLower $
                toList $ toSubtags tg
            )
    prop "equals the toSyntaxTag implementation up to case" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        renderBCP47 tg
          === Syn.renderBCP47 (toSyntaxTag tg)
    prop "generates text that validates to the same tag" $
      forAllShrink genValidTag shrinkValidTag $ \tg ->
        let tg' = fmap validateBCP47 $ Syn.parseBCP47 $ renderBCP47 tg
         in tg' === Right (Right tg)
    it "renders all the examples correctly" $ do
      let badPair (x, y) = renderBCP47 x /= y
      badPair `shouldNotFind` renderingExamples
    it "round-trips on the current registry" $ do
      let entries = getRegistryEntries registryText
      anInvalidRegistryEntry `shouldNotMatch` entries
  describe "redundantTrie" $ do
    let errSyn = fromRight (error "ill-formed redundant trie tag") . Syn.parseBCP47FromSubtags
    let errVal = fromRight (error "invalid redundant trie tag") . validateBCP47
    let rendpath (sts, r) = (errVal $ errSyn $ NE.fromList sts, r)
    let redpaths = List.sortOn snd $ rendpath <$> Trie.fromTrie redundantTrie
    it "has paths that validate to their nodes" $ do
      List.find (\(x, y) -> recognizeRedundantTag x /= Just y) redpaths `shouldBe` Nothing
    it "has exactly the list of redundant tags as nodes" $ do
      findFirstDiff (snd <$> redpaths) [minBound .. maxBound] `shouldBe` Nothing
  describe "recognizeRedundant" $ do
    it "recognizes all of the redundant tags correctly" $ do
      let rendred = redundantToNormalTag <$> [minBound .. maxBound]
      let unrecog x = case recognizeRedundantNormal x of
            Just a
              | redundantToNormalTag a == x -> Nothing
            _ -> Just x
      mapMaybe unrecog rendred `shouldBe` []
    prop "recognizes redundant tags like a simple find would" $
      forAllShrink genValidTag shrinkValidTag $ \tg -> do
        let rendred = [minBound .. maxBound]
        recognizeRedundantTag tg === List.find ((== tg) . redundantToValidTag) rendred
