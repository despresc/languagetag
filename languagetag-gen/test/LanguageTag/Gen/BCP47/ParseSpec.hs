{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Testing registry parsing
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module LanguageTag.Gen.BCP47.ParseSpec (spec) where

import Data.Char (isSpace)
import Data.List.NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.Hspec
import LanguageTag.BCP47.Registry (Deprecation (..), Scope (..))
import LanguageTag.Gen.BCP47.Parse
import LanguageTag.Gen.Jar (parseJarFile)

-- | Take a raw record and render it back to 'Text' lines. Note that
-- this is dependent on the registry's normal field ordering, which
-- may change (in which case one of the @parseRegistry@ tests will
-- fail)
renderRecord :: TagRecord -> [Text]
renderRecord (TagRecord t tt descs dep) = case tt of
  Language mscr mmacro mscope ->
    ["Type: language", "Subtag: " <> t] <> renderDescs <> renderDep
      <> renderSuppress mscr
      <> renderMacro mmacro
      <> renderScope mscope
  Extlang mpref pfx mscr mmacro mscope ->
    ["Type: extlang", "Subtag: " <> t] <> renderDescs <> renderDep <> renderPreferred mpref
      <> ["Prefix: " <> pfx]
      <> renderSuppress mscr
      <> renderMacro mmacro
      <> renderScope mscope
  Script -> ["Type: script", "Subtag: " <> t] <> renderDescs <> renderDep
  Region -> ["Type: region", "Subtag: " <> t] <> renderDescs <> renderDep
  Variant vs ->
    ["Type: variant", "Subtag: " <> t] <> renderDescs <> renderDep
      <> fmap ("Prefix: " <>) vs
  Grandfathered -> ["Type: grandfathered", "Tag: " <> t] <> renderDescs <> renderDep
  Redundant -> ["Type: redundant", "Tag: " <> t] <> renderDescs <> renderDep
  where
    renderPreferred Nothing = []
    renderPreferred (Just x) = ["Preferred-Value: " <> x]
    takeFirstLine = fst . T.span (/= '\n')
    renderSuppress Nothing = []
    renderSuppress (Just x) = ["Suppress-Script: " <> x]
    renderMacro Nothing = []
    renderMacro (Just x) = ["Macrolanguage: " <> x]
    renderDep = case dep of
      NotDeprecated -> []
      DeprecatedSimple -> ["Deprecated"]
      DeprecatedPreferred v -> ["Deprecated", "Preferred-Value: " <> v]
    renderDescs = toList $ fmap (\d -> takeFirstLine $ "Description: " <> d) descs
    renderScope (Just Macrolanguage) = ["Scope: macrolanguage"]
    renderScope (Just Collection) = ["Scope: collection"]
    renderScope (Just Special) = ["Scope: special"]
    renderScope (Just PrivateUseScope) = ["Scope: private-use"]
    renderScope Nothing = []

mParsed :: Text -> Maybe Text
mParsed t
  | Just _ <- T.stripPrefix "Comments" t =
    Nothing
  | Just _ <- T.stripPrefix "Added" t =
    Nothing
  | Just (c, _) <- T.uncons t,
    isSpace c =
    Nothing
  | t == "%%" =
    Nothing
  | Just _ <- T.stripPrefix "Deprecated" t =
    Just "Deprecated"
  | otherwise = Just t

-- | Take a raw registry file and discard all of the line folds and
-- record separators
rerenderRegistryFile :: Text -> [Text]
rerenderRegistryFile = mapMaybe mParsed . T.lines

-- | Render a 'RawRegistry' back to its constituent lines
renderRegistry :: RawRegistry -> [Text]
renderRegistry (RawRegistry rdate rs) =
  ("File-Date: " <> T.pack (show rdate)) :
  concatMap renderRecord rs

firstDiff :: [Text] -> [Text] -> Either (Either [Text] [Text]) [(Text, Text)]
firstDiff (x : xs) (y : ys)
  | x == y = firstDiff xs ys
  | otherwise = Right $ (x, y) : take 10 (zip xs ys)
firstDiff (x : xs) [] = Left $ Left $ take 10 (x : xs)
firstDiff [] (x : xs) = Left $ Right $ take 10 (x : xs)
firstDiff [] [] = Right []

spec :: Spec
spec = do
  describe "parseRegistry" $ do
    it "seems to round trip with rendering correctly" $ do
      -- Test that naive rendering of the parsed raw registry is equal
      -- to the original registry up to some necessary stripping.
      t <- T.readFile "../languagetag-bcp47/data/registry"
      let trerend = rerenderRegistryFile t
      let trend = fmap (renderRegistry . snd) . parseRegistry <$> parseJarFile t
      let trend' = case trend of
            Left _ -> error "couldn't parse BCP47 registry jar"
            Right a -> case a of
              Left _ -> error "couldn't parse BCP47 registry jar"
              Right a' -> a'
      firstDiff trend' trerend `shouldBe` Right []
