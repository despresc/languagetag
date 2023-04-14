{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Actual code generation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- A quick executable to parse the content of various registries and
-- render some internal modules. The sources for the registries:
--
-- * The BCP47 data in @registry/bcp47@ can be obtained from
--  <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>,
--  which periodically updates in-place.
module LanguageTag.Gen where

-- TODO: qualify exports, get rid of the old functions

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified LanguageTag.BCP47.Dynamic.RecordJar as Jar
import LanguageTag.Gen.BCP47.Parse
import LanguageTag.Gen.BCP47.Render
import LanguageTag.Gen.Jar
import System.Directory (doesDirectoryExist)

{-
If we get more registries, then much of this module should be pushed
down to a new Gen.BCP47.hs
-}

-- TODO: actually throw errors nicely
parseRegistryThrowNew :: Text -> IO Jar.JarRegistry
parseRegistryThrowNew inp = case Jar.parseJarRegistry inp of
  Left t -> fail $ show t
  Right a -> pure a

-- | Parse the BCP47 registry. This does /not/ unpack ranges.
parseRegistryThrow :: Text -> IO ([(LineNum, Text)], RawRegistry)
parseRegistryThrow inp = do
  let jarErr (lineNum, t) =
        fail $
          "couldn't parse tag "
            <> T.unpack t
            <> " at line "
            <> show lineNum
  let withLine n t = "at line " <> show n <> "\n" <> t
  let tagErr (ErrUnknownType n) = fail $ withLine n "unknown type"
      tagErr (ErrRecord rs) = fail $ List.intercalate "\n" $ fmap allErrs rs
        where
          allErrs (tt, k, l, t) =
            withLine l $
              "record type " <> show tt <> "; field " <> T.unpack k
                <> "\n"
                <> T.unpack t
      tagErr ErrBadDate = fail "bad date record"
      tagErr ErrEmptyInput = fail "empty input"
  jars <- either jarErr pure $ parseJarFile inp
  either tagErr pure $ parseRegistry jars

-- | Read the BCP47 registry in the package in the given directory
readLocalRegistry :: FilePath -> IO ([(LineNum, Text)], RawRegistry)
readLocalRegistry reg = T.readFile reg >>= parseRegistryThrow

readLocalRegistryNew :: FilePath -> IO Jar.JarRegistry
readLocalRegistryNew reg = T.readFile reg >>= parseRegistryThrowNew

-- | Read the BCP47 registry, then write the internal modules
defaultMain :: IO ()
defaultMain = do
  b <- doesDirectoryExist "./languagetag-bcp47"
  let pref = if b then "./languagetag-bcp47" else "../languagetag-bcp47"
  let regpath = pref <> "/data/registry"
  r <- readLocalRegistryNew regpath
  -- unless (null u) $ do
  --   putStrLn "Unrecognized BCP47 registry tag fields:"
  --   print u
  putStrLn "writing the internal modules"
  case parseRegistryNew r of
    Left t -> error $ T.unpack t
    Right r' -> renderSplitRegistry pref r'
