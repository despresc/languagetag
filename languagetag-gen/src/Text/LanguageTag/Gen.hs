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
module Text.LanguageTag.Gen where

import Control.Monad (unless)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesDirectoryExist)
import Text.LanguageTag.Gen.BCP47.Parse
import Text.LanguageTag.Gen.BCP47.Render
import Text.LanguageTag.Gen.Jar

{-
If we get more registries, then much of this module should be pushed
down to a new Gen.BCP47.hs
-}

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
readLocalRegistry pref = T.readFile (pref <> "/registry/bcp47") >>= parseRegistryThrow

-- | Read the BCP47 registry, then write the internal modules
defaultMain :: IO ()
defaultMain = do
  b <- doesDirectoryExist "./languagetag-gen"
  let prefgen = if b then "./languagetag-gen" else "../languagetag-gen"
  let preflang = if b then "./languagetag-bcp47" else "../languagetag-bcp47"
  (u, r) <- readLocalRegistry prefgen
  unless (null u) $ do
    putStrLn "Unrecognized BCP47 registry tag fields:"
    print u
  putStrLn "writing the internal modules"
  renderSplitRegistry preflang $ splitRegistry r