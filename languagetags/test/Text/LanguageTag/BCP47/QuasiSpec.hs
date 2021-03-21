{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.LanguageTag.BCP47.QuasiSpec (spec) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Text.LanguageTag.BCP47.Canonicalization (canonicalizeBCP47)
import Text.LanguageTag.BCP47.Quasi (canontag, subtag, syntag, validtag)
import Text.LanguageTag.BCP47.Registry (BCP47)
import Text.LanguageTag.BCP47.Subtag (Subtag, parseSubtag)
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.BCP47.Validation (validateBCP47)

-- TODO: look into a quasi-quoting testing library, since this
-- obviously doesn't test any failing cases.

orExplode :: Text -> (Text -> Either b c) -> Text -> c
orExplode l f x = case f x of
  Left _ -> error $ T.unpack $ l <> " " <> x
  Right a -> a

subtagExamples :: [(Text, Subtag)]
subtagExamples =
  [ ("sOm3sUb", [subtag|sOm3sUb|]),
    ("other", [subtag|other|]),
    ("12345", [subtag|12345|])
  ]

syntagExamples :: [(Text, Syn.BCP47)]
syntagExamples =
  [ ("odd-tag-more-123-a-foo-x-bar", [syntag|odd-tag-more-123-a-foo-x-bar|])
  ]

validtagExamples :: [(Text, BCP47)]
validtagExamples =
  [ ("en-gb-oed", [validtag|en-gb-oed|]),
    ("x-private-tag", [validtag|x-private-tag|]),
    ("en-gb-oxendict-a-123-x-456", [validtag|en-gb-oxendict-a-123-x-456|])
  ]

canontagExamples :: [(Text, BCP47)]
canontagExamples =
  [ ("en-gb-oed", [canontag|en-gb-oed|]),
    ("x-private-tag", [canontag|x-private-tag|]),
    ("en-gb-oxendict-a-123-x-456", [canontag|en-gb-oxendict-a-123-x-456|])
  ]

-- We don't test correctness directly here, since that is already done
-- in the parse/validate/canonicalize specs elsewhere. We only test
-- that the quasi-quotes parse the examples like the functions do.
spec :: Spec
spec = do
  let pSub = orExplode "failed to parse subtag" parseSubtag
  let pSyn = orExplode "failed to parse tag" Syn.parseBCP47
  let pVal = orExplode "failed to validate subtag" (validateBCP47 . pSyn)
  describe "subtag" $ do
    let test (l, x) =
          it ("parses " <> T.unpack l <> " correctly") $
            x `shouldBe` pSub l
    traverse_ test subtagExamples
  describe "syntag" $ do
    let test (l, x) =
          it ("parses " <> T.unpack l <> " correctly") $
            x `shouldBe` pSyn l
    traverse_ test syntagExamples
  describe "validtag" $ do
    let test (l, x) =
          it ("parses " <> T.unpack l <> " correctly") $
            x `shouldBe` pVal l
    traverse_ test validtagExamples
  describe "canontag" $ do
    let test (l, x) =
          it ("parses " <> T.unpack l <> " correctly") $
            x `shouldBe` canonicalizeBCP47 (pVal l)
    traverse_ test canontagExamples
