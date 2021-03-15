{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Text.LanguageTag.BCP47.Syntax

{- TODO:

- test case insensitivity in the grandfathered tags

- test that the canonicalization of a redundant tag considered as a
  normal tag exactly matches its preferred value in the associated
  record (this is actually quite important!)

- test all of the parsing and conversion functions (usual suspects,
  but also things like wrapSubtag - will want to go module-by-module)

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

-- TODO: Tests to check:
-- - renderer is correct in the right direction
-- - case insensitivity of parsing
-- - case-insensitive grandfathered tag parsing
-- - that regular grandfathered tags forming an initial segment of a
--   tag get parsed properly
-- - that irregular grandfathered tags with any components after them
--   are never parsed
-- - error positions
-- - that random normal tags are parsed correctly, and random
--   almost-normal tags are not

main :: IO ()
main = hspec $
  parallel $ do
    describe "Syntax" $ do
      describe "parses the irregular tag" $ do
        let test (l, t) =
              it (T.unpack l) $
                parseBCP47 l `shouldBe` Right (grandfatheredSyntax t)
        traverse_ test irregularTags
      describe "parses the regular tag" $ do
        let test (l, t) =
              it (T.unpack l) $
                parseBCP47 l `shouldBe` Right (grandfatheredSyntax t)
        traverse_ test regularTags
