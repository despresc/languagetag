{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Text.LanguageTag.BCP47.Syntax

{- TODO:

- test case insensitivity in the grandfathered tags

-}

irregularTags :: [(Text, LanguageTag)]
irregularTags =
  [ ("en-GB-oed", enGbOed),
    ("i-ami", iAmi),
    ("i-bnn", iBnn),
    ("i-default", iDefault),
    ("i-enochian", iEnochian),
    ("i-hak", iHak),
    ("i-klingon", iKlingon),
    ("i-lux", iLux),
    ("i-mingo", iMingo),
    ("i-navajo", iNavajo),
    ("i-pwn", iPwn),
    ("i-tao", iTao),
    ("i-tay", iTay),
    ("i-tsu", iTsu),
    ("sgn-BE-FR", sgnBeFr),
    ("sgn-BE-NL", sgnBeNl),
    ("sgn-CH-DE", sgnChDe)
  ]

regularTags :: [(Text, LanguageTag)]
regularTags =
  [ ("art-lojban", artLojban),
    ("cel-gaulish", celGaulish),
    ("no-bok", noBok),
    ("no-nyn", noNyn),
    ("zh-guoyu", zhGuoyu),
    ("zh-hakka", zhHakka),
    ("zh-min", zhMin),
    ("zh-min-nan", zhMinNan),
    ("zh-xiang", zhXiang)
  ]

main :: IO ()
main = hspec $
  parallel $ do
    describe "Syntax" $ do
      describe "parses the irregular tag" $ do
        let test (l, t) =
              it (T.unpack l) $
                parseBCP47 l `shouldBe` (Right t)
        traverse_ test irregularTags
      describe "parses the regular tag" $ do
        let test (l, t) =
              it (T.unpack l) $
                parseBCP47 l `shouldBe` (Right t)
        traverse_ test regularTags
