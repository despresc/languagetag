{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Text.LanguageTag.BCP47.Syntax

{- TODO:

test:

- Subtag.toSubtag should be an order homomorphism

- case insensitivity in the parsing

- canonicalization of a redundant tag considered as a normal tag
  should exactly match its preferred value in the associated record
  (this is actually quite important!)

- canonicalizeBCP47 and normalToExtlangForm should be idempotent

- validateBCP47 . toSyntaxTag should be Right

- parseBCP47 . renderBCP47 should be Right

- the registry vectors should have length equal to the number of
  constructors in their associated types, and the lookup functions
  should all be total

- if a subtag occurs as a preferred value, it should not itself be
  deprecated - related to the component canonicalization functions
  being idempotent

- redundantToSyntaxTag should always produce a canonical tag, as
  should looking up the preferred value of deprecated redundant and
  grandfathered tags and the prefixes of variants

- the preferred value of an extlang should not be deprecated, and it
  should be equal to the correct language tag (can probably just use
  show to test this)

- parsing and validating should succeed/fail for various hand-written
  tags

- test all of the parsing and conversion functions (usual suspects,
  but also things like wrapSubtag - will want to go module-by-module)

- unit tests of the subtag-related functions

- parseSubtag . renderSubtagLower should be Just

- packChar . unpackCharLower should be Just

- parseSubtagMangled and packCharMangled should behave identically to
  their non-mangled counterparts on known-valid subtags and otherwise
  renderSubtagLower . parseSubtagMangled etc. should be the obvious
  thing

- tags with grandfathered tags as strict initial segments should be
  parsed properly (i.e. as normal tags and not at all for regular and
  irregular grandfathered tags, respectively)

- random normal-looking tags and subtags should be parsed properly,
  and random near-normal ones should not be

- unit testing of error positions in parsing

- think of things for tries (maybe just unit tests?)

- toLower . renderBCP47 should be the same as intercalate "-"
  . toSubtags for syntactic tags

- unsafeNormalTag et al. should be correct (unit tests)

- unit testing of validation (good and bad)

- wrapSubtag . unwrapSubtag should be Just

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

main :: IO ()
main = hspec $
  parallel $ do
    describe "Syntax" $ do
      describe "parses the irregular grandfathered tag" $ do
        let test (l, t) =
              it (T.unpack l) $
                parseBCP47 l `shouldBe` Right (grandfatheredSyntax t)
        traverse_ test irregularTags
      describe "parses the regular grandfathered tag" $ do
        let test (l, t) =
              it (T.unpack l) $
                parseBCP47 l `shouldBe` Right (grandfatheredSyntax t)
        traverse_ test regularTags
