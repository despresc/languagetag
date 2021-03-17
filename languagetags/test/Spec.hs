{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified Text.LanguageTag.BCP47.SubtagSpec as SubtagSpec
import qualified Text.LanguageTag.BCP47.SyntaxSpec as SyntaxSpec

{- TODO:

test:

- canonicalization of a redundant tag considered as a normal tag
  should exactly match its preferred value in the associated record
  (this is actually quite important!)

- canonicalizeBCP47 and normalToExtlangForm should be idempotent

- validateBCP47 . toSyntaxTag should be Right

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

- tags with grandfathered tags as strict initial segments should be
  parsed properly (i.e. as normal tags and not at all for regular and
  irregular grandfathered tags, respectively)

- think of things for tries (maybe just unit tests?)

- unit testing of validation (good and bad)
-}

{-
General spec convention: if one of the values fed to (===) is more
trusted (more likely to be the correct value or something like that)
then it goes on the right. Not all (===) invocations have such a
value, of course.
-}

main :: IO ()
main = hspec $
  modifyMaxSuccess (const 1000) $
    parallel $ do
      describe "Subtag" SubtagSpec.spec
      describe "Syntax" SyntaxSpec.spec
