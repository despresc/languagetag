{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Library testing
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified LanguageTag.BCP47.CanonicalizationSpec as CanonicalizationSpec
import qualified LanguageTag.BCP47.QuasiSpec as QuasiSpec
import qualified LanguageTag.BCP47.Registry.ExtlangSpec as ExtlangSpec
import qualified LanguageTag.BCP47.Registry.GrandfatheredSpec as GrandfatheredSpec
import qualified LanguageTag.BCP47.Registry.LanguageSpec as LanguageSpec
import qualified LanguageTag.BCP47.Registry.RedundantSpec as RedundantSpec
import qualified LanguageTag.BCP47.Registry.RegionSpec as RegionSpec
import qualified LanguageTag.BCP47.Registry.ScriptSpec as ScriptSpec
import qualified LanguageTag.BCP47.Registry.VariantSpec as VariantSpec
import qualified LanguageTag.BCP47.RegistrySpec as RegistrySpec
import qualified LanguageTag.BCP47.SubtagSpec as SubtagSpec
import qualified LanguageTag.BCP47.SyntaxSpec as SyntaxSpec
import qualified LanguageTag.BCP47.TrieSpec as TrieSpec

{-
General spec convention: if one of the values fed to (===) is more
trusted (more likely to be the correct value or something like that)
then it goes on the right. Not all (===) invocations have such a
value, of course.

Also note: the Validation tests are in RegistrySpec and in Registry.*
too (since the two module hierarchies are very closely related, there
isn't a sensible way to to split up the tests).
-}

main :: IO ()
main = hspec $
  modifyMaxSuccess (const 1000) $
    parallel $ do
      describe "Subtag" SubtagSpec.spec
      describe "Syntax" SyntaxSpec.spec
      describe "Registry" $ do
        RegistrySpec.spec
        describe "Language" LanguageSpec.spec
        describe "Extlang" ExtlangSpec.spec
        describe "Script" ScriptSpec.spec
        describe "Region" RegionSpec.spec
        describe "Variant" VariantSpec.spec
        describe "Redundant" RedundantSpec.spec
        describe "Grandfathered" GrandfatheredSpec.spec
      describe "Canonicalization" CanonicalizationSpec.spec
      describe "Quasi" QuasiSpec.spec
      describe "Trie" TrieSpec.spec
