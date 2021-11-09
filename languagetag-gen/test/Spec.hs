{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Library testing
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Main where

import Test.Hspec
import qualified LanguageTag.Gen.BCP47.ParseSpec as ParseSpec


main :: IO ()
main = hspec $ parallel $ do
  describe "Parse" ParseSpec.spec
