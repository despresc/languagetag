{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Tasty.Bench
import Text.BCP47.Syntax
import Control.DeepSeq (NFData(..))

import qualified Text.BCP47.SyntaxAlt as SA
import qualified Text.BCP47.Internal.SyntaxAlt as SA



instance NFData Err where
  rnf x = seq x ()
instance NFData LanguageTag where
  rnf x = seq x ()

instance NFData SA.Err where
  rnf x = seq x ()
instance NFData SA.LanguageTag where
  rnf x = seq x ()

main :: IO ()
main = defaultMain
  [ bgroup "simple"
     [ bench "stuff" $ nf parseBCP47 "en-GB-xxxxxx-xxxxxx-xxxxxx"
     , bench "other" $ nf parseBCP47 "en-GB-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "blah"  $ nf parseBCP47 "foo-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "privatenormal" $ nf parseBCP47 "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "privatealt" $ nf SA.parseBCP47 "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "privatenormal" $ nf parseBCP47 "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "privatealt" $ nf SA.parseBCP47 "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "regnormal" $ nf parseBCP47 "en-gb-1234-x-xxxxxx-foobar"
     , bench "regalt" $ nf SA.parseBCP47 "en-gb-1234-x-xxxxxx-foobar"
     ]
  ]
