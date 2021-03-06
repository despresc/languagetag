{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Tasty.Bench
import Control.DeepSeq (NFData(..))

import Text.LanguageTag.BCP47.Syntax

instance NFData Err where
  rnf x = seq x ()
instance NFData LanguageTag where
  rnf x = seq x ()

main :: IO ()
main = defaultMain
  [ bgroup "simple"
     [ bench "stuff" $ nf parseBCP47 "en-GB-xxxxxx-xxxxxx-xxxxxx"
     , bench "other" $ nf parseBCP47 "en-GB-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "blah"  $ nf parseBCP47 "foo-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "private" $ nf parseBCP47 "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx"
     , bench "regalt" $ nf parseBCP47 "en-gb-1234-x-xxxxxx-foobar"
     , bench "regUp" $ nf parseBCP47 "en-gb-1234-x-XXXXXX-FOOBAR"
     , bench "regUp2" $ nf parseBCP47 "en-123-1234-x-XXXXXX-FOOBAR"
     , bench "regUp3" $ nf parseBCP47 "en-hans-gb-1234-x-XXXXXX-FOOBAR"
     , bench "render1" $ nf (fmap renderLanguageTag . parseBCP47) "en-123-1234-x-XXXXXX-FOOBAR"
     , bench "render2" $ nf (fmap renderLanguageTag . parseBCP47) "en-hans-gb-1234-x-XXXXXX-FOOBAR"
     ]
  ]
