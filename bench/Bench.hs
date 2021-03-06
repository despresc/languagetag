{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.DeepSeq (NFData (..))
import Test.Tasty.Bench
import Text.LanguageTag.BCP47.Syntax
import Text.LanguageTag.BCP47.Validate
import Text.LanguageTag.Internal.Subtag (Subtag (..))
import Text.LanguageTag.Subtag (parseSubtag)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "simple"
        [ bench "stuff" $ nf parseBCP47 "en-GB-xxxxxx-xxxxxx-xxxxxx",
          bench "other" $ nf parseBCP47 "en-GB-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx",
          bench "blah" $ nf parseBCP47 "foo-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx",
          bench "blahlong" $ nf parseBCP47 "foo-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxx-xxxxx",
          bench "private" $ nf parseBCP47 "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx",
          bench "regalt" $ nf parseBCP47 "en-gb-1234-x-xxxxxx-foobar",
          bench "regUp" $ nf parseBCP47 "en-gb-1234-x-XXXXXX-FOOBAR",
          bench "regUp2" $ nf parseBCP47 "en-123-1234-x-XXXXXX-FOOBAR",
          bench "regUp3" $ nf parseBCP47 "en-hans-gb-1234-x-XXXXXX-FOOBAR",
          bench "render1" $ nf (fmap renderLanguageTag . parseBCP47) "en-123-1234-x-XXXXXX-FOOBAR",
          bench "render2" $ nf (fmap renderLanguageTag . parseBCP47) "en-hans-gb-1234-x-XXXXXX-FOOBAR",
          bench "langparse1" $ nf parseValidLang "en",
          bench "langparse2" $ nf parseValidLang "cmn",
          bench "langparse3" $ nf parseValidLang "xyzw"
        ]
    ]
  where
    parseValidLang t = parseSubtag t >>= parseLanguage
