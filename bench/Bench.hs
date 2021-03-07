{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.DeepSeq (NFData (..), rwhnf)
import Test.Tasty.Bench
import Text.LanguageTag.BCP47.Syntax
import qualified Text.LanguageTag.BCP47.SyntaxAlt as SA
import Text.LanguageTag.BCP47.Validate
import Text.LanguageTag.Internal.Subtag (Subtag (..))
import Text.LanguageTag.Subtag (parseSubtag)

instance NFData SA.SyntaxError where
  rnf = rwhnf

main :: IO ()
main =
  defaultMain
    [ bgroup
        "alt"
        [ bench "stuff" $ nf SA.parseLanguageTag "en-GB-xxxxxx-xxxxxx-xxxxxx",
          bench "other" $ nf SA.parseLanguageTag "en-GB-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx",
          bench "blah" $ nf SA.parseLanguageTag "foo-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx",
          bench "blahlong" $ nf SA.parseLanguageTag "foo-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxx-xxxxx",
          bench "private" $ nf SA.parseLanguageTag "x-XX-xxxxxx-xxxxxx-xxxxxx-xxxxxx-xxxxxx",
          bench "regalt" $ nf SA.parseLanguageTag "en-gb-1234-x-xxxxxx-foobar",
          bench "regUp" $ nf SA.parseLanguageTag "en-gb-1234-x-XXXXXX-FOOBAR",
          bench "regUp2" $ nf SA.parseLanguageTag "en-123-1234-x-XXXXXX-FOOBAR",
          bench "regUp3" $ nf SA.parseLanguageTag "en-hans-gb-1234-x-XXXXXX-FOOBAR",
          bench "render1" $ nf (fmap renderLanguageTag . SA.parseLanguageTag) "en-123-1234-x-XXXXXX-FOOBAR",
          bench "render2" $ nf (fmap renderLanguageTag . SA.parseLanguageTag) "en-hans-gb-1234-x-XXXXXX-FOOBAR",
          bench "simple" $ nf SA.parseLanguageTag "en"
        ],
      bgroup
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
          bench "langparse3" $ nf parseValidLang "xyzw",
          bench "simple" $ nf parseBCP47 "en"
        ]
    ]
  where
    parseValidLang t = parseSubtag t >>= parseLanguage
