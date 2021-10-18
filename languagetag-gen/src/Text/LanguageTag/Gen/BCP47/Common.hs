{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Common code gen functions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Text.LanguageTag.Gen.BCP47.Common
  ( parseSubtag',
    rendSubtag,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.LanguageTag.BCP47.Subtag
  ( Subtag,
    parseSubtagText,
    unwrapSubtag,
  )

-- | Parse a 'Subtag', throwing an exception if it could not be parsed
parseSubtag' :: Text -> Subtag
parseSubtag' t = case parseSubtagText t of
  Left _ -> error $ T.unpack $ "couldn't parse subtag: " <> t
  Right a -> a

-- | Render a subtag, assuming that it is well-formed, and otherwise
-- throw an exception
rendSubtag :: Text -> Text
rendSubtag x = T.pack $ "Subtag " <> show (unwrapSubtag $ parseSubtag' x)
