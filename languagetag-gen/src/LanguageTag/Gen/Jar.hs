{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Jar file parsing
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Jar' type and some parsing and query
-- functions related to it
module LanguageTag.Gen.Jar
  ( Jar,
    LineNum,
    lookupJar,
    jarLine,
    parseJar,
    parseJarFile,
  )
where

import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

-- | A line number in a 'Jar' file
type LineNum = Int

-- | A record-jar record (according to the BCP47 spec, anyway),
-- decorated with the line numbers of the start of every
-- component. Note that records may contain multiple values for a
-- particular field, hence the 'NonEmpty'.
type Jar = (LineNum, HashMap Text (NonEmpty (LineNum, Text)))

-- | Look up a field in a 'Jar' record
lookupJar :: Text -> Jar -> [(LineNum, Text)]
lookupJar t (_, j) = maybe [] toList $ HM.lookup t j

-- | Return the line number of a 'Jar' record
jarLine :: Jar -> LineNum
jarLine = fst

-- | Group the fields of a 'Jar' record together into @(linenum,
-- firstline, linefolds)@ triples
groupFields :: [(LineNum, Text)] -> [(LineNum, Text, [Text])]
groupFields ((ln, t) : ts) =
  let (folds, ts') = grabFolds id ts
   in (ln, t, folds) : groupFields ts'
  where
    grabFolds l ((n, y) : ys)
      | T.take 1 y == " " = grabFolds (l . (y :)) ys
      | otherwise = (l [], (n, y) : ys)
    grabFolds l [] = (l [], [])
groupFields [] = []

-- | Parse the grouped fields of a 'Jar' file, otherwise returning the
-- line number and text of the unparsable line encountered
parseFields :: [(LineNum, Text, [Text])] -> Either (LineNum, Text) [(LineNum, Text, Text)]
parseFields = traverse go
  where
    go (ln, t, ts)
      | (tag, rest) <- T.span (/= ':') t,
        not (T.null tag),
        not (T.any isSpace tag) =
        let rest' = T.drop 1 rest
            spacenorm = T.intercalate " " . T.split isSpace . T.strip
            body =
              T.intercalate "\n" $ fmap spacenorm $ rest' : ts
         in Right (ln, tag, body)
      | otherwise = Left (ln, t)

-- | Parse an entire 'Jar' record given the line number of its first
-- line and its decorated fields
parseJar :: (LineNum, [(LineNum, Text)]) -> Either (LineNum, Text) Jar
parseJar (ln, inp) = (,) ln . go <$> parseFields (groupFields inp)
  where
    go = HM.fromList . gather
    getTag (_, x, _) = x
    gather =
      fmap fuse . List.groupBy (\x y -> getTag x == getTag y)
        . List.sortBy (\x y -> getTag x `compare` getTag y)
    fuse ((l, tag, body) : rest) = (tag, (l, body) NE.:| ((\(x, _, z) -> (x, z)) <$> rest))
    fuse _ = error "internal error: parseJar.fuse"

-- | Group the records in a split jar file together
groupJars :: [(LineNum, Text)] -> [(LineNum, [(LineNum, Text)])]
groupJars inp = start : manyJars inp'
  where
    jarSpan n l = let (x, y) = List.span (\(_, t) -> t /= "%%") l in ((n, x), y)
    (start, inp') = jarSpan 0 inp
    manyJars [] = []
    manyJars ((l, _) : ts) =
      let (a, b) = jarSpan (l + 1) ts
       in a : manyJars b

-- | Parse an entire jar file
parseJarFile :: Text -> Either (LineNum, Text) [Jar]
parseJarFile = traverse parseJar . groupJars . zip [0 ..] . T.lines
