{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.LanguageTag.BCP47.SubtagSpec (spec) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( arbitrary,
    forAll,
    forAllShrink,
    shrink,
    suchThat,
    vectorOf,
    (===),
  )
import Text.LanguageTag.BCP47.Subtag
  ( SubtagError (..),
    containsDigit,
    containsLetter,
    containsOnlyDigits,
    containsOnlyLetters,
    isSubtagChar,
    packChar,
    parseSubtagText,
    renderSubtagLower,
    singleton,
    subtagHead,
    subtagLength,
    subtagLength',
    unpackCharLower,
    unpackSubtag,
    unsafeIndexSubtag,
    unwrapSubtag,
    wrapSubtag,
  )
import Text.LanguageTag.Internal.BCP47.Subtag
  ( SubtagChar (..),
  )

{-
TODO:

- add some ill-formed subtag parsing property/unit tests now that the
  error type is richer

- tests of parseSubtagWith? or perhaps the tests of the derived functions are
  enough.

- maybe add a parseSubtagTextFull or something to get rid of the fst's around
  the parseSubtagText invocations
-}

spec :: Spec
spec = do
  let isAsciiAlphaNum c = Char.isDigit c || Char.isAsciiUpper c || Char.isAsciiLower c
  -- limited down-casing, since toLower can map non-ASCII
  -- alphanumerics to ASCII alphanumerics (as the \8490 "Kelvin
  -- symbol" reminded me). renders the relevant tests a little less
  -- informative, unfortunately.
  let limitedDownCase c
        | isSubtagChar c' && not (isSubtagChar c) = c
        | otherwise = c'
        where
          c' = Char.toLower c
  let mapDownCase = T.map limitedDownCase
  let genSubtagThenGarbage = do
        t <- genSubtagText
        c <- arbitrary `suchThat` (not . isSubtagChar)
        t' <- genSubtagishText
        b <- arbitrary
        pure $ if b then t <> T.singleton c <> t' else t
  describe "parseSubtagText" $ do
    prop "acts as if subsequent invalid characters are absent" $ do
      let parse' t = case parseSubtagText start of
            Left EmptyInput -> Left EmptyInput
            Left (SubtagTooLong st c s) -> Left $ SubtagTooLong st c (s <> rest)
            Right (st, _) -> Right (st, rest)
            where
              (start, rest) = T.span isSubtagChar t
      forAllShrink genSubtagishText shrinkText $ \t ->
        parseSubtagText t === parse' t
    it "fails to parse empty input correctly" $
      parseSubtagText "" === Left EmptyInput
    prop "fails to parse input starting with an invalid character correctly" $ do
      let gen = do
            c <- arbitrary `suchThat` (not . isSubtagChar)
            t <- genSubtagishText
            pure $ T.cons c t
      forAll gen $ \t ->
        parseSubtagText t === Left EmptyInput
    prop "parses well-formed subtags completely" $ do
      let fixBad (Left e) = Just (Left e)
          fixBad (Right (st, t))
            | T.null t = Nothing
            | otherwise = Just (Right (st, t))
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        fixBad (parseSubtagText t) === Nothing
    prop "pops initial well-formed subtags correctly" $ do
      forAll genSubtagThenGarbage $ \t ->
        fmap snd (parseSubtagText t) === Right (T.dropWhile isSubtagChar t)
    prop "is case-insensitive on initial well-formed subtags" $ do
      let fixPop (Right (s, t)) = Right (s, T.toLower t)
          fixPop (Left EmptyInput) = Left EmptyInput
          fixPop (Left (SubtagTooLong st c s)) = Left $ SubtagTooLong st c $ T.toLower s
      forAll genSubtagThenGarbage $ \t ->
        fixPop (parseSubtagText t) === parseSubtagText (T.toLower t)
    prop "is case-insensitive on any candidate subtag" $ do
      let fixUp (Left (SubtagTooLong st c s)) = Left $ SubtagTooLong st c $ mapDownCase s
          fixUp (Left EmptyInput) = Left EmptyInput
          fixUp (Right (s, t)) = Right (s, mapDownCase t)
      forAllShrink genSubtagishText shrinkText $ \t ->
        fixUp (parseSubtagText t) === parseSubtagText (mapDownCase t)
    prop "generates subtags acceptable to wrapSubtag" $
      forAll genSubtagThenGarbage $ \t ->
        let mst = collapseLeft $ fst <$> parseSubtagText t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
    prop "generates subtags acceptable to wrapSubtag on candidates" $
      forAllShrink genSubtagishText shrinkText $ \t ->
        let mst = collapseLeft $ fst <$> parseSubtagText t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
    prop "fails to parse tags that are too long" $ do
      let genLarge = do
            t <- T.pack <$> vectorOf 9 genSubtagChar
            t' <- genSubtagishText
            pure $ t <> t'
          shrinkLarge t = case T.splitAt 9 t of
            (start, rest) -> (start <>) <$> (filter (not . T.null) $ shrinkText rest)
      let fixup (Left SubtagTooLong {}) = True
          fixup _ = False
      forAllShrink genLarge shrinkLarge $ \t ->
        fixup (parseSubtagText t) === True
  describe "unpackCharLower" $ do
    prop "is the right inverse of packChar, mostly" $
      forAllShrink genSubtagSubtagChar shrinkSubtagChar $ \c ->
        packChar (unpackCharLower c) === Just c
    prop "is the left inverse of packChar on well-formed Chars, mostly" $
      forAllShrink genSubtagChar shrink $ \c ->
        (unpackCharLower <$> packChar c) === Just (Char.toLower c)
  describe "renderSubtagLower" $ do
    prop "composes on the right with parseSubtag correctly" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        (fst <$> parseSubtagText (renderSubtagLower st)) === Right st
    prop "composes on the left with parseSubtag correctly" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (renderSubtagLower . fst <$> parseSubtagText t) === Right (T.toLower t)
    prop "is an order homomorphism" $
      forAllShrink genSubtag shrinkSubtag $ \st1 ->
        forAllShrink genSubtag shrinkSubtag $ \st2 ->
          (st1 < st2) == (renderSubtagLower st1 < renderSubtagLower st2)
  describe "subtagLength" $ do
    prop "equals the text length of well-formed tags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (subtagLength' . fst <$> parseSubtagText t) === Right (T.length t)
  describe "subtagHead" $ do
    prop "equals the text head of well-formed tags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (unpackCharLower . subtagHead . fst <$> parseSubtagText t)
          === Right (Char.toLower $ T.head t)
  describe "unsafeIndexSubtag" $ do
    prop "equals the text index of well-formed tags" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        let t = renderSubtagLower st
            idxs = [0 .. (T.length t - 1)]
            badIdx =
              List.find
                ( \n ->
                    T.index t n
                      /= unpackCharLower (unsafeIndexSubtag st (fromIntegral n))
                )
                idxs
         in badIdx === Nothing
    prop "is zero for nearly in-bounds indices" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        let idxs = [subtagLength st .. 7]
            badIdx = List.find ((/= SubtagChar 0) . unsafeIndexSubtag st) idxs
         in badIdx === Nothing
  describe "singleton" $ do
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genSubtagSubtagChar shrinkSubtagChar $ \c ->
        wrapSubtag (unwrapSubtag $ singleton c) === Just (singleton c)
    prop "has the correct head" $
      forAllShrink genSubtagSubtagChar shrinkSubtagChar $ \c ->
        subtagHead (singleton c) === c
    prop "is length 1" $
      forAllShrink genSubtagSubtagChar shrinkSubtagChar $ \c ->
        subtagLength (singleton c) === 1
  describe "containsLetter" $ do
    let isLetter c = Char.isAsciiUpper c || Char.isAsciiLower c
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsLetter . fst <$> parseSubtagText t) === Right (T.any isLetter t)
  describe "containsOnlyLetters" $ do
    let isLetter c = Char.isAsciiUpper c || Char.isAsciiLower c
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsOnlyLetters . fst <$> parseSubtagText t) === Right (T.all isLetter t)
  describe "containsDigit" $ do
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsDigit . fst <$> parseSubtagText t) === Right (T.any Char.isDigit t)
  describe "containsOnlyDigits" $ do
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsOnlyDigits . fst <$> parseSubtagText t) === Right (T.all Char.isDigit t)
  describe "unpackSubtag" $ do
    prop "unpacks subtags correctly" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        T.pack (unpackCharLower <$> unpackSubtag st) === renderSubtagLower st
  describe "isSubtagChar" $ do
    prop "behaves like isAsciiAlphaNum" $
      forAllShrink genSubtagishChar shrink $ \c ->
        isSubtagChar c === isAsciiAlphaNum c
