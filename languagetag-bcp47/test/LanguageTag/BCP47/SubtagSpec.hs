{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LanguageTag.BCP47.SubtagSpec (spec) where

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
import LanguageTag.BCP47.Subtag
  ( PopError (..),
    containsDigit,
    containsLetter,
    containsOnlyDigits,
    containsOnlyLetters,
    isSubtagChar,
    packChar,
    popSubtagText,
    renderSubtagLower,
    singleton,
    subtagHead,
    subtagHeadIsDigit,
    subtagLength,
    subtagLength',
    unpackCharLower,
    unpackSubtag,
    unsafeIndexSubtag,
    unwrapSubtag,
    wrapSubtag,
  )
import LanguageTag.Internal.BCP47.Subtag
  ( SubtagChar (..),
  )

{-
TODO:

- add some ill-formed subtag parsing property/unit tests now that the
  error type is richer

- tests of popSubtagWith? or perhaps the tests of the derived functions are
  enough.

- maybe add a popSubtagTextFull or something to get rid of the fst's around
  the popSubtagText invocations
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
  describe "popSubtagText" $ do
    prop "acts as if subsequent invalid characters are absent" $ do
      let parse' t = case popSubtagText start of
            Left e -> Left e
            Right (st, _) -> Right (st, rest)
            where
              (start, rest) = T.span isSubtagChar t
      forAllShrink genSubtagishText shrinkText $ \t ->
        popSubtagText t === parse' t
    it "fails to parse empty input correctly" $
      popSubtagText "" === Left PopEmptySubtag
    prop "fails to parse input starting with an invalid character correctly" $ do
      let gen = do
            c <- arbitrary `suchThat` (not . isSubtagChar)
            t <- genSubtagishText
            pure $ T.cons c t
      forAll gen $ \t ->
        popSubtagText t === Left PopEmptySubtag
    prop "parses well-formed subtags completely" $ do
      let fixBad (Left e) = Just (Left e)
          fixBad (Right (st, t))
            | T.null t = Nothing
            | otherwise = Just (Right (st, t))
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        fixBad (popSubtagText t) === Nothing
    prop "pops initial well-formed subtags correctly" $ do
      forAllShrink genPopSubtagText shrinkPopSubtagText $ \t ->
        fmap snd (popSubtagText t) === Right (T.dropWhile isSubtagChar t)
    prop "is case-insensitive on initial well-formed subtags" $ do
      let fixPop (Right (s, t)) = Right (s, T.toLower t)
          fixPop (Left e) = Left e
      forAllShrink genPopSubtagText shrinkPopSubtagText $ \t ->
        fixPop (popSubtagText t) === popSubtagText (T.toLower t)
    prop "is case-insensitive on any candidate subtag" $ do
      let fixUp (Left e) = Left e
          fixUp (Right (s, t)) = Right (s, mapDownCase t)
      forAllShrink genSubtagishText shrinkText $ \t ->
        fixUp (popSubtagText t) === popSubtagText (mapDownCase t)
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genPopSubtagText shrinkPopSubtagText $ \t ->
        let mst = collapseLeft $ fst <$> popSubtagText t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
    prop "generates subtags acceptable to wrapSubtag on candidates" $
      forAllShrink genSubtagishText shrinkText $ \t ->
        let mst = collapseLeft $ fst <$> popSubtagText t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
    prop "fails to parse tags that are too long" $ do
      let genLarge = do
            t <- T.pack <$> vectorOf 9 genSubtagChar
            t' <- genSubtagishText
            pure $ t <> t'
          shrinkLarge t = case T.splitAt 9 t of
            (start, rest) -> (start <>) <$> (filter (not . T.null) $ shrinkText rest)
      let fixup (Left PopSubtagTooLong {}) = True
          fixup _ = False
      forAllShrink genLarge shrinkLarge $ \t ->
        fixup (popSubtagText t) === True
  describe "unpackCharLower" $ do
    prop "is the right inverse of packChar, mostly" $
      forAllShrink genSubtagSubtagChar shrinkSubtagChar $ \c ->
        packChar (unpackCharLower c) === Just c
    prop "is the left inverse of packChar on well-formed Chars, mostly" $
      forAllShrink genSubtagChar shrink $ \c ->
        (unpackCharLower <$> packChar c) === Just (Char.toLower c)
  describe "renderSubtagLower" $ do
    prop "composes on the right with popSubtagText correctly" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        (fst <$> popSubtagText (renderSubtagLower st)) === Right st
    prop "composes on the left with popSubtagText correctly" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (renderSubtagLower . fst <$> popSubtagText t) === Right (T.toLower t)
    prop "is an order homomorphism" $
      forAllShrink genSubtag shrinkSubtag $ \st1 ->
        forAllShrink genSubtag shrinkSubtag $ \st2 ->
          (st1 < st2) == (renderSubtagLower st1 < renderSubtagLower st2)
  describe "subtagLength" $ do
    prop "equals the text length of well-formed tags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (subtagLength' . fst <$> popSubtagText t) === Right (T.length t)
  describe "subtagHead" $ do
    prop "equals the text head of well-formed tags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (unpackCharLower . subtagHead . fst <$> popSubtagText t)
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
        (containsLetter . fst <$> popSubtagText t) === Right (T.any isLetter t)
  describe "containsOnlyLetters" $ do
    let isLetter c = Char.isAsciiUpper c || Char.isAsciiLower c
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsOnlyLetters . fst <$> popSubtagText t) === Right (T.all isLetter t)
  describe "containsDigit" $ do
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsDigit . fst <$> popSubtagText t) === Right (T.any Char.isDigit t)
  describe "containsOnlyDigits" $ do
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsOnlyDigits . fst <$> popSubtagText t) === Right (T.all Char.isDigit t)
  describe "unpackSubtag" $ do
    prop "unpacks subtags correctly" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        T.pack (unpackCharLower <$> unpackSubtag st) === renderSubtagLower st
  describe "isSubtagChar" $ do
    prop "behaves like isAsciiAlphaNum" $
      forAllShrink genSubtagishChar shrink $ \c ->
        isSubtagChar c === isAsciiAlphaNum c
  describe "subtagHeadIsDigit" $ do
    prop "behaves like the naive implementation" $ do
      let isDigit (SubtagChar n) = n <= 57
      forAllShrink genSubtag shrinkSubtag $ \st ->
        subtagHeadIsDigit st === isDigit (subtagHead st)
