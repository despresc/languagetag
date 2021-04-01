{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.LanguageTag.BCP47.SubtagSpec (spec) where

import qualified Data.Char as Char
import Data.Either (isRight)
import qualified Data.List as List
import qualified Data.Text as T
import Test.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( forAllShrink,
    shrink,
    suchThat,
    vectorOf,
    (===),
  )
import Text.LanguageTag.BCP47.Subtag
  ( SyntaxError (..),
    containsDigit,
    containsLetter,
    containsOnlyDigits,
    containsOnlyLetters,
    packChar,
    parseSubtag,
    popSubtag,
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
-}

spec :: Spec
spec = do
  describe "popSubtag" $ do
    -- Note that this test and the failing trailing terminator test
    -- are important for the correctness of parseSubtag, since it
    -- shows that popSubtag succeeds precisely on the inputs
    -- <subtag><eof> and <subtag> '-' <chars>.
    prop "pops initial well-formed subtags" $ do
      let pop' t = case T.break (== '-') t of
            (start, rest) -> case T.uncons rest of
              Just (c, t')
                | c /= '-' -> Nothing
                | otherwise -> (,t') <$> collapseLeft (parseSubtag start)
              _ -> (,"") <$> collapseLeft (parseSubtag start)
      forAllShrink genPopSubtagText shrinkPopSubtagText $ \t ->
        collapseLeft (popSubtag t) === pop' t
    prop "fails to parse trailing terminator subtags correctly" $ do
      let fixBad (Left (TrailingTerminator st)) = Just st
          fixBad _ = Nothing
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        fixBad (popSubtag $ t <> "-") === collapseLeft (fst <$> popSubtag t)
    it "fails to parse empty input correctly" $
      popSubtag "" === Left EmptyInput
    prop "is case-insensitive on initial well-formed subtags" $ do
      let fixPop (Right (s, t)) = Right (s, T.toLower t)
          fixPop (Left e) = Left e
      forAllShrink genPopSubtagText shrinkPopSubtagText $ \t ->
        fixPop (popSubtag t) === popSubtag (T.toLower t)
    prop "is case-insensitive on any candidate subtag" $ do
      let fixUp (Left (InvalidChar n c)) = Left $ InvalidChar n $ Char.toLower c
          fixUp (Left e) = Left e
          fixUp (Right (s, t)) = Right (s, T.toLower t)
      forAllShrink genSubtagishText shrinkText $ \t ->
        fixUp (popSubtag t) === popSubtag (T.toLower t)
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genPopSubtagText shrinkPopSubtagText $ \t ->
        let mst = collapseLeft $ fst <$> popSubtag t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
  describe "parseSubtag" $ do
    prop "parses any well-formed subtag" $
      forAllShrink genSubtagText shrinkSubtagText $ isRight . parseSubtag
    prop "is case-insensitive on well-formed subtags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        parseSubtag t === parseSubtag (T.toLower t)
    prop "is case-insensitive on any candidate subtag" $ do
      let fixError (Left (InvalidChar n c)) = Left $ InvalidChar n $ Char.toLower c
          fixError x = x
      forAllShrink genSubtagishText shrinkText $ \t ->
        fixError (parseSubtag t) === parseSubtag (T.toLower t)
    prop "fails to parse tags that are too long" $ do
      let genLarge = do
            t <- T.pack <$> vectorOf 8 genSubtagChar
            let p x = case T.uncons x of
                  Just (c, _) -> c /= '-'
                  Nothing -> False
            t' <- genSubtagishText `suchThat` p
            pure $ t <> t'
          shrinkLarge t = case T.splitAt 8 t of
            (start, rest) -> (start <>) <$> (filter (not . T.null) $ shrinkText rest)
      forAllShrink genLarge shrinkLarge $ \t ->
        parseSubtag t === Left TagTooLong
    it "fails to parse empty input correctly" $
      parseSubtag "" === Left EmptyInput
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        let mst = collapseLeft $ parseSubtag t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
    prop "generates subtags acceptable to wrapSubtag on candidates" $
      forAllShrink genSubtagishText shrinkText $ \t ->
        let mst = collapseLeft $ parseSubtag t
            wrapst = mst >>= (wrapSubtag . unwrapSubtag)
         in mst === wrapst
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
        parseSubtag (renderSubtagLower st) === Right st
    prop "composes on the left with parseSubtag correctly" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (renderSubtagLower <$> parseSubtag t) === Right (T.toLower t)
    prop "is an order homomorphism" $
      forAllShrink genSubtag shrinkSubtag $ \st1 ->
        forAllShrink genSubtag shrinkSubtag $ \st2 ->
          (st1 < st2) == (renderSubtagLower st1 < renderSubtagLower st2)
  describe "subtagLength" $ do
    prop "equals the text length of well-formed tags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (subtagLength' <$> parseSubtag t) === Right (T.length t)
  describe "subtagHead" $ do
    prop "equals the text head of well-formed tags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (unpackCharLower . subtagHead <$> parseSubtag t) === Right (Char.toLower $ T.head t)
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
        (containsLetter <$> parseSubtag t) === Right (T.any isLetter t)
  describe "containsOnlyLetters" $ do
    let isLetter c = Char.isAsciiUpper c || Char.isAsciiLower c
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsOnlyLetters <$> parseSubtag t) === Right (T.all isLetter t)
  describe "containsDigit" $ do
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsDigit <$> parseSubtag t) === Right (T.any Char.isDigit t)
  describe "containsOnlyDigits" $ do
    prop "behaves like the Text implementation" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (containsOnlyDigits <$> parseSubtag t) === Right (T.all Char.isDigit t)
  describe "unpackSubtag" $ do
    prop "unpacks subtags correctly" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        T.pack (unpackCharLower <$> unpackSubtag st) === renderSubtagLower st
