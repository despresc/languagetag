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
  ( forAll,
    forAllShrink,
    shrink,
    (===),
  )
import Text.LanguageTag.BCP47.Subtag
  ( SyntaxError (..),
    containsDigit,
    containsLetter,
    containsOnlyDigits,
    containsOnlyLetters,
    packChar,
    packCharMangled,
    parseSubtag,
    parseSubtagMangled,
    popSubtag,
    renderSubtagLower,
    singleton,
    subtagHead,
    subtagLength,
    subtagLength',
    unpackCharLower,
    unpackSubtag,
    unsafeIndexSubtag,
    unsafePopChar,
    unwrapChar,
    unwrapSubtag,
    wrapSubtag,
  )
import Text.LanguageTag.Internal.BCP47.Subtag (SubtagChar (..))

spec :: Spec
spec = do
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
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
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
  describe "parseSubtagMangled" $ do
    prop "equals parseSubtag on well-formed subtags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        Right (parseSubtagMangled t) === parseSubtag t
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genSubtagishText shrinkText $ \t ->
        let st = parseSubtagMangled t
         in wrapSubtag (unwrapSubtag st) === Just st
    prop "equals what you'd expect on text strings after renderSubtagLower" $
      let isAsciiAlphaNum c = Char.isAsciiUpper c || Char.isAsciiLower c || Char.isDigit c
          postTreat t
            | T.null t = "a"
            | otherwise =
              T.take 8 $
                T.map (\c -> if isAsciiAlphaNum c then Char.toLower c else 'a') t
       in forAllShrink genSubtagishText shrinkText $ \t ->
            renderSubtagLower (parseSubtagMangled t) === postTreat t
  describe "packCharMangled" $ do
    prop "equals packChar on well-formed tags" $
      forAllShrink genSubtagChar shrink $ \c ->
        Just (packCharMangled c) === packChar c
    prop "generates characters acceptable to packChar" $
      forAllShrink genSubtagishChar shrink $ \c ->
        let sc = packCharMangled c
         in packChar (toEnum $ fromIntegral $ unwrapChar sc) === Just sc
    prop "equals what you'd expect on characters after Char.toLower" $
      let isAsciiAlphaNum c = Char.isAsciiUpper c || Char.isAsciiLower c || Char.isDigit c
          postTreat c = if isAsciiAlphaNum c then Char.toLower c else 'a'
       in forAllShrink genSubtagishChar shrink $ \t ->
            unpackCharLower (packCharMangled t) === postTreat t
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
  describe "popSubtag" $ do
    prop "pops initial well-formed subtags" $ do
      let gen = do
            t <- genSubtagText
            t' <- genSubtagishText
            pure $ if T.null t' then t else t <> "-" <> t'
          pop' t = case T.span (/= '-') t of
            (start, rest) -> case T.uncons rest of
              Just (c, t')
                | c /= '-' -> Nothing
                | otherwise -> (,t') <$> collapseLeft (parseSubtag start)
              _ -> (,"") <$> collapseLeft (parseSubtag start)
      forAll gen $ \t ->
        collapseLeft (popSubtag t) === pop' t
  describe "unpackSubtag" $ do
    prop "unpacks subtags correctly" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        T.pack (unpackCharLower <$> unpackSubtag st) === renderSubtagLower st
  describe "unsafePopChar" $ do
    prop "behaves like unpackSubtag" $
      forAllShrink genSubtag shrinkSubtag $ \st ->
        let chars = go st 0 id []
            len = subtagLength st
            go x n acc
              | n == len = acc
              | otherwise = let (c, x') = unsafePopChar x in go x' (n + 1) (acc . (c :))
         in chars === unpackSubtag st
