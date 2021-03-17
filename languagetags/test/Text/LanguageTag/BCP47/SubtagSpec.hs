{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.BCP47.SubtagSpec (spec) where

import qualified Data.Char as Char
import Data.Maybe (isJust)
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
  ( packChar,
    packCharMangled,
    parseSubtag,
    parseSubtagMangled,
    renderSubtagLower,
    unpackCharLower,
    unwrapChar,
    unwrapSubtag,
    wrapSubtag,
  )

{- TODO:

correctness (quickcheck, unit where appropriate) of:

- subtagLength
- subtagHead
- indexSubtag, probably unsafeIndexSubtag too (should be zero for only
  slightly out-of-bounds indices)
- singleton
- containsLetter et al.
- popSubtag
- unsafePopChar

-}

spec :: Spec
spec = do
  describe "parseSubtag" $ do
    prop "parses any well-formed subtag" $
      forAllShrink genSubtagText shrinkSubtagText $ isJust . parseSubtag
    prop "is case-insensitive on well-formed subtags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        parseSubtag t === parseSubtag (T.toLower t)
    prop "is case-insensitive on any candidate subtag" $
      forAllShrink genSubtagishText shrinkText $ \t ->
        parseSubtag t === parseSubtag (T.toLower t)
    prop "generates subtags acceptable to wrapSubtag" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        let mst = parseSubtag t
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
        parseSubtag (renderSubtagLower st) === Just st
    prop "composes on the left with parseSubtag correctly" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        (renderSubtagLower <$> parseSubtag t) === Just (T.toLower t)
    prop "is an order homomorphism" $
      forAllShrink genSubtag shrinkSubtag $ \st1 ->
        forAllShrink genSubtag shrinkSubtag $ \st2 ->
          (st1 < st2) == (renderSubtagLower st1 < renderSubtagLower st2)
  describe "parseSubtagMangled" $ do
    prop "equals parseSubtag on well-formed subtags" $
      forAllShrink genSubtagText shrinkSubtagText $ \t ->
        Just (parseSubtagMangled t) === parseSubtag t
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
