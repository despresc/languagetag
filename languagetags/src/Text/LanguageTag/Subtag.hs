{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Text.LanguageTag.Subtag
-- Description : Generic lanuage subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The 'Subtag' and related types in this module provide a compact and
-- uniform representation for language tag components that are common
-- to many different standards.
module Text.LanguageTag.Subtag
  ( -- * Subtags
    Subtag,
    parseSubtag,
    popSubtag,
    containsLetter,
    containsOnlyLetters,
    containsDigit,
    containsOnlyDigits,
    parseSubtagMangled,
    unpackSubtag,
    unwrapSubtag,
    wrapSubtag,
    renderSubtagLower,
    subtagLength,
    subtagLength',
    subtagHead,
    indexSubtag,
    singleton,

    -- * Subtags that might not be present
    MaybeSubtag,
    maybeSubtag,
    justSubtag,
    nullSubtag,

    -- * Subtag characters
    SubtagChar,
    packChar,
    unpackCharLower,
    packCharMangled,
    unwrapChar,

    -- * Additional rendering functions
    renderSubtagUpper,
    renderSubtagTitle,
    renderSubtagBuilderLower,
    renderSubtagBuilderUpper,
    renderSubtagBuilderTitle,

    -- * Unsafe functions
    unsafeUnpackUpperLetter,
    unsafeIndexSubtag,
    unsafePopChar,
  )
where

import qualified Data.Bits as Bit
import qualified Data.ByteString.Internal as BI
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word64, Word8)
import Text.LanguageTag.Internal.Subtag

----------------------------------------------------------------
-- Subtags
----------------------------------------------------------------

-- | Render a 'Subtag' to a strict lower case text value
renderSubtagLower :: Subtag -> Text
renderSubtagLower = TL.toStrict . TB.toLazyText . renderSubtagBuilderLower

-- | Render a 'Subtag' to a strict upper case text value
renderSubtagUpper :: Subtag -> Text
renderSubtagUpper = TL.toStrict . TB.toLazyText . renderSubtagBuilderUpper

-- | Render a 'Subtag' to a strict title case text value
renderSubtagTitle :: Subtag -> Text
renderSubtagTitle = TL.toStrict . TB.toLazyText . renderSubtagBuilderTitle

-- | 'containsLetter' is 'True' exactly when the given 'Subtag'
-- contains a letter (an ASCII alphabetic character)
containsLetter :: Subtag -> Bool
containsLetter (Subtag n) = n `Bit.testBit` 4

-- | 'containsOnlyLetters' is 'True' exactly when the given 'Subtag'
-- contains only letters (ASCII alphabetic characters)
containsOnlyLetters :: Subtag -> Bool
containsOnlyLetters = not . containsDigit

-- | 'containsLetter' is 'True' exactly when the given 'Subtag'
-- contains a digit (an ASCII numeral)
containsDigit :: Subtag -> Bool
containsDigit (Subtag n) = n `Bit.testBit` 5

-- | 'containsLetter' is 'True' exactly when the given 'Subtag'
-- contains only digits (ASCII numerals)
containsOnlyDigits :: Subtag -> Bool
containsOnlyDigits = not . containsLetter

-- | Index a subtag.
indexSubtag :: Subtag -> Word8 -> Maybe SubtagChar
indexSubtag t idx
  | subtagLength t >= idx = Nothing
  | otherwise = Just $ unsafeIndexSubtag t idx

readSubtag :: Word64 -> [SubtagChar] -> Subtag
readSubtag len = Subtag . fst . List.foldl' go (len, 57)
  where
    go :: (Word64, Int) -> SubtagChar -> (Word64, Int)
    go (!acc, !idx) (SubtagChar !n) = (acc + Bit.shiftL (fromIntegral n) idx, idx - 7)

-- | Attempt to parse a text string as a subtag. This also returns
-- whether or not we saw a letter or a digit, respectively.
parseSubtag :: Text -> Maybe Subtag
parseSubtag inp
  | Just (c, t) <- T.uncons inp,
    T.length inp <= 8 =
    wchars (fromIntegral $ T.length inp) c t
  | otherwise = Nothing
  where
    fixup !len (!b, !l, !sc)
      | b = Nothing
      | otherwise = Just $ recordSeen sc $ readSubtag len $ l []
    wchars !len !c !t = case packCharDetail c of
      Just (w, sc) -> fixup len $ T.foldl' go (False, (w :), toSeenChar sc) t
      Nothing -> Nothing
    go (!b, !l, !sc) c = case packCharDetail c of
      Just (w, sc') -> (b, l . (w :), reportChar sc' sc)
      Nothing -> (True, l, sc)

-- | Given the initial character of a subtag and the remainder of a
-- 'Text' stream, attempt to parse the rest of the subtag, failing if
-- an invalid character is encountered and returning the resulting
-- 'Subtag' and the remainder of the stream.
popSubtag :: Char -> Text -> Maybe (Subtag, Text)
popSubtag initchar inp = case packCharDetail initchar of
  Just (c, sc) -> go 1 (c :) (toSeenChar sc) inp
  Nothing -> Nothing
  where
    go idx !l !sc !t
      | idx == 8 = Just (recordSeen sc $ readSubtag idx (l []), t)
      | otherwise = case T.uncons t of
        Just (c, t')
          | c == '-' -> Just (recordSeen sc $ readSubtag idx (l []), t)
          | otherwise -> case packCharDetail c of
            Just (!w, !sc') -> go (idx + 1) (l . (w :)) (reportChar sc' sc) t'
            Nothing -> Nothing
        Nothing -> Just (recordSeen sc $ readSubtag idx (l []), t)

-- | Read a tag from the given text value, truncating it or replacing
-- it with the singleton "a" if necessary, and replacing any
-- characters other than ASCII digits or letters with @\'a\'@.
parseSubtagMangled :: Text -> Subtag
parseSubtagMangled t = readSubtag (fromIntegral len) (wchars [])
  where
    tlen = T.length t
    (t', len)
      | tlen == 0 = (T.singleton 'a', 1)
      | otherwise = (T.take 8 t, min 8 tlen)
    wchars = T.foldl' go id t'
    go l c = l . (packCharMangled c :)

----------------------------------------------------------------
-- Subtag characters
----------------------------------------------------------------

-- | Unwrap a subtag character
unwrapChar :: SubtagChar -> Word8
unwrapChar (SubtagChar w) = w

onChar ::
  r ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  Char ->
  r
onChar bad f g h c
  | c > 'z' = bad
  | c >= 'a' = f $! BI.c2w c
  | c > 'Z' = bad
  | c >= 'A' = g $! BI.c2w c
  | c <= '9' && c >= '0' = h $! BI.c2w c
  | otherwise = bad

-- | Pack an ASCII alphanumeric character into the first 7 bits of a
-- 'Word8', if it is valid for a tag. This function also converts all
-- letters to lower case, and reports whether the character was a
-- letter or digit.
packCharDetail :: Char -> Maybe (SubtagChar, Bool)
packCharDetail = onChar Nothing low high dig
  where
    low w = Just (SubtagChar w, False)
    high w = Just (SubtagChar $ w + 32, False)
    dig w = Just (SubtagChar w, True)

-- | Like 'packChar', but replaces any invalid character with the
-- letter a.
packCharMangled :: Char -> SubtagChar
packCharMangled = fromMaybe (SubtagChar 97) . packChar

-- | Pack a normal 'Char' into a 'SubtagChar'.
packChar :: Char -> Maybe SubtagChar
packChar = fmap fst . packCharDetail

-- | Convert a packed letter to an unpacked upper case letter. The
-- input must in fact be an upper case letter.
unsafeUnpackUpperLetter :: SubtagChar -> Char
unsafeUnpackUpperLetter (SubtagChar w) =
  BI.w2c $ w - 32

-- | Pop a character from the head of a 'Subtag'. Note that this will
-- mangle the stored length of a subtag, so the resulting 'Subtag'
-- should only be passed to functions like 'unsafeIndexSubtag',
-- 'subtagHead', or 'unsafePopChar' itself.
unsafePopChar :: Subtag -> (SubtagChar, Subtag)
unsafePopChar (Subtag n) = (SubtagChar $ fromIntegral $ Bit.shiftR n 57, Subtag $ Bit.shiftL n 7)

data SeenChar
  = OnlyLetter
  | OnlyDigit
  | Both
  deriving (Enum)

recordSeen :: SeenChar -> Subtag -> Subtag
recordSeen OnlyLetter (Subtag n) = Subtag $ n `Bit.setBit` 4
recordSeen OnlyDigit (Subtag n) = Subtag $ n `Bit.setBit` 5
recordSeen Both (Subtag n) = Subtag $ n Bit..|. 48

-- False for letter, True for digit
reportChar :: Bool -> SeenChar -> SeenChar
reportChar True OnlyLetter = Both
reportChar False OnlyDigit = Both
reportChar _ s = s

toSeenChar :: Bool -> SeenChar
toSeenChar = toEnum . fromEnum

-- | Convert a 'SubtagChar' into a 'Subtag' of length one
singleton :: SubtagChar -> Subtag
singleton (SubtagChar c) = recordSeen sc $ Subtag $ recordLen $ Bit.shiftL (fromIntegral c) 57
  where
    recordLen = (Bit..|. 1)
    sc
      | c <= 57 = OnlyDigit
      | otherwise = OnlyLetter
