{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : BCP47 subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The 'Subtag' and related types in this module provide a compact and
-- uniform representation for BCP47 subtags.
module Text.LanguageTag.BCP47.Subtag
  ( -- * Subtags
    Subtag,

    -- ** Parsing and construction
    parseSubtag,
    popSubtag,
    singleton,

    -- ** Rendering and conversion
    renderSubtagLower,
    unpackSubtag,

    -- ** Query
    subtagHead,
    indexSubtag,
    subtagLength,
    subtagLength',
    containsLetter,
    containsOnlyLetters,
    containsDigit,
    containsOnlyDigits,

    -- * Subtags that might be absent
    MaybeSubtag,
    maybeSubtag,
    justSubtag,
    nullSubtag,

    -- * Subtag characters
    SubtagChar,
    packChar,
    unpackCharLower,

    -- * Additional rendering functions
    renderSubtagUpper,
    renderSubtagTitle,
    renderSubtagBuilderLower,
    renderSubtagBuilderUpper,
    renderSubtagBuilderTitle,

    -- * Low-level conversions
    unwrapSubtag,
    wrapSubtag,
    unwrapChar,
    wrapChar,

    -- * Errors
    SyntaxError (..),

    -- * Unsafe functions
    unsafeUnpackUpperLetter,
    unsafeIndexSubtag,
  )
where

import Control.DeepSeq (NFData (..))
import qualified Data.Bits as Bit
import Data.Char (ord)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word64, Word8)
import Text.LanguageTag.Internal.BCP47.Subtag

-- From bytestring's Data.ByteString.Internal
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

----------------------------------------------------------------
-- Subtags
----------------------------------------------------------------

-- | Render a 'Subtag' to a strict text value in lower case
renderSubtagLower :: Subtag -> Text
renderSubtagLower = TL.toStrict . TB.toLazyText . renderSubtagBuilderLower

-- | Render a 'Subtag' to a strict text value in upper case
renderSubtagUpper :: Subtag -> Text
renderSubtagUpper = TL.toStrict . TB.toLazyText . renderSubtagBuilderUpper

-- | Render a 'Subtag' to a strict text value in title case
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

-- | 'containsDigit' is 'True' exactly when the given 'Subtag'
-- contains a digit (an ASCII numeral)
containsDigit :: Subtag -> Bool
containsDigit (Subtag n) = n `Bit.testBit` 5

-- | 'containsOnlyDigits' is 'True' exactly when the given 'Subtag'
-- contains only digits (ASCII numerals)
containsOnlyDigits :: Subtag -> Bool
containsOnlyDigits = not . containsLetter

-- | Return the 'SubtagChar' at the given index
indexSubtag :: Subtag -> Word8 -> Maybe SubtagChar
indexSubtag t idx
  | subtagLength t >= idx = Nothing
  | otherwise = Just $ unsafeIndexSubtag t idx

-- read a subtag given its length and constituent characters
readSubtag :: Word64 -> [SubtagChar] -> Subtag
readSubtag len = fixup . List.foldl' go (len, 57, False, False)
  where
    toSeen z w
      | z = if w then Both else OnlyLetter
      | otherwise = OnlyDigit
    fixup (x, _, z, w) = recordSeen (toSeen z w) $ Subtag x
    go (!acc, !idx, !seenLetter, !seenDigit) (SubtagChar !n) =
      (acc + Bit.shiftL (fromIntegral n) idx, idx - 7, seenLetter', seenDigit')
      where
        (seenLetter', seenDigit')
          | n <= 57 = (seenLetter, True)
          | otherwise = (True, seenDigit)

-- | A possible syntax error that may be detected during parsing
data SyntaxError
  = -- | the input was empty
    EmptyInput
  | -- | a @\'-\'@ was found at the beginning of the subtag
    EmptySubtag
  | -- | the subtag continued for more than 8 characters
    TagTooLong
  | -- | a @\'-\'@ was found at the end of an otherwise well-formed subtag
    TrailingTerminator Subtag
  | -- | an invalid 'Char' was found at that offset
    InvalidChar Int Char
  deriving (Eq, Ord, Show)

instance NFData SyntaxError where
  rnf EmptyInput = ()
  rnf EmptySubtag = ()
  rnf TagTooLong = ()
  rnf (TrailingTerminator s) = rnf s
  rnf (InvalidChar n c) = rnf n `seq` rnf c

-- | Attempt to parse an entire text string as a 'Subtag'

-- Observe that popSubtag only ever succeeds on the inputs <subtag><eof>
-- and <subtag> '-' <char>+.
parseSubtag :: Text -> Either SyntaxError Subtag
parseSubtag t = case popSubtag t of
  Left e -> Left e
  Right (s, t')
    | T.null t' -> Right s
    | otherwise -> Left $ InvalidChar (subtagLength' s) '-'

-- | Attempt to parse a 'Subtag' from the input 'Text'
-- stream. Consumes the trailing @\'-\'@ character if it exists and is
-- not followed by the end of input.
popSubtag :: Text -> Either SyntaxError (Subtag, Text)
popSubtag inp = case T.uncons inp of
  Just (c, t) -> case packCharDetail c of
    Just (w, sc) -> go 1 (w :) (toSeenChar sc) t
    Nothing
      | c == '-' -> Left EmptySubtag
      | otherwise -> Left $ InvalidChar 0 c
  Nothing -> Left EmptyInput
  where
    go !pos !l !sc t = case T.uncons t of
      Just (c, t')
        | pos > 8 -> Left TagTooLong
        | c == '-' ->
          if T.null t'
            then Left $ TrailingTerminator $ recordSeen sc $ readSubtag (fromIntegral pos) $ l []
            else Right (recordSeen sc $ readSubtag (fromIntegral pos) $ l [], t')
        | otherwise -> case packCharDetail c of
          Just (w, sc') -> go (pos + 1) (l . (w :)) (reportChar sc' sc) t'
          Nothing -> Left $ InvalidChar pos c
      Nothing -> Right (recordSeen sc $ readSubtag (fromIntegral pos) $ l [], "")

----------------------------------------------------------------
-- Subtag characters
----------------------------------------------------------------

-- | Return the internal representation of a 'SubtagChar'
unwrapChar :: SubtagChar -> Word8
unwrapChar (SubtagChar w) = w

-- | Convert the internal representation of a 'SubtagChar' back to a
-- 'SubtagChar'.
wrapChar :: Word8 -> Maybe SubtagChar
wrapChar w
  | w > 122 = Nothing
  | w < 48 = Nothing
  | w <= 57 = Just $ SubtagChar w
  | w >= 97 = Just $ SubtagChar w
  | otherwise = Nothing

onChar ::
  r ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  Char ->
  r
onChar bad f g h c
  | c > 'z' = bad
  | c >= 'a' = f $! c2w c
  | c > 'Z' = bad
  | c >= 'A' = g $! c2w c
  | c <= '9' && c >= '0' = h $! c2w c
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

-- | Pack a normal 'Char' into a 'SubtagChar', converting it it to
-- lower case if it is not already
packChar :: Char -> Maybe SubtagChar
packChar = fmap fst . packCharDetail

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
