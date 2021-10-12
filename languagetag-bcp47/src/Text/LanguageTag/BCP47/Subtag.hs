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
    popSubtagLax,
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
    isSubtagChar,

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

    -- * Temporary subtag stream exports
    SubtagErr (..),
    parseSubtagWith,
    parseSubtagText,
  )
where

import Control.DeepSeq (NFData (..))
import qualified Data.Bits as Bit
import Data.Char (ord)
import Data.Maybe (isJust)
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

-- | Write a character starting at the given bit position. That bit
-- and the following six bits must not be set, and the position must
-- be of the form @57 - k * 7@ for @k@ between 0 and 7.
unsafeSetChar :: Word8 -> SubtagChar -> Word64 -> Word64
unsafeSetChar idx (SubtagChar c) n = n Bit..|. Bit.shiftL (fromIntegral c) (fromIntegral idx)

-- | Set the 'Subtag' length. There cannot be any other length
-- information recorded.
unsafeSetLen :: Word8 -> Word64 -> Word64
unsafeSetLen len w = w Bit..|. fromIntegral len

-- | A possible syntax error that may be detected during parsing
data SyntaxError
  = -- | the input was empty
    EmptyInput
  | -- | a @\'-\'@ was found at the beginning of the input
    EmptySubtag
  | -- | the subtag continued for more than 8 characters
    TagTooLong
  | -- | a @\'-\'@ followed by the end of input was found at the end of an otherwise well-formed subtag
    TrailingTerminator Subtag
  | -- | an invalid 'Char' was found at some position
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
-- not followed by the end of input. The 'popSubtagLax' function is a
-- more tolerant alternative to this function.
popSubtag :: Text -> Either SyntaxError (Subtag, Text)
popSubtag inp = case T.uncons inp of
  Just (c, t) -> case packCharDetail c of
    Just (w, sc) -> go 1 50 (unsafeSetChar 57 w 0) (toSeenChar sc) t
    Nothing
      | c == '-' -> Left EmptySubtag
      | otherwise -> Left $ InvalidChar 0 c
  Nothing -> Left EmptyInput
  where
    finish sc len stw = recordSeen sc $ Subtag $ unsafeSetLen len stw
    go !len !bitIdx !stw !sc t
      | len > 8 = Left TagTooLong
      | otherwise = case T.uncons t of
        Just (c, t')
          | c == '-' ->
            if T.null t'
              then Left $ TrailingTerminator $ finish sc len stw
              else Right (finish sc len stw, t')
          | len == 8 -> Left TagTooLong
          | otherwise -> case packCharDetail c of
            Just (w, sc') ->
              go
                (len + 1)
                (bitIdx - 7)
                (unsafeSetChar bitIdx w stw)
                (reportChar sc' sc)
                t'
            Nothing -> Left $ InvalidChar (fromIntegral len) c
        Nothing -> Right (finish sc len stw, "")

-- | Attempt to parse a 'Subtag' from the input 'Text' stream. This
-- function will take as many 'Subtag' characters as possible from the
-- input, stopping when either an invalid character is encountered or
-- eight such characters have been parsed. This function returns
-- 'Nothing' when there are no initial 'Subtag' characters.
popSubtagLax :: Text -> Maybe (Subtag, Text)
popSubtagLax inp = do
  (c, t) <- T.uncons inp
  (w, sc) <- packCharDetail c
  pure $ go 1 50 (unsafeSetChar 57 w 0) (toSeenChar sc) t
  where
    go !len !bitIdx !stw !sc t
      | len < 8,
        Just (c, t') <- T.uncons t,
        Just (w, sc') <- packCharDetail c =
        go (len + 1) (bitIdx - 7) (unsafeSetChar bitIdx w stw) (reportChar sc' sc) t'
      | otherwise = (recordSeen sc $ Subtag $ unsafeSetLen len stw, t)

----------------------------------------------------------------
-- Subtag characters
----------------------------------------------------------------

-- | Return the internal representation of a 'SubtagChar'
unwrapChar :: SubtagChar -> Word8
unwrapChar (SubtagChar w) = w

-- | Convert the internal representation of a 'SubtagChar' back to a
-- 'SubtagChar'
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

-- | Convert an ASCII alphanumeric 'Char' to a 'SubtagChar'. This
-- function also converts all letters to lower case, and reports
-- whether the character was a letter or digit.
packCharDetail :: Char -> Maybe (SubtagChar, Bool)
packCharDetail = onChar Nothing low high dig
  where
    low w = Just (SubtagChar w, False)
    high w = Just (SubtagChar $ w + 32, False)
    dig w = Just (SubtagChar w, True)

-- | Convert an ASCII alphanumeric 'Char' to a 'SubtagChar',
-- converting it it to lower case if it is not already
packChar :: Char -> Maybe SubtagChar
packChar = fmap fst . packCharDetail

-- | Tests whether or not the 'Char' is a valid subtag character (an
-- ASCII alphanumeric character)
isSubtagChar :: Char -> Bool
isSubtagChar = isJust . packCharDetail

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

--------- New stream-agnostic parser

-- | A possible error that may occur during subtag parsing
data SubtagErr s
  = -- | input was empty
    ErrEmptyInput
  | -- | an eight-character subtag, the ninth subtag character encountered, the
    -- stream after that ninth character
    ErrSubtagTooLong !Subtag !SubtagChar s
  deriving (Eq, Ord, Show)

-- | Parse a subtag from the given stream using the given function to pop bytes
-- from the stream, returning the resulting 'Subtag' and the remainder of the
-- stream if successful.
parseSubtagWith :: (s -> Maybe (SubtagChar, s)) -> s -> Either (SubtagErr s) (Subtag, s)
parseSubtagWith unc = \s -> case unc s of
  Just (w, s') ->
    go 1 50 (unsafeSetChar 57 w 0) (toSeenChar' w) s'
  Nothing -> Left ErrEmptyInput
  where
    toSeenChar' (SubtagChar w)
      | w < 97 = OnlyDigit
      | otherwise = OnlyLetter
    reportChar' (SubtagChar w) OnlyLetter | w < 97 = Both
    reportChar' (SubtagChar w) OnlyDigit | w > 57 = Both
    reportChar' _ s = s
    finish !sc !len !stw = recordSeen sc $ Subtag $ unsafeSetLen len stw
    go !len !bitIdx !stw !sc s = case unc s of
      Just (w, s')
        | len == 8 -> Left $ ErrSubtagTooLong (finish sc len stw) w s'
        | otherwise ->
          go
            (len + 1)
            (bitIdx - 7)
            (unsafeSetChar bitIdx w stw)
            (reportChar' w sc)
            s'
      Nothing -> Right (finish sc len stw, s)

parseSubtagText :: Text -> Either (SubtagErr Text) (Subtag, Text)
parseSubtagText = parseSubtagWith go
  where
    go t = do
      (c, t') <- T.uncons t
      w <- packChar c
      pure (w, t')
