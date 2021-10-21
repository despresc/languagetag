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
    parseSubtagText,
    parseSubtagWith,
    singleton,
    popSubtagText,
    popSubtagWith,

    -- ** Rendering and conversion
    renderSubtagLower,
    unpackSubtag,

    -- ** Query
    subtagHead,
    indexSubtag,
    subtagLength,
    subtagLength',
    PresentCharacters (..),
    containsCharacters,
    containsLetter,
    containsOnlyLetters,
    containsDigit,
    containsOnlyDigits,
    subtagHeadIsDigit,

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
    isSubtagByte,

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
    PopError (..),
    ParseError (..),

    -- * Unsafe functions
    unsafeUnpackUpperLetter,
    unsafeIndexSubtag,
  )
where

import Control.DeepSeq (NFData (..))
import qualified Data.Bits as Bit
import Data.Char (ord)
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

-- | The characters that may be present in a particular 'Subtag'
data PresentCharacters
  = OnlyDigits
  | OnlyLetters
  | DigitsAndLetters
  deriving (Eq, Ord, Show)

-- | Returns the category of characters present in a 'Subtag'
containsCharacters :: Subtag -> PresentCharacters
containsCharacters st
  | isDigit (subtagHead st) = loopDig 1
  | otherwise = loopLet 1
  where
    isDigit (SubtagChar x) = x <= 57
    isLetter (SubtagChar x) = x >= 97
    len = subtagLength st
    loopDig idx
      | idx == len = OnlyDigits
      | isLetter (unsafeIndexSubtag st idx) = DigitsAndLetters
      | otherwise = loopDig (idx + 1)
    loopLet idx
      | idx == len = OnlyLetters
      | isDigit (unsafeIndexSubtag st idx) = DigitsAndLetters
      | otherwise = loopLet (idx + 1)

-- | 'containsLetter' is 'True' exactly when the given 'Subtag'
-- contains a letter (an ASCII alphabetic character)
containsLetter :: Subtag -> Bool
containsLetter = (/= OnlyDigits) . containsCharacters

-- | 'containsOnlyLetters' is 'True' exactly when the given 'Subtag'
-- contains only letters (ASCII alphabetic characters)
containsOnlyLetters :: Subtag -> Bool
containsOnlyLetters = (== OnlyLetters) . containsCharacters

-- | 'containsDigit' is 'True' exactly when the given 'Subtag'
-- contains a digit (an ASCII numeral)
containsDigit :: Subtag -> Bool
containsDigit = (/= OnlyLetters) . containsCharacters

-- | 'containsOnlyDigits' is 'True' exactly when the given 'Subtag'
-- contains only digits (ASCII numerals)
containsOnlyDigits :: Subtag -> Bool
containsOnlyDigits = (== OnlyDigits) . containsCharacters

-- | 'subtagHeadIsDigit' is 'True' exactly when the head of the given 'Subtag'
-- is a digit (ASCII numeral). (Yes, this is unusually specialized, but it is
-- needed elsewhere).
subtagHeadIsDigit :: Subtag -> Bool
subtagHeadIsDigit (Subtag n) = (n Bit..&. sel) <= selectedNumber9
  where
    -- highest 7 bits
    sel = 18302628885633695744
    -- subtag "9" with the selector above applied to it
    selectedNumber9 = 8214565720323784705

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

-- | A possible syntax error that may be detected during parsing with
-- 'popSubtagWith' and related functions
data PopError
  = -- | there were no subtag characters at the beginning of input
    PopEmptySubtag
  | -- | a full subtag was parsed but there were also subsequent subtag
    -- characters
    PopSubtagTooLong Subtag
  deriving (Eq, Ord, Show)

instance NFData PopError where
  rnf PopEmptySubtag = ()
  rnf (PopSubtagTooLong x) = rnf x

-- | Parse a 'Subtag' from a 'Text' stream, stopping either at the end of input
-- or the first non-subtag character encountered. Uses 'parseSubtagWith'.
popSubtagText :: Text -> Either PopError (Subtag, Text)
popSubtagText = either (Left . fst) Right . popSubtagWith go
  where
    -- TODO: perhaps export this go as its own function? duplicated in
    -- 'popBCP47Len'. esp. if we have a Text/ByteString hierarchy split.
    go t = do
      (c, t') <- T.uncons t
      w <- packChar c
      pure (w, t')

-- TODO: perhaps create variants of parseSubtagText (e.g. for bytestring), maybe
-- also unsafe ones (i.e. ones that assume that the input is already
-- well-formed), maybe attempt a monadic one again?

-- | Parse a subtag from the given stream using the given function to pop bytes
-- from the stream, returning the resulting 'Subtag' and the remainder of the
-- stream if successful. If a syntax error is encountered, this function will
-- also return the stream at that point.
popSubtagWith :: (s -> Maybe (SubtagChar, s)) -> s -> Either (PopError, s) (Subtag, s)
popSubtagWith unc = \s -> case unc s of
  Just (w, s') ->
    go 1 50 (unsafeSetChar 57 w 0) s'
  Nothing -> Left (PopEmptySubtag, s)
  where
    finish !len !stw = Subtag $ unsafeSetLen len stw
    go !len !bitIdx !stw s = case unc s of
      Just (w, s')
        | len == 8 -> Left (PopSubtagTooLong (finish len stw), s)
        | otherwise ->
          go
            (len + 1)
            (bitIdx - 7)
            (unsafeSetChar bitIdx w stw)
            s'
      Nothing -> Right (finish len stw, s)

-- | An error that may occur when parsing a 'Subtag' that constitutes the entire
-- input stream
data ParseError c
  = ParseEmptySubtag
  | ParseSubtagTooLong Subtag
  | ParseInvalidChar (Maybe Subtag) Int c
  deriving (Eq, Ord, Show)

-- TODO: the direct definition would be more efficient

-- | @'parseSubtagWith' uncons toChar s@ parses a 'Subtag' from @s@, returning
-- an error if @s@ does not consist of a single well-formed 'Subtag'
parseSubtagWith ::
  (s -> Maybe (c, s)) ->
  (c -> Maybe SubtagChar) ->
  s ->
  Either (ParseError c) Subtag
parseSubtagWith uncC toC = \s -> case popSubtagWith unc s of
  Left (PopEmptySubtag, _)
    | Just (c, _) <- uncC s ->
      Left $ ParseInvalidChar Nothing 0 c
    | otherwise -> Left ParseEmptySubtag
  Left (PopSubtagTooLong st, _) -> Left $ ParseSubtagTooLong st
  Right (st, s')
    | Just (c, _) <- uncC s' ->
      Left $ ParseInvalidChar (Just st) (subtagLength' st) c
    | otherwise -> Right st
  where
    unc s = do
      (c, s') <- uncC s
      c' <- toC c
      pure (c', s')

-- TODO: test this

-- | Uses 'parseSubtagWith' to parse a 'Subtag' from a 'Text' stream
parseSubtagText :: Text -> Either (ParseError Char) Subtag
parseSubtagText = parseSubtagWith T.uncons packChar

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
  | w <= 57 = Just $! SubtagChar w
  | w >= 97 = Just $! SubtagChar w
  | otherwise = Nothing

-- TODO: perhaps model after wrapChar?
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

-- | Convert an ASCII alphanumeric 'Char' to a 'SubtagChar',
-- converting it it to lower case if it is not already
packChar :: Char -> Maybe SubtagChar
packChar = onChar Nothing low high dig
  where
    low w = Just $ SubtagChar w
    high w = Just $ SubtagChar $ w + 32
    dig w = Just $ SubtagChar w

-- | Tests whether or not the 'Char' is a valid subtag character (an
-- ASCII alphanumeric character)
isSubtagChar :: Char -> Bool
isSubtagChar = onChar False true true true
  where
    true = const True

-- | Convert a 'SubtagChar' into a 'Subtag' of length one
singleton :: SubtagChar -> Subtag
singleton (SubtagChar c) = Subtag $ recordLen $ Bit.shiftL (fromIntegral c) 57
  where
    recordLen = (Bit..|. 1)
