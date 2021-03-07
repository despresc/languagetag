{-# LANGUAGE BangPatterns #-}

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
    wrapSubtag,
    unwrapSubtag,
    renderSubtag,
    renderSubtagBuilder,
    subtagLength,
    subtagLength',
    subtagHead,
    indexSubtag,

    -- * Subtags that might not be present
    MaybeSubtag,
    maybeSubtag,
    justSubtag,
    nullSubtag,

    -- * Subtag characters
    SubtagChar,
    packChar,
    unpackChar,
    packCharMangled,
    unwrapChar,

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

renderSubtag :: Subtag -> Text
renderSubtag = TL.toStrict . TB.toLazyText . renderSubtagBuilder

containsLetter :: Subtag -> Bool
containsLetter (Subtag n) = n `Bit.testBit` 4

containsOnlyLetters :: Subtag -> Bool
containsOnlyLetters = not . containsDigit

containsDigit :: Subtag -> Bool
containsDigit (Subtag n) = n `Bit.testBit` 5

containsOnlyDigits :: Subtag -> Bool
containsOnlyDigits = not . containsLetter

-- | Index a subtag.
indexSubtag :: Subtag -> Word8 -> Maybe SubtagChar
indexSubtag t idx
  | subtagLength t >= idx = Nothing
  | otherwise = Just $ unsafeIndexSubtag t idx

-- | Return the head of the 'Subtag'. Subtags are always non-empty, so
-- this function is total.
subtagHead :: Subtag -> SubtagChar
subtagHead = (`unsafeIndexSubtag` 0)

readSubtag :: Word64 -> [SubtagChar] -> Subtag
readSubtag len = Subtag . fst . List.foldl' go (len, 57)
  where
    go :: (Word64, Int) -> SubtagChar -> (Word64, Int)
    go (!acc, !idx) (SubtagChar !n) = (acc + Bit.shiftL (fromIntegral n) idx, idx - 7)

-- TODO: call this parseSubtagDetail? and toSubtag -> parseSubtag?

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
-- Maybe subtags
----------------------------------------------------------------

-- | Deconstruct a 'MaybeSubtag'
maybeSubtag :: a -> (Subtag -> a) -> MaybeSubtag -> a
maybeSubtag x f (MaybeSubtag (Subtag n))
  | n == 0 = x
  | otherwise = f $ Subtag n

-- | Convert a 'Subtag' to a 'MaybeSubtag' that is present
justSubtag :: Subtag -> MaybeSubtag
justSubtag = MaybeSubtag

-- | A 'MaybeSubtag' that is not present
nullSubtag :: MaybeSubtag
nullSubtag = MaybeSubtag (Subtag 0)

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

-- TODO: instead have a packCharLow (lower-cases) and packChar
-- (preserves case).
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
recordSeen Both (Subtag n) = Subtag $ n Bit..&. 48

-- False for letter, True for digit
reportChar :: Bool -> SeenChar -> SeenChar
reportChar True OnlyLetter = Both
reportChar False OnlyDigit = Both
reportChar _ s = s

toSeenChar :: Bool -> SeenChar
toSeenChar = toEnum . fromEnum
