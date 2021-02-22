{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: remove this file or integrate it with the other
-- is alternate internal syntax stuff.

module Text.BCP47.Internal.SyntaxAlt where

import qualified Data.Bits as Bit
import qualified Data.ByteString.Internal as BI
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64, Word8)

{-

TODO: may want to define some constants for some of these magic
numbers (the 15 and 63 selectors, the indices starting at 58, the
shifts)

-}

-- | A compact representation of a BCP47 subtag (a string of ASCII
-- letters and digits of length between one and eight). The 'Ord'
-- instance is identical to that of 'Text', in that for two subtags
-- @x@ and @y@, we have @x < y@ if and only if @'tagToText' x <
-- 'tagToText' y@.

-- The three lowest bits encode the length of the tag, and the highest
-- chunks of 6 bits encode the actual characters (first character the
-- highest). (This leaves us with 13 bits left over, in fact, not that
-- this is useful to us at the moment).
--
-- TODO: read instance
--
-- TODO: may want to store whether or not there are letters or digits
-- in the tag in the lower bits as well (requires another two bits).
--
-- TODO: add a test that toSubtag is actually an order homomorphism
newtype Subtag = Subtag {unSubtag :: Word64}
  deriving (Eq, Ord)

-- TODO: temporary show instance
newtype MaybeSubtag = MaybeSubtag Subtag
  deriving (Show)

-- relies on the fact that valid 'Subtag' values will never be zero
fromMaybeSubtag :: MaybeSubtag -> Maybe Subtag
fromMaybeSubtag (MaybeSubtag (Subtag n))
  | n == 0 = Nothing
  | otherwise = Just $ Subtag n
{-# INLINE fromMaybeSubtag #-}

justSubtag :: Subtag -> MaybeSubtag
justSubtag = MaybeSubtag
{-# INLINE justSubtag #-}

nullSubtag :: MaybeSubtag
nullSubtag = MaybeSubtag (Subtag 0)
{-# INLINE nullSubtag #-}

-- TODO: temporary show instance
newtype SubtagChar = SubtagChar {unSubtagChar :: Word8}
  deriving (Eq, Show)

subtagChara :: SubtagChar
subtagChara = SubtagChar 36
{-# INLINE subtagChara #-}

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 59
{-# INLINE subtagCharx #-}

-- | this uses 'toSubtagLax', so it will silently mangle ill-formed
-- subtags
instance Show Subtag where
    showsPrec p t r = showsPrec p (tagToText t) r

instance Read Subtag where
    readsPrec p str = [(toSubtagLax $ T.pack x,y) | (x,y) <- readsPrec p str]

-- | this uses 'toSubtagLax', so it will silently mangle ill-formed
-- subtags
instance IsString Subtag where
  fromString = toSubtagLax . T.pack

onChar ::
  Char ->
  r ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  (Word8 -> r) ->
  r
onChar c bad f g h
  | c > 'z' = bad
  | c >= 'a' = f $! BI.c2w c
  | c > 'Z' = bad
  | c >= 'A' = g $! BI.c2w c
  | c <= '9' && c >= '0' = h $! BI.c2w c
  | otherwise = bad
{-# INLINE onChar #-}

-- | Pack an ASCII alphanumeric character into the first 6 bits of a
-- 'Word8', if it is valid. Digits have 48 subtracted from their
-- 'fromEnum' values, upper case letters have 55 subtracted, and lower
-- case letters have 61 subtracted. Also return whether it was a
-- letter or digit.

-- TODO: rewrite with onChar
packCharDetail :: Char -> Maybe (SubtagChar, Bool, Bool)
packCharDetail c
  | c > 'z' = Nothing
  | c >= 'a' = Just (SubtagChar $ BI.c2w c - 61, True, False)
  | c > 'Z' = Nothing
  | c >= 'A' = Just (SubtagChar $ BI.c2w c - 55, True, False)
  | c > '9' = Nothing
  | '0' <= c && c <= '9' = Just (SubtagChar $ BI.c2w c - 48, False, True)
  | otherwise = Nothing
{-# INLINE packCharDetail #-}

packChar :: Char -> Maybe SubtagChar
packChar = fmap (\(x, _, _) -> x) . packCharDetail
{-# INLINE packChar #-}

-- | Like 'packChar', but also shifts the case of upper case letters.

-- TODO: rewrite with onChar
packCharLow :: Char -> Maybe SubtagChar
packCharLow c
  | c > 'z' = Nothing
  | c >= 'a' = Just $! SubtagChar $ BI.c2w c - 61
  | c > 'Z' = Nothing
  | c >= 'A' = Just $! SubtagChar $ BI.c2w c - 29
  | c > '9' = Nothing
  | '0' <= c && c <= '9' = Just $! SubtagChar $ BI.c2w c - 48
  | otherwise = Nothing
{-# INLINE packCharLow #-}

-- | Like 'packChar', but maps any invalid character to the value @36@
-- (what @a@ is mapped to).
packCharLax :: Char -> SubtagChar
packCharLax = fromMaybe (SubtagChar 36) . packChar
{-# INLINE packCharLax #-}

-- | Unpack an ASCII alphanumeric character from a 'Word8'. If not
-- given a valid character, this may produce weird results.
unpackChar :: SubtagChar -> Char
unpackChar (SubtagChar w)
  | w < 10 = BI.w2c $ w + 48
  | w < 36 = BI.w2c $ w + 55
  | otherwise = BI.w2c $ w + 61
{-# INLINE unpackChar #-}

-- Unnormalizing version first, I suppose. Also takes the length of
-- the tag. Obviously needs to be fed valid characters.
readTag :: Word64 -> [SubtagChar] -> Subtag
readTag len = Subtag . fst . List.foldl' go (len, 58)
  where
    go :: (Word64, Int) -> SubtagChar -> (Word64, Int)
    go (!acc, !idx) (SubtagChar !n) = (acc + Bit.shiftL (fromIntegral n) idx, idx - 6)
{-# INLINE readTag #-}

tagLength :: Subtag -> Word8
tagLength = fromIntegral . (Bit..&.) sel . unSubtag
  where
    sel = 15
{-# INLINE tagLength #-}

-- map over the constituent letters of a subtag. the given function
-- must, of course, preserve the encoding.
imapSubtag :: (Word8 -> SubtagChar -> SubtagChar) -> Subtag -> Subtag
imapSubtag f n = Subtag n'
  where
    len = tagLength n
    n' = go (fromIntegral len) 0 n
    go !acc !idx !w
      | idx == len = acc
      | otherwise =
        let (c, w') = popChar w
         in go
              ( acc
                  + Bit.shiftL
                    (fromIntegral $ unSubtagChar $ f idx c)
                    -- TODO: I think this is right?
                    (fromIntegral $ 58 - 6 * idx)
              )
              (idx + 1)
              w'
{-# INLINE imapSubtag #-}

-- will mangle the length of a subtag!
popChar :: Subtag -> (SubtagChar, Subtag)
popChar (Subtag n) = (SubtagChar $ fromIntegral $ Bit.shiftR n 58, Subtag $ Bit.shiftL n 6)
{-# INLINE popChar #-}

-- performs no bounds checking, though (unsafeIndexSubtag 0) will
-- always be valid.
unsafeIndexSubtag :: Subtag -> Word8 -> SubtagChar
unsafeIndexSubtag (Subtag n) idx =
  SubtagChar $
    fromIntegral $
      Bit.shiftR n (58 - 6 * fromIntegral idx) Bit..&. sel
  where
    sel = 63

subtagHead :: Subtag -> SubtagChar
subtagHead = (`unsafeIndexSubtag` 0)
{-# INLINE subtagHead #-}

writeTag :: Subtag -> [SubtagChar]
writeTag inp = List.unfoldr go (0, inp)
  where
    len = tagLength inp
    go (idx, n)
      | idx == len = Nothing
      | otherwise =
        let (!w, !n') = popChar n
         in Just (w, (idx + 1, n'))
{-# INLINE writeTag #-}

-- Also returns whether or not we saw a letter or a digit,
-- respectively. Will probably want a pop version.
toSubtagDetail :: Text -> Maybe (Subtag, Bool, Bool)
toSubtagDetail t
  | len == 0 || len > 8 = Nothing
  | otherwise = (\(st, sl, sd) -> (readTag (fromIntegral len) st, sl, sd)) <$> wchars
  where
    len = T.length t
    fixup (b, l, sl, sd)
      | b = Nothing
      | otherwise = Just (l [], sl, sd)
    wchars = fixup $ T.foldl' go (False, id, False, False) t
    go (b, l, sl, sd) c = case packCharDetail c of
      Just (w, sl', sd') -> (b, l . (w :), sl' || sl, sd' || sd)
      Nothing -> (True, l, sl, sd)
{-# INLINE toSubtagDetail #-}

-- Does not normalize the tag, yet.

-- TODO: Could even return the length and what the first character was
popSubtagDetail :: Char -> Text -> Maybe (Subtag, Bool, Bool, Text)
popSubtagDetail initchar inp = case packCharDetail initchar of
  Just (c, sl, sd) -> go 1 (c :) sl sd inp
  Nothing -> Nothing
  where
    go idx l sl sd t
      | idx == 8 = Just (readTag idx (l []), sl, sd, t)
      | otherwise = case T.uncons t of
        Just (c, t')
          | c == '-' -> Just (readTag idx (l []), sl, sd, t)
          | otherwise -> case packCharDetail c of
            Just (w, sl', sd') -> go (idx + 1) (l . (w :)) (sl || sl') (sd || sd') t'
            Nothing -> Nothing
        Nothing -> Just (readTag idx (l []), sl, sd, t)
{-# INLINE popSubtagDetail #-}

toSubtag :: Text -> Maybe Subtag
toSubtag = fmap (\(x, _, _) -> x) . toSubtagDetail
{-# INLINE toSubtag #-}

-- | Read a tag from the given text value, truncating it or replacing
-- it with the singleton "a" if necessary, and replacing any
-- characters other than ASCII digits or letters with @'a'@.
toSubtagLax :: Text -> Subtag
toSubtagLax t = readTag (fromIntegral len) (wchars [])
  where
    tlen = T.length t
    (t', len)
      | tlen == 0 = (T.singleton 'a', 1)
      | otherwise = (T.take 8 t, min 8 tlen)
    wchars = T.foldl' go id t'
    go l c = l . (packCharLax c :)
{-# INLINE toSubtagLax #-}

tagToText :: Subtag -> Text
tagToText w = T.unfoldrN (fromIntegral len) go (w, 0)
  where
    len = tagLength w
    go (n, idx)
      | idx == len = Nothing
      | otherwise =
        let (c, n') = popChar n
         in Just (unpackChar c, (n', idx + 1))
{-# INLINE tagToText #-}

-- | A well-formed BCP47 language tag
data LanguageTag
  = NormalTag {-# UNPACK #-} !Normal
  | PrivateTag {-# UNPACK #-} !(NonEmpty Subtag)
  deriving (Show)

-- TODO: temporary show instance
data Normal = Normal
  { primlang :: {-# UNPACK #-} !Subtag,
    extlang1 :: {-# UNPACK #-} !MaybeSubtag,
    extlang2 :: {-# UNPACK #-} !MaybeSubtag,
    extlang3 :: {-# UNPACK #-} !MaybeSubtag,
    script :: {-# UNPACK #-} !MaybeSubtag,
    region :: {-# UNPACK #-} !MaybeSubtag,
    variants :: ![Subtag],
    extensions :: ![Extension],
    privateUse :: ![Subtag]
  }
  deriving (Show)

data Extension = Extension
  { extSingleton :: {-# UNPACK #-} !SubtagChar,
    extTags :: {-# UNPACK #-} !(NonEmpty Subtag)
  }
  deriving (Show)

-- simple internal convenience class
class Finishing a where
  finish :: a -> LanguageTag

instance
  Finishing
    ( MaybeSubtag ->
      MaybeSubtag ->
      MaybeSubtag ->
      MaybeSubtag ->
      MaybeSubtag ->
      [Subtag] ->
      [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con nullSubtag
  {-# INLINE finish #-}

instance
  Finishing
    ( MaybeSubtag ->
      MaybeSubtag ->
      MaybeSubtag ->
      MaybeSubtag ->
      [Subtag] ->
      [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con nullSubtag
  {-# INLINE finish #-}

instance
  Finishing
    ( MaybeSubtag ->
      MaybeSubtag ->
      MaybeSubtag ->
      [Subtag] ->
      [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con nullSubtag
  {-# INLINE finish #-}

instance
  Finishing
    ( MaybeSubtag ->
      MaybeSubtag ->
      [Subtag] ->
      [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con nullSubtag
  {-# INLINE finish #-}

instance
  Finishing
    ( MaybeSubtag ->
      [Subtag] ->
      [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con nullSubtag
  {-# INLINE finish #-}

instance
  Finishing
    ( [Subtag] ->
      [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con []
  {-# INLINE finish #-}

instance
  Finishing
    ( [Extension] ->
      [Subtag] ->
      LanguageTag
    )
  where
  finish con = finish $ con []
  {-# INLINE finish #-}

instance Finishing ([Subtag] -> LanguageTag) where
  finish con = finish $ con []
  {-# INLINE finish #-}

instance Finishing (LanguageTag) where
  finish = id
  {-# INLINE finish #-}
