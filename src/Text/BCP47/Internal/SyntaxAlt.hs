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

TODO: hashable, nfdata instances for subtag et al?

TODO HERE: Add pretty-printing to tagToText. Also consider changing
names of things.

TODO: test that the contents are actually accurate

-}

-- | A compact representation of a BCP47 subtag (a string of ASCII
-- letters and digits of length between one and eight). The 'Ord'
-- instance is identical to that of 'Text', in that for two subtags
-- @x@ and @y@, we have @x < y@ if and only if @'tagToText' x <
-- 'tagToText' y@.
--
-- These tags are always stored and printed entirely in lower case
-- when on their own; in the context of a full tag, they may be
-- printed in upper or title case depending on their position and
-- length.

-- The three lowest bits encode the length of the tag. The next two
-- bits encode whether or not the tag has letters or digits (in that
-- order). The highest chunks of 6 bits encode the actual characters
-- (first character the highest). (This leaves us with 11 bits left
-- over, in fact, not that this is useful to us at the moment).
--
-- TODO: add a test that toSubtag is actually an order homomorphism
newtype Subtag = Subtag {unSubtag :: Word64}
  deriving (Eq, Ord)

-- TODO: temporary show instance. replace with an actual one.
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

-- | this uses 'toSubtagLax', so it will silently mangle ill-formed
-- subtags
instance Show Subtag where
  showsPrec p t r = showsPrec p (tagToText t) r

instance Read Subtag where
  readsPrec p str = [(toSubtagLax $ T.pack x, y) | (x, y) <- readsPrec p str]

-- | this uses 'toSubtagLax', so it will silently mangle ill-formed
-- subtags
instance IsString Subtag where
  fromString = toSubtagLax . T.pack

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
{-# INLINE onChar #-}

data SeenChar
  = OnlyLetter
  | OnlyDigit
  | Both
  deriving (Enum)

-- False for letter, True for digit
reportChar :: Bool -> SeenChar -> SeenChar
reportChar True OnlyLetter = Both
reportChar False OnlyDigit = Both
reportChar _ s = s
{-# INLINE reportChar #-}

toSeenChar :: Bool -> SeenChar
toSeenChar = toEnum . fromEnum

-- | Pack an ASCII alphanumeric character into the first 6 bits of a
-- 'Word8', if it is valid. Digits are mapped to @[0..9]@ and the
-- lower and upper case letters are both mapped to @[36..61]@. (The
-- gap is there in case we want to represent upper case letters in
-- tags while retaining backward compatibility.)
--
-- We also report whether the character was a letter or digit.
packCharDetail :: Char -> Maybe (SubtagChar, Bool)
packCharDetail = onChar Nothing low high dig
  where
    low w = Just (SubtagChar $ w - 61, False)
    high w = Just (SubtagChar $ w - 29, False)
    dig w = Just (SubtagChar $ w - 48, True)
{-# INLINE packCharDetail #-}

-- | Like 'packChar', but replaces any invalid character with the
-- letter a.
packCharLax :: Char -> SubtagChar
packCharLax = fromMaybe (SubtagChar 36) . packChar
{-# INLINE packCharLax #-}

packChar :: Char -> Maybe SubtagChar
packChar = fmap fst . packCharDetail
{-# INLINE packChar #-}

-- | Unpack an ASCII alphanumeric character from a 'Word8'. If not
-- given a valid character, this may produce weird results.
unpackChar :: SubtagChar -> Char
unpackChar (SubtagChar w)
  | w < 10 = BI.w2c $ w + 48
  | otherwise = BI.w2c $ w + 61
{-# INLINE unpackChar #-}

readSubtag :: Word64 -> [SubtagChar] -> Subtag
readSubtag len = Subtag . fst . List.foldl' go (len, 58)
  where
    go :: (Word64, Int) -> SubtagChar -> (Word64, Int)
    go (!acc, !idx) (SubtagChar !n) = (acc + Bit.shiftL (fromIntegral n) idx, idx - 6)
{-# INLINE readSubtag #-}

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

-- will mangle the length and letter/digit details of a subtag!
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
toSubtagDetail :: Text -> Maybe (Subtag, SeenChar)
toSubtagDetail inp
  | Just (c, t) <- T.uncons inp,
    T.length inp <= 8 =
    wchars (fromIntegral $ T.length inp) c t
  | otherwise = Nothing
  where
    fixup !len (!b, !l, !sc)
      | b = Nothing
      | otherwise = Just (readSubtag len $ l [], sc)
    wchars !len !c !t = case packCharDetail c of
      Just (w, sc) -> fixup len $ T.foldl' go (False, (w :), toSeenChar sc) t
      Nothing -> Nothing
    go (!b, !l, !sc) c = case packCharDetail c of
      Just (w, sc') -> (b, l . (w :), reportChar sc' sc)
      Nothing -> (True, l, sc)
{-# INLINE toSubtagDetail #-}

popSubtagDetail :: Char -> Text -> Maybe (Subtag, SeenChar, Text)
popSubtagDetail initchar inp = case packCharDetail initchar of
  Just (c, sc) -> go 1 (c :) (toSeenChar sc) inp
  Nothing -> Nothing
  where
    go idx !l !sc !t
      | idx == 8 = Just (readSubtag idx (l []), sc, t)
      | otherwise = case T.uncons t of
        Just (c, t')
          | c == '-' -> Just (readSubtag idx (l []), sc, t)
          | otherwise -> case packCharDetail c of
            Just (!w, !sc') -> go (idx + 1) (l . (w :)) (reportChar sc' sc) t'
            Nothing -> Nothing
        Nothing -> Just (readSubtag idx (l []), sc, t)
{-# INLINE popSubtagDetail #-}

-- | Like popSubtagDetail, but when we don't care about the 'SeenChar'.
popSubtag :: Char -> Text -> Maybe (Subtag, Text)
popSubtag initchar inp = case packCharDetail initchar of
  Just (c, _) -> go 1 (c :) inp
  Nothing -> Nothing
  where
    go !idx !l !t
      | idx == 8 = Just (readSubtag idx (l []), t)
      | otherwise = case T.uncons t of
        Just (c, t')
          | c == '-' -> Just (readSubtag idx (l []), t)
          | otherwise -> case packChar c of
            Just !w -> go (idx + 1) (l . (w :)) t'
            Nothing -> Nothing
        Nothing -> Just (readSubtag idx (l []), t)
{-# INLINE popSubtag #-}

toSubtag :: Text -> Maybe Subtag
toSubtag = fmap fst . toSubtagDetail
{-# INLINE toSubtag #-}

-- | Read a tag from the given text value, truncating it or replacing
-- it with the singleton "a" if necessary, and replacing any
-- characters other than ASCII digits or letters with @'a'@.
toSubtagLax :: Text -> Subtag
toSubtagLax t = readSubtag (fromIntegral len) (wchars [])
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
  | PrivateTag !(NonEmpty Subtag)
  | RegularGrandfathered !RegularGrandfathered
  | IrregularGrandfathered !IrregularGrandfathered
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

data RegularGrandfathered
  = -- | @art-lojban@
    Artlojban
  | -- | @cel-gaulish@
    Celgaulish
  | -- | @no-bok@
    Nobok
  | -- | @no-nyn@
    Nonyn
  | -- | @zh-guoyu@
    Zhguoyu
  | -- | @zh-hakka@
    Zhhakka
  | -- | @zh-min@
    Zhmin
  | -- | @zh-min-nan@
    Zhminnan
  | -- | @zh-xiang@
    Zhxiang
  deriving (Eq, Ord, Show, Read)

data IrregularGrandfathered
  = -- | @en-GB-oed@
    EnGBoed
  | -- | @i-ami@
    Iami
  | -- | @i-bnn@
    Ibnn
  | -- | @i-default@
    Idefault
  | -- | @i-enochian@
    Ienochian
  | -- | @i-hak@
    Ihak
  | -- | @i-klingon@
    Iklingon
  | -- | @i-lux@
    Ilux
  | -- | @i-mingo@
    Imingo
  | -- | @i-navajo@
    Inavajo
  | -- | @i-pwn@
    Ipwn
  | -- | @i-tao@
    Itao
  | -- | @i-tay@
    Itay
  | -- | @i-tsu@
    Itsu
  | -- | @sgn-BE-FR@
    SgnBEFR
  | -- | @sgn-BE-NL@
    SgnBENL
  | -- | @sgn-CH-DE@
    SgnCHDE
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------
-- Various tag constants
----------------------------------------------------------------

subtagChara :: SubtagChar
subtagChara = SubtagChar 36
{-# INLINE subtagChara #-}

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 59
{-# INLINE subtagCharx #-}

----------------------------------------------------------------
-- Internal convenience class
----------------------------------------------------------------

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

instance Finishing LanguageTag where
  finish = id
  {-# INLINE finish #-}

----------------------------------------------------------------
-- File auto-generated below this line. Do not edit by hand!
----------------------------------------------------------------
