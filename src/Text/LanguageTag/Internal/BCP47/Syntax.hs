{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.Syntax where

import qualified Data.Bits as Bit
import qualified Data.ByteString.Internal as BI
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word64, Word8)

{-

TODO: may want to define some constants for some of these magic
numbers (the 15 and 63 selectors, the indices starting at 58, the
shifts)

TODO: hashable, nfdata instances for subtag et al?

TODO HERE: Add pretty-printing to tagToText. Also consider changing
names of things.

TODO: test that the contents are actually accurate

TODO: docs on various function properties (e.g. fmap renderLanguageTag
. parseBCP47 = Right up to case).

-}

-- | A compact representation of a BCP47 subtag (a string of ASCII
-- letters and digits of length between one and eight). The 'Ord'
-- instance is identical to that of 'Text', in that for two subtags
-- @x@ and @y@, we have @x < y@ if and only if @'renderLanguageTag' x
-- < 'renderLanguageTag' y@.
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

-- TODO: check that this is half the inverse of parse
instance Show Subtag where
  showsPrec p ps r = showsPrec p (renderSubtagLow ps) r

instance Show MaybeSubtag where
  showsPrec p (MaybeSubtag t) r
    | unSubtag t == 0 = showsPrec p ("" :: String) r
    | otherwise = showsPrec p (renderSubtagLow t) r

-- | Unwrap the internal representation of a 'Subtag'
unwrapSubtag :: Subtag -> Word64
unwrapSubtag = unSubtag
{-# INLINE unwrapSubtag #-}

-- | Convert the internal representation of a 'Subtag' back to a
-- 'Subtag'
wrapSubtag :: Word64 -> Maybe Subtag
wrapSubtag = undefined
{-# INLINE wrapSubtag #-}

-- | Convert the internal representation of a 'Subtag' back to a
-- 'Subtag' without checking the validity of the input. Invalid
-- subtags constructed with this function may violate invariants that
-- other functions in this library depend on, and so cause
-- unpredictable behaviour.
unsafeWrapSubtag :: Word64 -> Subtag
unsafeWrapSubtag = Subtag
{-# INLINE unsafeWrapSubtag #-}

-- | A subtag that may not be present. Equivalent to @Maybe
-- Subtag@. Use 'justSubtag' and 'nullSubtag' to construct these, and
-- 'fromMaybeSubtag' to eliminate them.
newtype MaybeSubtag = MaybeSubtag Subtag
  deriving (Eq, Ord)

-- | Deconstruct a 'MaybeSubtag'
fromMaybeSubtag :: MaybeSubtag -> Maybe Subtag
fromMaybeSubtag (MaybeSubtag (Subtag n))
  | n == 0 = Nothing
  | otherwise = Just $ Subtag n
{-# INLINE fromMaybeSubtag #-}

-- | Convert a 'Subtag' to a 'MaybeSubtag' that is present
justSubtag :: Subtag -> MaybeSubtag
justSubtag = MaybeSubtag
{-# INLINE justSubtag #-}

-- | A 'MaybeSubtag' that is not present
nullSubtag :: MaybeSubtag
nullSubtag = MaybeSubtag (Subtag 0)
{-# INLINE nullSubtag #-}

-- | The encoding of a valid subtag character (an ASCII alphabetic
-- character or digit)
newtype SubtagChar = SubtagChar {unSubtagChar :: Word8}
  deriving (Eq, Ord, Show)

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

-- | Pack a normal 'Char' into a 'SubtagChar'.
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

-- | Convert a packed letter to an unpacked upper case letter.
unpackUpperLetter :: SubtagChar -> Char
unpackUpperLetter (SubtagChar w) =
  BI.w2c $ w + 29
{-# INLINE unpackUpperLetter #-}

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

-- | Pop a character from the head of a 'Subtag'. Note that this will
-- mangle the stored length of a subtag!
unsafePopChar :: Subtag -> (SubtagChar, Subtag)
unsafePopChar (Subtag n) = (SubtagChar $ fromIntegral $ Bit.shiftR n 58, Subtag $ Bit.shiftL n 6)
{-# INLINE unsafePopChar #-}

-- | Index a subtag without bounds checking. Note that
-- @'unsafeIndexSubtag' 0@ is equivalent to 'subtagHead'.
unsafeIndexSubtag :: Subtag -> Word8 -> SubtagChar
unsafeIndexSubtag (Subtag n) idx =
  SubtagChar $
    fromIntegral $
      Bit.shiftR n (58 - 6 * fromIntegral idx) Bit..&. sel
  where
    sel = 63
{-# INLINE unsafeIndexSubtag #-}

indexSubtag :: Subtag -> Word8 -> Maybe SubtagChar
indexSubtag t idx
  | tagLength t >= idx = Nothing
  | otherwise = Just $ unsafeIndexSubtag t idx
{-# INLINE indexSubtag #-}

-- | Return the head of the 'Subtag'. Subtags are always non-empty, so
-- this function is total.
subtagHead :: Subtag -> SubtagChar
subtagHead = (`unsafeIndexSubtag` 0)
{-# INLINE subtagHead #-}

-- | Unpack a 'Subtag' into its constituent 'SubtagChar' elements.
unpackSubtag :: Subtag -> [SubtagChar]
unpackSubtag inp = List.unfoldr go (0, inp)
  where
    len = tagLength inp
    go (idx, n)
      | idx == len = Nothing
      | otherwise =
        let (!w, !n') = unsafePopChar n
         in Just (w, (idx + 1, n'))
{-# INLINE unpackSubtag #-}

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
-- characters other than ASCII digits or letters with @\'a\'@.
toMangledSubtag :: Text -> Subtag
toMangledSubtag t = readSubtag (fromIntegral len) (wchars [])
  where
    tlen = T.length t
    (t', len)
      | tlen == 0 = (T.singleton 'a', 1)
      | otherwise = (T.take 8 t, min 8 tlen)
    wchars = T.foldl' go id t'
    go l c = l . (packCharLax c :)
{-# INLINE toMangledSubtag #-}

renderRegion :: MaybeSubtag -> TB.Builder
renderRegion (MaybeSubtag s)
  | unSubtag s == 0 = ""
  | tagLength s == 2 = TB.fromString $ '-' : List.unfoldr capgo (s, 0)
  | otherwise = renderSubtagLow s
  where
    capgo (n, idx)
      | idx == (2 :: Word8) = Nothing
      | otherwise =
        let (c, n') = unsafePopChar n
         in Just (unpackUpperLetter c, (n', idx + 1))
{-# INLINE renderRegion #-}

-- TODO: should probably just do this more normally.
renderScript :: MaybeSubtag -> TB.Builder
renderScript (MaybeSubtag s)
  | unSubtag s == 0 = ""
  | otherwise = TB.fromString $ '-' : List.unfoldr go (s, 0 :: Word8)
  where
    go (n, idx)
      | idx == 4 = Nothing
      | idx == 0 =
        let (c, n') = unsafePopChar n
         in Just (unpackUpperLetter c, (n', idx + 1))
      | otherwise =
        let (c, n') = unsafePopChar n
         in Just (unpackChar c, (n', idx + 1))
{-# INLINE renderScript #-}

renderSubtagLow :: Subtag -> TB.Builder
renderSubtagLow w = TB.fromString $ List.unfoldr go (w, 0)
  where
    len = tagLength w
    go (n, idx)
      | idx == len = Nothing
      | otherwise =
        let (c, n') = unsafePopChar n
         in Just (unpackChar c, (n', idx + 1))
{-# INLINE renderSubtagLow #-}

-- | A syntactically well-formed BCP47 tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the full
-- details. Note that the 'Ord' instance does not correspond to that
-- of 'Text', in the sense that there are language tags @x@ and @y@
-- such that @x < y@ and yet @'renderLanguageTag' x >
-- 'renderLanguageTag' y@.
data LanguageTag
  = NormalTag {-# UNPACK #-} !Normal
  | PrivateTag !(NonEmpty Subtag)
  | RegularGrandfathered !RegularGrandfathered
  | IrregularGrandfathered !IrregularGrandfathered
  deriving (Eq, Ord)

-- TODO: test that this is half the inverse of parse
instance Show LanguageTag where
  showsPrec p ps r = showsPrec p (renderLanguageTagBuilder ps) r

-- | Render a language tag to a lazy text builder
renderLanguageTagBuilder :: LanguageTag -> TB.Builder
renderLanguageTagBuilder (NormalTag (Normal pr e1 e2 e3 sc reg vars exts pu)) =
  pr' <> e1' <> e2' <> e3' <> sc' <> reg'
    <> vars'
    <> exts'
    <> pu'
  where
    renderLowPref s = "-" <> renderSubtagLow s
    pr' = renderSubtagLow pr
    e1' = maybe "" renderLowPref $ fromMaybeSubtag e1
    e2' = maybe "" renderLowPref $ fromMaybeSubtag e2
    e3' = maybe "" renderLowPref $ fromMaybeSubtag e3
    sc' = renderScript sc
    reg' = renderRegion reg
    vars' = foldMap renderLowPref vars
    exts' = foldMap renderExtension exts
    renderExtension (Extension s t) = "-" <> TB.singleton (unpackChar s) <> foldMap renderLowPref t
    pu'
      | null pu = ""
      | otherwise = "-" <> TB.singleton 'x' <> foldMap renderLowPref pu
renderLanguageTagBuilder (PrivateTag l) = TB.singleton 'x' <> foldMap renderSubtagLow l
renderLanguageTagBuilder (RegularGrandfathered t) = case t of
  Artlojban -> "art-lojban"
  Celgaulish -> "cel-gaulish"
  Nobok -> "no-bok"
  Nonyn -> "no-nyn"
  Zhguoyu -> "zh-guoyu"
  Zhhakka -> "zh-hakka"
  Zhmin -> "zh-min"
  Zhminnan -> "zh-min-nan"
  Zhxiang -> "zh-xiang"
renderLanguageTagBuilder (IrregularGrandfathered t) = case t of
  EnGBoed -> "en-GB-oed"
  Iami -> "i-ami"
  Ibnn -> "i-bnn"
  Idefault -> "i-default"
  Ienochian -> "i-enochian"
  Ihak -> "i-hak"
  Iklingon -> "i-klingon"
  Ilux -> "i-lux"
  Imingo -> "i-mingo"
  Inavajo -> "i-navajo"
  Ipwn -> "i-pwn"
  Itao -> "i-tao"
  Itay -> "i-tay"
  Itsu -> "i-tsu"
  SgnBEFR -> "sgn-BE-FR"
  SgnBENL -> "sgn-BE-NL"
  SgnCHDE -> "sgn-CH-DE"
{-# INLINE renderLanguageTagBuilder #-}

-- | Render a language tag to a strict text string
renderLanguageTag :: LanguageTag -> Text
renderLanguageTag = TL.toStrict . TB.toLazyText . renderLanguageTagBuilder
{-# INLINE renderLanguageTag #-}

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
  deriving (Eq, Ord)

data Extension = Extension
  { extSingleton :: {-# UNPACK #-} !SubtagChar,
    extTags :: {-# UNPACK #-} !(NonEmpty Subtag)
  }
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
-- Value construction
----------------------------------------------------------------

-- $valueconstruction

-- | Construct a normal tag from its components. This function uses
-- 'toMangledSubtag' to construct the subtags from the given
-- components, so the warnings that accompany that function also apply
-- here. This function will also not check if the input is a
-- grandfathered language tag, and will not check if the subtags are
-- appropriate for their sections. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the exact
-- grammar. A summary of the rules to follow to ensure that the input
-- is well-formed, with /letter/ meaning ASCII alphabetic character
-- and /digit/ meaning ASCII numeric character:
--
-- * Primary language: between two and eight letters.
--
-- * Extended language: exactly three letters. If the primary language
--   is four letters or longer then the extended language must be
--   empty.
--
-- * Script: exactly four letters.
--
-- * Region: either exactly two letters or exactly three digits.
--
-- * Variant: between four and eight letters or digits. If the variant
--   has length four then it must begin with a digit.
--
-- * Extension sections: the character must be a digit or a letter
--   other than @x@ or @X@ and the 'Text' values must be between two
--   and eight digits or letters long.
--
-- * Private use subtags: between one and eight digits or letters.
--
-- All types of subtags but the primary language subtag are optional;
-- the empty text value @""@ should be used for subtags that are not
-- present. All 'Text' values in the lists should be non-empty. Also
-- note that a 'LanguageTag' is case-insensitive, so the subtag @en@
-- and the subtag @EN@ will result in the same value.
--
-- Examples of well-formed normal tags:
--
-- >>> unsafeNormalTag "en" "" "" "US" [] [] []
-- "en-US"
-- >>> unsafeNormalTag "cmn" "" "" "" [] [] []
-- "cmn"
-- >>> unsafeNormalTag "zh" "" "Hant" "HK" [] [] []
-- "zh-Hant-HK"
-- >>> unsafeNormalTag "es" "" "" "419" [] [] []
-- "es-419"
--
-- And a tag with all the parts labelled:
--
-- @
-- "fr-frm-Armi-AU-1606nict-a-strange-x-tag"
-- -- primary language  "fr"
-- -- extended language "frm"
-- -- script            \"Armi\"
-- -- region            \"AU\"
-- -- variants          ["1606nict"]
-- -- extensions        [(\'a\', "strange" :| [])]
-- -- private use       ["tag"]
-- @
--
-- (which mean something like: the Australian dialect of the Middle
-- French that is roughly exemplified by Jean Nicot's 1606 dictionary,
-- written in the Imperial Aramaic script, also including certain
-- extensions and private use subtags).
unsafeNormalTag ::
  -- | primary language
  Text ->
  -- | extended language
  Text ->
  -- | script
  Text ->
  -- | region
  Text ->
  -- | variant subtags
  [Text] ->
  -- | extension sections
  [(Char, NonEmpty Text)] ->
  -- | private use subtags
  [Text] ->
  LanguageTag
unsafeNormalTag l me = unsafeFullNormalTag l me "" ""
{-# INLINE unsafeNormalTag #-}

-- | Construct a full normal tag from its components. You probably
-- want 'unsafeNormalTag' instead of this function, since the third
-- extended language will always absent in valid tags, and the only
-- valid tag with a second extended language is the regular
-- grandfathered (and deprecated) tag @zh-min-nan@, which should be
-- constructed with 'zhMinNan'. The warnings for 'unsafeNormalTag'
-- also apply to this function.
unsafeFullNormalTag ::
  -- | primary language
  Text ->
  -- | extended language
  Text ->
  -- | a second extended language
  Text ->
  -- | a third extended language
  Text ->
  -- | script
  Text ->
  -- | region
  Text ->
  -- | variant subtags
  [Text] ->
  -- | extension sections
  [(Char, NonEmpty Text)] ->
  -- | private use subtags
  [Text] ->
  LanguageTag
unsafeFullNormalTag l me me2 me3 ms mr vs es pus =
  NormalTag $
    Normal
      { primlang = toMangledSubtag l,
        extlang1 = mmangled me,
        extlang2 = mmangled me2,
        extlang3 = mmangled me3,
        script = mmangled ms,
        region = mmangled mr,
        variants = strictMap toMangledSubtag vs,
        extensions = strictMap toExtension es,
        privateUse = strictMap toMangledSubtag pus
      }
  where
    toExtension (c, ext) = Extension (packCharLax c) (strictMapNE toMangledSubtag ext)
    mmangled t
      | T.null t = nullSubtag
      | otherwise = justSubtag $ toMangledSubtag t
{-# INLINE unsafeFullNormalTag #-}

-- | A private use tag starts with @x-@, which is followed by one or
-- more private use subtags, each of which is between one and eight
-- digits or letters long. This function constructs such a tag given
-- those private use subtags. This function uses 'toMangledSubtag',
-- and so the warnings for that function apply here as well.
unsafePrivateTag :: NonEmpty Text -> LanguageTag
unsafePrivateTag = PrivateTag . strictMapNE toMangledSubtag
{-# INLINE unsafePrivateTag #-}

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

strictMap :: (a -> b) -> [a] -> [b]
strictMap f = ($ []) . List.foldl' go id
  where
    go l a = l . strictCons (f a)

strictMapNE :: (a -> b) -> NonEmpty a -> NonEmpty b
strictMapNE f (x NE.:| xs) = strictNE (f x) $ strictMap f xs

strictNE :: a -> [a] -> NE.NonEmpty a
strictNE !x !y = x NE.:| y

strictCons :: a -> [a] -> [a]
strictCons !x !y = x : y

----------------------------------------------------------------
-- File auto-generated below this line. Do not edit by hand!
----------------------------------------------------------------

-- TODO: actually generate this (or the documentation, anyway)
-- automatically

-- | Tag @en-GB-oed@. English, Oxford English Dictionary
-- spelling. Deprecated. Preferred value: @en-GB-oxendict@.
enGbOed :: LanguageTag
enGbOed = IrregularGrandfathered $ EnGBoed

-- | Tag @i-ami@. Amis. Deprecated. Preferred value: @ami@.
iAmi :: LanguageTag
iAmi = IrregularGrandfathered $ Iami

-- | Tag @-ibnn@. Bunun. Deprecated. Preferred value: @bnn@.
iBnn :: LanguageTag
iBnn = IrregularGrandfathered $ Ibnn

-- | Tag @i-default@. Default Language.
iDefault :: LanguageTag
iDefault = IrregularGrandfathered $ Idefault

-- | Tag @i-enochian@. Enochian. Deprecated.
iEnochian :: LanguageTag
iEnochian = IrregularGrandfathered $ Ienochian

-- | Tag @i-hak@. Hakka. Deprecated. Preferred value: @hak@.
iHak :: LanguageTag
iHak = IrregularGrandfathered $ Ihak

-- | Tag @i-klingon@. Klingon. Deprecated. Preferred value: @tlh@.
iKlingon :: LanguageTag
iKlingon = IrregularGrandfathered $ Iklingon

-- | Tag @i-lux@. Luxembourgish. Deprecated. Preferred value: @lb@.
iLux :: LanguageTag
iLux = IrregularGrandfathered $ Ilux

-- | Tag @i-mingo@. Mingo.
iMingo :: LanguageTag
iMingo = IrregularGrandfathered $ Imingo

-- | Tag @i-navajo@. Navajo. Deprecated. Preferred value: @nv@.
iNavajo :: LanguageTag
iNavajo = IrregularGrandfathered $ Inavajo

-- | Tag @i-pwn@. Paiwan. Deprecated. Preferred value: @pwn@.
iPwn :: LanguageTag
iPwn = IrregularGrandfathered $ Ipwn

-- | Tag @i-tao@. Tao. Deprecated. Preferred value: @i-tao@.
iTao :: LanguageTag
iTao = IrregularGrandfathered $ Itao

-- | Tag @i-tay@. Tayal. Deprecated. Preferred value: @i-tay@.
iTay :: LanguageTag
iTay = IrregularGrandfathered $ Itay

-- | Tag @i-tsu@. Tsou. Deprecated. Preferred value: @i-tsu@.
iTsu :: LanguageTag
iTsu = IrregularGrandfathered $ Itsu

-- | Tag @sgn-BE-FR@. Belgian-French Sign
-- Language. Deprecated. Preferred value: @sfb@.
sgnBeFr :: LanguageTag
sgnBeFr = IrregularGrandfathered $ SgnBEFR

-- | Tag @sgn-BE-NL@. Belgian-Flemish Sign
-- Language. Deprecated. Preferred value: @vgt@.
sgnBeNl :: LanguageTag
sgnBeNl = IrregularGrandfathered $ SgnBENL

-- | Tag @sgn-CH-DE@. Swiss-German Sign
-- Language. Deprecated. Preferred value: @sgg@.
sgnChDe :: LanguageTag
sgnChDe = IrregularGrandfathered $ SgnCHDE

-- | Tag @art-lojban@. Lojban. Deprecated. Preferred value: @jbo@.
artLojban :: LanguageTag
artLojban = RegularGrandfathered $ Artlojban

-- | Tag @cel-gaulish@. Gaulish. Deprecated. See @xcg@ (Cisalpine
-- Gaulish), @xga@ (Galatian), @xtg@ (Transalpine Gaulish).
celGaulish :: LanguageTag
celGaulish = RegularGrandfathered $ Celgaulish

-- | Tag @no-bok@. Norwegian Bokmal. Deprecated. Preferred value:
-- @nb@.
noBok :: LanguageTag
noBok = RegularGrandfathered $ Nobok

-- | Tag @no-nyn@. Norwegian Nynorsk. Deprecated. Preferred value:
-- @nn@.
noNyn :: LanguageTag
noNyn = RegularGrandfathered $ Nonyn

-- | Tag @zh-guoyu@. Mandarin or Standard
-- Chinese. Deprecated. Preferred value: @cmn@.
zhGuoyu :: LanguageTag
zhGuoyu = RegularGrandfathered $ Zhguoyu

-- | Tag @zh-hakka@. Hakka. Deprecated. Preferred value: @hak@.
zhHakka :: LanguageTag
zhHakka = RegularGrandfathered $ Zhhakka

-- | Tag @zh-min@. Min, Fuzhou, Hokkien, Amoy, or
-- Taiwanese. Deprecated. See @cdo@ (Min Dong Chinese), @cpx@ (Pu-Xian
-- Chinese), @czo@ (Min Zhong Chinese), @mnp@ (Min Bei Chinese), @nan@
-- (Min Nan Chinese).
zhMin :: LanguageTag
zhMin = RegularGrandfathered $ Zhmin

-- | Tag @zh-min-nan@. Minnan, Hokkien, Amoy, Taiwanese, Southern Min,
-- Southern Fujian, Hoklo, Southern Fukien,
-- Ho-lo. Deprecated. Preferred value: @nan@.
zhMinNan :: LanguageTag
zhMinNan = RegularGrandfathered $ Zhminnan

-- | Tag @zh-xiang@. Xiang or Hunanese. Deprecated. Preferred value:
-- @hsn@.
zhXiang :: LanguageTag
zhXiang = RegularGrandfathered $ Zhxiang
