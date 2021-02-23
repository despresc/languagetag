{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: remove this file or integrate it with the other
-- is alternate internal syntax stuff.

module Text.BCP47.Internal.SyntaxAlt where

import qualified Data.Bits as Bit
import qualified Data.ByteString.Internal as BI
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
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

TODO HERE: just remove the fucking show and read instances. Then call
normalTag and fullNormalTag "unsafe*" instead. Also remove the lax
functions.

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

-- | A subtag that may not be present. Equivalent to @Maybe
-- Subtag@. Use 'justSubtag' and 'nullSubtag' to construct these, and
-- 'fromMaybeSubtag' to eliminate them.
newtype MaybeSubtag = MaybeSubtag Subtag

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

renderRegion :: MaybeSubtag -> [Text]
renderRegion (MaybeSubtag s)
  | unSubtag s == 0 = []
  | tagLength s == 2 = [T.unfoldrN 2 capgo (s, 0)]
  | otherwise = [renderSubtagLow s]
  where
    capgo (n, idx)
      | idx == (2 :: Word8) = Nothing
      | otherwise =
        let (c, n') = popChar n
         in Just (unpackUpperLetter c, (n', idx + 1))
{-# INLINE renderRegion #-}

-- TODO: should probably just do this more normally.
renderScript :: MaybeSubtag -> [Text]
renderScript (MaybeSubtag s)
  | unSubtag s == 0 = []
  | otherwise = [T.unfoldrN 4 go (s, 0 :: Word8)]
  where
    go (n, idx)
      | idx == 4 = Nothing
      | idx == 0 =
        let (c, n') = popChar n
         in Just (unpackUpperLetter c, (n', idx + 1))
      | otherwise =
        let (c, n') = popChar n
         in Just (unpackChar c, (n', idx + 1))
{-# INLINE renderScript #-}

renderSubtagLow :: Subtag -> Text
renderSubtagLow w = T.unfoldrN (fromIntegral len) go (w, 0)
  where
    len = tagLength w
    go (n, idx)
      | idx == len = Nothing
      | otherwise =
        let (c, n') = popChar n
         in Just (unpackChar c, (n', idx + 1))
{-# INLINE renderSubtagLow #-}

-- | A syntactically well-formed BCP47 tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the full
-- details.
data LanguageTag
  = NormalTag {-# UNPACK #-} !Normal
  | PrivateTag !(NonEmpty Subtag)
  | RegularGrandfathered !RegularGrandfathered
  | IrregularGrandfathered !IrregularGrandfathered

-- TODO: put this and other functions into the main module, I
-- think. Also document this.
renderLanguageTag :: LanguageTag -> Text
renderLanguageTag (NormalTag (Normal pr e1 e2 e3 sc reg vars exts pu)) =
  T.intercalate "-" $
    pr' :
    ( e1' <> e2' <> e3' <> sc' <> reg'
        <> vars'
        <> exts'
        <> pu'
    )
  where
    pr' = renderSubtagLow pr
    e1' = toList $ renderSubtagLow <$> fromMaybeSubtag e1
    e2' = toList $ renderSubtagLow <$> fromMaybeSubtag e2
    e3' = toList $ renderSubtagLow <$> fromMaybeSubtag e3
    sc' = renderScript sc
    reg' = renderRegion reg
    vars' = renderSubtagLow <$> vars
    exts' = concatMap renderExtension exts
    renderExtension (Extension s t) =
      T.singleton (unpackChar s) :
      toList (renderSubtagLow <$> t)
    pu'
      | null pu = []
      | otherwise = T.singleton 'x' : (renderSubtagLow <$> pu)
renderLanguageTag (PrivateTag l) =
  T.intercalate "-" $
    T.singleton 'x' : toList (renderSubtagLow <$> l)
renderLanguageTag (RegularGrandfathered t) = case t of
  Artlojban -> "art-lojban"
  Celgaulish -> "cel-gaulish"
  Nobok -> "no-bok"
  Nonyn -> "no-nyn"
  Zhguoyu -> "zh-guoyu"
  Zhhakka -> "zh-hakka"
  Zhmin -> "zh-min"
  Zhminnan -> "zh-min-nan"
  Zhxiang -> "zh-xiang"
renderLanguageTag (IrregularGrandfathered t) = case t of
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

data Extension = Extension
  { extSingleton :: {-# UNPACK #-} !SubtagChar,
    extTags :: {-# UNPACK #-} !(NonEmpty Subtag)
  }

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

-- | Construct a normal tag from its components. Keep in mind the
-- warnings for 'toSubtagLax' when using this function with the
-- 'IsString' instances for 'Subtag' and 'MaybeSubtag', since they
-- will silently mangle ill-formed subtags (though the result will
-- still be well-formed). This function will also not check if the
-- input is a regular grandfathered language tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the exact
-- grammar. A summary of the rules to follow to ensure that the input
-- is well-formed:
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

-- TODO: check for the regular grandfathered tags?
--
-- TODO: link to the section that has all of the regular/irregular
-- grandfathered tag constants in it.
--
-- TODO: have this function and fullNormalTag actually mangle the
-- input tags. Also check to make sure that the extension sections
-- don't have the x singleton.
normalTag ::
  -- | primary language
  Subtag ->
  -- | extended language
  MaybeSubtag ->
  -- | script
  MaybeSubtag ->
  -- | region
  MaybeSubtag ->
  -- | variant subtags
  [Subtag] ->
  -- | extension sections
  [(Char, NonEmpty Subtag)] ->
  -- | private use subtags
  [Subtag] ->
  LanguageTag
normalTag l me ms mr mv es pus =
  NormalTag $
    Normal
      { primlang = l,
        extlang1 = me,
        extlang2 = nullSubtag,
        extlang3 = nullSubtag,
        script = ms,
        region = mr,
        variants = mv,
        extensions = uncurry (Extension . packCharLax) <$> es,
        privateUse = pus
      }

-- | Construct a full normal tag from its components. You probably
-- want 'normalTag' instead of this function, since the third extended
-- language will always be null for valid tags and the only valid tag
-- with a non-null second extended language is the regular
-- grandfathered tag @zh-min-nan@, which should be constructed with
-- 'zhMinNan'. The warnings for 'normalTag' also apply to this
-- function.
fullNormalTag ::
  -- | primary language
  Subtag ->
  -- | extended language
  MaybeSubtag ->
  -- | a second extended language
  MaybeSubtag ->
  -- | a third extended language
  MaybeSubtag ->
  -- | script
  MaybeSubtag ->
  -- | region
  MaybeSubtag ->
  -- | variant subtags
  [Subtag] ->
  -- | extension sections
  [(Char, NonEmpty Subtag)] ->
  -- | private use subtags
  [Subtag] ->
  LanguageTag
fullNormalTag l me me2 me3 ms mr mv es pus =
  NormalTag $
    Normal
      { primlang = l,
        extlang1 = me,
        extlang2 = me2,
        extlang3 = me3,
        script = ms,
        region = mr,
        variants = mv,
        extensions = uncurry (Extension . packCharLax) <$> es,
        privateUse = pus
      }

-- | A private use tag starts with @x-@, which is followed by one or
-- more private use subtags, each of which is between one and eight
-- digits or letters long. This function constructs such a tag given
-- those private use subtags. The condition on the 'Text' values in
-- the input list is not checked, and if it does not hold then certain
-- functions in this library may behave unpredictably when given the
-- resulting tag.
privateTag :: NonEmpty Subtag -> LanguageTag
privateTag = PrivateTag

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
