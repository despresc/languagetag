{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.Syntax
  ( LanguageTag (..),
    renderLanguageTag,
    renderLanguageTagBuilder,
    RegularGrandfathered (..),
    IrregularGrandfathered (..),
    Normal (..),
    unsafeNormalTag,
    unsafeFullNormalTag,
    unsafePrivateTag,
    enGbOed,
    iAmi,
    iBnn,
    iDefault,
    iEnochian,
    iHak,
    iKlingon,
    iLux,
    iMingo,
    iNavajo,
    iPwn,
    iTao,
    iTay,
    iTsu,
    sgnBeFr,
    sgnBeNl,
    sgnChDe,
    artLojban,
    celGaulish,
    noBok,
    noNyn,
    zhGuoyu,
    zhHakka,
    zhMin,
    zhMinNan,
    zhXiang,
    Extension (..),
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Hashable (Hashable (..), hashUsing)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import Text.LanguageTag.Subtag

renderRegion :: MaybeSubtag -> TB.Builder
renderRegion = maybeSubtag "" go
  where
    go s
      | subtagLength s == 2 = TB.fromString $ '-' : List.unfoldr capgo (s, 0)
      | otherwise = renderSubtagBuilder s
    capgo (n, idx)
      | idx == (2 :: Word8) = Nothing
      | otherwise =
        let (c, n') = unsafePopChar n
         in Just (unsafeUnpackUpperLetter c, (n', idx + 1))

renderScript :: MaybeSubtag -> TB.Builder
renderScript = maybeSubtag "" $ \s -> TB.fromString $ '-' : List.unfoldr go (s, 0 :: Word8)
  where
    go (n, idx)
      | idx == 4 = Nothing
      | idx == 0 =
        let (c, n') = unsafePopChar n
         in Just (unsafeUnpackUpperLetter c, (n', idx + 1))
      | otherwise =
        let (c, n') = unsafePopChar n
         in Just (unpackChar c, (n', idx + 1))

-- | A syntactically well-formed BCP47 tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the full
-- details. Note that the 'Ord' instance does not correspond to that
-- of 'Text', in the sense that there are language tags @x@ and @y@
-- such that @x < y@ and yet @'renderLanguageTag' x >
-- 'renderLanguageTag' y@.
data LanguageTag
  = NormalTag {-# UNPACK #-} !Normal
  | RegularGrandfathered !RegularGrandfathered
  | IrregularGrandfathered !IrregularGrandfathered
  deriving (Eq, Ord)

instance NFData LanguageTag where
  rnf (NormalTag x) = rnf x
  rnf (RegularGrandfathered _) = ()
  rnf (IrregularGrandfathered _) = ()

-- TODO: test that this is half the inverse of parse
instance Show LanguageTag where
  showsPrec p ps r = showsPrec p (renderLanguageTagBuilder ps) r

instance Hashable LanguageTag where
  hashWithSalt s (NormalTag t) =
    s
      `hashWithSalt` (0 :: Int)
      `hashWithSalt` t
  hashWithSalt s (RegularGrandfathered t) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` t
  hashWithSalt s (IrregularGrandfathered t) =
    s
      `hashWithSalt` (2 :: Int)
      `hashWithSalt` t

-- | Render a language tag to a lazy text builder
renderLanguageTagBuilder :: LanguageTag -> TB.Builder
renderLanguageTagBuilder (NormalTag (Normal pr e1 e2 e3 sc reg vars exts pu)) =
  pr' <> e1' <> e2' <> e3' <> sc' <> reg'
    <> vars'
    <> exts'
    <> pu'
  where
    renderLowPref s = "-" <> renderSubtagBuilder s
    pr' = maybeSubtag "" renderSubtagBuilder pr
    e1' = maybeSubtag "" renderLowPref e1
    e2' = maybeSubtag "" renderLowPref e2
    e3' = maybeSubtag "" renderLowPref e3
    sc' = renderScript sc
    reg' = renderRegion reg
    vars' = foldMap renderLowPref vars
    exts' = foldMap renderExtension exts
    renderExtension (Extension s t) = "-" <> TB.singleton (unpackChar s) <> foldMap renderLowPref t
    pu'
      | null pu = ""
      | otherwise = "-" <> TB.singleton 'x' <> foldMap renderLowPref pu
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

-- TODO HERE: might want to export a LanguageTag -> Normal function or
-- LanguageTag -> Maybe Normal function insteand of the entire
-- LanguageTag constructor and the other sub-constructors.
-- Also consider:
--
-- - replacing [Extension] et al. with Vector Extension
--
-- - making things a little lazier? (in parsing and such, though if we
--   use vector then things will naturally be less lazy).
--
-- - getting rid of the Finish class and making parsing more
--   straightforward?
--
-- Also should probably consolidate the two grandfathered types into
-- one! Then we don't need to recreate the type in the Grandfathered
-- module. Also consider defining and using the Ext type here.

-- | Invariants:
--
-- * if 'primlang' is 'nullSubtag' then everything must be empty
--   except for 'privateUse', which must be non-empty.
--
-- * if an @extlang@ component is not 'nullSubtag' then all the
--   previous @extlang@ components must not be 'nullSubtag' either.
data Normal = Normal
  { primlang :: {-# UNPACK #-} !MaybeSubtag,
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

instance Hashable Normal where
  hashWithSalt s (Normal p e1 e2 e3 sc r v e pv) =
    s `hashWithSalt` p `hashWithSalt` e1 `hashWithSalt` e2
      `hashWithSalt` e3
      `hashWithSalt` sc
      `hashWithSalt` r
      `hashWithSalt` v
      `hashWithSalt` e
      `hashWithSalt` pv

instance NFData Normal where
  rnf (Normal _ _ _ _ _ _ x y z) = rnf x `seq` rnf y `seq` rnf z

data Extension = Extension
  { extSingleton :: {-# UNPACK #-} !SubtagChar,
    extTags :: {-# UNPACK #-} !(NonEmpty Subtag)
  }
  deriving (Eq, Ord)

instance NFData Extension where
  rnf (Extension _ x) = rnf x

instance Hashable Extension where
  hashWithSalt s (Extension c t) =
    s `hashWithSalt` c `hashWithSalt` t

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
  deriving (Eq, Ord, Enum)

instance NFData RegularGrandfathered where
  rnf = rwhnf

instance Hashable RegularGrandfathered where
  hashWithSalt = hashUsing fromEnum

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
  deriving (Eq, Ord, Enum)

instance Hashable IrregularGrandfathered where
  hashWithSalt = hashUsing fromEnum

instance NFData IrregularGrandfathered where
  rnf = rwhnf

----------------------------------------------------------------
-- Value construction
----------------------------------------------------------------

-- $valueconstruction

-- | Construct a normal tag from its components. This function uses
-- 'parseSubtagMangled' and 'packCharMangled' to construct the subtags
-- from the given components, so the warnings that accompany that
-- function also apply here. This function will also not check if the
-- input is a grandfathered language tag, and will not check if the
-- subtags are appropriate for their sections. See
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
      { primlang = justSubtag $ parseSubtagMangled l,
        extlang1 = mmangled me,
        extlang2 = mmangled me2,
        extlang3 = mmangled me3,
        script = mmangled ms,
        region = mmangled mr,
        variants = parseSubtagMangled <$> vs,
        extensions = toExtension <$> es,
        privateUse = parseSubtagMangled <$> pus
      }
  where
    toExtension (c, ext) = Extension (packCharMangled c) (parseSubtagMangled <$> ext)
    mmangled t
      | T.null t = nullSubtag
      | otherwise = justSubtag $ parseSubtagMangled t
{-# INLINE unsafeFullNormalTag #-}

-- | A private use tag starts with @x-@, which is followed by one or
-- more private use subtags, each of which is between one and eight
-- digits or letters long. This function constructs such a tag given
-- those private use subtags. This function uses 'parseSubtagMangled',
-- and so the warnings for that function apply here as well.
unsafePrivateTag :: NonEmpty Text -> LanguageTag
unsafePrivateTag = unsafeNormalTag "" "" "" "" [] [] . NE.toList
{-# INLINE unsafePrivateTag #-}

----------------------------------------------------------------
-- File auto-generated below this line. Do not edit by hand!
----------------------------------------------------------------

-- TODO: actually generate this (or the documentation, anyway)
-- automatically

-- | Tag @en-GB-oed@. English, Oxford English Dictionary
-- spelling. Deprecated. Preferred value: @en-GB-oxendict@.
enGbOed :: LanguageTag
enGbOed = IrregularGrandfathered EnGBoed

-- | Tag @i-ami@. Amis. Deprecated. Preferred value: @ami@.
iAmi :: LanguageTag
iAmi = IrregularGrandfathered Iami

-- | Tag @-ibnn@. Bunun. Deprecated. Preferred value: @bnn@.
iBnn :: LanguageTag
iBnn = IrregularGrandfathered Ibnn

-- | Tag @i-default@. Default Language.
iDefault :: LanguageTag
iDefault = IrregularGrandfathered Idefault

-- | Tag @i-enochian@. Enochian. Deprecated.
iEnochian :: LanguageTag
iEnochian = IrregularGrandfathered Ienochian

-- | Tag @i-hak@. Hakka. Deprecated. Preferred value: @hak@.
iHak :: LanguageTag
iHak = IrregularGrandfathered Ihak

-- | Tag @i-klingon@. Klingon. Deprecated. Preferred value: @tlh@.
iKlingon :: LanguageTag
iKlingon = IrregularGrandfathered Iklingon

-- | Tag @i-lux@. Luxembourgish. Deprecated. Preferred value: @lb@.
iLux :: LanguageTag
iLux = IrregularGrandfathered Ilux

-- | Tag @i-mingo@. Mingo.
iMingo :: LanguageTag
iMingo = IrregularGrandfathered Imingo

-- | Tag @i-navajo@. Navajo. Deprecated. Preferred value: @nv@.
iNavajo :: LanguageTag
iNavajo = IrregularGrandfathered Inavajo

-- | Tag @i-pwn@. Paiwan. Deprecated. Preferred value: @pwn@.
iPwn :: LanguageTag
iPwn = IrregularGrandfathered Ipwn

-- | Tag @i-tao@. Tao. Deprecated. Preferred value: @i-tao@.
iTao :: LanguageTag
iTao = IrregularGrandfathered Itao

-- | Tag @i-tay@. Tayal. Deprecated. Preferred value: @i-tay@.
iTay :: LanguageTag
iTay = IrregularGrandfathered Itay

-- | Tag @i-tsu@. Tsou. Deprecated. Preferred value: @i-tsu@.
iTsu :: LanguageTag
iTsu = IrregularGrandfathered Itsu

-- | Tag @sgn-BE-FR@. Belgian-French Sign
-- Language. Deprecated. Preferred value: @sfb@.
sgnBeFr :: LanguageTag
sgnBeFr = IrregularGrandfathered SgnBEFR

-- | Tag @sgn-BE-NL@. Belgian-Flemish Sign
-- Language. Deprecated. Preferred value: @vgt@.
sgnBeNl :: LanguageTag
sgnBeNl = IrregularGrandfathered SgnBENL

-- | Tag @sgn-CH-DE@. Swiss-German Sign
-- Language. Deprecated. Preferred value: @sgg@.
sgnChDe :: LanguageTag
sgnChDe = IrregularGrandfathered SgnCHDE

-- | Tag @art-lojban@. Lojban. Deprecated. Preferred value: @jbo@.
artLojban :: LanguageTag
artLojban = RegularGrandfathered Artlojban

-- | Tag @cel-gaulish@. Gaulish. Deprecated. See @xcg@ (Cisalpine
-- Gaulish), @xga@ (Galatian), @xtg@ (Transalpine Gaulish).
celGaulish :: LanguageTag
celGaulish = RegularGrandfathered Celgaulish

-- | Tag @no-bok@. Norwegian Bokmal. Deprecated. Preferred value:
-- @nb@.
noBok :: LanguageTag
noBok = RegularGrandfathered Nobok

-- | Tag @no-nyn@. Norwegian Nynorsk. Deprecated. Preferred value:
-- @nn@.
noNyn :: LanguageTag
noNyn = RegularGrandfathered Nonyn

-- | Tag @zh-guoyu@. Mandarin or Standard
-- Chinese. Deprecated. Preferred value: @cmn@.
zhGuoyu :: LanguageTag
zhGuoyu = RegularGrandfathered Zhguoyu

-- | Tag @zh-hakka@. Hakka. Deprecated. Preferred value: @hak@.
zhHakka :: LanguageTag
zhHakka = RegularGrandfathered Zhhakka

-- | Tag @zh-min@. Min, Fuzhou, Hokkien, Amoy, or
-- Taiwanese. Deprecated. See @cdo@ (Min Dong Chinese), @cpx@ (Pu-Xian
-- Chinese), @czo@ (Min Zhong Chinese), @mnp@ (Min Bei Chinese), @nan@
-- (Min Nan Chinese).
zhMin :: LanguageTag
zhMin = RegularGrandfathered Zhmin

-- | Tag @zh-min-nan@. Minnan, Hokkien, Amoy, Taiwanese, Southern Min,
-- Southern Fujian, Hoklo, Southern Fukien,
-- Ho-lo. Deprecated. Preferred value: @nan@.
zhMinNan :: LanguageTag
zhMinNan = RegularGrandfathered Zhminnan

-- | Tag @zh-xiang@. Xiang or Hunanese. Deprecated. Preferred value:
-- @hsn@.
zhXiang :: LanguageTag
zhXiang = RegularGrandfathered Zhxiang
