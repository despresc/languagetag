{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.LanguageTag.Internal.BCP47.Syntax
-- Description : Internal language tag types and functions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning: this is an internal module and may change or disappear
-- without regard to the PVP. The data constructors exported from this
-- module are also unsafe to use: the values they take are expected by
-- the rest of the library to satisfy particular invariants that the
-- type does not enforce. Other components of the library may
-- misbehave if ill-formed values are given to them.
module Text.LanguageTag.Internal.BCP47.Syntax
  ( LanguageTag (..),
    renderLanguageTag,
    renderLanguageTagBuilder,
    Normal (..),
    unsafeNormalTag,
    unsafeFullNormalTag,
    unsafePrivateTag,
    Extension (..),
    ExtensionChar (..),
    toExtensionChar,
    fromExtensionChar,
    unsafeSubtagCharToExtension,
    extensionCharToSubtag,
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import qualified Data.ByteString.Internal as BI
import Data.Hashable (Hashable (..), hashUsing)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.Subtag (SubtagChar (..))
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
  | PrivateUse !(NonEmpty Subtag)
  | Grandfathered !Grandfathered
  deriving (Eq, Ord)

instance NFData LanguageTag where
  rnf (NormalTag x) = rnf x
  rnf (PrivateUse x) = rnf x
  rnf (Grandfathered _) = ()

-- TODO: test that this is half the inverse of parse
instance Show LanguageTag where
  showsPrec p ps r = showsPrec p (renderLanguageTagBuilder ps) r

instance Hashable LanguageTag where
  hashWithSalt s (NormalTag t) =
    s
      `hashWithSalt` (0 :: Int)
      `hashWithSalt` t
  hashWithSalt s (PrivateUse t) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` t
  hashWithSalt s (Grandfathered t) =
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
    pr' = renderSubtagBuilder pr
    e1' = maybeSubtag "" renderLowPref e1
    e2' = maybeSubtag "" renderLowPref e2
    e3' = maybeSubtag "" renderLowPref e3
    sc' = renderScript sc
    reg' = renderRegion reg
    vars' = foldMap renderLowPref vars
    exts' = foldMap renderExtension exts
    renderExtension (Extension s t) = "-" <> TB.singleton (fromExtensionChar s) <> foldMap renderLowPref t
    pu'
      | null pu = ""
      | otherwise = "-" <> TB.singleton 'x' <> foldMap renderLowPref pu
renderLanguageTagBuilder (PrivateUse t) = TB.singleton 'x' <> foldMap renderLowPref t
  where
    renderLowPref s = "-" <> renderSubtagBuilder s
renderLanguageTagBuilder (Grandfathered t) = case t of
  ArtLojban -> "art-lojban"
  CelGaulish -> "cel-gaulish"
  EnGbOed -> "en-GB-oed"
  IAmi -> "i-ami"
  IBnn -> "i-bnn"
  IDefault -> "i-default"
  IEnochian -> "i-enochian"
  IHak -> "i-hak"
  IKlingon -> "i-klingon"
  ILux -> "i-lux"
  IMingo -> "i-mingo"
  INavajo -> "i-navajo"
  IPwn -> "i-pwn"
  ITao -> "i-tao"
  ITay -> "i-tay"
  ITsu -> "i-tsu"
  NoBok -> "no-bok"
  NoNyn -> "no-nyn"
  SgnBeFr -> "sgn-BE-FR"
  SgnBeNl -> "sgn-BE-NL"
  SgnChDe -> "sgn-CH-DE"
  ZhGuoyu -> "zh-guoyu"
  ZhHakka -> "zh-hakka"
  ZhMin -> "zh-min"
  ZhMinNan -> "zh-min-nan"
  ZhXiang -> "zh-xiang"
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

-- | The possible single character extensions; all the ASCII
-- alphanumeric characters (case-insensitive) except the letter X.
data ExtensionChar
  = Ext0
  | Ext1
  | Ext2
  | Ext3
  | Ext4
  | Ext5
  | Ext6
  | Ext7
  | Ext8
  | Ext9
  | ExtA
  | ExtB
  | ExtC
  | ExtD
  | ExtE
  | ExtF
  | ExtG
  | ExtH
  | ExtI
  | ExtJ
  | ExtK
  | ExtL
  | ExtM
  | ExtN
  | ExtO
  | ExtP
  | ExtQ
  | ExtR
  | ExtS
  | ExtT
  | ExtU
  | ExtV
  | ExtW
  | ExtY
  | ExtZ
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Convert an ASCII alphanumeric character other than X to an 'ExtensionChar'
toExtensionChar :: Char -> Maybe ExtensionChar
toExtensionChar c
  | c < '0' = Nothing
  | c <= '9' = toC 48 c
  | c < 'A' = Nothing
  | c <= 'W' = toC 55 c
  | c == 'X' = Nothing
  | c <= 'Z' = toC 56 c
  | c < 'a' = Nothing
  | c <= 'w' = toC 87 c
  | c == 'x' = Nothing
  | c <= 'z' = toC 88 c
  | otherwise = Nothing
  where
    toC n = Just . toEnum . subtract n . fromEnum

-- | Convert an 'ExtensionChar' to a lower case 'Char'.
fromExtensionChar :: ExtensionChar -> Char
fromExtensionChar ec
  | ec <= Ext9 = toC 48 ec
  | ec <= ExtW = toC 87 ec
  | otherwise = toC 88 ec
  where
    toC n = toEnum . (+ n) . fromEnum

-- | Convert a 'SubtagChar' to an 'ExtensionChar'. Note that the given
-- subtag character must not be @x@.

-- assumes that the subtag char is lower case or digit (currently
-- a valid assumption)
unsafeSubtagCharToExtension :: SubtagChar -> ExtensionChar
unsafeSubtagCharToExtension (SubtagChar n)
  | n < 58 = toC 48 n
  | n < 119 = toC 87 n
  | otherwise = toC 88 n
  where
    toC x = toEnum . subtract x . fromEnum

extensionCharToSubtag :: ExtensionChar -> Subtag
extensionCharToSubtag = go . fromExtensionChar
  where
    go = singleton . SubtagChar . fromIntegral . BI.c2w

instance Hashable ExtensionChar where
  hashWithSalt = hashUsing fromEnum

instance NFData ExtensionChar where
  rnf = rwhnf

-- | An extension section in a language tag
data Extension = Extension
  { extSingleton :: !ExtensionChar,
    extTags :: !(NonEmpty Subtag)
  }
  deriving (Eq, Ord)

instance NFData Extension where
  rnf (Extension _ x) = rnf x

instance Hashable Extension where
  hashWithSalt s (Extension c t) =
    s `hashWithSalt` c `hashWithSalt` t

----------------------------------------------------------------
-- Value construction
----------------------------------------------------------------

-- $valueconstruction

-- | Construct a normal tag from its components. This function uses
-- 'parseSubtagMangled' and 'packCharMangled' to construct the subtags
-- from the given components, in effect, so the warnings that
-- accompany those functions also apply here. This function will also
-- not check if the input is a grandfathered language tag, and will
-- not check if the subtags are appropriate for their sections. See
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
-- constructed with 'ZhMinNan'. The warnings for 'unsafeNormalTag'
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
      { primlang = parseSubtagMangled l,
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
    toExtension (c, ext) = Extension (fromMaybe ExtA $ toExtensionChar c) (parseSubtagMangled <$> ext)
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
unsafePrivateTag = PrivateUse . fmap parseSubtagMangled
{-# INLINE unsafePrivateTag #-}
