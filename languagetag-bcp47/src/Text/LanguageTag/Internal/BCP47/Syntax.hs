{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Internal language tag types and functions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use: the values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Other components of the library may misbehave if
-- ill-formed values are given to them.
module Text.LanguageTag.Internal.BCP47.Syntax
  ( BCP47 (..),
    renderBCP47,
    renderBCP47Builder,
    Normal (..),
    Extension (..),
    ExtensionChar (..),
    charToExtensionChar,
    extensionCharToChar,
    unsafeSubtagCharToExtension,
    extensionCharToSubtag,
    grandfatheredToSubtags,
    subtagX,
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Hashable (Hashable (..), hashUsing)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import Text.LanguageTag.BCP47.Subtag
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..), SubtagChar (..))

-- | Render a possibly-absent region subtag in the middle of a tag,
-- with an initial @-@
renderRegion :: MaybeSubtag -> TB.Builder
renderRegion = maybeSubtag "" go
  where
    go s
      | subtagLength s == 2 = TB.fromString $ '-' : List.unfoldr (capgo s) 0
      | otherwise = "-" <> renderSubtagBuilderLower s
    capgo s idx
      | idx == (2 :: Word8) = Nothing
      | otherwise =
        let c = unsafeIndexSubtag s idx
         in Just (unsafeUnpackUpperLetter c, idx + 1)

-- | Render a possibly-absent script subtag in the middle of a tag,
-- with an initial @-@
renderScript :: MaybeSubtag -> TB.Builder
renderScript = maybeSubtag "" $ \s -> TB.fromString $ '-' : List.unfoldr (go s) 0
  where
    go s idx
      | idx == 4 = Nothing
      | idx == 0 =
        let c = unsafeIndexSubtag s idx
         in Just (unsafeUnpackUpperLetter c, idx + 1)
      | otherwise =
        let c = unsafeIndexSubtag s idx
         in Just (unpackCharLower c, idx + 1)

-- | A syntactically well-formed BCP47 tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the full
-- details. Note that the 'Ord' instance is based on a
-- component-by-component comparison of tags, so that, e.g., a tag
-- with an absent region subtag will always be lower than the same tag
-- with an added region subtag. It also considers normal tags to be
-- lower than grandfathered tags, which are lower than private use
-- tags.
data BCP47
  = NormalTag {-# UNPACK #-} !Normal
  | GrandfatheredTag !Grandfathered
  | PrivateUse !(NonEmpty Subtag)
  deriving (Eq, Ord)

instance NFData BCP47 where
  rnf (NormalTag x) = rnf x
  rnf (PrivateUse x) = rnf x
  rnf (GrandfatheredTag _) = ()

instance Show BCP47 where
  showsPrec p ps r = showsPrec p (renderBCP47Builder ps) r

instance Hashable BCP47 where
  hashWithSalt s (NormalTag t) =
    s
      `hashWithSalt` (0 :: Int)
      `hashWithSalt` t
  hashWithSalt s (PrivateUse t) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` t
  hashWithSalt s (GrandfatheredTag t) =
    s
      `hashWithSalt` (2 :: Int)
      `hashWithSalt` t

-- | Render a language tag to a lazy text builder according to the
-- BCP47 guidelines
renderBCP47Builder :: BCP47 -> TB.Builder
renderBCP47Builder (NormalTag (Normal pr e1 e2 e3 sc reg vars exts pu)) =
  pr' <> e1' <> e2' <> e3' <> sc' <> reg'
    <> vars'
    <> exts'
    <> pu'
  where
    renderLowPref s = "-" <> renderSubtagBuilderLower s
    pr' = renderSubtagBuilderLower pr
    e1' = maybeSubtag "" renderLowPref e1
    e2' = maybeSubtag "" renderLowPref e2
    e3' = maybeSubtag "" renderLowPref e3
    sc' = renderScript sc
    reg' = renderRegion reg
    vars' = foldMap renderLowPref vars
    exts' = foldMap renderExtension exts
    renderExtension (Extension s t) = "-" <> TB.singleton (extensionCharToChar s) <> foldMap renderLowPref t
    pu'
      | null pu = ""
      | otherwise = "-" <> TB.singleton 'x' <> foldMap renderLowPref pu
renderBCP47Builder (PrivateUse t) = TB.singleton 'x' <> foldMap renderLowPref t
  where
    renderLowPref s = "-" <> renderSubtagBuilderLower s
renderBCP47Builder (GrandfatheredTag t) = case t of
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
{-# INLINE renderBCP47Builder #-}

-- | Render a language tag to a strict text string according to the BCP47 guidelines
renderBCP47 :: BCP47 -> Text
renderBCP47 = TL.toStrict . TB.toLazyText . renderBCP47Builder
{-# INLINE renderBCP47 #-}

-- | Convert a 'Grandfathered' tag to its constituent 'Subtag's

-- This has to live here instead of Registry.Grandfathered for module
-- cycle reasons, though it could conceivably be auto-generated and
-- put into one of the Internal.Registry.Grandfathered* modules
grandfatheredToSubtags :: Grandfathered -> NonEmpty Subtag
grandfatheredToSubtags ArtLojban = Subtag 14108546179528654867 :| [Subtag 15690354374758891542]
grandfatheredToSubtags CelGaulish = Subtag 14382069488147234835 :| [Subtag 14954113284221173783]
grandfatheredToSubtags EnGbOed = Subtag 14679482985414131730 :| [Subtag 14954202562683731986, Subtag 16111381376313327635]
grandfatheredToSubtags IAmi = Subtag 15132094747964866577 :| [Subtag 14102819922971197459]
grandfatheredToSubtags IBnn = Subtag 15132094747964866577 :| [Subtag 14248104991419006995]
grandfatheredToSubtags IDefault = Subtag 15132094747964866577 :| [Subtag 14526138628724883479]
grandfatheredToSubtags IEnochian = Subtag 15132094747964866577 :| [Subtag 14680466211245977112]
grandfatheredToSubtags IHak = Subtag 15132094747964866577 :| [Subtag 15098133032806121491]
grandfatheredToSubtags IKlingon = Subtag 15132094747964866577 :| [Subtag 15542853518732230679]
grandfatheredToSubtags ILux = Subtag 15132094747964866577 :| [Subtag 15697226132455686163]
grandfatheredToSubtags IMingo = Subtag 15132094747964866577 :| [Subtag 15827749698417983509]
grandfatheredToSubtags INavajo = Subtag 15132094747964866577 :| [Subtag 15962927641447628822]
grandfatheredToSubtags IPwn = Subtag 15132094747964866577 :| [Subtag 16275850723642572819]
grandfatheredToSubtags ITao = Subtag 15132094747964866577 :| [Subtag 16827550474088480787]
grandfatheredToSubtags ITay = Subtag 15132094747964866577 :| [Subtag 16827638435018702867]
grandfatheredToSubtags ITsu = Subtag 15132094747964866577 :| [Subtag 16847869448969781267]
grandfatheredToSubtags NoBok = Subtag 15977645578003677202 :| [Subtag 14249204503046782995]
grandfatheredToSubtags NoNyn = Subtag 15977645578003677202 :| [Subtag 15989872147304546323]
grandfatheredToSubtags SgnBeFr = Subtag 16690181889360658451 :| [Subtag 14237004322024980498, Subtag 14828101773117358098]
grandfatheredToSubtags SgnBeNl = Subtag 16690181889360658451 :| [Subtag 14237004322024980498, Subtag 15974267878283149330]
grandfatheredToSubtags SgnChDe = Subtag 16690181889360658451 :| [Subtag 14384497209821364242, Subtag 14525234698176692242]
grandfatheredToSubtags ZhGuoyu = Subtag 17699146535566049298 :| [Subtag 14976579405109788693]
grandfatheredToSubtags ZhHakka = Subtag 17699146535566049298 :| [Subtag 15098140437866610709]
grandfatheredToSubtags ZhMin = Subtag 17699146535566049298 :| [Subtag 15827742560719208467]
grandfatheredToSubtags ZhMinNan = Subtag 17699146535566049298 :| [Subtag 15827742560719208467, Subtag 15962850549540323347]
grandfatheredToSubtags ZhXiang = Subtag 17699146535566049298 :| [Subtag 17412902894784479253]
{-# INLINE grandfatheredToSubtags #-}

-- | Various well-formedness invariants:
--
-- * if 'primlang' has length at least 4 then each of the @extlang@
--   components must be 'nullSubtag'. The 'primlang' must also be
--   between two and eight letters.
--
-- * if an @extlang@ component is not 'nullSubtag' then all the
--   previous @extlang@ components must not be 'nullSubtag' either
--
-- * the 'script' component must be exactly four letters, if present
--
-- * the 'region' component must be exactly two letters or three
--   digits, if present
--
-- * the 'variants' must all be between four and eight letters or
--   digits, and if a variant has length four then it must begin with
--   a digit
--
-- * the extension subtags must all have length at least two
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
  deriving (Eq, Ord, Show)

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

-- | Convert an ASCII alphanumeric character other than @x@ or @X@ to
-- an 'ExtensionChar'
charToExtensionChar :: Char -> Maybe ExtensionChar
charToExtensionChar c
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
{-# INLINE charToExtensionChar #-}

-- | Convert an 'ExtensionChar' to a lower case 'Char'.
extensionCharToChar :: ExtensionChar -> Char
extensionCharToChar ec
  | ec <= Ext9 = toC 48 ec
  | ec <= ExtW = toC 87 ec
  | otherwise = toC 88 ec
  where
    toC n = toEnum . (+ n) . fromEnum
{-# INLINE extensionCharToChar #-}

extensionCharToSubtagChar :: ExtensionChar -> SubtagChar
extensionCharToSubtagChar ec
  | ec <= Ext9 = toC 48 ec
  | ec <= ExtW = toC 87 ec
  | otherwise = toC 88 ec
  where
    toC n = SubtagChar . fromIntegral . (+ n) . fromEnum
{-# INLINE extensionCharToSubtagChar #-}

-- | Convert a 'SubtagChar' to an 'ExtensionChar'. Note that the given
-- subtag character must not be @x@.

-- assumes that the subtag char is lower case or digit (currently
-- a valid assumption)
unsafeSubtagCharToExtension :: SubtagChar -> ExtensionChar
unsafeSubtagCharToExtension (SubtagChar n)
  | n < 58 = toC 48 n
  | n < 120 = toC 87 n
  | otherwise = toC 88 n
  where
    toC x = toEnum . subtract x . fromEnum

-- | Convert an 'ExtensionChar' to a singleton 'Subtag'

-- TODO: the direct definition might be a little more efficient
extensionCharToSubtag :: ExtensionChar -> Subtag
extensionCharToSubtag = singleton . extensionCharToSubtagChar

-- | The subtag @"x"@
subtagX :: Subtag
subtagX = Subtag 17293822569102704657

instance Hashable ExtensionChar where
  hashWithSalt = hashUsing fromEnum

instance NFData ExtensionChar where
  rnf = rwhnf

-- | An extension section in a language tag
data Extension = Extension
  { extSingleton :: !ExtensionChar,
    extTags :: !(NonEmpty Subtag)
  }
  deriving (Eq, Ord, Show)

instance NFData Extension where
  rnf (Extension _ x) = rnf x

instance Hashable Extension where
  hashWithSalt s (Extension c t) =
    s `hashWithSalt` c `hashWithSalt` t
