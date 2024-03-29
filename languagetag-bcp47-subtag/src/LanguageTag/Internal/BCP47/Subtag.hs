{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Internal subtag types and functions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use. The values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Parts of this library may misbehave (e.g., throw an
-- 'Control.Exception.ArithException') if ill-formed values are given
-- to them.
module LanguageTag.Internal.BCP47.Subtag
  ( -- * Subtags
    Subtag (..),
    unpackSubtag,
    unwrapSubtag,
    wrapSubtag,
    subtagHead,
    renderSubtagBuilderLower,
    renderSubtagBuilderUpper,
    renderSubtagBuilderTitle,
    subtagLength,
    subtagLength',

    -- * Subtags that might not be present
    MaybeSubtag (..),
    maybeSubtag,
    justSubtag,
    nullSubtag,

    -- * Subtag characters
    SubtagChar (..),
    unpackCharLower,
    unpackCharUpper,
    isSubtagByte,

    -- * Unsafe functions
    unsafeUnpackUpperLetter,
    unsafeIndexSubtag,
    unsafeSubtagPackLen,
    unsafeSetChar,
    unsafeSetLen,
  )
where

import Control.DeepSeq (NFData (..))
import qualified Data.Bits as Bit
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed (MVector, Unbox, Vector)
import Data.Word (Word64, Word8)
import GHC.Base (unsafeChr)

-- From bytestring's Data.ByteString.Internal
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

----------------------------------------------------------------
-- Subtags
----------------------------------------------------------------

-- | A 'Subtag' is a compact representation of a case-insensitive
-- string of ASCII alphanumeric characters of length between one and
-- eight. The 'Ord' instance is identical to that of 'Data.Text.Text',
-- in that for two subtags @x@ and @y@, we have @x < y@ if and only if
-- @'LanguageTag.BCP47.Syntax.renderLanguageTag' x <
-- 'LanguageTag.BCP47.Syntax.renderLanguageTag' y@.
--
-- These tags are always stored and printed entirely in lower case
-- when on their own.

-- The four lowest bits encode the length of the tag. The highest chunks of 7
-- bits encode the actual characters (first character the highest). (This leaves
-- us with 4 bits left over, in fact, not that this is useful to us at the
-- moment).
newtype Subtag = Subtag Word64
  deriving (Eq, Ord, Hashable, NFData)

-- | Return the internal representation of a 'Subtag'
unwrapSubtag :: Subtag -> Word64
unwrapSubtag (Subtag n) = n
{-# INLINE unwrapSubtag #-}

-- | Return the length of a subtag, which will be between 1 and 8
subtagLength :: Subtag -> Word8
subtagLength = fromIntegral . (Bit..&.) sel . unwrapSubtag
  where
    sel = 15

-- | Return the length of a subtag as an 'Int', which will be between 1 and 8
subtagLength' :: Subtag -> Int
subtagLength' = fromIntegral . (Bit..&.) sel . unwrapSubtag
  where
    sel = 15

instance Show Subtag where
  showsPrec p ps r = showsPrec p (renderSubtagBuilderLower ps) r

newtype instance MVector s Subtag = MV_Subtag (MVector s Word64)

newtype instance Vector Subtag = V_Subtag (Vector Word64)

instance VGM.MVector MVector Subtag where
  basicLength (MV_Subtag v) = VGM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Subtag v) = MV_Subtag $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Subtag v1) (MV_Subtag v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Subtag <$> VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Subtag v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n (Subtag x) = MV_Subtag <$> VGM.basicUnsafeReplicate n x
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Subtag v) i = Subtag <$> VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Subtag v) i (Subtag x) = VGM.basicUnsafeWrite v i x
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Subtag v) = VGM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Subtag v) (Subtag x) = VGM.basicSet v x
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Subtag v1) (MV_Subtag v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Subtag v1) (MV_Subtag v2) = VGM.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Subtag v) n = MV_Subtag <$> VGM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance VG.Vector Vector Subtag where
  basicUnsafeFreeze (MV_Subtag v) = V_Subtag <$> VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Subtag v) = MV_Subtag <$> VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Subtag v) = VG.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Subtag v) = V_Subtag $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Subtag v) i = Subtag <$> VG.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Subtag mv) (V_Subtag v) = VG.basicUnsafeCopy mv v
  elemseq _ (Subtag x) y = VG.elemseq (undefined :: Vector a) x y
  {-# INLINE elemseq #-}

instance Unbox Subtag

-- | Convert the internal representation of a 'Subtag' back to a
-- 'Subtag'

-- A brief reference: the four lower bits are the length `len`, which must be
-- between 1 and 8. The highest len * 7 bits are the subtag content, and must be
-- lower case ASCII letters or digits. All remaining bits (including bits 4
-- through 7 unconditionally) must be 0.

-- TODO: could be a little more efficient
wrapSubtag :: Word64 -> Maybe Subtag
wrapSubtag n
  | 1 <= len,
    len <= 8,
    allSubtagBytes 0,
    otherBits == 0 =
    Just $ Subtag n
  | otherwise = Nothing
  where
    len = n Bit..&. 15
    len' = fromIntegral len :: Word8
    allSubtagBytes idx =
      idx >= len'
        || ( isSubtagByte (unSubtagChar $ unsafeIndexSubtag (Subtag n) idx)
               && allSubtagBytes (idx + 1)
           )
    otherBits = Bit.shiftL (Bit.shiftR n 4) (4 + fromIntegral len * 7)
{-# INLINE wrapSubtag #-}

-- | Write a character starting at the given bit position. That bit
-- and the following six bits must not be set, and the position must
-- be of the form @57 - k * 7@ for @k@ between 0 and 7.
unsafeSetChar :: Word8 -> SubtagChar -> Word64 -> Word64
unsafeSetChar idx (SubtagChar c) n = n Bit..|. Bit.shiftL (fromIntegral c) (fromIntegral idx)

-- | Set the 'Subtag' length. There cannot be any other length
-- information recorded.
unsafeSetLen :: Word8 -> Word64 -> Word64
unsafeSetLen len w = w Bit..|. fromIntegral len

-- | Unsafely pack a list of subtag characters into a 'Subtag' given the length
-- of that list. That the length is valid and corresponds to the list of
-- characters is not checked.
unsafeSubtagPackLen :: Word8 -> [SubtagChar] -> Subtag
unsafeSubtagPackLen len = go 0 57
  where
    go !w !idx (x : xs) = go (unsafeSetChar idx x w) (idx - 7) xs
    go !w _ [] = Subtag $ unsafeSetLen len w

----------------------------------------------------------------
-- Maybe subtags
----------------------------------------------------------------

-- | A subtag that might be absent. Equivalent to @'Maybe'
-- 'Subtag'@. Use 'justSubtag' and 'nullSubtag' to construct these,
-- and 'maybeSubtag' to eliminate them.
newtype MaybeSubtag = MaybeSubtag {unMaybeSubtag :: Subtag}
  deriving (Eq, Ord, Hashable)

instance Show MaybeSubtag where
  showsPrec p (MaybeSubtag t) r
    | unwrapSubtag t == 0 = showsPrec p ("" :: String) r
    | otherwise = showsPrec p (renderSubtagBuilderLower t) r

-- | Eliminate a 'MaybeSubtag'
maybeSubtag :: a -> (Subtag -> a) -> MaybeSubtag -> a
maybeSubtag x f (MaybeSubtag (Subtag n))
  | n == 0 = x
  | otherwise = f $ Subtag n

-- | Convert a 'Subtag' to a 'MaybeSubtag' that is present
justSubtag :: Subtag -> MaybeSubtag
justSubtag = MaybeSubtag

-- | A 'MaybeSubtag' that is absent
nullSubtag :: MaybeSubtag
nullSubtag = MaybeSubtag (Subtag 0)

----------------------------------------------------------------
-- Subtag characters
----------------------------------------------------------------

-- | The encoding of a valid subtag character (a case-insensitive
-- ASCII alphabetic character or digit)
newtype SubtagChar = SubtagChar {unSubtagChar :: Word8}
  deriving (Eq, Ord, Hashable)

-- | shown like a 'Char'
instance Show SubtagChar where
  show = show . unpackCharLower

instance NFData SubtagChar where
  rnf (SubtagChar _) = ()

-- | Convert a 'SubtagChar' to a lower case 'Char'
unpackCharLower :: SubtagChar -> Char
unpackCharLower (SubtagChar w) = w2c w

-- | Convert a 'SubtagChar' to an upper case 'Char'
unpackCharUpper :: SubtagChar -> Char
unpackCharUpper (SubtagChar w)
  | w >= 97 = w2c $ w - 32
  | otherwise = w2c w

-- | Convert a 'SubtagChar' to an upper case 'Char'. The input must be
-- a letter.
unsafeUnpackUpperLetter :: SubtagChar -> Char
unsafeUnpackUpperLetter (SubtagChar w) =
  w2c $ w - 32

-- | Tests whether or not the 'Word8' is a valid UTF-8 encoded subtag character
-- (an ASCII alphanumeric character)
isSubtagByte :: Word8 -> Bool
isSubtagByte w =
  w <= 122 && w >= 97
    || w <= 90 && w >= 65
    || w <= 57 && w >= 48

----------------------------------------------------------------
-- Subtag indexing and rendering
----------------------------------------------------------------

-- | Index a subtag without bounds checking. Note that
-- @'unsafeIndexSubtag' 0@ is equivalent to 'subtagHead'.

-- TODO: efficiency? not sure if jumping based on idx to a precalculated
-- selector would be more efficient
unsafeIndexSubtag :: Subtag -> Word8 -> SubtagChar
unsafeIndexSubtag (Subtag n) idx =
  SubtagChar $
    fromIntegral $
      Bit.shiftR n (57 - 7 * fromIntegral idx) Bit..&. sel
  where
    sel = 127

-- | Return the head of the 'Subtag'. Subtags are always non-empty, so
-- this function is total.
subtagHead :: Subtag -> SubtagChar
subtagHead = (`unsafeIndexSubtag` 0)

-- | Unpack a 'Subtag' into its constituent 'SubtagChar' elements
unpackSubtag :: Subtag -> [SubtagChar]
unpackSubtag w = List.unfoldr go 0
  where
    len = subtagLength w
    go idx
      | idx == len = Nothing
      | otherwise =
        let c = unsafeIndexSubtag w idx
         in Just (c, idx + 1)
{-# INLINE unpackSubtag #-}

-- | Render a 'Subtag' to a lazy text builder in lower case
renderSubtagBuilderLower :: Subtag -> TB.Builder
renderSubtagBuilderLower = TB.fromString . fmap unpackCharLower . unpackSubtag
{-# INLINE renderSubtagBuilderLower #-}

-- | Render a 'Subtag' to a lazy text builder in upper case
renderSubtagBuilderUpper :: Subtag -> TB.Builder
renderSubtagBuilderUpper = TB.fromString . fmap unpackCharUpper . unpackSubtag
{-# INLINE renderSubtagBuilderUpper #-}

-- | Render a 'Subtag' to a lazy text builder in title case
renderSubtagBuilderTitle :: Subtag -> TB.Builder
renderSubtagBuilderTitle = TB.fromString . go . unpackSubtag
  where
    go (x : xs) = unpackCharUpper x : fmap unpackCharLower xs
    go [] = ""
{-# INLINE renderSubtagBuilderTitle #-}
