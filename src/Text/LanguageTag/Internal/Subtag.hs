{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Text.LanguageTag.Internal.Subtag
-- Description : Subtag types and functions
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
module Text.LanguageTag.Internal.Subtag
  ( -- * Subtags
    Subtag (..),
    unpackSubtag,
    unwrapSubtag,
    wrapSubtag,
    subtagHead,
    renderSubtagBuilder,
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
    unpackChar,
    unpackCharUpper,

    -- * Unsafe functions
    unsafeIndexSubtag,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Bits as Bit
import qualified Data.ByteString.Internal as BI
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed (MVector, Vector)
import Data.Word (Word64, Word8)

----------------------------------------------------------------
-- Subtags
----------------------------------------------------------------

-- | A 'Subtag' is a compact representation of a case-insensitive
-- string of ASCII alphanumeric characters of length between one and
-- eight. The 'Ord' instance is identical to that of 'Data.Text.Text',
-- in that for two subtags @x@ and @y@, we have @x < y@ if and only if
-- @'Text.LanguageTag.BCP47.Syntax.renderLanguageTag' x <
-- 'Text.LanguageTag.BCP47.Syntax.renderLanguageTag' y@.
--
-- These tags are always stored and printed entirely in lower case
-- when on their own.

-- The three lowest bits encode the length of the tag. The next two
-- bits record whether or not the tag contains a letter or digit. The
-- highest chunks of 7 bits encode the actual characters (first
-- character the highest). (This leaves us with 2 bits left over, in
-- fact, not that this is useful to us at the moment).
--
-- TODO: add a test that toSubtag is actually an order homomorphism
newtype Subtag = Subtag Word64
  deriving (Eq, Ord, Hashable, NFData)

-- | Return the internal representation of a 'Subtag'
unwrapSubtag :: Subtag -> Word64
unwrapSubtag (Subtag n) = n
{-# INLINE unwrapSubtag #-}

-- | Return the length of a subtag, which will be between 1 and 8.
subtagLength :: Subtag -> Word8
subtagLength = fromIntegral . (Bit..&.) sel . unwrapSubtag
  where
    sel = 15

-- | Return the length of a subtag, which will be between 1 and 8.
subtagLength' :: Subtag -> Int
subtagLength' = fromIntegral . (Bit..&.) sel . unwrapSubtag
  where
    sel = 15

-- TODO: check that this is half the inverse of parse
instance Show Subtag where
  showsPrec p ps r = showsPrec p (renderSubtagBuilder ps) r

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

-- | Convert the internal representation of a 'Subtag' back to a
-- 'Subtag'
wrapSubtag :: Word64 -> Maybe Subtag
wrapSubtag = undefined

----------------------------------------------------------------
-- Maybe subtags
----------------------------------------------------------------

-- | A subtag that may not be present. Equivalent to @Maybe
-- Subtag@. Use 'justSubtag' and 'nullSubtag' to construct these, and
-- 'maybeSubtag' to eliminate them.
newtype MaybeSubtag = MaybeSubtag {unMaybeSubtag :: Subtag}
  deriving (Eq, Ord, Hashable)

instance Show MaybeSubtag where
  showsPrec p (MaybeSubtag t) r
    | unwrapSubtag t == 0 = showsPrec p ("" :: String) r
    | otherwise = showsPrec p (renderSubtagBuilder t) r

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

-- | The encoding of a valid subtag character (an ASCII alphabetic
-- character or digit)
newtype SubtagChar = SubtagChar {unSubtagChar :: Word8}
  deriving (Eq, Ord, Show, Hashable)

-- | Unpack an ASCII alphanumeric character from a 'SubtagChar'
unpackChar :: SubtagChar -> Char
unpackChar (SubtagChar w) = BI.w2c w

-- | Unpack an ASCII alphanumeric character from a 'SubtagChar' to an
-- upper case 'Char'
unpackCharUpper :: SubtagChar -> Char
unpackCharUpper (SubtagChar w)
  | w >= 97 = BI.w2c $ w - 32
  | otherwise = BI.w2c w

----------------------------------------------------------------
-- Subtag indexing and rendering
----------------------------------------------------------------

-- | Index a subtag without bounds checking. Note that
-- @'unsafeIndexSubtag' 0@ is equivalent to 'subtagHead'.
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

-- | Unpack a 'Subtag' into its constituent 'SubtagChar' elements.
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

-- | Render a subtag in lower case to a lazy text builder
renderSubtagBuilder :: Subtag -> TB.Builder
renderSubtagBuilder = TB.fromString . fmap unpackChar . unpackSubtag
{-# INLINE renderSubtagBuilder #-}

-- | Render a subtag in upper case to a lazy text builder
renderSubtagBuilderUpper :: Subtag -> TB.Builder
renderSubtagBuilderUpper = TB.fromString . fmap unpackCharUpper . unpackSubtag
{-# INLINE renderSubtagBuilderUpper #-}

-- | Render a subtag in title case to a lazy text builder
renderSubtagBuilderTitle :: Subtag -> TB.Builder
renderSubtagBuilderTitle = TB.fromString . go . unpackSubtag
  where
    go (x : xs) = unpackCharUpper x : fmap unpackChar xs
    go [] = error "internal invariant violated: empty subtag"
{-# INLINE renderSubtagBuilderTitle #-}
