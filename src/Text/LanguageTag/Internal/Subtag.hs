{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.LanguageTag.Internal.Subtag
  ( -- * Subtags
    Subtag (..),
    unpackSubtag,
    wrapSubtag,
    renderSubtagBuilder,
    subtagLength,

    -- * Subtags that might not be present
    MaybeSubtag (..),

    -- * Subtag characters
    SubtagChar (..),
    unpackChar,

    -- * Unsafe functions
    unsafeIndexSubtag,
  )
where

import qualified Data.Bits as Bit
import qualified Data.ByteString.Internal as BI
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed (MVector, Vector)
import Data.Word (Word64, Word8)

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
-- bits record whether or not the tag contains a letter or digit. The
-- highest chunks of 7 bits encode the actual characters (first
-- character the highest). (This leaves us with 2 bits left over, in
-- fact, not that this is useful to us at the moment).
--
-- TODO: add a test that toSubtag is actually an order homomorphism
newtype Subtag = Subtag {unwrapSubtag :: Word64}
  deriving (Eq, Ord, Hashable)

-- | Return the length of a subtag, which will be between 1 and 8.
subtagLength :: Subtag -> Word8
subtagLength = fromIntegral . (Bit..&.) sel . unwrapSubtag
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

-- | A subtag that may not be present. Equivalent to @Maybe
-- Subtag@. Use 'justSubtag' and 'nullSubtag' to construct these, and
-- 'maybeSubtag' to eliminate them.
newtype MaybeSubtag = MaybeSubtag Subtag
  deriving (Eq, Ord, Hashable)

instance Show MaybeSubtag where
  showsPrec p (MaybeSubtag t) r
    | unwrapSubtag t == 0 = showsPrec p ("" :: String) r
    | otherwise = showsPrec p (renderSubtagBuilder t) r

-- | The encoding of a valid subtag character (an ASCII alphabetic
-- character or digit)
newtype SubtagChar = SubtagChar {unSubtagChar :: Word8}
  deriving (Eq, Ord, Show, Hashable)

-- | Unpack an ASCII alphanumeric character from a 'SubtagChar'
unpackChar :: SubtagChar -> Char
unpackChar (SubtagChar w) = BI.w2c w

-- | Index a subtag without bounds checking. Note that
-- @'unsafeIndexSubtag' 0@ is equivalent to 'subtagHead'.
unsafeIndexSubtag :: Subtag -> Word8 -> SubtagChar
unsafeIndexSubtag (Subtag n) idx =
  SubtagChar $
    fromIntegral $
      Bit.shiftR n (57 - 7 * fromIntegral idx) Bit..&. sel
  where
    sel = 127

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

renderSubtagBuilder :: Subtag -> TB.Builder
renderSubtagBuilder = TB.fromString . fmap unpackChar . unpackSubtag
