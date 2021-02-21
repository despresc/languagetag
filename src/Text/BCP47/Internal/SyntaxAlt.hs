{-# LANGUAGE FlexibleInstances #-}

-- TODO: remove this file or integrate it with the other
-- is alternate internal syntax stuff.

module Text.BCP47.Internal.SyntaxAlt where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word8)

-- TODO: replace these show instances with rendering ones

-- | A well-formed BCP47 language tag
data LanguageTag
  = NormalTag !Normal
  | PrivateTag !(NonEmpty ShortByteString)
  deriving (Show)

data Normal = Normal
  { primlang :: !ShortByteString,
    extlang1 :: !ShortByteString,
    extlang2 :: !ShortByteString,
    extlang3 :: !ShortByteString,
    script :: !ShortByteString,
    region :: !ShortByteString,
    variants :: ![ShortByteString],
    extensions :: ![Extension],
    privateUse :: ![ShortByteString]
  }
  deriving (Show)

data Extension = Extension
  { extSingleton :: !Word8,
    extTags :: !(NonEmpty ShortByteString)
  }
  deriving (Show)

-- simple internal convenience class
class Finishing a where
  finish :: a -> LanguageTag

instance
  Finishing
    ( ShortByteString ->
      ShortByteString ->
      ShortByteString ->
      ShortByteString ->
      ShortByteString ->
      [ShortByteString] ->
      [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con BS.empty
  {-# INLINE finish #-}

instance
  Finishing
    ( ShortByteString ->
      ShortByteString ->
      ShortByteString ->
      ShortByteString ->
      [ShortByteString] ->
      [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con BS.empty
  {-# INLINE finish #-}

instance
  Finishing
    ( ShortByteString ->
      ShortByteString ->
      ShortByteString ->
      [ShortByteString] ->
      [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con BS.empty
  {-# INLINE finish #-}

instance
  Finishing
    ( ShortByteString ->
      ShortByteString ->
      [ShortByteString] ->
      [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con BS.empty
  {-# INLINE finish #-}

instance
  Finishing
    ( ShortByteString ->
      [ShortByteString] ->
      [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con BS.empty
  {-# INLINE finish #-}

instance
  Finishing
    ( [ShortByteString] ->
      [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con []
  {-# INLINE finish #-}

instance
  Finishing
    ( [Extension] ->
      [ShortByteString] ->
      LanguageTag
    )
  where
  finish con = finish $ con []
  {-# INLINE finish #-}

instance Finishing ([ShortByteString] -> LanguageTag) where
  finish con = finish $ con []
  {-# INLINE finish #-}

instance Finishing (LanguageTag) where
  finish = id
  {-# INLINE finish #-}
