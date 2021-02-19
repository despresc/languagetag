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
  deriving Show

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
  deriving Show

data Extension = Extension
  { extSingleton :: !Word8,
    extTags :: ![ShortByteString]
  }
  deriving Show
