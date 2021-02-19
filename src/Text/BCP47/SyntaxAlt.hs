{-# LANGUAGE BangPatterns #-}

-- TODO: remove me or integrate me with Syntax

module Text.BCP47.SyntaxAlt where

import qualified Data.ByteString.Internal as BI
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.BCP47.Internal.SyntaxAlt

-- | The component just before what we're trying to parse
data Component
  = -- | just started
    Cbeginning
  | -- | primary language tag
    Cprimary
  | -- | first language extension
    Clext1
  | -- | second language extension
    Clext2
  | -- | the entire language tag
    Clanguage
  | -- | script tag
    Cscript
  | -- | region tag
    Cregion
  | -- | variant tag
    Cvariant
  | -- | extension tag
    Cextension
  | -- | private use tag
    Cprivateuse
  | -- | tag right after an initial @i-@
    CirregI
  deriving (Eq, Ord, Show, Read)

-- | An error that may occur during parsing
data Err = Err
  { -- | the start of the tag where the error occurred
    errPos :: Int,
    -- | the error itself
    errMessage :: ErrMessage
  }
  deriving (Eq, Ord, Show, Read)

-- | The content of an error message
data ErrMessage
  = -- | input was empty
    ErrEmpty
  | -- | invalid tag and the last thing that was parsed correctly
    ErrBadTag !Text !Component
  | -- | empty private use section
    ErrPrivateEmpty
  | -- | empty extension section
    ErrExtensionEmpty
  | -- | did not have exactly one tag after an @i-@
    ErrIrregINum
  deriving (Eq, Ord, Show, Read)

type InputStream = [(Text, Int)]

inputSplit :: Text -> InputStream
inputSplit = List.unfoldr go . (,) 0 . T.split (== '-')
  where
    go (!pos, tag : tags) = Just ((tag, pos), (pos + T.length tag + 1, tags))
    go (_, []) = Nothing

parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 = parseBCP47' . inputSplit

-- TODO: add normalish
parseBCP47' :: InputStream -> Either Err LanguageTag
parseBCP47' ((t, _) : ts)
  | len == 0 = Left $ Err 0 ErrEmpty
  | len == 1 = case T.head t of
    'x' -> PrivateTag <$> parsePrivate 0 ts
    'X' -> PrivateTag <$> parsePrivate 0 ts
    _ -> Left $ Err 0 $ ErrBadTag t Cbeginning
  | otherwise = Left $ Err 0 $ ErrBadTag t Cbeginning
  where
    len = T.length t
parseBCP47' [] = Left $ Err 0 ErrEmpty

-- Takes the start of the private section for errors
parsePrivate :: Int -> InputStream -> Either Err (NonEmpty ShortByteString)
parsePrivate !_ ((x, xpos) : xs) = do
  x' <- parsePrivateTag x xpos
  go (x' NE.:|) xs
  where
    parsePrivateTag t pos = case getPrivate t of
      Just t' -> pure t'
      Nothing -> Left $ Err pos $ ErrBadTag t Cprivateuse
    go l ((t, pos) : ts) = do
      t' <- parsePrivateTag t pos
      go (l . (t' :)) ts
    go l [] = pure $ l []
parsePrivate privatestart [] = Left $ Err privatestart ErrPrivateEmpty

munfold :: (Char -> Maybe Word8) -> Text -> Maybe [Word8]
munfold f t
  | T.length t >= 1,
    T.length t <= 8 =
    ending $ T.foldl' go (False, id) t
  | otherwise = Nothing
  where
    ending (b, l)
      | b = Nothing
      | otherwise = Just $ l []
    go (b, l) c
      | b = (b, l)
      | otherwise = case f c of
        Just w -> (b, l . (w :))
        Nothing -> (True, l)

-- | Parse a digit or letter, also lower-casing it if it is a letter.

-- TODO: optimize
lowAlphaNum :: Char -> Maybe Word8
lowAlphaNum c
  | c >= '0' && c <= '9' = Just $ BI.c2w c
  | c >= 'a' && c <= 'z' = Just $ BI.c2w c
  | c >= 'A' && c <= 'Z' = Just $ BI.c2w c + 32
  | otherwise = Nothing

-- TODO: use guard
getPrivate :: Text -> Maybe ShortByteString
getPrivate = go . fmap BS.pack . munfold lowAlphaNum
  where
    go Nothing = Nothing
    go (Just sbs)
      | BS.null sbs || BS.length sbs > 8 = Nothing
      | otherwise = Just sbs
