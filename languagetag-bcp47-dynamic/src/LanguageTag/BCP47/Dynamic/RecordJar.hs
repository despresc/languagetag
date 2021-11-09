{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Parsing record jar files
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module LanguageTag.BCP47.Dynamic.RecordJar
  ( JarRegistry,
    JarRecord (..),
    JarField (..),
    FieldBody (..),
    fieldBodyLF,
    parseJarRegistry,
    renderJarRegistryBuilder,

    -- * Errors
    ParseError (..),
    ParseErrorType (..),
    LineNum,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (fold)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB

-- TODO: move writeRegistryFile into tests? the registry file MUST be in NFC,
-- and also newlines can't be in field names or body lines, so we'd have to
-- maintain those invariants (and maybe depend on text-icu).

-- TODO tests that this parses any syntactically valid registry, that it parses
-- the official registry correctly (it should round-trip the official registry
-- with renderJarRegistryBuilder, in fact)

-- | Parse a language subtag registry file
parseJarRegistry :: Text -> Either ParseError JarRegistry
parseJarRegistry inp = do
  lns <- toLines 1 id inp
  case lns of
    [] -> Left $ ParseError 0 "" EmptyRegistry
    (ln : lns') -> do
      (r, lns'') <- popRecord ln lns'
      parseRegistry' (r :|) lns''
  where
    isLineChar c = c >= ' '
    isFieldNameChar c = isDigit c || isAsciiUpper c || isAsciiLower c || c == '-'

    -- split the whole file up into lines ending with CRLF, also checking the
    -- Char validity in each line
    toLines !linenum acc t
      | T.null t = Right $ acc []
      | otherwise = case T.span isLineChar t of
        (line, rest)
          | Just (c, rest') <- T.uncons rest ->
            case c of
              '\n' -> toLines (linenum + 1) (acc . ((linenum, line) :)) rest'
              '\r'
                | Just ('\n', rest'') <- T.uncons rest' ->
                  toLines (linenum + 1) (acc . ((linenum, line) :)) rest''
                | otherwise -> toLines (linenum + 1) (acc . ((linenum, line) :)) rest'
              _ -> Left $ ParseError linenum line $ InvalidChar c
          | otherwise -> Right $ acc [(linenum, line)]

    -- pop a valid field name from the start of a line
    popFieldName linenum wholeline
      | T.null fname || T.head fname == '-' || T.last fname == '-' =
        Left $ ParseError linenum wholeline $ BadFieldName fname
      | otherwise = Right (fname, t)
      where
        (fname, t) = T.span isFieldNameChar wholeline

    -- pop a field separator from the start of a line
    popFieldSep linenum wholeline t = case T.uncons t' of
      Just (':', t'')
        | T.null t''' -> Left $ ParseError linenum wholeline EmptyBodyLine
        | otherwise -> Right t'''
        where
          t''' = T.dropWhile (== ' ') t''
      _ -> Left $ ParseError linenum wholeline NoFieldSep
      where
        t' = T.dropWhile (== ' ') t

    -- parse the first line of a field
    parseFieldStart (linenum, wholeline) = do
      (fname, t) <- popFieldName linenum wholeline
      t' <- popFieldSep linenum wholeline t
      pure (fname, t')

    -- get all of the lines of a field body other than the first and add them to
    -- the accumulator
    popBodyLines acc (ln : lns)
      | Just (' ', t) <- T.uncons $ snd ln,
        t' <- T.dropWhile (== ' ') t =
        if T.null t'
          then Left $ uncurry ParseError ln EmptyBodyLine
          else popBodyLines (acc . (t' :)) lns
    popBodyLines acc lns = Right (acc [], lns)

    -- pop a full field given its first line and the remaining stream of lines
    popField ln lns = do
      (fname, firstLineText) <- parseFieldStart ln
      -- the grammar allows for the very first line of field body (the part
      -- coming after the field name and separator), but only if a body line
      -- follows immediately
      (bodyLines, lns') <-
        if T.null firstLineText
          then case lns of
            ((_, t) : lns')
              | Just (' ', t') <- T.uncons t,
                t'' <- T.dropWhile (== ' ') t',
                not (T.null t'') ->
                popBodyLines (t'' :) lns'
            _ -> Left $ uncurry ParseError ln EmptyBodyLine
          else popBodyLines (firstLineText :) lns
      pure (JarField (fst ln) fname $ FieldBody bodyLines, lns')

    -- get all of the fields of a record other than the first and add them to
    -- the accumulator
    popOtherFields acc (ln : lns)
      | snd ln /= "%%" = do
        (f, lns') <- popField ln lns
        popOtherFields (acc . (f :)) lns'
    popOtherFields acc lns = Right (acc [], lns)

    -- parse a record given its first line and the remaining stream of lines,
    -- returning the remainder
    popRecord ln lns = case popField ln lns of
      Left e
        | BadFieldName _ <- parseErrorType e,
          snd ln == "%%" ->
          Left e {parseErrorType = EmptyRecord}
        | otherwise -> Left e
      Right (firstField, lns') -> do
        (fields, lns'') <- popOtherFields (firstField :|) lns'
        pure (JarRecord (fst ln) fields, lns'')

    -- the structure of parsing happens to guarantee that we will always stop
    -- parsing a single record either on a "%%" line or on the end of input, so
    -- we can omit the separator check here
    parseRegistry' acc (sepLine : lns) = case lns of
      (ln : lns') -> do
        (r, lns'') <- popRecord ln lns'
        parseRegistry' (acc . (r :)) lns''
      [] -> Left $ ParseError (fst sepLine + 1) "" EmptyRecord
    parseRegistry' acc [] = Right $ acc []

-- | A parse error, including the line number and full text of the line on which
-- the error occurred
data ParseError = ParseError
  { parseErrorLineNum :: !LineNum,
    parseErrorLine :: !Text,
    parseErrorType :: !ParseErrorType
  }
  deriving (Eq, Ord, Show)

-- | The possible errors that may be encountered when parsing. Note that an
-- 'InvalidChar' is a character that has a code point less than @\' \'@ and also
-- isn't @\'\\r\'@ or @\'\\n\'@.
data ParseErrorType
  = -- | the registry had no records
    EmptyRegistry
  | -- | an invalid character was encountered
    InvalidChar !Char
  | -- | the field name was not well-formed
    BadFieldName !Text
  | -- | there was no field separator in the field
    NoFieldSep
  | -- | a record separator was not followed by at least one field
    EmptyRecord
  | -- | a line in a field body was blank (contained no characters other than
    -- possibly @\' \'@)
    EmptyBodyLine
  deriving (Eq, Ord, Show)

-- | Render a 'JarRegistry' to a 'TB.Builder'. This function uses LF line
-- endings, not CRLF, in keeping with the usual 'Text' convention.
renderJarRegistryBuilder :: JarRegistry -> TB.Builder
renderJarRegistryBuilder = fold . NE.intersperse recSep . fmap renderRec
  where
    eol = "\n"
    recSep = "%%\n"
    bodyEol = "\n  "
    fieldSep = ": "
    renderRec = foldMap renderField . recordBody
    renderField jf =
      TB.fromText (fieldName jf)
        <> fieldSep
        <> renderFieldBody (fieldBodyLines jf)
        <> eol
    renderFieldBody (FieldBody body) =
      fold (List.intersperse bodyEol $ TB.fromText <$> body)

-- | The raw language subtag registry, which has the following ABNF, from [RFC
-- 5646, section
-- 3.1.1](https://www.rfc-editor.org/rfc/rfc5646.html#section-3.1.1), based on
-- the "record-jar" file format.
--
-- >>> registry   = record *("%%" CRLF record)
-- >>> record     = 1*field
-- >>> field      = ( field-name field-sep field-body CRLF )
-- >>> field-name = (ALPHA / DIGIT) [*(ALPHA / DIGIT / "-") (ALPHA / DIGIT)]
-- >>> field-sep  = *SP ":" *SP
-- >>> field-body = *([[*SP CRLF] 1*SP] 1*CHARS)
-- >>> CHARS      = (%x21-10FFFF)      ; Unicode code points
--
-- The [IANA subtag
-- registry](https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry)
-- is also specified to be UTF-8 encoded. Note that while the above grammar
-- specifies that the line endings must be CRLF, it does not seem that the
-- registry is always transmitted with these line endings. Thus the
-- 'parseRegistry' function will accept 'Text' values with any mix of line
-- endings.
type JarRegistry = NonEmpty JarRecord

-- | A raw registry record, also recording the line number on which it starts
data JarRecord = JarRecord
  { recordLineNum :: !LineNum,
    recordBody :: NonEmpty JarField
  }
  deriving (Eq, Ord, Show)

-- | The lines that make up a single field body in a record jar field
newtype FieldBody = FieldBody [Text]
  deriving (Eq, Ord, Show)

-- | Convert a 'FieldBody' to a single line of 'Text' by adding newlines between
-- each line
fieldBodyLF :: FieldBody -> Text
fieldBodyLF (FieldBody x) = T.intercalate "\n" x

-- | A raw registry field, consisting of a field name and zero or more lines of
-- text as the body. The parser in this file strips all leading space from each
-- line of the body since the fields are "logically a single line of text", with
-- indentation used to represent line folds.
data JarField = JarField
  { fieldLineNum :: !LineNum,
    fieldName :: !Text,
    fieldBodyLines :: !FieldBody
  }
  deriving (Eq, Ord, Show)

-- | A line number in a registry file, starting from 1
type LineNum = Int
