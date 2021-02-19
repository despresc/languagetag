-- |
-- Module      : Text.BCP47.Internal.Syntax
-- Description : Internal syntax definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The syntax type definitions and certain auto-generated language tag
-- constants taken from the IANA registry.
module Text.BCP47.Internal.Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- | A syntactically well-formed BCP47 tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the full
-- details. Note that the 'Eq' instance is too discriminating for
-- comparisons between tags; it is case-sensitive, among other
-- things. Use 'compareTag' instead. See also 'equalTag'.
data LanguageTag
  = -- | A normal tag
    NormalTag !Normal
  | -- | A private use tag. See 'privateUse' for the expectations for
    -- the 'Text' values in the list. The boolean is @True@ when the
    -- initial tag was upper case.
    PrivateTag !PrivateUse
  | -- | A regular grandfathered tag
    RegularGrandfathered !RegularGrandfathered
  | -- | An irregular grandfathered tag
    IrregularGrandfathered !IrregularGrandfathered
  deriving (Eq, Ord, Show, Read)

-- | A normal language tag, as opposed to the private use and
-- grandfathered tags. A reasonable amount of meaning can be inferred
-- from the these tags based on the length and structure of their
-- subtags, hence the "normal" description. Note that the language
-- tags that have as a prefix one of the regular grandfathered tags
-- will be parsed as a 'RegularGrandfathered' tag and not a
-- 'NormalTag' tag.
data Normal = Normal
  { -- | Primary language subtag
    language :: !Language,
    -- | Optional script subtag
    script :: !(Maybe Script),
    -- | Optional region subtag
    region :: !(Maybe Region),
    -- | Optional variant subtags
    variants :: ![Variant],
    -- | Optional extension sections
    extensions :: ![Extension],
    -- | Optional private use section
    privateUse :: !(Maybe PrivateUse)
  }
  deriving (Eq, Ord, Show, Read)

-- | The primary language subtag.
data Language
  = -- | Two or three letter text, followed by optional extended
    -- language subtags
    Language !Text !(Maybe ExtendedLanguage)
  | -- | Text that is all letters, length between four and eight
    LanguageOther !Text
  deriving (Eq, Ord, Show, Read)

-- | A script subtag. Four letter text.
type Script = Text

-- | Extended language subtags. Note that the 'extlangRemainder'
-- corresponds to a permanently reserved section of the language tag,
-- so it will always be @Nothing@ when parsing a valid (not merely
-- well-formed) 'Normal' tag.
data ExtendedLanguage = ExtendedLanguage
  { -- | Required extended language subtag. Three letters.
    extlang :: !Text,
    -- | Up to two optional language extensions, each three letters
    extlangRemainder :: !(Maybe FurtherExtension)
  }
  deriving (Eq, Ord, Show, Read)

-- | An extended language tag (four letters) then an optional extended
-- language tag (again four letters). This should never appear in a
-- valid tag.
type FurtherExtension = (Text, Maybe Text)

-- | A region subtag. Either two letters or three digits
type Region = Text

-- | A registered variant subtag. The 'Text' is four to eight letters
-- or digits, and if it has length exactly four then it must start
-- with a digit.
type Variant = Text

-- | An extension section. A letter or digit other than @\'X\'@ or
-- @\'x\'@, then a non-empty list of text values that are between two
-- and eight letters long.
data Extension = Extension !Char !(NonEmpty Text)
  deriving (Eq, Ord, Show, Read)

-- | A private use section. Either @\'X\'@ or @\'x\'@ (the boolean is
-- true if it is the former), then a non-empty list of private use
-- subtags.
data PrivateUse = PrivateUse !Bool !(NonEmpty PrivateSubtag)
  deriving (Eq, Ord, Show, Read)

-- | A private use subtag. One to eight letters or digits.
type PrivateSubtag = Text

-- | Language tags that were grandfathered into the current standard
-- and conform to the normal BCP47 grammar. Unlike 'Normal' tags,
-- their secondary subtags are neither extended language or variant
-- subtags; their meaning is registration-dependent. All of these tags
-- are deprecated in favor of a particular canonical 'Normal' tag
-- representation.
--
-- This type stores the original 'Text' values that were parsed, so
-- that 'renderLanguageTag' will round-trip properly. Also note that these
-- tags may appear as prefixes of well-formed tags, in which case they
-- will be parsed and treated as a 'Normal' tag.
data RegularGrandfathered
  = -- | @art-lojban@
    Artlojban !Text !Text
  | -- | @cel-gaulish@
    Celgaulish !Text !Text
  | -- | @no-bok@
    Nobok !Text !Text
  | -- | @no-nyn@
    Nonyn !Text !Text
  | -- | @zh-guoyu@
    Zhguoyu !Text !Text
  | -- | @zh-hakka@
    Zhhakka !Text !Text
  | -- | @zh-min@
    Zhmin !Text !Text
  | -- | @zh-min-nan@
    Zhminnan !Text !Text !Text
  | -- | @zh-xiang@
    Zhxiang !Text !Text
  deriving (Eq, Ord, Show, Read)

-- | Language tags that were grandfathered into the current standard
-- and which do not conform to the normal BCP47 grammar. All of these
-- tags but 'Imingo' and 'Idefault' are currently deprecated, and all
-- of the deprecated tags but 'Ienochian' have a favored canonical
-- 'Normal' tag representation.
--
-- The original subtag text values are also stored in this type so
-- that 'renderLanguageTag' will round-trip properly.
data IrregularGrandfathered
  = -- | @en-GB-oed@
    EnGBoed !Text !Text !Text
  | -- | @i-ami@
    Iami !Text !Text
  | -- | @i-bnn@
    Ibnn !Text !Text
  | -- | @i-default@
    Idefault !Text !Text
  | -- | @i-enochian@
    Ienochian !Text !Text
  | -- | @i-hak@
    Ihak !Text !Text
  | -- | @i-klingon@
    Iklingon !Text !Text
  | -- | @i-lux@
    Ilux !Text !Text
  | -- | @i-mingo@
    Imingo !Text !Text
  | -- | @i-navajo@
    Inavajo !Text !Text
  | -- | @i-pwn@
    Ipwn !Text !Text
  | -- | @i-tao@
    Itao !Text !Text
  | -- | @i-tay@
    Itay !Text !Text
  | -- | @i-tsu@
    Itsu !Text !Text
  | -- | @sgn-BE-FR@
    SgnBEFR !Text !Text !Text
  | -- | @sgn-BE-NL@
    SgnBENL !Text !Text !Text
  | -- | @sgn-CH-DE@
    SgnCHDE !Text !Text !Text
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------
-- File auto-generated below this line. Do not edit by hand!
----------------------------------------------------------------
