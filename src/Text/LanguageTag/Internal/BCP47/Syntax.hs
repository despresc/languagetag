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

import Data.Text (Text)

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
