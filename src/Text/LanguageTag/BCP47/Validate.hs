{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Text.LanguageTag.BCP47.Validate
-- Description : BCP47 language tag parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'validateBCP47' function to validate a
-- syntactically well-formed 'LanguageTag', transforming it into a
-- 'BCP47Tag' value. This module also re-exports the (very large)
-- module "Text.LanguageTag.BCP47.RegisteredSubtags", which contains
-- the data types representing all of the subtags in the IANA
-- registry, the current version of which is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>. See
-- 'bcp47RegistryDate' for the version of the registry that this
-- library uses.
module Text.LanguageTag.BCP47.Validate
  ( -- * Valid tags
    BCP47Tag (..),
    Normal (..),
    ExtensionSubtag,
    toExtensionSubtag,
    fromExtensionSubtag,
    ExtensionChar (..),
    toExtensionChar,
    fromExtensionChar,

    -- * The registered subtags
    -- $thetags
    bcp47RegistryDate,

    -- ** Language subtags
    Language (..),

    -- ** Extended language subtags
    Extlang (..),

    -- ** Script subtags
    Script (..),

    -- ** Region subtags
    Region (..),

    -- ** Variant subtags
    Variant (..),

    -- ** Grandfathered subtags
    Grandfathered (..),

    -- ** Redundant subtags
    Redundant (..),
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..), hashUsing)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Text.LanguageTag.BCP47.Syntax (Subtag, subtagLength)
import Text.LanguageTag.Internal.BCP47.Extlang
import Text.LanguageTag.Internal.BCP47.Grandfathered
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Internal.BCP47.Redundant
import Text.LanguageTag.Internal.BCP47.Region
import Text.LanguageTag.Internal.BCP47.RegistryDate
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.Variant

-- | A valid (not merely well-formed) BCP47 language tag.
data BCP47Tag
  = NormalTag Normal
  | PrivateUse (NonEmpty Text)
  | GrandfatheredTag Grandfathered
  | RedundantTag Redundant

{-

The rules:

- primary subtags - be registered

- extended language subtags - be registered. recommended that the
  related primary language subtag be used instead. must only have one
  extended language subtag (zh-min-nan excluded, I suppose)

- script subtags - should be omitted if it adds no value or if the
  primary language subtag has a relevant suppress-script field.

- variants - must not be repeated. variants sharing a prefix should
  not be used in the same tag (variants that can be used in
  combination should have multiple prefix fields indicating that
  fact).

- extension subtags - a singleton must appear at most once (except in
  private use), must have been registered. should canonicalize by
  ordering the singletions by ascii order.

- grandfathered/redundant - must match exactly, of course.

valid tag:

- well-formed
- either grandfathered or all the primary language, extended language,
  script, region, and variant subtags appear in the IANA subtag
  registry.
- no duplicate variant subtags
- no duplicate singleton (extension) subtags

canonicalizing:

- extension sequences are ordered by singleton subtag (ascii
  case-insensitive)
- redundant or grandfathered tags are replaced by their preferred
  value
- subtags are replaced by their preferred value (notably this means
  that there will be no extlangs in canonical form)

(so maybe we define a separate canonical tag type? and the canonical
form -> extlang form transformation can be defined as a CanonicalTag
-> ValidTag function).

-}

-- | A normal language tag, as opposed to a private use, redundant, or
-- grandfathered tag.
data Normal = Normal
  { language :: Language,
    extlang :: Maybe Extlang,
    script :: Maybe Script,
    region :: Maybe Region,
    variants :: Set Variant,
    extensions :: Map ExtensionChar (NonEmpty ExtensionSubtag),
    privateuse :: [Subtag]
  }

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

-- | Convert an ASCII alphanumeric character other than X to an 'ExtensionChar'
toExtensionChar :: Char -> Maybe ExtensionChar
toExtensionChar c = case c of
  '0' -> Just Ext0
  '1' -> Just Ext1
  '2' -> Just Ext2
  '3' -> Just Ext3
  '4' -> Just Ext4
  '5' -> Just Ext5
  '6' -> Just Ext6
  '7' -> Just Ext7
  '8' -> Just Ext8
  '9' -> Just Ext9
  'a' -> Just ExtA
  'b' -> Just ExtB
  'c' -> Just ExtC
  'd' -> Just ExtD
  'e' -> Just ExtE
  'f' -> Just ExtF
  'g' -> Just ExtG
  'h' -> Just ExtH
  'i' -> Just ExtI
  'j' -> Just ExtJ
  'k' -> Just ExtK
  'l' -> Just ExtL
  'm' -> Just ExtM
  'n' -> Just ExtN
  'o' -> Just ExtO
  'p' -> Just ExtP
  'q' -> Just ExtQ
  'r' -> Just ExtR
  's' -> Just ExtS
  't' -> Just ExtT
  'u' -> Just ExtU
  'v' -> Just ExtV
  'w' -> Just ExtW
  'y' -> Just ExtY
  'z' -> Just ExtZ
  'A' -> Just ExtA
  'B' -> Just ExtB
  'C' -> Just ExtC
  'D' -> Just ExtD
  'E' -> Just ExtE
  'F' -> Just ExtF
  'G' -> Just ExtG
  'H' -> Just ExtH
  'I' -> Just ExtI
  'J' -> Just ExtJ
  'K' -> Just ExtK
  'L' -> Just ExtL
  'M' -> Just ExtM
  'N' -> Just ExtN
  'O' -> Just ExtO
  'P' -> Just ExtP
  'Q' -> Just ExtQ
  'R' -> Just ExtR
  'S' -> Just ExtS
  'T' -> Just ExtT
  'U' -> Just ExtU
  'V' -> Just ExtV
  'W' -> Just ExtW
  'Y' -> Just ExtY
  'Z' -> Just ExtZ
  _ -> Nothing

-- | Convert an 'ExtensionChar' to a lower case 'Char'.
fromExtensionChar :: ExtensionChar -> Char
fromExtensionChar c = case c of
  Ext0 -> '0'
  Ext1 -> '1'
  Ext2 -> '2'
  Ext3 -> '3'
  Ext4 -> '4'
  Ext5 -> '5'
  Ext6 -> '6'
  Ext7 -> '7'
  Ext8 -> '8'
  Ext9 -> '9'
  ExtA -> 'a'
  ExtB -> 'b'
  ExtC -> 'c'
  ExtD -> 'd'
  ExtE -> 'e'
  ExtF -> 'f'
  ExtG -> 'g'
  ExtH -> 'h'
  ExtI -> 'i'
  ExtJ -> 'j'
  ExtK -> 'k'
  ExtL -> 'l'
  ExtM -> 'm'
  ExtN -> 'n'
  ExtO -> 'o'
  ExtP -> 'p'
  ExtQ -> 'q'
  ExtR -> 'r'
  ExtS -> 's'
  ExtT -> 't'
  ExtU -> 'u'
  ExtV -> 'v'
  ExtW -> 'w'
  ExtY -> 'y'
  ExtZ -> 'z'

instance Hashable ExtensionChar where
  hashWithSalt = hashUsing fromEnum

instance NFData ExtensionChar where
  rnf a = seq a ()

-- | An extension subtag is a 'Subtag' that is at least two characters
-- long
newtype ExtensionSubtag = ExtensionSubtag Subtag
  deriving (Eq, Ord, Show, Hashable)

-- | Convert a subtag to an extension subtag
toExtensionSubtag :: Subtag -> Maybe ExtensionSubtag
toExtensionSubtag s
  | subtagLength s >= 2 = Just $ ExtensionSubtag s
  | otherwise = Nothing

-- | Convert an extension subtag to a subtag
fromExtensionSubtag :: ExtensionSubtag -> Subtag
fromExtensionSubtag (ExtensionSubtag t) = t

instance NFData ExtensionSubtag where
  rnf a = seq a ()

-- $thetags
--
-- Below are all of the subtags that are registered with the
-- IANA. Check 'bcp47RegistryDate' for the version of the registry
-- that this library uses.
--
-- The names of the subtag data constructors are roughly the same as
-- the subtags themselves; all of them are title case except for the
-- 'Region' tags, which are all upper case. Additionally, the
-- 'Extlang' constructors have an @Ext@ prefix, any tag that starts
-- with a digit is prefixed with the first letter of their type, and
-- the redundant and grandfathered tags have been converted from kebab
-- case to camel case. These modifications were necessary to arrive at
-- constructor names that are valid and free of name collisions. Some
-- examples:
--
-- @
--  -- for the language subtag en
-- 'En' :: 'Language'
-- -- for the extended language subtag sgg
-- 'ExtSgg' :: 'Extlang'
-- -- for the script subtag moon
-- 'Moon' :: 'Script'
-- -- for the region subtag AU
-- 'AU' :: 'Region'
-- -- for the region subtag 419
-- 'R419' :: 'Region'
-- -- for the variant subtag gascon
-- 'Gascon' :: 'Variant'
-- -- for the variant subtag 1606nict
-- 'V1606nict' :: 'Variant'
-- -- for the grandfathered subtag i-enochian
-- 'IEnochian' :: 'Grandfathered'
-- -- for the redundant subtag en-scouse
-- 'EnScouse' :: 'Redundant'
-- @
