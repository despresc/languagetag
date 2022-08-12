{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Well-formed language and extended language subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use: the values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Other components of the library may misbehave if
-- ill-formed values are given to them.
module LanguageTag.Internal.BCP47.Syntax.Language
  ( ShortLang (..),
    LongLang (..),
    Language (..),
    Extlang (..),
    extlangToShortLang,
    LangSec (..),
    Extlangs (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import LanguageTag.BCP47.Subtag
  ( IsSubtag (..),
    Subtag,
    ToSubtags (..),
    ToSubtagsNE (..),
    WrappedIsSubtag (..),
    WrappedToSubtagsNE (..),
    containsDigit,
    containsOnlyLetters,
    subtagLength,
  )

-- | A full language section in a 'LanguageTag.BCP47.Syntax.BCP47' tag,
-- consisting of a primary language subtag and possibly one or more extended
-- language subtags.
data LangSec
  = ShortLangSec ShortLang (Maybe Extlangs)
  | LongLangSec LongLang
  deriving stock (Eq, Ord, Show)
  deriving (ToSubtags) via WrappedToSubtagsNE LangSec

instance ToSubtagsNE LangSec where
  toSubtagsNE (ShortLangSec sl mextl) = toSubtag sl :| toSubtags mextl
  toSubtagsNE (LongLangSec ll) = toSubtagsNE ll

-- | A short language subtag; a subtag that is either two or three letters long.
-- All of the registered language subtags are currently short language subtags.
newtype ShortLang = ShortLang Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSubtags, ToSubtagsNE)

instance IsSubtag ShortLang where
  toSubtag (ShortLang s) = s
  fromSubtag st
    | len == 2 || len == 3,
      containsOnlyLetters st =
      Just $ ShortLang st
    | otherwise = Nothing
    where
      len = subtagLength st

-- | A long language subtag; a subtag that is between four and eight letters
-- long. While tags with these types of primary language subtags are all
-- well-formed, there are currently no such subtags registered, so those tags
-- are currently all invalid.
newtype LongLang = LongLang Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSubtags, ToSubtagsNE)

instance IsSubtag LongLang where
  toSubtag (LongLang st) = st
  fromSubtag st
    | subtagLength st >= 4,
      containsOnlyLetters st =
      Just $ LongLang st
    | otherwise = Nothing

-- | A possible primary language subtag in a normal BCP47 tag

-- TODO: I think I want Language to be a plain Language = Language Subtag, then
-- have a distinguishLanguage :: Language -> Either LongLang ShortLang, or
-- something like that. Maybe have a distinct newtype Lang = Lang Subtag in
-- analogy with ShortLang and LongLang.

data Language
  = ShortLanguage ShortLang
  | LongLanguage LongLang
  deriving stock (Eq, Ord, Show)
  deriving (ToSubtags, ToSubtagsNE) via WrappedIsSubtag Language

instance IsSubtag Language where
  toSubtag (ShortLanguage (ShortLang st)) = st
  toSubtag (LongLanguage (LongLang st)) = st
  fromSubtag st
    | containsDigit st = Nothing
    | subtagLength st >= 4 = Just $ LongLanguage $ LongLang st
    | subtagLength st >= 2 = Just $ ShortLanguage $ ShortLang st
    | otherwise = Nothing

-- | An extended langauge subtag; a subtag that is exactly three letters long.
newtype Extlang = Extlang Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSubtags, ToSubtagsNE)

-- | Every well-formed extended language subtag is also a well-formed short
-- language subtag
extlangToShortLang :: Extlang -> ShortLang
extlangToShortLang (Extlang st) = ShortLang st

instance IsSubtag Extlang where
  toSubtag (Extlang s) = s
  fromSubtag st
    | subtagLength st == 3,
      containsOnlyLetters st =
      Just $ Extlang st
    | otherwise = Nothing

-- | The possible arrangements of extended language subtags that can exist in a
-- well-formed tag. After the first 'Extlang', if it exists, between zero and
-- two 'Extlang' subtags may follow. In practice it is unlikely that there will
-- be more than one 'Extlang' subtag in a tag; among other reasons, any such tag
-- is automatically invalid, though still well-formed. The one exception is the
-- regular grandfathered tag "LanguageTag.BCP47.LegacyTag.ZhMinNan", which is
-- given special treatment by this library. Additionally, the standard
-- recommends that tags not have any 'Extlang' subtags in them at all; it is
-- better to use, e.g., the tag @cmn@ instead of the tag @zh-cmn@, unless you
-- are working with tools that are not aware of macrolanguage relationships or
-- otherwise expect the extlang form of tags. See
-- <https://www.rfc-editor.org/rfc/rfc5646.html#section-4.1.2> for details.
data Extlangs = Extlangs
  { firstExtlang :: Extlang,
    otherExtlangs :: Maybe (Extlang, Maybe Extlang)
  }
  deriving stock (Eq, Ord, Show)
  deriving (ToSubtags) via WrappedToSubtagsNE Extlangs

instance ToSubtagsNE Extlangs where
  toSubtagsNE (Extlangs e1 me23) = toSubtag e1 :| rest
    where
      rest = maybe [] (\(e2, me3) -> toSubtag e2 : maybe [] ((: []) . toSubtag) me3) me23
