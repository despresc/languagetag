{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Well-formed region subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use: the values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Other components of the library may misbehave if
-- ill-formed values are given to them.
module LanguageTag.Internal.BCP47.Syntax.Region
  ( Region (..),
  )
where

import Data.Coerce (coerce)
import LanguageTag.BCP47.Subtag
  ( IsSubtag (..),
    Subtag,
    ToSubtags (..),
    ToSubtagsNE (..),
    containsOnlyDigits,
    containsOnlyLetters,
    subtagLength,
  )

-- | A script subtag; a subtag that is either two letters or three digits.
newtype Region = Region Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSubtags, ToSubtagsNE)

instance IsSubtag Region where
  toSubtag = coerce
  fromSubtag st
    | len == 2,
      containsOnlyLetters st =
      Just $ Region st
    | len == 3,
      containsOnlyDigits st =
      Just $ Region st
    | otherwise = Nothing
    where
      len = subtagLength st
