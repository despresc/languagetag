{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Well-formed script subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use: the values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Other components of the library may misbehave if
-- ill-formed values are given to them.
module LanguageTag.Internal.BCP47.Syntax.Script
  ( Script (..),
  )
where

import LanguageTag.BCP47.Subtag
  ( IsSubtag (..),
    Subtag,
    ToSubtags (..),
    ToSubtagsNE (..),
    containsOnlyLetters,
    subtagLength,
  )

-- | A script subtag; a subtag that is exactly four letters long.
newtype Script = Script Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSubtags, ToSubtagsNE)

instance IsSubtag Script where
  toSubtag (Script st) = st
  fromSubtag st
    | subtagLength st == 4,
      containsOnlyLetters st =
      Just $ Script st
    | otherwise = Nothing
