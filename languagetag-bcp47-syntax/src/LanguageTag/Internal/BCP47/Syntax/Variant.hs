{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Well-formed variant subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: the data constructors exported from this module are
-- unsafe to use: the values they take are expected by the rest of the
-- library to satisfy particular invariants that the type does not
-- enforce. Other components of the library may misbehave if
-- ill-formed values are given to them.
module LanguageTag.Internal.BCP47.Syntax.Variant
  ( Variant (..),
  )
where

import Data.Coerce (coerce)
import LanguageTag.BCP47.Subtag
  ( IsSubtag (..),
    Subtag,
    ToSubtags (..),
    ToSubtagsNE (..),
    subtagHeadIsDigit,
    subtagLength,
  )

-- | A variant subtag; a subtag that is at least four characters long, and if
-- four then also starts with a digit.
newtype Variant = Variant Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToSubtags, ToSubtagsNE)

instance IsSubtag Variant where
  toSubtag = coerce
  fromSubtag st
    | len >= 5 = Just $ Variant st
    | len == 4,
      subtagHeadIsDigit st =
      Just $ Variant st
    | otherwise = Nothing
    where
      len = subtagLength st
