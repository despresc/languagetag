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
module LanguageTag.BCP47.Syntax.SingletonSection
  ( ExtensionSingleton (..),
    ExtensionSubtag,
    Extension (..),
    Singleton (..),
  )
where

import LanguageTag.Internal.BCP47.Syntax.SingletonSection
