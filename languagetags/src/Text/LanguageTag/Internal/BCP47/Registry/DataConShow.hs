{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Description : Showing the data constructors associated to subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning\: this is an internal module and may change or disappear
-- without regard to the PVP.
--
-- This module exists because, as it turns out, deriving a 'Show'
-- instance for a data type with over eight thousand constructors
-- takes a very long time. Writing the registered subtag types' show
-- instances manually using the functions in this module shaves off a
-- decent amount of compilation time, and these functions can also be
-- used to render the constructors during code generation.
module Text.LanguageTag.Internal.BCP47.Registry.DataConShow where

import Data.Char (isDigit)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Text.LanguageTag.BCP47.Subtag

-- | Show a subtag in the style of a language data constructor
languageConShow :: Subtag -> String
languageConShow = TL.unpack . TB.toLazyText . renderSubtagBuilderTitle

-- | Show a subtag in the style of an extlang data constructor
extlangConShow :: Subtag -> String
extlangConShow = ("Ext" <>) . TL.unpack . TB.toLazyText . renderSubtagBuilderTitle

-- | Show a subtag in the style of a script data constructor
scriptConShow :: Subtag -> String
scriptConShow = TL.unpack . TB.toLazyText . renderSubtagBuilderTitle

-- | Show a subtag in the style of a region data constructor
regionConShow :: Subtag -> String
regionConShow = go . TL.unpack . TB.toLazyText . renderSubtagBuilderUpper
  where
    go t@(x : _)
      | isDigit x = "Reg" <> t
      | otherwise = t
    go [] = []

-- | Show a subtag in the style of a variant data constructor
variantConShow :: Subtag -> String
variantConShow = go . TL.unpack . TB.toLazyText . renderSubtagBuilderTitle
  where
    go t@(x : _)
      | isDigit x = "Var" <> t
      | otherwise = t
    go [] = []
