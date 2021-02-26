{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- 'BCP47' value. Also exported are the data types representing all
-- the tags in the IANA registry, the current version of which is
-- available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Validate
  ( -- * The registered subtags
    -- $thetags
    bcp47RegistryDate,
    Language (..),
    Extlang (..),
    Script (..),
    Region (..),
    Variant (..),
    Grandfathered (..),
    Redundant (..),
  )
where

import Text.LanguageTag.Internal.BCP47.Extlang
import Text.LanguageTag.Internal.BCP47.Grandfathered
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Internal.BCP47.Redundant
import Text.LanguageTag.Internal.BCP47.Region
import Text.LanguageTag.Internal.BCP47.RegistryDate
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.Variant

-- $thetags
--
-- Below are all of the subtags that are registered with the
-- IANA. Check 'bcp47RegistryDate' for the version of the registry
-- that this library uses.
