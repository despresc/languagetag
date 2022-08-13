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
module LanguageTag.Internal.BCP47.Syntax.SingletonSection
  ( ExtensionSingleton (..),
    ExtensionSubtag (..),
    Extension (..),
    Singleton (..),
    subtagX,
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.Coerce (coerce)
import Data.Hashable (Hashable (..), hashUsing)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import LanguageTag.BCP47.Subtag
  ( IsSubtag (..),
    Subtag,
    ToSubtags (..),
    ToSubtagsNE (..),
    WrappedIsSubtag (..),
    WrappedToSubtagsNE (..),
    singleton,
    subtagHead,
    subtagLength,
  )
import LanguageTag.Internal.BCP47.Subtag (Subtag (..), SubtagChar (..))

-- TODO: remove the duplicate types in Internal.BCP47.Syntax

-- | The possible single character extensions; all the ASCII
-- alphanumeric characters (case-insensitive) except the letter X.
data ExtensionSingleton
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
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (ToSubtags, ToSubtagsNE) via WrappedIsSubtag ExtensionSingleton

instance Hashable ExtensionSingleton where
  hashWithSalt = hashUsing fromEnum

instance NFData ExtensionSingleton where
  rnf = rwhnf

instance IsSubtag ExtensionSingleton where
  toSubtag ec
    | ec <= Ext9 = toC 48 ec
    | ec <= ExtW = toC 87 ec
    | otherwise = toC 88 ec
    where
      toC n = singleton . SubtagChar . fromIntegral . (+ n) . fromEnum
  fromSubtag st
    | subtagLength st > 1 = Nothing
    | c < 58 = toC 48 c
    | c < 120 = toC 87 c
    | c > 120 = toC 88 c
    | otherwise = Nothing
    where
      SubtagChar c = subtagHead st
      toC x = Just . toEnum . subtract x . fromEnum

newtype ExtensionSubtag = ExtensionSubtag Subtag
  deriving stock (Eq, Ord, Show)
  deriving newtype (NFData, Hashable, ToSubtags)

instance IsSubtag ExtensionSubtag where
  toSubtag = coerce
  fromSubtag st
    | subtagLength st >= 2 = Just $ ExtensionSubtag st
    | otherwise = Nothing

-- | An extension section in a language tag
data Extension = Extension
  { extensionSingleton :: ExtensionSingleton,
    extensionTags :: NonEmpty ExtensionSubtag
  }
  deriving stock (Eq, Ord, Show)
  deriving (ToSubtags) via WrappedToSubtagsNE Extension

instance NFData Extension where
  rnf (Extension x y) = rnf x `seq` rnf y

instance Hashable Extension where
  hashWithSalt s (Extension c t) =
    s `hashWithSalt` c `hashWithSalt` t

instance ToSubtagsNE Extension where
  toSubtagsNE (Extension x y) = toSubtag x :| (NE.toList $ fmap toSubtag y)

data Singleton
  = PrivateUseSingleton
  | OtherSingleton ExtensionSingleton
  deriving (Eq, Ord, Show)
  deriving (ToSubtags, ToSubtagsNE) via WrappedIsSubtag Singleton

instance IsSubtag Singleton where
  toSubtag PrivateUseSingleton = subtagX
  toSubtag (OtherSingleton e) = toSubtag e
  fromSubtag st
    | subtagLength st > 1 = Nothing
    | st == subtagX = Just PrivateUseSingleton
    | c < 58 = toC 48 c
    | c < 120 = toC 87 c
    | otherwise = toC 88 c
    where
      SubtagChar c = subtagHead st
      toC x = Just . OtherSingleton . toEnum . subtract x . fromEnum

-- | The subtag @\"x\"@
subtagX :: Subtag
subtagX = Subtag 17293822569102704641
