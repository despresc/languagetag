-- |
-- Description : Internal Grandfathered records
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Internal definitions for the records in the registry for 'Grandfathered' tags

-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home #-}

module LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords
  (lookupGrandfatheredRecord, grandfatheredDetails) where

import Prelude hiding (LT, GT)
import LanguageTag.Internal.BCP47.Registry.Grandfathered
import LanguageTag.Internal.BCP47.Registry.Types
import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import LanguageTag.Internal.BCP47.Registry.Language
import LanguageTag.Internal.BCP47.Registry.Region
import LanguageTag.Internal.BCP47.Registry.Variant

-- | All of the records for 'Grandfathered' tags, occurring in the same order as that type's constructors
grandfatheredDetails :: Vector RangeRecord
grandfatheredDetails = V.fromList
  [RangeRecord ("Lojban" :| []) (DeprecatedPreferred $ Normal Jbo Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Gaulish" :| []) DeprecatedSimple
  ,RangeRecord ("English, Oxford English Dictionary spelling" :| []) (DeprecatedPreferred $ Normal En Nothing Nothing (Just GB) (S.singleton Oxendict) M.empty [])
  ,RangeRecord ("Amis" :| []) (DeprecatedPreferred $ Normal Ami Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Bunun" :| []) (DeprecatedPreferred $ Normal Bnn Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Default Language" :| []) NotDeprecated
  ,RangeRecord ("Enochian" :| []) DeprecatedSimple
  ,RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ Normal Hak Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Klingon" :| []) (DeprecatedPreferred $ Normal Tlh Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Luxembourgish" :| []) (DeprecatedPreferred $ Normal Lb Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Mingo" :| []) NotDeprecated
  ,RangeRecord ("Navajo" :| []) (DeprecatedPreferred $ Normal Nv Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Paiwan" :| []) (DeprecatedPreferred $ Normal Pwn Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Tao" :| []) (DeprecatedPreferred $ Normal Tao Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Tayal" :| []) (DeprecatedPreferred $ Normal Tay Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Tsou" :| []) (DeprecatedPreferred $ Normal Tsu Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Norwegian Bokmal" :| []) (DeprecatedPreferred $ Normal Nb Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Norwegian Nynorsk" :| []) (DeprecatedPreferred $ Normal Nn Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Belgian-French Sign Language" :| []) (DeprecatedPreferred $ Normal Sfb Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Belgian-Flemish Sign Language" :| []) (DeprecatedPreferred $ Normal Vgt Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Swiss German Sign Language" :| []) (DeprecatedPreferred $ Normal Sgg Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Mandarin or Standard Chinese" :| []) (DeprecatedPreferred $ Normal Cmn Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ Normal Hak Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Min, Fuzhou, Hokkien, Amoy, or Taiwanese" :| []) DeprecatedSimple
  ,RangeRecord ("Minnan, Hokkien, Amoy, Taiwanese, Southern Min, Southern\nFujian, Hoklo, Southern Fukien, Ho-lo" :| []) (DeprecatedPreferred $ Normal Nan Nothing Nothing Nothing S.empty M.empty [])
  ,RangeRecord ("Xiang or Hunanese" :| []) (DeprecatedPreferred $ Normal Hsn Nothing Nothing Nothing S.empty M.empty [])]

-- | Look up the record details associated to the given 'Grandfathered' tag.
lookupGrandfatheredRecord :: Grandfathered -> RangeRecord
lookupGrandfatheredRecord = V.unsafeIndex grandfatheredDetails . fromEnum
