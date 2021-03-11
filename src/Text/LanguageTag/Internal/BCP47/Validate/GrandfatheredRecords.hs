-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.Validate.GrandfatheredRecords
  (lookupGrandfatheredRecord) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Validate.Grandfathered
import Text.LanguageTag.Internal.BCP47.Validate.RecordTypes
import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.Internal.BCP47.Validate.Language
import Text.LanguageTag.Internal.BCP47.Validate.Region
import Text.LanguageTag.Internal.BCP47.Validate.Variant

grandfatheredDetails :: Vector RangeRecord
grandfatheredDetails = V.fromList
  [(RangeRecord ("Lojban" :| []) (DeprecatedPreferred $ NormalTag $ Normal Jbo Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Gaulish" :| []) DeprecatedSimple)
  ,(RangeRecord ("English, Oxford English Dictionary spelling" :| []) (DeprecatedPreferred $ NormalTag $ Normal En Nothing Nothing (Just GB) (S.singleton Oxendict) M.empty []))
  ,(RangeRecord ("Amis" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ami Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Bunun" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bnn Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Default Language" :| []) NotDeprecated)
  ,(RangeRecord ("Enochian" :| []) DeprecatedSimple)
  ,(RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hak Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Klingon" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tlh Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Luxembourgish" :| []) (DeprecatedPreferred $ NormalTag $ Normal Lb Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Mingo" :| []) NotDeprecated)
  ,(RangeRecord ("Navajo" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nv Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Paiwan" :| []) (DeprecatedPreferred $ NormalTag $ Normal Pwn Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Tao" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tao Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Tayal" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tay Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Tsou" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tsu Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Norwegian Bokmal" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nb Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Norwegian Nynorsk" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nn Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Belgian-French Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sfb Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Belgian-Flemish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Vgt Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Swiss German Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sgg Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Mandarin or Standard Chinese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hak Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Min, Fuzhou, Hokkien, Amoy, or Taiwanese" :| []) DeprecatedSimple)
  ,(RangeRecord ("Minnan, Hokkien, Amoy, Taiwanese, Southern Min, Southern\nFujian, Hoklo, Southern Fukien, Ho-lo" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nan Nothing Nothing Nothing S.empty M.empty []))
  ,(RangeRecord ("Xiang or Hunanese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hsn Nothing Nothing Nothing S.empty M.empty []))]

-- | Look up the subtag and record details associated to the given 'Grandfathered' tag.
lookupGrandfatheredRecord :: Grandfathered -> RangeRecord
lookupGrandfatheredRecord= V.unsafeIndex grandfatheredDetails . fromEnum
