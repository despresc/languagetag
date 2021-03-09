-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.GrandfatheredRecords
  (lookupGrandfatheredDetails) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Validate
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry.Language
import Text.LanguageTag.BCP47.Registry.Region
import Text.LanguageTag.BCP47.Registry.Variant

grandfatheredTable :: [(Grandfathered, RangeRecord)]
grandfatheredTable =
  [(ArtLojban, RangeRecord ("Lojban" :| []) (DeprecatedPreferred $ NormalTag $ Normal Jbo Nothing Nothing Nothing S.empty M.empty []))
  ,(CelGaulish, RangeRecord ("Gaulish" :| []) DeprecatedSimple)
  ,(EnGbOed, RangeRecord ("English, Oxford English Dictionary spelling" :| []) (DeprecatedPreferred $ NormalTag $ Normal En Nothing Nothing (Just GB) (S.singleton Oxendict) M.empty []))
  ,(IAmi, RangeRecord ("Amis" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ami Nothing Nothing Nothing S.empty M.empty []))
  ,(IBnn, RangeRecord ("Bunun" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bnn Nothing Nothing Nothing S.empty M.empty []))
  ,(IDefault, RangeRecord ("Default Language" :| []) NotDeprecated)
  ,(IEnochian, RangeRecord ("Enochian" :| []) DeprecatedSimple)
  ,(IHak, RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hak Nothing Nothing Nothing S.empty M.empty []))
  ,(IKlingon, RangeRecord ("Klingon" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tlh Nothing Nothing Nothing S.empty M.empty []))
  ,(ILux, RangeRecord ("Luxembourgish" :| []) (DeprecatedPreferred $ NormalTag $ Normal Lb Nothing Nothing Nothing S.empty M.empty []))
  ,(IMingo, RangeRecord ("Mingo" :| []) NotDeprecated)
  ,(INavajo, RangeRecord ("Navajo" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nv Nothing Nothing Nothing S.empty M.empty []))
  ,(IPwn, RangeRecord ("Paiwan" :| []) (DeprecatedPreferred $ NormalTag $ Normal Pwn Nothing Nothing Nothing S.empty M.empty []))
  ,(ITao, RangeRecord ("Tao" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tao Nothing Nothing Nothing S.empty M.empty []))
  ,(ITay, RangeRecord ("Tayal" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tay Nothing Nothing Nothing S.empty M.empty []))
  ,(ITsu, RangeRecord ("Tsou" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tsu Nothing Nothing Nothing S.empty M.empty []))
  ,(NoBok, RangeRecord ("Norwegian Bokmal" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nb Nothing Nothing Nothing S.empty M.empty []))
  ,(NoNyn, RangeRecord ("Norwegian Nynorsk" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nn Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnBeFr, RangeRecord ("Belgian-French Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sfb Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnBeNl, RangeRecord ("Belgian-Flemish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Vgt Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnChDe, RangeRecord ("Swiss German Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sgg Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhGuoyu, RangeRecord ("Mandarin or Standard Chinese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhHakka, RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hak Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhMin, RangeRecord ("Min, Fuzhou, Hokkien, Amoy, or Taiwanese" :| []) DeprecatedSimple)
  ,(ZhMinNan, RangeRecord ("Minnan, Hokkien, Amoy, Taiwanese, Southern Min, Southern\nFujian, Hoklo, Southern Fukien, Ho-lo" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nan Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhXiang, RangeRecord ("Xiang or Hunanese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hsn Nothing Nothing Nothing S.empty M.empty []))]

lookupGrandfatheredDetails :: Grandfathered -> RangeRecord
lookupGrandfatheredDetails x = case HM.lookup x tab of
  Just r -> r
  Nothing -> error $ "internal invariant violated: subtag " <> show x <> " does not have an associated record"
  where
    tab = HM.fromList  grandfatheredTable
