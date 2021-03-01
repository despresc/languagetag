-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.GrandfatheredRecords 
  (lookupGrandfatheredDetails, lookupSubtagGrandfathered) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Grandfathered
import Text.LanguageTag.Internal.BCP47.Validate
import Data.List.NonEmpty (NonEmpty(..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Internal.BCP47.Region
import Text.LanguageTag.Internal.BCP47.Variant

grandfatheredTable :: [(Grandfathered, Syn.LanguageTag, RangeRecord)]
grandfatheredTable =
  [(ArtLojban, Syn.RegularGrandfathered Syn.Artlojban, RangeRecord ("Lojban" :| []) (DeprecatedPreferred $ NormalTag $ Normal Jbo Nothing Nothing Nothing S.empty M.empty []))
  ,(CelGaulish, Syn.RegularGrandfathered Syn.Celgaulish, RangeRecord ("Gaulish" :| []) DeprecatedSimple)
  ,(EnGbOed, Syn.IrregularGrandfathered Syn.EnGBoed, RangeRecord ("English, Oxford English Dictionary spelling" :| []) (DeprecatedPreferred $ NormalTag $ Normal En Nothing Nothing (Just GB) (S.singleton Oxendict) M.empty []))
  ,(IAmi, Syn.IrregularGrandfathered Syn.Iami, RangeRecord ("Amis" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ami Nothing Nothing Nothing S.empty M.empty []))
  ,(IBnn, Syn.IrregularGrandfathered Syn.Ibnn, RangeRecord ("Bunun" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bnn Nothing Nothing Nothing S.empty M.empty []))
  ,(IDefault, Syn.IrregularGrandfathered Syn.Idefault, RangeRecord ("Default Language" :| []) NotDeprecated)
  ,(IEnochian, Syn.IrregularGrandfathered Syn.Ienochian, RangeRecord ("Enochian" :| []) DeprecatedSimple)
  ,(IHak, Syn.IrregularGrandfathered Syn.Ihak, RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hak Nothing Nothing Nothing S.empty M.empty []))
  ,(IKlingon, Syn.IrregularGrandfathered Syn.Iklingon, RangeRecord ("Klingon" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tlh Nothing Nothing Nothing S.empty M.empty []))
  ,(ILux, Syn.IrregularGrandfathered Syn.Ilux, RangeRecord ("Luxembourgish" :| []) (DeprecatedPreferred $ NormalTag $ Normal Lb Nothing Nothing Nothing S.empty M.empty []))
  ,(IMingo, Syn.IrregularGrandfathered Syn.Imingo, RangeRecord ("Mingo" :| []) NotDeprecated)
  ,(INavajo, Syn.IrregularGrandfathered Syn.Inavajo, RangeRecord ("Navajo" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nv Nothing Nothing Nothing S.empty M.empty []))
  ,(IPwn, Syn.IrregularGrandfathered Syn.Ipwn, RangeRecord ("Paiwan" :| []) (DeprecatedPreferred $ NormalTag $ Normal Pwn Nothing Nothing Nothing S.empty M.empty []))
  ,(ITao, Syn.IrregularGrandfathered Syn.Itao, RangeRecord ("Tao" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tao Nothing Nothing Nothing S.empty M.empty []))
  ,(ITay, Syn.IrregularGrandfathered Syn.Itay, RangeRecord ("Tayal" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tay Nothing Nothing Nothing S.empty M.empty []))
  ,(ITsu, Syn.IrregularGrandfathered Syn.Itsu, RangeRecord ("Tsou" :| []) (DeprecatedPreferred $ NormalTag $ Normal Tsu Nothing Nothing Nothing S.empty M.empty []))
  ,(NoBok, Syn.RegularGrandfathered Syn.Nobok, RangeRecord ("Norwegian Bokmal" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nb Nothing Nothing Nothing S.empty M.empty []))
  ,(NoNyn, Syn.RegularGrandfathered Syn.Nonyn, RangeRecord ("Norwegian Nynorsk" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nn Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnBeFr, Syn.IrregularGrandfathered Syn.SgnBEFR, RangeRecord ("Belgian-French Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sfb Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnBeNl, Syn.IrregularGrandfathered Syn.SgnBENL, RangeRecord ("Belgian-Flemish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Vgt Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnChDe, Syn.IrregularGrandfathered Syn.SgnCHDE, RangeRecord ("Swiss German Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sgg Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhGuoyu, Syn.RegularGrandfathered Syn.Zhguoyu, RangeRecord ("Mandarin or Standard Chinese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhHakka, Syn.RegularGrandfathered Syn.Zhhakka, RangeRecord ("Hakka" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hak Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhMin, Syn.RegularGrandfathered Syn.Zhmin, RangeRecord ("Min, Fuzhou, Hokkien, Amoy, or Taiwanese" :| []) DeprecatedSimple)
  ,(ZhMinNan, Syn.RegularGrandfathered Syn.Zhminnan, RangeRecord ("Minnan, Hokkien, Amoy, Taiwanese, Southern Min, Southern\nFujian, Hoklo, Southern Fukien, Ho-lo" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nan Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhXiang, Syn.RegularGrandfathered Syn.Zhxiang, RangeRecord ("Xiang or Hunanese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Hsn Nothing Nothing Nothing S.empty M.empty []))]

lookupGrandfatheredDetails :: Grandfathered -> (Syn.LanguageTag, RangeRecord)
lookupGrandfatheredDetails x = case HM.lookup x tab of
  Nothing -> error $ "internal invariant violated: subtag " <> show x <> " does not have an associated record"
  Just r -> r
  where
    tab = HM.fromList $ (\(a, b, c) -> (a, (b, c))) <$> grandfatheredTable

lookupSubtagGrandfathered :: Syn.LanguageTag -> Maybe Grandfathered
lookupSubtagGrandfathered = flip HM.lookup tab
  where
    tab = HM.fromList $ (\(a, b, _) -> (b, a)) <$> grandfatheredTable
