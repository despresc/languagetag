-- |
-- Description : Redundant record definitions
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Internal definitions for the records in the registry for 'Redundant' tags

-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home #-}

module LanguageTag.Internal.BCP47.Registry.RedundantRecords
  (lookupRedundantDetails, redundantToValidNormal, redundantToSyntaxNormal, lookupRedundantRecord, redundantDetails) where

import Prelude hiding (LT, GT)
import LanguageTag.BCP47.LegacyTag (Redundant(..))
import LanguageTag.Internal.BCP47.Registry.Types
import Data.List.NonEmpty (NonEmpty(..))
import qualified LanguageTag.Internal.BCP47.Syntax as Syn
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import LanguageTag.BCP47.Subtag (nullSubtag, justSubtag)
import LanguageTag.Internal.BCP47.Registry.Script
import LanguageTag.Internal.BCP47.Registry.Language
import LanguageTag.Internal.BCP47.Registry.Region
import LanguageTag.Internal.BCP47.Registry.Extlang
import LanguageTag.Internal.BCP47.Registry.Variant
import LanguageTag.Internal.BCP47.Subtag (Subtag(..))

-- | All of the records for 'Redundant' tags, together with their corresponding valid and well-formed forms, occurring in the same order as that type's constructors
redundantDetails :: Vector (Normal, Syn.Normal, RangeRecord)
redundantDetails = V.fromList
  [(Normal Az Nothing (Just Arab) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14116533031992819714) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953028)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Arabic script" :| []) NotDeprecated)
  ,(Normal Az Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14116533031992819714) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708036)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Az Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14116533031992819714) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Latin script" :| []) NotDeprecated)
  ,(Normal Be Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14237004322024980482) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Belarusian in Latin script" :| []) NotDeprecated)
  ,(Normal Bs Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14252766920720777218) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708036)) nullSubtag [] [] [], RangeRecord ("Bosnian in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Bs Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14252766920720777218) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Bosnian in Latin script" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing Nothing (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 7126246090126393348] [] [], RangeRecord ("German, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing Nothing (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 7126325598560976900] [] [], RangeRecord ("German, orthography of 1996" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just AT) (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763970)) [Subtag 7126246090126393348] [] [], RangeRecord ("German, Austrian variant, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just AT) (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763970)) [Subtag 7126325598560976900] [] [], RangeRecord ("German, Austrian variant, orthography of 1996" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just CH) (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364226)) [Subtag 7126246090126393348] [] [], RangeRecord ("German, Swiss variant, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just CH) (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364226)) [Subtag 7126325598560976900] [] [], RangeRecord ("German, Swiss variant, orthography of 1996" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just DE) (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692226)) [Subtag 7126246090126393348] [] [], RangeRecord ("German, German variant, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just DE) (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692226) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692226)) [Subtag 7126325598560976900] [] [], RangeRecord ("German, German variant, orthography of 1996" :| []) NotDeprecated)
  ,(Normal En Nothing Nothing Nothing (S.fromList [Boont]) M.empty [], Syn.Normal (Subtag 14679482985414131714) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 14249247308838338565] [] [], RangeRecord ("Boontling" :| []) NotDeprecated)
  ,(Normal En Nothing Nothing Nothing (S.fromList [Scouse]) M.empty [], Syn.Normal (Subtag 14679482985414131714) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 16685695188168867846] [] [], RangeRecord ("Scouse" :| []) NotDeprecated)
  ,(Normal Es Nothing Nothing (Just Reg419) (S.fromList []) M.empty [], Syn.Normal (Subtag 14685112484948344834) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 7549660252682059779)) [] [] [], RangeRecord ("Latin American Spanish" :| []) NotDeprecated)
  ,(Normal Iu Nothing (Just Cans) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15263825037065453570) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14377591383445733380)) nullSubtag [] [] [], RangeRecord ("Inuktitut in Canadian Aboriginal Syllabic script" :| []) NotDeprecated)
  ,(Normal Iu Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15263825037065453570) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Inuktitut in Latin script" :| []) NotDeprecated)
  ,(Normal Mn Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15832404490020978690) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708036)) nullSubtag [] [] [], RangeRecord ("Mongolian in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Mn Nothing (Just Mong) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15832404490020978690) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15834505038266368004)) nullSubtag [] [] [], RangeRecord ("Mongolian in Mongolian script" :| []) NotDeprecated)
  ,(Normal Sgn Nothing Nothing (Just BR) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14251641020813934594)) [] [] [], RangeRecord ("Brazilian Sign Language" :| []) (DeprecatedPreferred $ Normal Bzs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just CO) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14392378509169262594)) [] [] [], RangeRecord ("Colombian Sign Language" :| []) (DeprecatedPreferred $ Normal Csn Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just DE) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692226)) [] [] [], RangeRecord ("German Sign Language" :| []) (DeprecatedPreferred $ Normal Gsg Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just DK) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14531990097617747970)) [] [] [], RangeRecord ("Danish Sign Language" :| []) (DeprecatedPreferred $ Normal Dsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just ES) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14685112484948344834)) [] [] [], RangeRecord ("Spanish Sign Language" :| []) (DeprecatedPreferred $ Normal Ssp Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just FR) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14828101773117358082)) [] [] [], RangeRecord ("French Sign Language" :| []) (DeprecatedPreferred $ Normal Fsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just GB) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14954202562683731970)) [] [] [], RangeRecord ("British Sign Language" :| []) (DeprecatedPreferred $ Normal Bfi Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just GR) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14972216961193213954)) [] [] [], RangeRecord ("Greek Sign Language" :| []) (DeprecatedPreferred $ Normal Gss Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just IE) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15245810638555971586)) [] [] [], RangeRecord ("Irish Sign Language" :| []) (DeprecatedPreferred $ Normal Isg Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just IT) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15262699137158610946)) [] [] [], RangeRecord ("Italian Sign Language" :| []) (DeprecatedPreferred $ Normal Ise Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just JP) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15402310725607096322)) [] [] [], RangeRecord ("Japanese Sign Language" :| []) (DeprecatedPreferred $ Normal Jsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just MX) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15843663489089404930)) [] [] [], RangeRecord ("Mexican Sign Language" :| []) (DeprecatedPreferred $ Normal Mfs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just NI) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15970890178562621442)) [] [] [], RangeRecord ("Nicaraguan Sign Language" :| []) (DeprecatedPreferred $ Normal Ncs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just NL) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15974267878283149314)) [] [] [], RangeRecord ("Dutch Sign Language" :| []) (DeprecatedPreferred $ Normal Dse Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just NO) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15977645578003677186)) [] [] [], RangeRecord ("Norwegian Sign Language" :| []) (DeprecatedPreferred $ Normal Nsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just PT) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16271505453689602050)) [] [] [], RangeRecord ("Portuguese Sign Language" :| []) (DeprecatedPreferred $ Normal Psr Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just SE) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16686962519314530306)) [] [] [], RangeRecord ("Swedish Sign Language" :| []) (DeprecatedPreferred $ Normal Swl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just US) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16990955494162038786)) [] [] [], RangeRecord ("American Sign Language" :| []) (DeprecatedPreferred $ Normal Ase Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just ZA) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658435) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 17691265236218150914)) [] [] [], RangeRecord ("South African Sign Language" :| []) (DeprecatedPreferred $ Normal Sfs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sl Nothing Nothing Nothing (S.fromList [Nedis]) M.empty [], Syn.Normal (Subtag 16694843818662428674) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 15967273465522683909] [] [], RangeRecord ("Natisone dialect, Nadiza dialect" :| []) NotDeprecated)
  ,(Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty [], Syn.Normal (Subtag 16694843818662428674) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 16555186176353370117] [] [], RangeRecord ("Resian, Resianic, Rezijan" :| []) NotDeprecated)
  ,(Normal Sr Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16701599218103484418) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708036)) nullSubtag [] [] [], RangeRecord ("Serbian in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Sr Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16701599218103484418) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Serbian in Latin script" :| []) NotDeprecated)
  ,(Normal Tg Nothing (Just Arab) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16833329507204071426) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953028)) nullSubtag [] [] [], RangeRecord ("Tajik in Arabic script" :| []) NotDeprecated)
  ,(Normal Tg Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16833329507204071426) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708036)) nullSubtag [] [] [], RangeRecord ("Tajik in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Uz Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16998836793509937154) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708036)) nullSubtag [] [] [], RangeRecord ("Uzbek in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Uz Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16998836793509937154) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Uzbek in Latin script" :| []) NotDeprecated)
  ,(Normal Yi Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17556157247397036034) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185796)) nullSubtag [] [] [], RangeRecord ("Yiddish, in Latin script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) nullSubtag [] [] [], RangeRecord ("simplified Chinese" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just CN) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) (justSubtag (Subtag 14391252609262419970)) [] [] [], RangeRecord ("PRC Mainland Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just HK) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) (justSubtag (Subtag 15108450849921171458)) [] [] [], RangeRecord ("Hong Kong Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just MO) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) (justSubtag (Subtag 15833530389927821314)) [] [] [], RangeRecord ("Macao Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just SG) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) (justSubtag (Subtag 16689214319128215554)) [] [] [], RangeRecord ("Singapore Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just TW) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) (justSubtag (Subtag 16851343905713553410)) [] [] [], RangeRecord ("Taiwan Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) nullSubtag [] [] [], RangeRecord ("traditional Chinese" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just CN) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) (justSubtag (Subtag 14391252609262419970)) [] [] [], RangeRecord ("PRC Mainland Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just HK) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) (justSubtag (Subtag 15108450849921171458)) [] [] [], RangeRecord ("Hong Kong Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just MO) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) (justSubtag (Subtag 15833530389927821314)) [] [] [], RangeRecord ("Macao Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just SG) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) (justSubtag (Subtag 16689214319128215554)) [] [] [], RangeRecord ("Singapore Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just TW) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) (justSubtag (Subtag 16851343905713553410)) [] [] [], RangeRecord ("Taiwan Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh (Just ExtCmn) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) (justSubtag (Subtag 14391094279588020227)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Mandarin Chinese" :| []) (DeprecatedPreferred $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtCmn) (Just Hans) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) (justSubtag (Subtag 14391094279588020227)) nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012740)) nullSubtag [] [] [], RangeRecord ("Mandarin Chinese (Simplified)" :| []) (DeprecatedPreferred $ Normal Cmn Nothing (Just Hans) Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtCmn) (Just Hant) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) (justSubtag (Subtag 14391094279588020227)) nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489476)) nullSubtag [] [] [], RangeRecord ("Mandarin Chinese (Traditional)" :| []) (DeprecatedPreferred $ Normal Cmn Nothing (Just Hant) Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtGan) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) (justSubtag (Subtag 14954044233009332227)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Kan or Gan" :| []) (DeprecatedPreferred $ Normal Gan Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtWuu) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) (justSubtag (Subtag 17282466813011034115)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Shanghaiese or Wu" :| []) (DeprecatedPreferred $ Normal Wuu Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtYue) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049282) (justSubtag (Subtag 17570556451674390531)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Cantonese" :| []) (DeprecatedPreferred $ Normal Yue Nothing Nothing Nothing S.empty M.empty []))]

-- | Look up the tag and record details associated to the given 'Redundant' tag
lookupRedundantDetails :: Redundant -> (Normal, Syn.Normal, RangeRecord)
lookupRedundantDetails = V.unsafeIndex redundantDetails . fromEnum

-- | Convert a 'Redundant' tag to a 'Normal' validated tag
redundantToValidNormal :: Redundant -> Normal
redundantToValidNormal = (\(x, _, _) -> x) . lookupRedundantDetails

-- | Convert a 'Redundant' tag to a merely well-formed tag
redundantToSyntaxNormal :: Redundant -> Syn.Normal
redundantToSyntaxNormal = (\(_, y, _) -> y) . lookupRedundantDetails

-- | Look up the record associated to the given 'Redundant' tag
lookupRedundantRecord :: Redundant -> RangeRecord
lookupRedundantRecord = (\(_, _, z) -> z) . lookupRedundantDetails
