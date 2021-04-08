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

module Text.LanguageTag.Internal.BCP47.Registry.RedundantRecords
  (lookupRedundantDetails, redundantToValidNormal, redundantToSyntaxNormal, lookupRedundantRecord, redundantDetails) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Registry.Redundant
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Data.List.NonEmpty (NonEmpty(..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.BCP47.Subtag (nullSubtag, justSubtag)
import Text.LanguageTag.Internal.BCP47.Registry.Script
import Text.LanguageTag.Internal.BCP47.Registry.Language
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.Extlang
import Text.LanguageTag.Internal.BCP47.Registry.Variant
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag(..))

-- | All of the records for 'Redundant' tags, together with their corresponding valid and well-formed forms, occurring in the same order as that type's constructors
redundantDetails :: Vector (Normal, Syn.Normal, RangeRecord)
redundantDetails = V.fromList
  [(Normal Az Nothing (Just Arab) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14116533031992819730) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953044)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Arabic script" :| []) NotDeprecated)
  ,(Normal Az Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14116533031992819730) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Az Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14116533031992819730) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Latin script" :| []) NotDeprecated)
  ,(Normal Be Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14237004322024980498) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Belarusian in Latin script" :| []) NotDeprecated)
  ,(Normal Bs Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14252766920720777234) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Bosnian in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Bs Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 14252766920720777234) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Bosnian in Latin script" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing Nothing (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 7126246090126393380] [] [], RangeRecord ("German, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing Nothing (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 7126325598560976932] [] [], RangeRecord ("German, orthography of 1996" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just AT) (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763986)) [Subtag 7126246090126393380] [] [], RangeRecord ("German, Austrian variant, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just AT) (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763986)) [Subtag 7126325598560976932] [] [], RangeRecord ("German, Austrian variant, orthography of 1996" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just CH) (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364242)) [Subtag 7126246090126393380] [] [], RangeRecord ("German, Swiss variant, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just CH) (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364242)) [Subtag 7126325598560976932] [] [], RangeRecord ("German, Swiss variant, orthography of 1996" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just DE) (S.fromList [Var1901]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) [Subtag 7126246090126393380] [] [], RangeRecord ("German, German variant, traditional orthography" :| []) NotDeprecated)
  ,(Normal De Nothing Nothing (Just DE) (S.fromList [Var1996]) M.empty [], Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) [Subtag 7126325598560976932] [] [], RangeRecord ("German, German variant, orthography of 1996" :| []) NotDeprecated)
  ,(Normal En Nothing Nothing Nothing (S.fromList [Boont]) M.empty [], Syn.Normal (Subtag 14679482985414131730) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 14249247308838338581] [] [], RangeRecord ("Boontling" :| []) NotDeprecated)
  ,(Normal En Nothing Nothing Nothing (S.fromList [Scouse]) M.empty [], Syn.Normal (Subtag 14679482985414131730) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 16685695188168867862] [] [], RangeRecord ("Scouse" :| []) NotDeprecated)
  ,(Normal Es Nothing Nothing (Just Reg419) (S.fromList []) M.empty [], Syn.Normal (Subtag 14685112484948344850) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 7549660252682059811)) [] [] [], RangeRecord ("Latin American Spanish" :| []) NotDeprecated)
  ,(Normal Iu Nothing (Just Cans) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15263825037065453586) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14377591383445733396)) nullSubtag [] [] [], RangeRecord ("Inuktitut in Canadian Aboriginal Syllabic script" :| []) NotDeprecated)
  ,(Normal Iu Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15263825037065453586) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Inuktitut in Latin script" :| []) NotDeprecated)
  ,(Normal Mn Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15832404490020978706) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Mongolian in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Mn Nothing (Just Mong) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 15832404490020978706) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15834505038266368020)) nullSubtag [] [] [], RangeRecord ("Mongolian in Mongolian script" :| []) NotDeprecated)
  ,(Normal Sgn Nothing Nothing (Just BR) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14251641020813934610)) [] [] [], RangeRecord ("Brazilian Sign Language" :| []) (DeprecatedPreferred $ Normal Bzs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just CO) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14392378509169262610)) [] [] [], RangeRecord ("Colombian Sign Language" :| []) (DeprecatedPreferred $ Normal Csn Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just DE) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) [] [] [], RangeRecord ("German Sign Language" :| []) (DeprecatedPreferred $ Normal Gsg Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just DK) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14531990097617747986)) [] [] [], RangeRecord ("Danish Sign Language" :| []) (DeprecatedPreferred $ Normal Dsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just ES) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14685112484948344850)) [] [] [], RangeRecord ("Spanish Sign Language" :| []) (DeprecatedPreferred $ Normal Ssp Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just FR) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14828101773117358098)) [] [] [], RangeRecord ("French Sign Language" :| []) (DeprecatedPreferred $ Normal Fsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just GB) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14954202562683731986)) [] [] [], RangeRecord ("British Sign Language" :| []) (DeprecatedPreferred $ Normal Bfi Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just GR) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14972216961193213970)) [] [] [], RangeRecord ("Greek Sign Language" :| []) (DeprecatedPreferred $ Normal Gss Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just IE) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15245810638555971602)) [] [] [], RangeRecord ("Irish Sign Language" :| []) (DeprecatedPreferred $ Normal Isg Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just IT) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15262699137158610962)) [] [] [], RangeRecord ("Italian Sign Language" :| []) (DeprecatedPreferred $ Normal Ise Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just JP) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15402310725607096338)) [] [] [], RangeRecord ("Japanese Sign Language" :| []) (DeprecatedPreferred $ Normal Jsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just MX) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15843663489089404946)) [] [] [], RangeRecord ("Mexican Sign Language" :| []) (DeprecatedPreferred $ Normal Mfs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just NI) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15970890178562621458)) [] [] [], RangeRecord ("Nicaraguan Sign Language" :| []) (DeprecatedPreferred $ Normal Ncs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just NL) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15974267878283149330)) [] [] [], RangeRecord ("Dutch Sign Language" :| []) (DeprecatedPreferred $ Normal Dse Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just NO) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15977645578003677202)) [] [] [], RangeRecord ("Norwegian Sign Language" :| []) (DeprecatedPreferred $ Normal Nsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just PT) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16271505453689602066)) [] [] [], RangeRecord ("Portuguese Sign Language" :| []) (DeprecatedPreferred $ Normal Psr Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just SE) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16686962519314530322)) [] [] [], RangeRecord ("Swedish Sign Language" :| []) (DeprecatedPreferred $ Normal Swl Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just US) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16990955494162038802)) [] [] [], RangeRecord ("American Sign Language" :| []) (DeprecatedPreferred $ Normal Ase Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sgn Nothing Nothing (Just ZA) (S.fromList []) M.empty [], Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 17691265236218150930)) [] [] [], RangeRecord ("South African Sign Language" :| []) (DeprecatedPreferred $ Normal Sfs Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Sl Nothing Nothing Nothing (S.fromList [Nedis]) M.empty [], Syn.Normal (Subtag 16694843818662428690) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 15967273465522683925] [] [], RangeRecord ("Natisone dialect, Nadiza dialect" :| []) NotDeprecated)
  ,(Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty [], Syn.Normal (Subtag 16694843818662428690) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [Subtag 16555186176353370133] [] [], RangeRecord ("Resian, Resianic, Rezijan" :| []) NotDeprecated)
  ,(Normal Sr Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16701599218103484434) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Serbian in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Sr Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16701599218103484434) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Serbian in Latin script" :| []) NotDeprecated)
  ,(Normal Tg Nothing (Just Arab) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16833329507204071442) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953044)) nullSubtag [] [] [], RangeRecord ("Tajik in Arabic script" :| []) NotDeprecated)
  ,(Normal Tg Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16833329507204071442) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Tajik in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Uz Nothing (Just Cyrl) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16998836793509937170) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Uzbek in Cyrillic script" :| []) NotDeprecated)
  ,(Normal Uz Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 16998836793509937170) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Uzbek in Latin script" :| []) NotDeprecated)
  ,(Normal Yi Nothing (Just Latn) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17556157247397036050) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Yiddish, in Latin script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) nullSubtag [] [] [], RangeRecord ("simplified Chinese" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just CN) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 14391252609262419986)) [] [] [], RangeRecord ("PRC Mainland Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just HK) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 15108450849921171474)) [] [] [], RangeRecord ("Hong Kong Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just MO) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 15833530389927821330)) [] [] [], RangeRecord ("Macao Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just SG) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 16689214319128215570)) [] [] [], RangeRecord ("Singapore Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hans) (Just TW) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 16851343905713553426)) [] [] [], RangeRecord ("Taiwan Chinese in simplified script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) nullSubtag [] [] [], RangeRecord ("traditional Chinese" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just CN) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 14391252609262419986)) [] [] [], RangeRecord ("PRC Mainland Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just HK) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 15108450849921171474)) [] [] [], RangeRecord ("Hong Kong Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just MO) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 15833530389927821330)) [] [] [], RangeRecord ("Macao Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just SG) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 16689214319128215570)) [] [] [], RangeRecord ("Singapore Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh Nothing (Just Hant) (Just TW) (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 16851343905713553426)) [] [] [], RangeRecord ("Taiwan Chinese in traditional script" :| []) NotDeprecated)
  ,(Normal Zh (Just ExtCmn) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Mandarin Chinese" :| []) (DeprecatedPreferred $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtCmn) (Just Hans) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) nullSubtag [] [] [], RangeRecord ("Mandarin Chinese (Simplified)" :| []) (DeprecatedPreferred $ Normal Cmn Nothing (Just Hans) Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtCmn) (Just Hant) Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) nullSubtag [] [] [], RangeRecord ("Mandarin Chinese (Traditional)" :| []) (DeprecatedPreferred $ Normal Cmn Nothing (Just Hant) Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtGan) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14954044233009332243)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Kan or Gan" :| []) (DeprecatedPreferred $ Normal Gan Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtWuu) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 17282466813011034131)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Shanghaiese or Wu" :| []) (DeprecatedPreferred $ Normal Wuu Nothing Nothing Nothing S.empty M.empty []))
  ,(Normal Zh (Just ExtYue) Nothing Nothing (S.fromList []) M.empty [], Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 17570556451674390547)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Cantonese" :| []) (DeprecatedPreferred $ Normal Yue Nothing Nothing Nothing S.empty M.empty []))]

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
