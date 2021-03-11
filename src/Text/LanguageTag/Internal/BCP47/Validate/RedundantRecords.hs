-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.Validate.RedundantRecords
  (lookupRedundantDetails, lookupRedundantTag, lookupRedundantRecord) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Validate.Redundant
import Text.LanguageTag.Internal.BCP47.Validate.RecordTypes
import Data.List.NonEmpty (NonEmpty(..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.Internal.BCP47.Validate.Script
import Text.LanguageTag.Internal.BCP47.Validate.Language
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import Text.LanguageTag.Subtag (nullSubtag, justSubtag)

redundantDetails :: Vector (Syn.LanguageTag, RangeRecord)
redundantDetails = V.fromList
  [(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14116533031992819730)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953044)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Arabic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14116533031992819730)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Cyrillic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14116533031992819730)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Azerbaijani in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14237004322024980498)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Belarusian in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14252766920720777234)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Bosnian in Cyrillic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14252766920720777234)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Bosnian in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [(Subtag 7126246090126393380)] [] [], RangeRecord ("German, traditional orthography" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [(Subtag 7126325598560976932)] [] [], RangeRecord ("German, orthography of 1996" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763986)) [(Subtag 7126246090126393380)] [] [], RangeRecord ("German, Austrian variant, traditional orthography" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763986)) [(Subtag 7126325598560976932)] [] [], RangeRecord ("German, Austrian variant, orthography of 1996" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364242)) [(Subtag 7126246090126393380)] [] [], RangeRecord ("German, Swiss variant, traditional orthography" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364242)) [(Subtag 7126325598560976932)] [] [], RangeRecord ("German, Swiss variant, orthography of 1996" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) [(Subtag 7126246090126393380)] [] [], RangeRecord ("German, German variant, traditional orthography" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14525234698176692242)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) [(Subtag 7126325598560976932)] [] [], RangeRecord ("German, German variant, orthography of 1996" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14679482985414131730)) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [(Subtag 14249247308838338581)] [] [], RangeRecord ("Boontling" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14679482985414131730)) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [(Subtag 16685695188168867862)] [] [], RangeRecord ("Scouse" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 14685112484948344850)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 7549660252682059811)) [] [] [], RangeRecord ("Latin American Spanish" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 15263825037065453586)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14377591383445733396)) nullSubtag [] [] [], RangeRecord ("Inuktitut in Canadian Aboriginal Syllabic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 15263825037065453586)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Inuktitut in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 15832404490020978706)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Mongolian in Cyrillic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 15832404490020978706)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15834505038266368020)) nullSubtag [] [] [], RangeRecord ("Mongolian in Mongolian script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14251641020813934610)) [] [] [], RangeRecord ("Brazilian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bzs Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14392378509169262610)) [] [] [], RangeRecord ("Colombian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Csn Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) [] [] [], RangeRecord ("German Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Gsg Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14531990097617747986)) [] [] [], RangeRecord ("Danish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Dsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14685112484948344850)) [] [] [], RangeRecord ("Spanish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ssp Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14828101773117358098)) [] [] [], RangeRecord ("French Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Fsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14954202562683731986)) [] [] [], RangeRecord ("British Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bfi Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14972216961193213970)) [] [] [], RangeRecord ("Greek Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Gss Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15245810638555971602)) [] [] [], RangeRecord ("Irish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Isg Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15262699137158610962)) [] [] [], RangeRecord ("Italian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ise Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15402310725607096338)) [] [] [], RangeRecord ("Japanese Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Jsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15843663489089404946)) [] [] [], RangeRecord ("Mexican Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Mfs Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15970890178562621458)) [] [] [], RangeRecord ("Nicaraguan Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ncs Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15974267878283149330)) [] [] [], RangeRecord ("Dutch Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Dse Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15977645578003677202)) [] [] [], RangeRecord ("Norwegian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nsl Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16271505453689602066)) [] [] [], RangeRecord ("Portuguese Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Psr Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16686962519314530322)) [] [] [], RangeRecord ("Swedish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Swl Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16990955494162038802)) [] [] [], RangeRecord ("American Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ase Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16690181889360658451)) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 17691265236218150930)) [] [] [], RangeRecord ("South African Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sfs Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16694843818662428690)) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [(Subtag 15967273465522683925)] [] [], RangeRecord ("Natisone dialect, Nadiza dialect" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16694843818662428690)) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag [(Subtag 16555186176353370133)] [] [], RangeRecord ("Resian, Resianic, Rezijan" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16701599218103484434)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Serbian in Cyrillic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16701599218103484434)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Serbian in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16833329507204071442)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953044)) nullSubtag [] [] [], RangeRecord ("Tajik in Arabic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16833329507204071442)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Tajik in Cyrillic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16998836793509937170)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag [] [] [], RangeRecord ("Uzbek in Cyrillic script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 16998836793509937170)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Uzbek in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17556157247397036050)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag [] [] [], RangeRecord ("Yiddish, in Latin script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) nullSubtag [] [] [], RangeRecord ("simplified Chinese" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 14391252609262419986)) [] [] [], RangeRecord ("PRC Mainland Chinese in simplified script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 15108450849921171474)) [] [] [], RangeRecord ("Hong Kong Chinese in simplified script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 15833530389927821330)) [] [] [], RangeRecord ("Macao Chinese in simplified script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 16689214319128215570)) [] [] [], RangeRecord ("Singapore Chinese in simplified script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 16851343905713553426)) [] [] [], RangeRecord ("Taiwan Chinese in simplified script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) nullSubtag [] [] [], RangeRecord ("traditional Chinese" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 14391252609262419986)) [] [] [], RangeRecord ("PRC Mainland Chinese in traditional script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 15108450849921171474)) [] [] [], RangeRecord ("Hong Kong Chinese in traditional script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 15833530389927821330)) [] [] [], RangeRecord ("Macao Chinese in traditional script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 16689214319128215570)) [] [] [], RangeRecord ("Singapore Chinese in traditional script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 16851343905713553426)) [] [] [], RangeRecord ("Taiwan Chinese in traditional script" :| []) NotDeprecated)
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Mandarin Chinese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) nullSubtag [] [] [], RangeRecord ("Mandarin Chinese (Simplified)" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing (Just Hans) Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) nullSubtag [] [] [], RangeRecord ("Mandarin Chinese (Traditional)" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing (Just Hant) Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) (justSubtag (Subtag 14954044233009332243)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Kan or Gan" :| []) (DeprecatedPreferred $ NormalTag $ Normal Gan Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) (justSubtag (Subtag 17282466813011034131)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Shanghaiese or Wu" :| []) (DeprecatedPreferred $ NormalTag $ Normal Wuu Nothing Nothing Nothing S.empty M.empty []))
  ,(Syn.NormalTag $ Syn.Normal (justSubtag (Subtag 17699146535566049298)) (justSubtag (Subtag 17570556451674390547)) nullSubtag nullSubtag nullSubtag nullSubtag [] [] [], RangeRecord ("Cantonese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Yue Nothing Nothing Nothing S.empty M.empty []))]

-- | Look up the tag and record details associated to the given 'Redundant' tag
lookupRedundantDetails :: Redundant -> (Syn.LanguageTag, RangeRecord)
lookupRedundantDetails = V.unsafeIndex redundantDetails . fromEnum

lookupRedundantTag :: Redundant -> Syn.LanguageTag
lookupRedundantTag = fst . lookupRedundantDetails

lookupRedundantRecord :: Redundant -> RangeRecord
lookupRedundantRecord = snd . lookupRedundantDetails
