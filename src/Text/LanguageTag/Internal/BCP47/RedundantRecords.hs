-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.RedundantRecords 
  (lookupRedundantDetails, lookupTagRedundant) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Redundant
import Text.LanguageTag.Internal.BCP47.Validate
import Data.List.NonEmpty (NonEmpty(..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Subtag (nullSubtag, justSubtag)
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import qualified Data.Vector.Unboxed as V

redundantTable :: [(Redundant, Syn.LanguageTag, RangeRecord)]
redundantTable =
  [(AzArab, Syn.NormalTag $ Syn.Normal (Subtag 14116533031992819730) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953044)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Azerbaijani in Arabic script" :| []) NotDeprecated)
  ,(AzCyrl, Syn.NormalTag $ Syn.Normal (Subtag 14116533031992819730) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Azerbaijani in Cyrillic script" :| []) NotDeprecated)
  ,(AzLatn, Syn.NormalTag $ Syn.Normal (Subtag 14116533031992819730) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Azerbaijani in Latin script" :| []) NotDeprecated)
  ,(BeLatn, Syn.NormalTag $ Syn.Normal (Subtag 14237004322024980498) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Belarusian in Latin script" :| []) NotDeprecated)
  ,(BsCyrl, Syn.NormalTag $ Syn.Normal (Subtag 14252766920720777234) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Bosnian in Cyrillic script" :| []) NotDeprecated)
  ,(BsLatn, Syn.NormalTag $ Syn.Normal (Subtag 14252766920720777234) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Bosnian in Latin script" :| []) NotDeprecated)
  ,(De1901, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList [(Subtag 7126246090126393380)]) [] mempty, RangeRecord ("German, traditional orthography" :| []) NotDeprecated)
  ,(De1996, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList [(Subtag 7126325598560976932)]) [] mempty, RangeRecord ("German, orthography of 1996" :| []) NotDeprecated)
  ,(DeAt1901, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763986)) (V.fromList [(Subtag 7126246090126393380)]) [] mempty, RangeRecord ("German, Austrian variant, traditional orthography" :| []) NotDeprecated)
  ,(DeAt1996, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14109777632551763986)) (V.fromList [(Subtag 7126325598560976932)]) [] mempty, RangeRecord ("German, Austrian variant, orthography of 1996" :| []) NotDeprecated)
  ,(DeCh1901, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364242)) (V.fromList [(Subtag 7126246090126393380)]) [] mempty, RangeRecord ("German, Swiss variant, traditional orthography" :| []) NotDeprecated)
  ,(DeCh1996, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14384497209821364242)) (V.fromList [(Subtag 7126325598560976932)]) [] mempty, RangeRecord ("German, Swiss variant, orthography of 1996" :| []) NotDeprecated)
  ,(DeDe1901, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) (V.fromList [(Subtag 7126246090126393380)]) [] mempty, RangeRecord ("German, German variant, traditional orthography" :| []) NotDeprecated)
  ,(DeDe1996, Syn.NormalTag $ Syn.Normal (Subtag 14525234698176692242) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) (V.fromList [(Subtag 7126325598560976932)]) [] mempty, RangeRecord ("German, German variant, orthography of 1996" :| []) NotDeprecated)
  ,(EnBoont, Syn.NormalTag $ Syn.Normal (Subtag 14679482985414131730) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList [(Subtag 14249247308838338581)]) [] mempty, RangeRecord ("Boontling" :| []) NotDeprecated)
  ,(EnScouse, Syn.NormalTag $ Syn.Normal (Subtag 14679482985414131730) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList [(Subtag 16685695188168867862)]) [] mempty, RangeRecord ("Scouse" :| []) NotDeprecated)
  ,(Es419, Syn.NormalTag $ Syn.Normal (Subtag 14685112484948344850) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 7549660252682059811)) (V.fromList []) [] mempty, RangeRecord ("Latin American Spanish" :| []) NotDeprecated)
  ,(IuCans, Syn.NormalTag $ Syn.Normal (Subtag 15263825037065453586) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14377591383445733396)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Inuktitut in Canadian Aboriginal Syllabic script" :| []) NotDeprecated)
  ,(IuLatn, Syn.NormalTag $ Syn.Normal (Subtag 15263825037065453586) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Inuktitut in Latin script" :| []) NotDeprecated)
  ,(MnCyrl, Syn.NormalTag $ Syn.Normal (Subtag 15832404490020978706) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Mongolian in Cyrillic script" :| []) NotDeprecated)
  ,(MnMong, Syn.NormalTag $ Syn.Normal (Subtag 15832404490020978706) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15834505038266368020)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Mongolian in Mongolian script" :| []) NotDeprecated)
  ,(SgnBr, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14251641020813934610)) (V.fromList []) [] mempty, RangeRecord ("Brazilian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bzs Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnCo, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14392378509169262610)) (V.fromList []) [] mempty, RangeRecord ("Colombian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Csn Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnDe, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14525234698176692242)) (V.fromList []) [] mempty, RangeRecord ("German Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Gsg Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnDk, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14531990097617747986)) (V.fromList []) [] mempty, RangeRecord ("Danish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Dsl Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnEs, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14685112484948344850)) (V.fromList []) [] mempty, RangeRecord ("Spanish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ssp Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnFr, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14828101773117358098)) (V.fromList []) [] mempty, RangeRecord ("French Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Fsl Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnGb, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14954202562683731986)) (V.fromList []) [] mempty, RangeRecord ("British Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Bfi Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnGr, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14972216961193213970)) (V.fromList []) [] mempty, RangeRecord ("Greek Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Gss Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnIe, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15245810638555971602)) (V.fromList []) [] mempty, RangeRecord ("Irish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Isg Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnIt, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15262699137158610962)) (V.fromList []) [] mempty, RangeRecord ("Italian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ise Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnJp, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15402310725607096338)) (V.fromList []) [] mempty, RangeRecord ("Japanese Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Jsl Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnMx, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15843663489089404946)) (V.fromList []) [] mempty, RangeRecord ("Mexican Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Mfs Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnNi, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15970890178562621458)) (V.fromList []) [] mempty, RangeRecord ("Nicaraguan Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ncs Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnNl, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15974267878283149330)) (V.fromList []) [] mempty, RangeRecord ("Dutch Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Dse Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnNo, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15977645578003677202)) (V.fromList []) [] mempty, RangeRecord ("Norwegian Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Nsl Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnPt, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16271505453689602066)) (V.fromList []) [] mempty, RangeRecord ("Portuguese Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Psr Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnSe, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16686962519314530322)) (V.fromList []) [] mempty, RangeRecord ("Swedish Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Swl Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnUs, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 16990955494162038802)) (V.fromList []) [] mempty, RangeRecord ("American Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Ase Nothing Nothing Nothing S.empty M.empty []))
  ,(SgnZa, Syn.NormalTag $ Syn.Normal (Subtag 16690181889360658451) nullSubtag nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 17691265236218150930)) (V.fromList []) [] mempty, RangeRecord ("South African Sign Language" :| []) (DeprecatedPreferred $ NormalTag $ Normal Sfs Nothing Nothing Nothing S.empty M.empty []))
  ,(SlNedis, Syn.NormalTag $ Syn.Normal (Subtag 16694843818662428690) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList [(Subtag 15967273465522683925)]) [] mempty, RangeRecord ("Natisone dialect, Nadiza dialect" :| []) NotDeprecated)
  ,(SlRozaj, Syn.NormalTag $ Syn.Normal (Subtag 16694843818662428690) nullSubtag nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList [(Subtag 16555186176353370133)]) [] mempty, RangeRecord ("Resian, Resianic, Rezijan" :| []) NotDeprecated)
  ,(SrCyrl, Syn.NormalTag $ Syn.Normal (Subtag 16701599218103484434) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Serbian in Cyrillic script" :| []) NotDeprecated)
  ,(SrLatn, Syn.NormalTag $ Syn.Normal (Subtag 16701599218103484434) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Serbian in Latin script" :| []) NotDeprecated)
  ,(TgArab, Syn.NormalTag $ Syn.Normal (Subtag 16833329507204071442) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14108385788269953044)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Tajik in Arabic script" :| []) NotDeprecated)
  ,(TgCyrl, Syn.NormalTag $ Syn.Normal (Subtag 16833329507204071442) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Tajik in Cyrillic script" :| []) NotDeprecated)
  ,(UzCyrl, Syn.NormalTag $ Syn.Normal (Subtag 16998836793509937170) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 14404647684545708052)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Uzbek in Cyrillic script" :| []) NotDeprecated)
  ,(UzLatn, Syn.NormalTag $ Syn.Normal (Subtag 16998836793509937170) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Uzbek in Latin script" :| []) NotDeprecated)
  ,(YiLatn, Syn.NormalTag $ Syn.Normal (Subtag 17556157247397036050) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15674680509089185812)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Yiddish, in Latin script" :| []) NotDeprecated)
  ,(ZhHans, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("simplified Chinese" :| []) NotDeprecated)
  ,(ZhHansCn, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 14391252609262419986)) (V.fromList []) [] mempty, RangeRecord ("PRC Mainland Chinese in simplified script" :| []) NotDeprecated)
  ,(ZhHansHk, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 15108450849921171474)) (V.fromList []) [] mempty, RangeRecord ("Hong Kong Chinese in simplified script" :| []) NotDeprecated)
  ,(ZhHansMo, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 15833530389927821330)) (V.fromList []) [] mempty, RangeRecord ("Macao Chinese in simplified script" :| []) NotDeprecated)
  ,(ZhHansSg, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 16689214319128215570)) (V.fromList []) [] mempty, RangeRecord ("Singapore Chinese in simplified script" :| []) NotDeprecated)
  ,(ZhHansTw, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) (justSubtag (Subtag 16851343905713553426)) (V.fromList []) [] mempty, RangeRecord ("Taiwan Chinese in simplified script" :| []) NotDeprecated)
  ,(ZhHant, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("traditional Chinese" :| []) NotDeprecated)
  ,(ZhHantCn, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 14391252609262419986)) (V.fromList []) [] mempty, RangeRecord ("PRC Mainland Chinese in traditional script" :| []) NotDeprecated)
  ,(ZhHantHk, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 15108450849921171474)) (V.fromList []) [] mempty, RangeRecord ("Hong Kong Chinese in traditional script" :| []) NotDeprecated)
  ,(ZhHantMo, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 15833530389927821330)) (V.fromList []) [] mempty, RangeRecord ("Macao Chinese in traditional script" :| []) NotDeprecated)
  ,(ZhHantSg, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 16689214319128215570)) (V.fromList []) [] mempty, RangeRecord ("Singapore Chinese in traditional script" :| []) NotDeprecated)
  ,(ZhHantTw, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) nullSubtag nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) (justSubtag (Subtag 16851343905713553426)) (V.fromList []) [] mempty, RangeRecord ("Taiwan Chinese in traditional script" :| []) NotDeprecated)
  ,(ZhCmn, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList []) [] mempty, RangeRecord ("Mandarin Chinese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhCmnHans, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag (justSubtag (Subtag 15098167323825012756)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Mandarin Chinese (Simplified)" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing (Just Hans) Nothing S.empty M.empty []))
  ,(ZhCmnHant, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14391094279588020243)) nullSubtag nullSubtag (justSubtag (Subtag 15098167392544489492)) nullSubtag (V.fromList []) [] mempty, RangeRecord ("Mandarin Chinese (Traditional)" :| []) (DeprecatedPreferred $ NormalTag $ Normal Cmn Nothing (Just Hant) Nothing S.empty M.empty []))
  ,(ZhGan, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 14954044233009332243)) nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList []) [] mempty, RangeRecord ("Kan or Gan" :| []) (DeprecatedPreferred $ NormalTag $ Normal Gan Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhWuu, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 17282466813011034131)) nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList []) [] mempty, RangeRecord ("Shanghaiese or Wu" :| []) (DeprecatedPreferred $ NormalTag $ Normal Wuu Nothing Nothing Nothing S.empty M.empty []))
  ,(ZhYue, Syn.NormalTag $ Syn.Normal (Subtag 17699146535566049298) (justSubtag (Subtag 17570556451674390547)) nullSubtag nullSubtag nullSubtag nullSubtag (V.fromList []) [] mempty, RangeRecord ("Cantonese" :| []) (DeprecatedPreferred $ NormalTag $ Normal Yue Nothing Nothing Nothing S.empty M.empty []))]

lookupRedundantDetails :: Redundant -> (Syn.LanguageTag, RangeRecord)
lookupRedundantDetails x = case HM.lookup x tab of
  Nothing -> error $ "internal invariant violated: subtag " <> show x <> " does not have an associated record"
  Just r -> r
  where
    tab = HM.fromList $ (\(a, b, c) -> (a, (b, c))) <$> redundantTable

lookupTagRedundant :: Syn.LanguageTag -> Maybe Redundant
lookupTagRedundant = flip HM.lookup tab
  where
    tab = HM.fromList $ (\(a, b, _) -> (b, a)) <$> redundantTable
