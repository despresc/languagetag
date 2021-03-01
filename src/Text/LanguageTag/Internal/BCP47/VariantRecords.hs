-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.VariantRecords 
  (lookupVariantDetails, lookupSubtagVariant) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Variant
import Text.LanguageTag.Internal.BCP47.Validate
import Data.List.NonEmpty (NonEmpty(..))
import Text.LanguageTag.Internal.BCP47.Syntax (Subtag(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.Internal.BCP47.Language
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.Region

variantTable :: [(Variant, Subtag, VariantRecord)]
variantTable =
  [(V1606nict, Subtag 7122868793502725128, VariantRecord ("Late Middle French (to 1606)" :| []) NotDeprecated [NormalTag $ Normal Frm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(V1694acad, Subtag 7122947813896414216, VariantRecord ("Early Modern French" :| []) NotDeprecated [NormalTag $ Normal Fr Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(V1901, Subtag 7126246090126393348, VariantRecord ("Traditional German orthography" :| []) NotDeprecated [NormalTag $ Normal De Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(V1959acad, Subtag 7126290672842236936, VariantRecord ("\"Academic\" (\"governmental\") variant of Belarusian as\ncodified in 1959" :| []) NotDeprecated [NormalTag $ Normal Be Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(V1994, Subtag 7126325461122023428, VariantRecord ("Standardized Resian orthography" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Biske]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Njiva]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Osojs]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Solba]) M.empty []])
  ,(V1996, Subtag 7126325598560976900, VariantRecord ("German orthography of 1996" :| []) NotDeprecated [NormalTag $ Normal De Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Abl1943, Subtag 14090464810350772231, VariantRecord ("Orthographic formulation of 1943 - Official in Brazil\n(Formul\225rio Ortogr\225fico de 1943 - Oficial no Brasil)" :| []) NotDeprecated [NormalTag $ Normal Pt Nothing Nothing (Just BR) (S.fromList []) M.empty []])
  ,(Akuapem, Subtag 14100680402619760647, VariantRecord ("Akuapem Twi" :| []) NotDeprecated [NormalTag $ Normal Tw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Alalc97, Subtag 14101631129414762503, VariantRecord ("ALA-LC Romanization, 1997 edition" :| []) NotDeprecated [])
  ,(Aluku, Subtag 14101806991978528773, VariantRecord ("Aluku dialect" :| ["Boni dialect"]) NotDeprecated [NormalTag $ Normal Djk Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ao1990, Subtag 14104583089388781574, VariantRecord ("Portuguese Language Orthographic Agreement of 1990 (Acordo\nOrtogr\225fico da L\237ngua Portuguesa de 1990)" :| []) NotDeprecated [NormalTag $ Normal Pt Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Gl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Aranes, Subtag 14108386667609980934, VariantRecord ("Aranese" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Arevela, Subtag 14108422401711702023, VariantRecord ("Eastern Armenian" :| []) DeprecatedSimple [NormalTag $ Normal Hy Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Arevmda, Subtag 14108422405973114887, VariantRecord ("Western Armenian" :| []) DeprecatedSimple [NormalTag $ Normal Hy Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Arkaika, Subtag 14108473737304113159, VariantRecord ("Arcaicam Esperantom" :| ["Arkaika Esperanto"]) NotDeprecated [NormalTag $ Normal Eo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Asante, Subtag 14109512575511166982, VariantRecord ("Asante Twi" :| ["Ashanti Twi"]) NotDeprecated [NormalTag $ Normal Tw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Auvern, Subtag 14111948473767034886, VariantRecord ("Auvergnat" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Baku1926, Subtag 14233449971077166600, VariantRecord ("Unified Turkic Latin Alphabet (Historical)" :| []) NotDeprecated [NormalTag $ Normal Az Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Ba Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Crh Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Kk Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Krc Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Ky Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sah Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Tk Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Tt Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Uz Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Balanka, Subtag 14233457425741021191, VariantRecord ("The Balanka dialect of Anii" :| []) NotDeprecated [NormalTag $ Normal Blo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Barla, Subtag 14233510950782107653, VariantRecord ("The Barlavento dialect group of Kabuverdianu" :| []) NotDeprecated [NormalTag $ Normal Kea Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Basiceng, Subtag 14233519542217697032, VariantRecord ("Basic English" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Bauddha, Subtag 14233536791355359239, VariantRecord ("Buddhist Hybrid Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Biscayan, Subtag 14242526328165297672, VariantRecord ("Biscayan dialect of Basque" :| []) NotDeprecated [NormalTag $ Normal Eu Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Biske, Subtag 14242526879557877765, VariantRecord ("The San Giorgio dialect of Resian" :| ["The Bila dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Bohoric, Subtag 14249185804276563975, VariantRecord ("Slovene in Bohori\269 alphabet" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Boont, Subtag 14249247308838338565, VariantRecord ("Boontling" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Bornholm, Subtag 14249273691144088840, VariantRecord ("Bornholmsk" :| []) NotDeprecated [NormalTag $ Normal Da Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Cisaup, Subtag 14386641389498662918, VariantRecord ("Cisalpine" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Colb1945, Subtag 14393335248271848712, VariantRecord ("Portuguese-Brazilian Orthographic Convention of 1945\n(Conven\231\227o Ortogr\225fica Luso-Brasileira de 1945)" :| []) NotDeprecated [NormalTag $ Normal Pt Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Cornu, Subtag 14393388885730131973, VariantRecord ("Cornu-English" :| ["Cornish English","Anglo-Cornish"]) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Creiss, Subtag 14396651892052590598, VariantRecord ("Occitan variants of the Croissant area" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Dajnko, Subtag 14521671101462872070, VariantRecord ("Slovene in Dajnko alphabet" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ekavsk, Subtag 14676966677803958278, VariantRecord ("Serbian with Ekavian pronunciation" :| []) NotDeprecated [NormalTag $ Normal Sr Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Latn) Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Cyrl) Nothing (S.fromList []) M.empty []])
  ,(Emodeng, Subtag 14679340378469138439, VariantRecord ("Early Modern English (1500-1700)" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Fonipa, Subtag 14825698919710720006, VariantRecord ("International Phonetic Alphabet" :| []) NotDeprecated [])
  ,(Fonkirsh, Subtag 14825699053466675208, VariantRecord ("Kirshenbaum Phonetic Alphabet" :| []) NotDeprecated [])
  ,(Fonnapa, Subtag 14825699255321133063, VariantRecord ("North American Phonetic Alphabet" :| ["Americanist Phonetic Notation"]) NotDeprecated [])
  ,(Fonupa, Subtag 14825699744344440838, VariantRecord ("Uralic Phonetic Alphabet" :| []) NotDeprecated [])
  ,(Fonxsamp, Subtag 14825699952117084168, VariantRecord ("X-SAMPA transcription" :| []) NotDeprecated [])
  ,(Gascon, Subtag 14954095076756684806, VariantRecord ("Gascon" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Grclass, Subtag 14973095248668491783, VariantRecord ("Classical Occitan orthography" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Grital, Subtag 14973148574949310470, VariantRecord ("Italian-inspired Occitan orthography" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Grmistr, Subtag 14973183013108121607, VariantRecord ("Mistralian or Mistralian-inspired Occitan orthography" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Hepburn, Subtag 15102687410702974983, VariantRecord ("Hepburn romanization" :| []) NotDeprecated [NormalTag $ Normal Ja Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Heploc, Subtag 15102688094609997830, VariantRecord ("Hepburn romanization, Library of Congress method" :| []) (DeprecatedPreferred Alalc97) [NormalTag $ Normal Ja Nothing (Just Latn) Nothing (S.fromList [Hepburn]) M.empty []])
  ,(Hognorsk, Subtag 15113868066346887944, VariantRecord ("Norwegian in H\248gnorsk (High Norwegian) orthography" :| []) NotDeprecated [NormalTag $ Normal Nn Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Hsistemo, Subtag 15118389604387319560, VariantRecord ("Standard H-system orthographic fallback for spelling\nEsperanto" :| []) NotDeprecated [NormalTag $ Normal Eo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ijekavsk, Subtag 15252335949044640520, VariantRecord ("Serbian with Ijekavian pronunciation" :| []) NotDeprecated [NormalTag $ Normal Sr Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Latn) Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Cyrl) Nothing (S.fromList []) M.empty []])
  ,(Itihasa, Subtag 15263629926313525255, VariantRecord ("Epic Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ivanchov, Subtag 15265811770727986696, VariantRecord ("Bulgarian in 1899 orthography" :| []) NotDeprecated [NormalTag $ Normal Bg Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Jauer, Subtag 15386458371758489605, VariantRecord ("Jauer dialect of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Jyutping, Subtag 15413480999685154568, VariantRecord ("Jyutping Cantonese Romanization" :| []) NotDeprecated [NormalTag $ Normal Yue Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Kkcor, Subtag 15541674916423139333, VariantRecord ("Common Cornish orthography of Revived Cornish" :| []) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Kociewie, Subtag 15546178097256916232, VariantRecord ("The Kociewie dialect of Polish" :| []) NotDeprecated [NormalTag $ Normal Pl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Kscor, Subtag 15550682115677880325, VariantRecord ("Standard Cornish orthography of Revived Cornish" :| ["Kernowek Standard"]) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Laukika, Subtag 15674689155847192583, VariantRecord ("Classical Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Lemosin, Subtag 15679122666969038855, VariantRecord ("Limousin" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Lengadoc, Subtag 15679130903621657352, VariantRecord ("Languedocien" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Lipaw, Subtag 15683651694506278917, VariantRecord ("The Lipovaz dialect of Resian" :| ["The Lipovec dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Luna1918, Subtag 15697144863862077448, VariantRecord ("Post-1917 Russian orthography" :| []) NotDeprecated [NormalTag $ Normal Ru Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Metelko, Subtag 15823298736751607815, VariantRecord ("Slovene in Metelko alphabet" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Monoton, Subtag 15834505650768379911, VariantRecord ("Monotonic Greek" :| []) NotDeprecated [NormalTag $ Normal El Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ndyuka, Subtag 15966333104314908678, VariantRecord ("Ndyuka dialect" :| ["Aukan dialect"]) NotDeprecated [NormalTag $ Normal Djk Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Nedis, Subtag 15967273465522683909, VariantRecord ("Natisone dialect" :| ["Nadiza dialect"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Newfound, Subtag 15967440383478555656, VariantRecord ("Newfoundland English" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing (Just CA) (S.fromList []) M.empty []])
  ,(Nicard, Subtag 15971767719183777798, VariantRecord ("Ni\231ard" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Njiva, Subtag 15972947829211529221, VariantRecord ("The Gniva dialect of Resian" :| ["The Njiva dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Nulik, Subtag 15985358228481376261, VariantRecord ("Volap\252k nulik" :| ["Volap\252k perevid\246l","Volap\252k nul\228dik","de Jong's Volap\252k","New Volap\252k","Revised Volap\252k","Modern Volap\252k"]) NotDeprecated [NormalTag $ Normal Vo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Osojs, Subtag 16127248078037057541, VariantRecord ("The Oseacco dialect of Resian" :| ["The Osojane dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Oxendict, Subtag 16132789883909567496, VariantRecord ("Oxford English Dictionary spelling" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Pahawh2, Subtag 16251034879248629767, VariantRecord ("Pahawh Hmong Second Stage Reduced orthography" :| []) NotDeprecated [NormalTag $ Normal Mww Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hnj Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Pahawh3, Subtag 16251034879248662535, VariantRecord ("Pahawh Hmong Third Stage Reduced orthography" :| []) NotDeprecated [NormalTag $ Normal Mww Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hnj Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Pahawh4, Subtag 16251034879248695303, VariantRecord ("Pahawh Hmong Final Version orthography" :| []) NotDeprecated [NormalTag $ Normal Mww Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hnj Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Pamaka, Subtag 16251078853240291334, VariantRecord ("Pamaka dialect" :| []) NotDeprecated [NormalTag $ Normal Djk Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Peano, Subtag 16255477794845229061, VariantRecord ("Latino Sine Flexione" :| ["Interlingua de API","Interlingua de Peano"]) NotDeprecated [NormalTag $ Normal La Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Petr1708, Subtag 16255645162436835336, VariantRecord ("Petrine orthography" :| []) NotDeprecated [NormalTag $ Normal Ru Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Pinyin, Subtag 16260096496836280326, VariantRecord ("Pinyin romanization" :| []) NotDeprecated [NormalTag $ Normal Zh Nothing (Just Latn) Nothing (S.fromList []) M.empty [],NormalTag $ Normal Bo Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Polyton, Subtag 16266834310004670471, VariantRecord ("Polytonic Greek" :| []) NotDeprecated [NormalTag $ Normal El Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Provenc, Subtag 16270238183788216327, VariantRecord ("Proven\231al" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Puter, Subtag 16273658702257455109, VariantRecord ("Puter idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Rigik, Subtag 16548264201437577221, VariantRecord ("Volap\252k rigik" :| ["Schleyer's Volap\252k","Original Volap\252k","Classic Volap\252k"]) NotDeprecated [NormalTag $ Normal Vo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Rozaj, Subtag 16555186176353370117, VariantRecord ("Resian" :| ["Resianic","Rezijan"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Rumgr, Subtag 16561827643196964869, VariantRecord ("Rumantsch Grischun" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Scotland, Subtag 16685695115678147592, VariantRecord ("Scottish Standard English" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Scouse, Subtag 16685695188168867846, VariantRecord ("Scouse" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Simple, Subtag 16692432648068399110, VariantRecord ("Simplified form" :| []) NotDeprecated [])
  ,(Solba, Subtag 16699178283014553605, VariantRecord ("The Stolvizza dialect of Resian" :| ["The Solbica dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Sotav, Subtag 16699248594313543685, VariantRecord ("The Sotavento dialect group of Kabuverdianu" :| []) NotDeprecated [NormalTag $ Normal Kea Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Spanglis, Subtag 16700208254209553160, VariantRecord ("Spanglish" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Es Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Surmiran, Subtag 16705987219704311304, VariantRecord ("Surmiran idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Sursilv, Subtag 16705987631996665863, VariantRecord ("Sursilvan idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Sutsilv, Subtag 16706005224182710279, VariantRecord ("Sutsilvan idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Tarask, Subtag 16827583590345736198, VariantRecord ("Belarusian in Taraskievica orthography" :| []) NotDeprecated [NormalTag $ Normal Be Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Tongyong, Subtag 16843311420227938056, VariantRecord ("Tongyong Pinyin romanization" :| []) NotDeprecated [NormalTag $ Normal Zh Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Tunumiit, Subtag 16850067775273890824, VariantRecord ("Tunumiisiut" :| ["East Greenlandic","\216stgr\248nlandsk"]) NotDeprecated [NormalTag $ Normal Kl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Uccor, Subtag 16973819597926957061, VariantRecord ("Unified Cornish orthography of Revived Cornish" :| []) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ucrcor, Subtag 16973950713556107270, VariantRecord ("Unified Cornish Revised orthography of Revived Cornish" :| []) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Ulster, Subtag 16984093771673108486, VariantRecord ("Ulster dialect of Scots" :| []) NotDeprecated [NormalTag $ Normal Sco Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Unifon, Subtag 16986256653835829254, VariantRecord ("Unifon phonetic alphabet" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hup Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Kyh Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Tol Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Yur Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Vaidika, Subtag 17115735002453147655, VariantRecord ("Vedic Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Valencia, Subtag 17115761462102778120, VariantRecord ("Valencian" :| []) NotDeprecated [NormalTag $ Normal Ca Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Vallader, Subtag 17115761936163861000, VariantRecord ("Vallader idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Vecdruka, Subtag 17120185830396518664, VariantRecord ("Latvian orthography used before 1920s (\"vec\257 druka\")" :| []) NotDeprecated [NormalTag $ Normal Lv Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Vivaraup, Subtag 17124856349549326344, VariantRecord ("Vivaro-Alpine" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Wadegile, Subtag 17259806277701625096, VariantRecord ("Wade-Giles romanization" :| []) NotDeprecated [NormalTag $ Normal Zh Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Xsistemo, Subtag 17424232613601013512, VariantRecord ("Standard X-system orthographic fallback for spelling\nEsperanto" :| []) NotDeprecated [NormalTag $ Normal Eo Nothing Nothing Nothing (S.fromList []) M.empty []])]

lookupVariantDetails :: Variant -> (Subtag, VariantRecord)
lookupVariantDetails x = case HM.lookup x tab of
  Nothing -> error $ "internal invariant violated: subtag " <> show x <> " does not have an associated record"
  Just r -> r
  where
    tab = HM.fromList $ (\(a, b, c) -> (a, (b, c))) <$> variantTable

lookupSubtagVariant :: Subtag -> Maybe Variant
lookupSubtagVariant = flip HM.lookup tab
  where
    tab = HM.fromList $ (\(a, b, _) -> (b, a)) <$> variantTable
