-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.Validate.VariantRecords
  (lookupVariantDetails, parseVariant, variantToSubtag, lookupVariantRecord) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Validate.Variant
import Text.LanguageTag.Internal.BCP47.Validate.RecordTypes
import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.Internal.BCP47.Validate.Language
import Text.LanguageTag.Internal.BCP47.Validate.Script
import Text.LanguageTag.Internal.BCP47.Validate.Region

-- | The subtag and record information associated to the 'Variant' type.
variantDetails :: Vector (Subtag, VariantRecord)
variantDetails = V.fromList
  [(Subtag 7122868793502725176, VariantRecord ("Late Middle French (to 1606)" :| []) NotDeprecated [NormalTag $ Normal Frm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 7122947813896414264, VariantRecord ("Early Modern French" :| []) NotDeprecated [NormalTag $ Normal Fr Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 7126246090126393380, VariantRecord ("Traditional German orthography" :| []) NotDeprecated [NormalTag $ Normal De Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 7126290672842236984, VariantRecord ("\"Academic\" (\"governmental\") variant of Belarusian as\ncodified in 1959" :| []) NotDeprecated [NormalTag $ Normal Be Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 7126325461122023460, VariantRecord ("Standardized Resian orthography" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Biske]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Njiva]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Osojs]) M.empty [],NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj, Solba]) M.empty []])
  ,(Subtag 7126325598560976932, VariantRecord ("German orthography of 1996" :| []) NotDeprecated [NormalTag $ Normal De Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14090464810350772279, VariantRecord ("Orthographic formulation of 1943 - Official in Brazil\n(Formul\225rio Ortogr\225fico de 1943 - Oficial no Brasil)" :| []) NotDeprecated [NormalTag $ Normal Pt Nothing Nothing (Just BR) (S.fromList []) M.empty []])
  ,(Subtag 14100680402619760663, VariantRecord ("Akuapem Twi" :| []) NotDeprecated [NormalTag $ Normal Tw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14101631129414762551, VariantRecord ("ALA-LC Romanization, 1997 edition" :| []) NotDeprecated [])
  ,(Subtag 14101806991978528789, VariantRecord ("Aluku dialect" :| ["Boni dialect"]) NotDeprecated [NormalTag $ Normal Djk Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14104583089388781622, VariantRecord ("Portuguese Language Orthographic Agreement of 1990 (Acordo\nOrtogr\225fico da L\237ngua Portuguesa de 1990)" :| []) NotDeprecated [NormalTag $ Normal Pt Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Gl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14108386667609980950, VariantRecord ("Aranese" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14108422401711702039, VariantRecord ("Eastern Armenian" :| []) DeprecatedSimple [NormalTag $ Normal Hy Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14108422405973114903, VariantRecord ("Western Armenian" :| []) DeprecatedSimple [NormalTag $ Normal Hy Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14108473737304113175, VariantRecord ("Arcaicam Esperantom" :| ["Arkaika Esperanto"]) NotDeprecated [NormalTag $ Normal Eo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14109512575511166998, VariantRecord ("Asante Twi" :| ["Ashanti Twi"]) NotDeprecated [NormalTag $ Normal Tw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14111948473767034902, VariantRecord ("Auvergnat" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14233449971077166648, VariantRecord ("Unified Turkic Latin Alphabet (Historical)" :| []) NotDeprecated [NormalTag $ Normal Az Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Ba Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Crh Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Kk Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Krc Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Ky Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sah Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Tk Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Tt Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Uz Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14233457425741021207, VariantRecord ("The Balanka dialect of Anii" :| []) NotDeprecated [NormalTag $ Normal Blo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14233510950782107669, VariantRecord ("The Barlavento dialect group of Kabuverdianu" :| []) NotDeprecated [NormalTag $ Normal Kea Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14233519542217697048, VariantRecord ("Basic English" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14233536791355359255, VariantRecord ("Buddhist Hybrid Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14242526328165297688, VariantRecord ("Biscayan dialect of Basque" :| []) NotDeprecated [NormalTag $ Normal Eu Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14242526879557877781, VariantRecord ("The San Giorgio dialect of Resian" :| ["The Bila dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Subtag 14249185804276563991, VariantRecord ("Slovene in Bohori\269 alphabet" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14249247308838338581, VariantRecord ("Boontling" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14249273691144088856, VariantRecord ("Bornholmsk" :| []) NotDeprecated [NormalTag $ Normal Da Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14386641389498662934, VariantRecord ("Cisalpine" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14393335248271848760, VariantRecord ("Portuguese-Brazilian Orthographic Convention of 1945\n(Conven\231\227o Ortogr\225fica Luso-Brasileira de 1945)" :| []) NotDeprecated [NormalTag $ Normal Pt Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14393388885730131989, VariantRecord ("Cornu-English" :| ["Cornish English","Anglo-Cornish"]) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14396651892052590614, VariantRecord ("Occitan variants of the Croissant area" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14521671101462872086, VariantRecord ("Slovene in Dajnko alphabet" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14676966677803958294, VariantRecord ("Serbian with Ekavian pronunciation" :| []) NotDeprecated [NormalTag $ Normal Sr Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Latn) Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Cyrl) Nothing (S.fromList []) M.empty []])
  ,(Subtag 14679340378469138455, VariantRecord ("Early Modern English (1500-1700)" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14825698919710720022, VariantRecord ("International Phonetic Alphabet" :| []) NotDeprecated [])
  ,(Subtag 14825699053466675224, VariantRecord ("Kirshenbaum Phonetic Alphabet" :| []) NotDeprecated [])
  ,(Subtag 14825699255321133079, VariantRecord ("North American Phonetic Alphabet" :| ["Americanist Phonetic Notation"]) NotDeprecated [])
  ,(Subtag 14825699744344440854, VariantRecord ("Uralic Phonetic Alphabet" :| []) NotDeprecated [])
  ,(Subtag 14825699952117084184, VariantRecord ("X-SAMPA transcription" :| []) NotDeprecated [])
  ,(Subtag 14954095076756684822, VariantRecord ("Gascon" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14973095248668491799, VariantRecord ("Classical Occitan orthography" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14973148574949310486, VariantRecord ("Italian-inspired Occitan orthography" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 14973183013108121623, VariantRecord ("Mistralian or Mistralian-inspired Occitan orthography" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15102687410702974999, VariantRecord ("Hepburn romanization" :| []) NotDeprecated [NormalTag $ Normal Ja Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Subtag 15102688094609997846, VariantRecord ("Hepburn romanization, Library of Congress method" :| []) (DeprecatedPreferred Alalc97) [NormalTag $ Normal Ja Nothing (Just Latn) Nothing (S.fromList [Hepburn]) M.empty []])
  ,(Subtag 15113868066346887960, VariantRecord ("Norwegian in H\248gnorsk (High Norwegian) orthography" :| []) NotDeprecated [NormalTag $ Normal Nn Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15118389604387319576, VariantRecord ("Standard H-system orthographic fallback for spelling\nEsperanto" :| []) NotDeprecated [NormalTag $ Normal Eo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15252335949044640536, VariantRecord ("Serbian with Ijekavian pronunciation" :| []) NotDeprecated [NormalTag $ Normal Sr Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Latn) Nothing (S.fromList []) M.empty [],NormalTag $ Normal Sr Nothing (Just Cyrl) Nothing (S.fromList []) M.empty []])
  ,(Subtag 15263629926313525271, VariantRecord ("Epic Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15265811770727986712, VariantRecord ("Bulgarian in 1899 orthography" :| []) NotDeprecated [NormalTag $ Normal Bg Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15386458371758489621, VariantRecord ("Jauer dialect of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15413480999685154584, VariantRecord ("Jyutping Cantonese Romanization" :| []) NotDeprecated [NormalTag $ Normal Yue Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15541674916423139349, VariantRecord ("Common Cornish orthography of Revived Cornish" :| []) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15546178097256916248, VariantRecord ("The Kociewie dialect of Polish" :| []) NotDeprecated [NormalTag $ Normal Pl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15550682115677880341, VariantRecord ("Standard Cornish orthography of Revived Cornish" :| ["Kernowek Standard"]) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15674689155847192599, VariantRecord ("Classical Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15679122666969038871, VariantRecord ("Limousin" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15679130903621657368, VariantRecord ("Languedocien" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15683651694506278933, VariantRecord ("The Lipovaz dialect of Resian" :| ["The Lipovec dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Subtag 15697144863862077496, VariantRecord ("Post-1917 Russian orthography" :| []) NotDeprecated [NormalTag $ Normal Ru Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15823298736751607831, VariantRecord ("Slovene in Metelko alphabet" :| []) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15834505650768379927, VariantRecord ("Monotonic Greek" :| []) NotDeprecated [NormalTag $ Normal El Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15966333104314908694, VariantRecord ("Ndyuka dialect" :| ["Aukan dialect"]) NotDeprecated [NormalTag $ Normal Djk Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15967273465522683925, VariantRecord ("Natisone dialect" :| ["Nadiza dialect"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15967440383478555672, VariantRecord ("Newfoundland English" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing (Just CA) (S.fromList []) M.empty []])
  ,(Subtag 15971767719183777814, VariantRecord ("Ni\231ard" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 15972947829211529237, VariantRecord ("The Gniva dialect of Resian" :| ["The Njiva dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Subtag 15985358228481376277, VariantRecord ("Volap\252k nulik" :| ["Volap\252k perevid\246l","Volap\252k nul\228dik","de Jong's Volap\252k","New Volap\252k","Revised Volap\252k","Modern Volap\252k"]) NotDeprecated [NormalTag $ Normal Vo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16127248078037057557, VariantRecord ("The Oseacco dialect of Resian" :| ["The Osojane dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Subtag 16132789883909567512, VariantRecord ("Oxford English Dictionary spelling" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16251034879248629815, VariantRecord ("Pahawh Hmong Second Stage Reduced orthography" :| []) NotDeprecated [NormalTag $ Normal Mww Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hnj Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16251034879248662583, VariantRecord ("Pahawh Hmong Third Stage Reduced orthography" :| []) NotDeprecated [NormalTag $ Normal Mww Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hnj Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16251034879248695351, VariantRecord ("Pahawh Hmong Final Version orthography" :| []) NotDeprecated [NormalTag $ Normal Mww Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hnj Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16251078853240291350, VariantRecord ("Pamaka dialect" :| []) NotDeprecated [NormalTag $ Normal Djk Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16255477794845229077, VariantRecord ("Latino Sine Flexione" :| ["Interlingua de API","Interlingua de Peano"]) NotDeprecated [NormalTag $ Normal La Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16255645162436835384, VariantRecord ("Petrine orthography" :| []) NotDeprecated [NormalTag $ Normal Ru Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16260096496836280342, VariantRecord ("Pinyin romanization" :| []) NotDeprecated [NormalTag $ Normal Zh Nothing (Just Latn) Nothing (S.fromList []) M.empty [],NormalTag $ Normal Bo Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Subtag 16266834310004670487, VariantRecord ("Polytonic Greek" :| []) NotDeprecated [NormalTag $ Normal El Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16270238183788216343, VariantRecord ("Proven\231al" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16273658702257455125, VariantRecord ("Puter idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16548264201437577237, VariantRecord ("Volap\252k rigik" :| ["Schleyer's Volap\252k","Original Volap\252k","Classic Volap\252k"]) NotDeprecated [NormalTag $ Normal Vo Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16555186176353370133, VariantRecord ("Resian" :| ["Resianic","Rezijan"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16561827643196964885, VariantRecord ("Rumantsch Grischun" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16685695115678147608, VariantRecord ("Scottish Standard English" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16685695188168867862, VariantRecord ("Scouse" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16692432648068399126, VariantRecord ("Simplified form" :| []) NotDeprecated [])
  ,(Subtag 16699178283014553621, VariantRecord ("The Stolvizza dialect of Resian" :| ["The Solbica dialect of Resian"]) NotDeprecated [NormalTag $ Normal Sl Nothing Nothing Nothing (S.fromList [Rozaj]) M.empty []])
  ,(Subtag 16699248594313543701, VariantRecord ("The Sotavento dialect group of Kabuverdianu" :| []) NotDeprecated [NormalTag $ Normal Kea Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16700208254209553176, VariantRecord ("Spanglish" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Es Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16705987219704311320, VariantRecord ("Surmiran idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16705987631996665879, VariantRecord ("Sursilvan idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16706005224182710295, VariantRecord ("Sutsilvan idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16827583590345736214, VariantRecord ("Belarusian in Taraskievica orthography" :| []) NotDeprecated [NormalTag $ Normal Be Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16843311420227938072, VariantRecord ("Tongyong Pinyin romanization" :| []) NotDeprecated [NormalTag $ Normal Zh Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Subtag 16850067775273890840, VariantRecord ("Tunumiisiut" :| ["East Greenlandic","\216stgr\248nlandsk"]) NotDeprecated [NormalTag $ Normal Kl Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16973819597926957077, VariantRecord ("Unified Cornish orthography of Revived Cornish" :| []) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16973950713556107286, VariantRecord ("Unified Cornish Revised orthography of Revived Cornish" :| []) NotDeprecated [NormalTag $ Normal Kw Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16984093771673108502, VariantRecord ("Ulster dialect of Scots" :| []) NotDeprecated [NormalTag $ Normal Sco Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 16986256653835829270, VariantRecord ("Unifon phonetic alphabet" :| []) NotDeprecated [NormalTag $ Normal En Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Hup Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Kyh Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Tol Nothing Nothing Nothing (S.fromList []) M.empty [],NormalTag $ Normal Yur Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 17115735002453147671, VariantRecord ("Vedic Sanskrit" :| []) NotDeprecated [NormalTag $ Normal Sa Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 17115761462102778136, VariantRecord ("Valencian" :| []) NotDeprecated [NormalTag $ Normal Ca Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 17115761936163861016, VariantRecord ("Vallader idiom of Romansh" :| []) NotDeprecated [NormalTag $ Normal Rm Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 17120185830396518680, VariantRecord ("Latvian orthography used before 1920s (\"vec\257 druka\")" :| []) NotDeprecated [NormalTag $ Normal Lv Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 17124856349549326360, VariantRecord ("Vivaro-Alpine" :| []) NotDeprecated [NormalTag $ Normal Oc Nothing Nothing Nothing (S.fromList []) M.empty []])
  ,(Subtag 17259806277701625112, VariantRecord ("Wade-Giles romanization" :| []) NotDeprecated [NormalTag $ Normal Zh Nothing (Just Latn) Nothing (S.fromList []) M.empty []])
  ,(Subtag 17424232613601013528, VariantRecord ("Standard X-system orthographic fallback for spelling\nEsperanto" :| []) NotDeprecated [NormalTag $ Normal Eo Nothing Nothing Nothing (S.fromList []) M.empty []])]

-- | Look up the subtag and record details associated to the given 'Variant'
lookupVariantDetails :: Variant -> (Subtag, VariantRecord)
lookupVariantDetails = V.unsafeIndex variantDetails . fromEnum

-- | Validate the given 'Subtag' against the variant records in the registry
parseVariant :: Subtag -> Maybe Variant
parseVariant = fmap toEnum . flip (binSearchIndexOn fst) variantDetails

-- | Look up the 'Subtag' associated to the given 'Variant'
variantToSubtag :: Variant -> Subtag
variantToSubtag = fst . lookupVariantDetails

-- | Look up the 'VariantRecord' associated to the given 'Variant'
lookupVariantRecord :: Variant -> VariantRecord
lookupVariantRecord = snd . lookupVariantDetails
