-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}

module Text.LanguageTag.Internal.BCP47.Variant where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..), hashUsing)
import Text.LanguageTag.Internal.BCP47.Syntax (Subtag(..), unwrapSubtag)
import qualified Data.HashMap.Strict as HM

-- | The BCP47 variant tags as of 2021-02-23.
data Variant
  = V1606nict -- ^ @1606nict@. Description: Late Middle French (to 1606).
  | V1694acad -- ^ @1694acad@. Description: Early Modern French.
  | V1901 -- ^ @1901@. Description: Traditional German orthography.
  | V1959acad -- ^ @1959acad@. Description: \"Academic\" (\"governmental\") variant of Belarusian as codified in 1959.
  | V1994 -- ^ @1994@. Description: Standardized Resian orthography.
  | V1996 -- ^ @1996@. Description: German orthography of 1996.
  | Abl1943 -- ^ @abl1943@. Description: Orthographic formulation of 1943 - Official in Brazil (Formulário Ortográfico de 1943 - Oficial no Brasil).
  | Akuapem -- ^ @akuapem@. Description: Akuapem Twi.
  | Alalc97 -- ^ @alalc97@. Description: ALA-LC Romanization, 1997 edition.
  | Aluku -- ^ @aluku@. Description: Aluku dialect; Boni dialect.
  | Ao1990 -- ^ @ao1990@. Description: Portuguese Language Orthographic Agreement of 1990 (Acordo Ortográfico da Língua Portuguesa de 1990).
  | Aranes -- ^ @aranes@. Description: Aranese.
  | Arevela -- ^ @arevela@. Description: Eastern Armenian. Deprecated.
  | Arevmda -- ^ @arevmda@. Description: Western Armenian. Deprecated.
  | Arkaika -- ^ @arkaika@. Description: Arcaicam Esperantom; Arkaika Esperanto.
  | Asante -- ^ @asante@. Description: Asante Twi; Ashanti Twi.
  | Auvern -- ^ @auvern@. Description: Auvergnat.
  | Baku1926 -- ^ @baku1926@. Description: Unified Turkic Latin Alphabet (Historical).
  | Balanka -- ^ @balanka@. Description: The Balanka dialect of Anii.
  | Barla -- ^ @barla@. Description: The Barlavento dialect group of Kabuverdianu.
  | Basiceng -- ^ @basiceng@. Description: Basic English.
  | Bauddha -- ^ @bauddha@. Description: Buddhist Hybrid Sanskrit.
  | Biscayan -- ^ @biscayan@. Description: Biscayan dialect of Basque.
  | Biske -- ^ @biske@. Description: The San Giorgio dialect of Resian; The Bila dialect of Resian.
  | Bohoric -- ^ @bohoric@. Description: Slovene in Bohorič alphabet.
  | Boont -- ^ @boont@. Description: Boontling.
  | Bornholm -- ^ @bornholm@. Description: Bornholmsk.
  | Cisaup -- ^ @cisaup@. Description: Cisalpine.
  | Colb1945 -- ^ @colb1945@. Description: Portuguese-Brazilian Orthographic Convention of 1945 (Convenção Ortográfica Luso-Brasileira de 1945).
  | Cornu -- ^ @cornu@. Description: Cornu-English; Cornish English; Anglo-Cornish.
  | Creiss -- ^ @creiss@. Description: Occitan variants of the Croissant area.
  | Dajnko -- ^ @dajnko@. Description: Slovene in Dajnko alphabet.
  | Ekavsk -- ^ @ekavsk@. Description: Serbian with Ekavian pronunciation.
  | Emodeng -- ^ @emodeng@. Description: Early Modern English (1500-1700).
  | Fonipa -- ^ @fonipa@. Description: International Phonetic Alphabet.
  | Fonkirsh -- ^ @fonkirsh@. Description: Kirshenbaum Phonetic Alphabet.
  | Fonnapa -- ^ @fonnapa@. Description: North American Phonetic Alphabet; Americanist Phonetic Notation.
  | Fonupa -- ^ @fonupa@. Description: Uralic Phonetic Alphabet.
  | Fonxsamp -- ^ @fonxsamp@. Description: X-SAMPA transcription.
  | Gascon -- ^ @gascon@. Description: Gascon.
  | Grclass -- ^ @grclass@. Description: Classical Occitan orthography.
  | Grital -- ^ @grital@. Description: Italian-inspired Occitan orthography.
  | Grmistr -- ^ @grmistr@. Description: Mistralian or Mistralian-inspired Occitan orthography.
  | Hepburn -- ^ @hepburn@. Description: Hepburn romanization.
  | Heploc -- ^ @heploc@. Description: Hepburn romanization, Library of Congress method. Deprecated. Preferred value: alalc97.
  | Hognorsk -- ^ @hognorsk@. Description: Norwegian in Høgnorsk (High Norwegian) orthography.
  | Hsistemo -- ^ @hsistemo@. Description: Standard H-system orthographic fallback for spelling Esperanto.
  | Ijekavsk -- ^ @ijekavsk@. Description: Serbian with Ijekavian pronunciation.
  | Itihasa -- ^ @itihasa@. Description: Epic Sanskrit.
  | Ivanchov -- ^ @ivanchov@. Description: Bulgarian in 1899 orthography.
  | Jauer -- ^ @jauer@. Description: Jauer dialect of Romansh.
  | Jyutping -- ^ @jyutping@. Description: Jyutping Cantonese Romanization.
  | Kkcor -- ^ @kkcor@. Description: Common Cornish orthography of Revived Cornish.
  | Kociewie -- ^ @kociewie@. Description: The Kociewie dialect of Polish.
  | Kscor -- ^ @kscor@. Description: Standard Cornish orthography of Revived Cornish; Kernowek Standard.
  | Laukika -- ^ @laukika@. Description: Classical Sanskrit.
  | Lemosin -- ^ @lemosin@. Description: Limousin.
  | Lengadoc -- ^ @lengadoc@. Description: Languedocien.
  | Lipaw -- ^ @lipaw@. Description: The Lipovaz dialect of Resian; The Lipovec dialect of Resian.
  | Luna1918 -- ^ @luna1918@. Description: Post-1917 Russian orthography.
  | Metelko -- ^ @metelko@. Description: Slovene in Metelko alphabet.
  | Monoton -- ^ @monoton@. Description: Monotonic Greek.
  | Ndyuka -- ^ @ndyuka@. Description: Ndyuka dialect; Aukan dialect.
  | Nedis -- ^ @nedis@. Description: Natisone dialect; Nadiza dialect.
  | Newfound -- ^ @newfound@. Description: Newfoundland English.
  | Nicard -- ^ @nicard@. Description: Niçard.
  | Njiva -- ^ @njiva@. Description: The Gniva dialect of Resian; The Njiva dialect of Resian.
  | Nulik -- ^ @nulik@. Description: Volapük nulik; Volapük perevidöl; Volapük nulädik; de Jong\'s Volapük; New Volapük; Revised Volapük; Modern Volapük.
  | Osojs -- ^ @osojs@. Description: The Oseacco dialect of Resian; The Osojane dialect of Resian.
  | Oxendict -- ^ @oxendict@. Description: Oxford English Dictionary spelling.
  | Pahawh2 -- ^ @pahawh2@. Description: Pahawh Hmong Second Stage Reduced orthography.
  | Pahawh3 -- ^ @pahawh3@. Description: Pahawh Hmong Third Stage Reduced orthography.
  | Pahawh4 -- ^ @pahawh4@. Description: Pahawh Hmong Final Version orthography.
  | Pamaka -- ^ @pamaka@. Description: Pamaka dialect.
  | Peano -- ^ @peano@. Description: Latino Sine Flexione; Interlingua de API; Interlingua de Peano.
  | Petr1708 -- ^ @petr1708@. Description: Petrine orthography.
  | Pinyin -- ^ @pinyin@. Description: Pinyin romanization.
  | Polyton -- ^ @polyton@. Description: Polytonic Greek.
  | Provenc -- ^ @provenc@. Description: Provençal.
  | Puter -- ^ @puter@. Description: Puter idiom of Romansh.
  | Rigik -- ^ @rigik@. Description: Volapük rigik; Schleyer\'s Volapük; Original Volapük; Classic Volapük.
  | Rozaj -- ^ @rozaj@. Description: Resian; Resianic; Rezijan.
  | Rumgr -- ^ @rumgr@. Description: Rumantsch Grischun.
  | Scotland -- ^ @scotland@. Description: Scottish Standard English.
  | Scouse -- ^ @scouse@. Description: Scouse.
  | Simple -- ^ @simple@. Description: Simplified form.
  | Solba -- ^ @solba@. Description: The Stolvizza dialect of Resian; The Solbica dialect of Resian.
  | Sotav -- ^ @sotav@. Description: The Sotavento dialect group of Kabuverdianu.
  | Spanglis -- ^ @spanglis@. Description: Spanglish.
  | Surmiran -- ^ @surmiran@. Description: Surmiran idiom of Romansh.
  | Sursilv -- ^ @sursilv@. Description: Sursilvan idiom of Romansh.
  | Sutsilv -- ^ @sutsilv@. Description: Sutsilvan idiom of Romansh.
  | Tarask -- ^ @tarask@. Description: Belarusian in Taraskievica orthography.
  | Tongyong -- ^ @tongyong@. Description: Tongyong Pinyin romanization.
  | Tunumiit -- ^ @tunumiit@. Description: Tunumiisiut; East Greenlandic; Østgrønlandsk.
  | Uccor -- ^ @uccor@. Description: Unified Cornish orthography of Revived Cornish.
  | Ucrcor -- ^ @ucrcor@. Description: Unified Cornish Revised orthography of Revived Cornish.
  | Ulster -- ^ @ulster@. Description: Ulster dialect of Scots.
  | Unifon -- ^ @unifon@. Description: Unifon phonetic alphabet.
  | Vaidika -- ^ @vaidika@. Description: Vedic Sanskrit.
  | Valencia -- ^ @valencia@. Description: Valencian.
  | Vallader -- ^ @vallader@. Description: Vallader idiom of Romansh.
  | Vecdruka -- ^ @vecdruka@. Description: Latvian orthography used before 1920s (\"vecā druka\").
  | Vivaraup -- ^ @vivaraup@. Description: Vivaro-Alpine.
  | Wadegile -- ^ @wadegile@. Description: Wade-Giles romanization.
  | Xsistemo -- ^ @xsistemo@. Description: Standard X-system orthographic fallback for spelling Esperanto.

  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData Variant where
  rnf a = seq a ()

instance Hashable Variant where
  hashWithSalt = hashUsing fromEnum

parseVariant :: Subtag -> Maybe Variant
parseVariant = flip HM.lookup table . unwrapSubtag
  where
    table = HM.fromList
      [ (7122868793502725128, V1606nict)
      , (7122947813896414216, V1694acad)
      , (7126246090126393348, V1901)
      , (7126290672842236936, V1959acad)
      , (7126325461122023428, V1994)
      , (7126325598560976900, V1996)
      , (14090464810350772231, Abl1943)
      , (14100680402619760647, Akuapem)
      , (14101631129414762503, Alalc97)
      , (14101806991978528773, Aluku)
      , (14104583089388781574, Ao1990)
      , (14108386667609980934, Aranes)
      , (14108422401711702023, Arevela)
      , (14108422405973114887, Arevmda)
      , (14108473737304113159, Arkaika)
      , (14109512575511166982, Asante)
      , (14111948473767034886, Auvern)
      , (14233449971077166600, Baku1926)
      , (14233457425741021191, Balanka)
      , (14233510950782107653, Barla)
      , (14233519542217697032, Basiceng)
      , (14233536791355359239, Bauddha)
      , (14242526328165297672, Biscayan)
      , (14242526879557877765, Biske)
      , (14249185804276563975, Bohoric)
      , (14249247308838338565, Boont)
      , (14249273691144088840, Bornholm)
      , (14386641389498662918, Cisaup)
      , (14393335248271848712, Colb1945)
      , (14393388885730131973, Cornu)
      , (14396651892052590598, Creiss)
      , (14521671101462872070, Dajnko)
      , (14676966677803958278, Ekavsk)
      , (14679340378469138439, Emodeng)
      , (14825698919710720006, Fonipa)
      , (14825699053466675208, Fonkirsh)
      , (14825699255321133063, Fonnapa)
      , (14825699744344440838, Fonupa)
      , (14825699952117084168, Fonxsamp)
      , (14954095076756684806, Gascon)
      , (14973095248668491783, Grclass)
      , (14973148574949310470, Grital)
      , (14973183013108121607, Grmistr)
      , (15102687410702974983, Hepburn)
      , (15102688094609997830, Heploc)
      , (15113868066346887944, Hognorsk)
      , (15118389604387319560, Hsistemo)
      , (15252335949044640520, Ijekavsk)
      , (15263629926313525255, Itihasa)
      , (15265811770727986696, Ivanchov)
      , (15386458371758489605, Jauer)
      , (15413480999685154568, Jyutping)
      , (15541674916423139333, Kkcor)
      , (15546178097256916232, Kociewie)
      , (15550682115677880325, Kscor)
      , (15674689155847192583, Laukika)
      , (15679122666969038855, Lemosin)
      , (15679130903621657352, Lengadoc)
      , (15683651694506278917, Lipaw)
      , (15697144863862077448, Luna1918)
      , (15823298736751607815, Metelko)
      , (15834505650768379911, Monoton)
      , (15966333104314908678, Ndyuka)
      , (15967273465522683909, Nedis)
      , (15967440383478555656, Newfound)
      , (15971767719183777798, Nicard)
      , (15972947829211529221, Njiva)
      , (15985358228481376261, Nulik)
      , (16127248078037057541, Osojs)
      , (16132789883909567496, Oxendict)
      , (16251034879248629767, Pahawh2)
      , (16251034879248662535, Pahawh3)
      , (16251034879248695303, Pahawh4)
      , (16251078853240291334, Pamaka)
      , (16255477794845229061, Peano)
      , (16255645162436835336, Petr1708)
      , (16260096496836280326, Pinyin)
      , (16266834310004670471, Polyton)
      , (16270238183788216327, Provenc)
      , (16273658702257455109, Puter)
      , (16548264201437577221, Rigik)
      , (16555186176353370117, Rozaj)
      , (16561827643196964869, Rumgr)
      , (16685695115678147592, Scotland)
      , (16685695188168867846, Scouse)
      , (16692432648068399110, Simple)
      , (16699178283014553605, Solba)
      , (16699248594313543685, Sotav)
      , (16700208254209553160, Spanglis)
      , (16705987219704311304, Surmiran)
      , (16705987631996665863, Sursilv)
      , (16706005224182710279, Sutsilv)
      , (16827583590345736198, Tarask)
      , (16843311420227938056, Tongyong)
      , (16850067775273890824, Tunumiit)
      , (16973819597926957061, Uccor)
      , (16973950713556107270, Ucrcor)
      , (16984093771673108486, Ulster)
      , (16986256653835829254, Unifon)
      , (17115735002453147655, Vaidika)
      , (17115761462102778120, Valencia)
      , (17115761936163861000, Vallader)
      , (17120185830396518664, Vecdruka)
      , (17124856349549326344, Vivaraup)
      , (17259806277701625096, Wadegile)
      , (17424232613601013512, Xsistemo)]
variantToSubtag :: Variant -> Subtag
variantToSubtag x = case x of
  V1606nict -> Subtag 7122868793502725128
  V1694acad -> Subtag 7122947813896414216
  V1901 -> Subtag 7126246090126393348
  V1959acad -> Subtag 7126290672842236936
  V1994 -> Subtag 7126325461122023428
  V1996 -> Subtag 7126325598560976900
  Abl1943 -> Subtag 14090464810350772231
  Akuapem -> Subtag 14100680402619760647
  Alalc97 -> Subtag 14101631129414762503
  Aluku -> Subtag 14101806991978528773
  Ao1990 -> Subtag 14104583089388781574
  Aranes -> Subtag 14108386667609980934
  Arevela -> Subtag 14108422401711702023
  Arevmda -> Subtag 14108422405973114887
  Arkaika -> Subtag 14108473737304113159
  Asante -> Subtag 14109512575511166982
  Auvern -> Subtag 14111948473767034886
  Baku1926 -> Subtag 14233449971077166600
  Balanka -> Subtag 14233457425741021191
  Barla -> Subtag 14233510950782107653
  Basiceng -> Subtag 14233519542217697032
  Bauddha -> Subtag 14233536791355359239
  Biscayan -> Subtag 14242526328165297672
  Biske -> Subtag 14242526879557877765
  Bohoric -> Subtag 14249185804276563975
  Boont -> Subtag 14249247308838338565
  Bornholm -> Subtag 14249273691144088840
  Cisaup -> Subtag 14386641389498662918
  Colb1945 -> Subtag 14393335248271848712
  Cornu -> Subtag 14393388885730131973
  Creiss -> Subtag 14396651892052590598
  Dajnko -> Subtag 14521671101462872070
  Ekavsk -> Subtag 14676966677803958278
  Emodeng -> Subtag 14679340378469138439
  Fonipa -> Subtag 14825698919710720006
  Fonkirsh -> Subtag 14825699053466675208
  Fonnapa -> Subtag 14825699255321133063
  Fonupa -> Subtag 14825699744344440838
  Fonxsamp -> Subtag 14825699952117084168
  Gascon -> Subtag 14954095076756684806
  Grclass -> Subtag 14973095248668491783
  Grital -> Subtag 14973148574949310470
  Grmistr -> Subtag 14973183013108121607
  Hepburn -> Subtag 15102687410702974983
  Heploc -> Subtag 15102688094609997830
  Hognorsk -> Subtag 15113868066346887944
  Hsistemo -> Subtag 15118389604387319560
  Ijekavsk -> Subtag 15252335949044640520
  Itihasa -> Subtag 15263629926313525255
  Ivanchov -> Subtag 15265811770727986696
  Jauer -> Subtag 15386458371758489605
  Jyutping -> Subtag 15413480999685154568
  Kkcor -> Subtag 15541674916423139333
  Kociewie -> Subtag 15546178097256916232
  Kscor -> Subtag 15550682115677880325
  Laukika -> Subtag 15674689155847192583
  Lemosin -> Subtag 15679122666969038855
  Lengadoc -> Subtag 15679130903621657352
  Lipaw -> Subtag 15683651694506278917
  Luna1918 -> Subtag 15697144863862077448
  Metelko -> Subtag 15823298736751607815
  Monoton -> Subtag 15834505650768379911
  Ndyuka -> Subtag 15966333104314908678
  Nedis -> Subtag 15967273465522683909
  Newfound -> Subtag 15967440383478555656
  Nicard -> Subtag 15971767719183777798
  Njiva -> Subtag 15972947829211529221
  Nulik -> Subtag 15985358228481376261
  Osojs -> Subtag 16127248078037057541
  Oxendict -> Subtag 16132789883909567496
  Pahawh2 -> Subtag 16251034879248629767
  Pahawh3 -> Subtag 16251034879248662535
  Pahawh4 -> Subtag 16251034879248695303
  Pamaka -> Subtag 16251078853240291334
  Peano -> Subtag 16255477794845229061
  Petr1708 -> Subtag 16255645162436835336
  Pinyin -> Subtag 16260096496836280326
  Polyton -> Subtag 16266834310004670471
  Provenc -> Subtag 16270238183788216327
  Puter -> Subtag 16273658702257455109
  Rigik -> Subtag 16548264201437577221
  Rozaj -> Subtag 16555186176353370117
  Rumgr -> Subtag 16561827643196964869
  Scotland -> Subtag 16685695115678147592
  Scouse -> Subtag 16685695188168867846
  Simple -> Subtag 16692432648068399110
  Solba -> Subtag 16699178283014553605
  Sotav -> Subtag 16699248594313543685
  Spanglis -> Subtag 16700208254209553160
  Surmiran -> Subtag 16705987219704311304
  Sursilv -> Subtag 16705987631996665863
  Sutsilv -> Subtag 16706005224182710279
  Tarask -> Subtag 16827583590345736198
  Tongyong -> Subtag 16843311420227938056
  Tunumiit -> Subtag 16850067775273890824
  Uccor -> Subtag 16973819597926957061
  Ucrcor -> Subtag 16973950713556107270
  Ulster -> Subtag 16984093771673108486
  Unifon -> Subtag 16986256653835829254
  Vaidika -> Subtag 17115735002453147655
  Valencia -> Subtag 17115761462102778120
  Vallader -> Subtag 17115761936163861000
  Vecdruka -> Subtag 17120185830396518664
  Vivaraup -> Subtag 17124856349549326344
  Wadegile -> Subtag 17259806277701625096
  Xsistemo -> Subtag 17424232613601013512
