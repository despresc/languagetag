-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.LanguageTag.Internal.BCP47.Variant where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import Text.LanguageTag.Subtag (renderSubtag)

-- | A valid BCP47 variant tag as of 2021-02-23
newtype Variant = Variant Subtag
  deriving (Eq, Ord, Hashable, NFData)

instance Show Variant where
  show (Variant t) = T.unpack $ renderSubtag t

 -- | @1606nict@. Description: Late Middle French (to 1606).
pattern Var1606nict :: Variant
pattern Var1606nict = Variant (Subtag 0)

 -- | @1694acad@. Description: Early Modern French.
pattern Var1694acad :: Variant
pattern Var1694acad = Variant (Subtag 0)

 -- | @1901@. Description: Traditional German orthography.
pattern Var1901 :: Variant
pattern Var1901 = Variant (Subtag 7126246090126393380)

 -- | @1959acad@. Description: \"Academic\" (\"governmental\") variant of Belarusian as codified in 1959.
pattern Var1959acad :: Variant
pattern Var1959acad = Variant (Subtag 0)

 -- | @1994@. Description: Standardized Resian orthography.
pattern Var1994 :: Variant
pattern Var1994 = Variant (Subtag 7126325461122023460)

 -- | @1996@. Description: German orthography of 1996.
pattern Var1996 :: Variant
pattern Var1996 = Variant (Subtag 7126325598560976932)

 -- | @abl1943@. Description: Orthographic formulation of 1943 - Official in Brazil (Formulário Ortográfico de 1943 - Oficial no Brasil).
pattern Abl1943 :: Variant
pattern Abl1943 = Variant (Subtag 0)

 -- | @akuapem@. Description: Akuapem Twi.
pattern Akuapem :: Variant
pattern Akuapem = Variant (Subtag 14100680402619760663)

 -- | @alalc97@. Description: ALA-LC Romanization, 1997 edition.
pattern Alalc97 :: Variant
pattern Alalc97 = Variant (Subtag 0)

 -- | @aluku@. Description: Aluku dialect; Boni dialect.
pattern Aluku :: Variant
pattern Aluku = Variant (Subtag 14101806991978528789)

 -- | @ao1990@. Description: Portuguese Language Orthographic Agreement of 1990 (Acordo Ortográfico da Língua Portuguesa de 1990).
pattern Ao1990 :: Variant
pattern Ao1990 = Variant (Subtag 0)

 -- | @aranes@. Description: Aranese.
pattern Aranes :: Variant
pattern Aranes = Variant (Subtag 14108386667609980950)

 -- | @arevela@. Description: Eastern Armenian. Deprecated.
pattern Arevela :: Variant
pattern Arevela = Variant (Subtag 14108422401711702039)

 -- | @arevmda@. Description: Western Armenian. Deprecated.
pattern Arevmda :: Variant
pattern Arevmda = Variant (Subtag 14108422405973114903)

 -- | @arkaika@. Description: Arcaicam Esperantom; Arkaika Esperanto.
pattern Arkaika :: Variant
pattern Arkaika = Variant (Subtag 14108473737304113175)

 -- | @asante@. Description: Asante Twi; Ashanti Twi.
pattern Asante :: Variant
pattern Asante = Variant (Subtag 14109512575511166998)

 -- | @auvern@. Description: Auvergnat.
pattern Auvern :: Variant
pattern Auvern = Variant (Subtag 14111948473767034902)

 -- | @baku1926@. Description: Unified Turkic Latin Alphabet (Historical).
pattern Baku1926 :: Variant
pattern Baku1926 = Variant (Subtag 0)

 -- | @balanka@. Description: The Balanka dialect of Anii.
pattern Balanka :: Variant
pattern Balanka = Variant (Subtag 14233457425741021207)

 -- | @barla@. Description: The Barlavento dialect group of Kabuverdianu.
pattern Barla :: Variant
pattern Barla = Variant (Subtag 14233510950782107669)

 -- | @basiceng@. Description: Basic English.
pattern Basiceng :: Variant
pattern Basiceng = Variant (Subtag 14233519542217697048)

 -- | @bauddha@. Description: Buddhist Hybrid Sanskrit.
pattern Bauddha :: Variant
pattern Bauddha = Variant (Subtag 14233536791355359255)

 -- | @biscayan@. Description: Biscayan dialect of Basque.
pattern Biscayan :: Variant
pattern Biscayan = Variant (Subtag 14242526328165297688)

 -- | @biske@. Description: The San Giorgio dialect of Resian; The Bila dialect of Resian.
pattern Biske :: Variant
pattern Biske = Variant (Subtag 14242526879557877781)

 -- | @bohoric@. Description: Slovene in Bohorič alphabet.
pattern Bohoric :: Variant
pattern Bohoric = Variant (Subtag 14249185804276563991)

 -- | @boont@. Description: Boontling.
pattern Boont :: Variant
pattern Boont = Variant (Subtag 14249247308838338581)

 -- | @bornholm@. Description: Bornholmsk.
pattern Bornholm :: Variant
pattern Bornholm = Variant (Subtag 14249273691144088856)

 -- | @cisaup@. Description: Cisalpine.
pattern Cisaup :: Variant
pattern Cisaup = Variant (Subtag 14386641389498662934)

 -- | @colb1945@. Description: Portuguese-Brazilian Orthographic Convention of 1945 (Convenção Ortográfica Luso-Brasileira de 1945).
pattern Colb1945 :: Variant
pattern Colb1945 = Variant (Subtag 0)

 -- | @cornu@. Description: Cornu-English; Cornish English; Anglo-Cornish.
pattern Cornu :: Variant
pattern Cornu = Variant (Subtag 14393388885730131989)

 -- | @creiss@. Description: Occitan variants of the Croissant area.
pattern Creiss :: Variant
pattern Creiss = Variant (Subtag 14396651892052590614)

 -- | @dajnko@. Description: Slovene in Dajnko alphabet.
pattern Dajnko :: Variant
pattern Dajnko = Variant (Subtag 14521671101462872086)

 -- | @ekavsk@. Description: Serbian with Ekavian pronunciation.
pattern Ekavsk :: Variant
pattern Ekavsk = Variant (Subtag 14676966677803958294)

 -- | @emodeng@. Description: Early Modern English (1500-1700).
pattern Emodeng :: Variant
pattern Emodeng = Variant (Subtag 14679340378469138455)

 -- | @fonipa@. Description: International Phonetic Alphabet.
pattern Fonipa :: Variant
pattern Fonipa = Variant (Subtag 14825698919710720022)

 -- | @fonkirsh@. Description: Kirshenbaum Phonetic Alphabet.
pattern Fonkirsh :: Variant
pattern Fonkirsh = Variant (Subtag 14825699053466675224)

 -- | @fonnapa@. Description: North American Phonetic Alphabet; Americanist Phonetic Notation.
pattern Fonnapa :: Variant
pattern Fonnapa = Variant (Subtag 14825699255321133079)

 -- | @fonupa@. Description: Uralic Phonetic Alphabet.
pattern Fonupa :: Variant
pattern Fonupa = Variant (Subtag 14825699744344440854)

 -- | @fonxsamp@. Description: X-SAMPA transcription.
pattern Fonxsamp :: Variant
pattern Fonxsamp = Variant (Subtag 14825699952117084184)

 -- | @gascon@. Description: Gascon.
pattern Gascon :: Variant
pattern Gascon = Variant (Subtag 14954095076756684822)

 -- | @grclass@. Description: Classical Occitan orthography.
pattern Grclass :: Variant
pattern Grclass = Variant (Subtag 14973095248668491799)

 -- | @grital@. Description: Italian-inspired Occitan orthography.
pattern Grital :: Variant
pattern Grital = Variant (Subtag 14973148574949310486)

 -- | @grmistr@. Description: Mistralian or Mistralian-inspired Occitan orthography.
pattern Grmistr :: Variant
pattern Grmistr = Variant (Subtag 14973183013108121623)

 -- | @hepburn@. Description: Hepburn romanization.
pattern Hepburn :: Variant
pattern Hepburn = Variant (Subtag 15102687410702974999)

 -- | @heploc@. Description: Hepburn romanization, Library of Congress method. Deprecated. Preferred value: alalc97.
pattern Heploc :: Variant
pattern Heploc = Variant (Subtag 15102688094609997846)

 -- | @hognorsk@. Description: Norwegian in Høgnorsk (High Norwegian) orthography.
pattern Hognorsk :: Variant
pattern Hognorsk = Variant (Subtag 15113868066346887960)

 -- | @hsistemo@. Description: Standard H-system orthographic fallback for spelling Esperanto.
pattern Hsistemo :: Variant
pattern Hsistemo = Variant (Subtag 15118389604387319576)

 -- | @ijekavsk@. Description: Serbian with Ijekavian pronunciation.
pattern Ijekavsk :: Variant
pattern Ijekavsk = Variant (Subtag 15252335949044640536)

 -- | @itihasa@. Description: Epic Sanskrit.
pattern Itihasa :: Variant
pattern Itihasa = Variant (Subtag 15263629926313525271)

 -- | @ivanchov@. Description: Bulgarian in 1899 orthography.
pattern Ivanchov :: Variant
pattern Ivanchov = Variant (Subtag 15265811770727986712)

 -- | @jauer@. Description: Jauer dialect of Romansh.
pattern Jauer :: Variant
pattern Jauer = Variant (Subtag 15386458371758489621)

 -- | @jyutping@. Description: Jyutping Cantonese Romanization.
pattern Jyutping :: Variant
pattern Jyutping = Variant (Subtag 15413480999685154584)

 -- | @kkcor@. Description: Common Cornish orthography of Revived Cornish.
pattern Kkcor :: Variant
pattern Kkcor = Variant (Subtag 15541674916423139349)

 -- | @kociewie@. Description: The Kociewie dialect of Polish.
pattern Kociewie :: Variant
pattern Kociewie = Variant (Subtag 15546178097256916248)

 -- | @kscor@. Description: Standard Cornish orthography of Revived Cornish; Kernowek Standard.
pattern Kscor :: Variant
pattern Kscor = Variant (Subtag 15550682115677880341)

 -- | @laukika@. Description: Classical Sanskrit.
pattern Laukika :: Variant
pattern Laukika = Variant (Subtag 15674689155847192599)

 -- | @lemosin@. Description: Limousin.
pattern Lemosin :: Variant
pattern Lemosin = Variant (Subtag 15679122666969038871)

 -- | @lengadoc@. Description: Languedocien.
pattern Lengadoc :: Variant
pattern Lengadoc = Variant (Subtag 15679130903621657368)

 -- | @lipaw@. Description: The Lipovaz dialect of Resian; The Lipovec dialect of Resian.
pattern Lipaw :: Variant
pattern Lipaw = Variant (Subtag 15683651694506278933)

 -- | @luna1918@. Description: Post-1917 Russian orthography.
pattern Luna1918 :: Variant
pattern Luna1918 = Variant (Subtag 0)

 -- | @metelko@. Description: Slovene in Metelko alphabet.
pattern Metelko :: Variant
pattern Metelko = Variant (Subtag 15823298736751607831)

 -- | @monoton@. Description: Monotonic Greek.
pattern Monoton :: Variant
pattern Monoton = Variant (Subtag 15834505650768379927)

 -- | @ndyuka@. Description: Ndyuka dialect; Aukan dialect.
pattern Ndyuka :: Variant
pattern Ndyuka = Variant (Subtag 15966333104314908694)

 -- | @nedis@. Description: Natisone dialect; Nadiza dialect.
pattern Nedis :: Variant
pattern Nedis = Variant (Subtag 15967273465522683925)

 -- | @newfound@. Description: Newfoundland English.
pattern Newfound :: Variant
pattern Newfound = Variant (Subtag 15967440383478555672)

 -- | @nicard@. Description: Niçard.
pattern Nicard :: Variant
pattern Nicard = Variant (Subtag 15971767719183777814)

 -- | @njiva@. Description: The Gniva dialect of Resian; The Njiva dialect of Resian.
pattern Njiva :: Variant
pattern Njiva = Variant (Subtag 15972947829211529237)

 -- | @nulik@. Description: Volapük nulik; Volapük perevidöl; Volapük nulädik; de Jong\'s Volapük; New Volapük; Revised Volapük; Modern Volapük.
pattern Nulik :: Variant
pattern Nulik = Variant (Subtag 15985358228481376277)

 -- | @osojs@. Description: The Oseacco dialect of Resian; The Osojane dialect of Resian.
pattern Osojs :: Variant
pattern Osojs = Variant (Subtag 16127248078037057557)

 -- | @oxendict@. Description: Oxford English Dictionary spelling.
pattern Oxendict :: Variant
pattern Oxendict = Variant (Subtag 16132789883909567512)

 -- | @pahawh2@. Description: Pahawh Hmong Second Stage Reduced orthography.
pattern Pahawh2 :: Variant
pattern Pahawh2 = Variant (Subtag 0)

 -- | @pahawh3@. Description: Pahawh Hmong Third Stage Reduced orthography.
pattern Pahawh3 :: Variant
pattern Pahawh3 = Variant (Subtag 0)

 -- | @pahawh4@. Description: Pahawh Hmong Final Version orthography.
pattern Pahawh4 :: Variant
pattern Pahawh4 = Variant (Subtag 0)

 -- | @pamaka@. Description: Pamaka dialect.
pattern Pamaka :: Variant
pattern Pamaka = Variant (Subtag 16251078853240291350)

 -- | @peano@. Description: Latino Sine Flexione; Interlingua de API; Interlingua de Peano.
pattern Peano :: Variant
pattern Peano = Variant (Subtag 16255477794845229077)

 -- | @petr1708@. Description: Petrine orthography.
pattern Petr1708 :: Variant
pattern Petr1708 = Variant (Subtag 0)

 -- | @pinyin@. Description: Pinyin romanization.
pattern Pinyin :: Variant
pattern Pinyin = Variant (Subtag 16260096496836280342)

 -- | @polyton@. Description: Polytonic Greek.
pattern Polyton :: Variant
pattern Polyton = Variant (Subtag 16266834310004670487)

 -- | @provenc@. Description: Provençal.
pattern Provenc :: Variant
pattern Provenc = Variant (Subtag 16270238183788216343)

 -- | @puter@. Description: Puter idiom of Romansh.
pattern Puter :: Variant
pattern Puter = Variant (Subtag 16273658702257455125)

 -- | @rigik@. Description: Volapük rigik; Schleyer\'s Volapük; Original Volapük; Classic Volapük.
pattern Rigik :: Variant
pattern Rigik = Variant (Subtag 16548264201437577237)

 -- | @rozaj@. Description: Resian; Resianic; Rezijan.
pattern Rozaj :: Variant
pattern Rozaj = Variant (Subtag 16555186176353370133)

 -- | @rumgr@. Description: Rumantsch Grischun.
pattern Rumgr :: Variant
pattern Rumgr = Variant (Subtag 16561827643196964885)

 -- | @scotland@. Description: Scottish Standard English.
pattern Scotland :: Variant
pattern Scotland = Variant (Subtag 16685695115678147608)

 -- | @scouse@. Description: Scouse.
pattern Scouse :: Variant
pattern Scouse = Variant (Subtag 16685695188168867862)

 -- | @simple@. Description: Simplified form.
pattern Simple :: Variant
pattern Simple = Variant (Subtag 16692432648068399126)

 -- | @solba@. Description: The Stolvizza dialect of Resian; The Solbica dialect of Resian.
pattern Solba :: Variant
pattern Solba = Variant (Subtag 16699178283014553621)

 -- | @sotav@. Description: The Sotavento dialect group of Kabuverdianu.
pattern Sotav :: Variant
pattern Sotav = Variant (Subtag 16699248594313543701)

 -- | @spanglis@. Description: Spanglish.
pattern Spanglis :: Variant
pattern Spanglis = Variant (Subtag 16700208254209553176)

 -- | @surmiran@. Description: Surmiran idiom of Romansh.
pattern Surmiran :: Variant
pattern Surmiran = Variant (Subtag 16705987219704311320)

 -- | @sursilv@. Description: Sursilvan idiom of Romansh.
pattern Sursilv :: Variant
pattern Sursilv = Variant (Subtag 16705987631996665879)

 -- | @sutsilv@. Description: Sutsilvan idiom of Romansh.
pattern Sutsilv :: Variant
pattern Sutsilv = Variant (Subtag 16706005224182710295)

 -- | @tarask@. Description: Belarusian in Taraskievica orthography.
pattern Tarask :: Variant
pattern Tarask = Variant (Subtag 16827583590345736214)

 -- | @tongyong@. Description: Tongyong Pinyin romanization.
pattern Tongyong :: Variant
pattern Tongyong = Variant (Subtag 16843311420227938072)

 -- | @tunumiit@. Description: Tunumiisiut; East Greenlandic; Østgrønlandsk.
pattern Tunumiit :: Variant
pattern Tunumiit = Variant (Subtag 16850067775273890840)

 -- | @uccor@. Description: Unified Cornish orthography of Revived Cornish.
pattern Uccor :: Variant
pattern Uccor = Variant (Subtag 16973819597926957077)

 -- | @ucrcor@. Description: Unified Cornish Revised orthography of Revived Cornish.
pattern Ucrcor :: Variant
pattern Ucrcor = Variant (Subtag 16973950713556107286)

 -- | @ulster@. Description: Ulster dialect of Scots.
pattern Ulster :: Variant
pattern Ulster = Variant (Subtag 16984093771673108502)

 -- | @unifon@. Description: Unifon phonetic alphabet.
pattern Unifon :: Variant
pattern Unifon = Variant (Subtag 16986256653835829270)

 -- | @vaidika@. Description: Vedic Sanskrit.
pattern Vaidika :: Variant
pattern Vaidika = Variant (Subtag 17115735002453147671)

 -- | @valencia@. Description: Valencian.
pattern Valencia :: Variant
pattern Valencia = Variant (Subtag 17115761462102778136)

 -- | @vallader@. Description: Vallader idiom of Romansh.
pattern Vallader :: Variant
pattern Vallader = Variant (Subtag 17115761936163861016)

 -- | @vecdruka@. Description: Latvian orthography used before 1920s (\"vecā druka\").
pattern Vecdruka :: Variant
pattern Vecdruka = Variant (Subtag 17120185830396518680)

 -- | @vivaraup@. Description: Vivaro-Alpine.
pattern Vivaraup :: Variant
pattern Vivaraup = Variant (Subtag 17124856349549326360)

 -- | @wadegile@. Description: Wade-Giles romanization.
pattern Wadegile :: Variant
pattern Wadegile = Variant (Subtag 17259806277701625112)

 -- | @xsistemo@. Description: Standard X-system orthographic fallback for spelling Esperanto.
pattern Xsistemo :: Variant
pattern Xsistemo = Variant (Subtag 17424232613601013528)
