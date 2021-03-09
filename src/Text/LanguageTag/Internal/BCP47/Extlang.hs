-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.LanguageTag.Internal.BCP47.Extlang where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import Text.LanguageTag.Subtag (renderSubtag)

-- | A valid BCP47 extended language tag as of 2021-02-23. The patterns for 'Extlang' subtags are prefixed with "Ext" because they may overlap with primary language subtags. Note that if extended language subtags have a preferred value in the documentation, then it refers to a primary subtag.
newtype Extlang = Extlang Subtag
  deriving (Eq, Ord, Hashable, NFData)

instance Show Extlang where
  show (Extlang t) = T.unpack $ (T.pack "Ext" <>) $ renderSubtag  t

 -- | @aao@. Description: Algerian Saharan Arabic. Preferred value: aao.
pattern ExtAao :: Extlang
pattern ExtAao = Extlang (Subtag 14089361900647219219)

 -- | @abh@. Description: Tajiki Arabic. Preferred value: abh.
pattern ExtAbh :: Extlang
pattern ExtAbh = Extlang (Subtag 14090426227902906387)

 -- | @abv@. Description: Baharna Arabic. Preferred value: abv.
pattern ExtAbv :: Extlang
pattern ExtAbv = Extlang (Subtag 14090549373205217299)

 -- | @acm@. Description: Mesopotamian Arabic. Preferred value: acm.
pattern ExtAcm :: Extlang
pattern ExtAcm = Extlang (Subtag 14091596108274860051)

 -- | @acq@. Description: Ta\'izzi-Adeni Arabic. Preferred value: acq.
pattern ExtAcq :: Extlang
pattern ExtAcq = Extlang (Subtag 14091631292646948883)

 -- | @acw@. Description: Hijazi Arabic. Preferred value: acw.
pattern ExtAcw :: Extlang
pattern ExtAcw = Extlang (Subtag 14091684069205082131)

 -- | @acx@. Description: Omani Arabic. Preferred value: acx.
pattern ExtAcx :: Extlang
pattern ExtAcx = Extlang (Subtag 14091692865298104339)

 -- | @acy@. Description: Cypriot Arabic. Preferred value: acy.
pattern ExtAcy :: Extlang
pattern ExtAcy = Extlang (Subtag 14091701661391126547)

 -- | @adf@. Description: Dhofari Arabic. Preferred value: adf.
pattern ExtAdf :: Extlang
pattern ExtAdf = Extlang (Subtag 14092660435530547219)

 -- | @ads@. Description: Adamorobe Sign Language. Preferred value: ads.
pattern ExtAds :: Extlang
pattern ExtAds = Extlang (Subtag 14092774784739835923)

 -- | @aeb@. Description: Tunisian Arabic. Preferred value: aeb.
pattern ExtAeb :: Extlang
pattern ExtAeb = Extlang (Subtag 14093751151065301011)

 -- | @aec@. Description: Saidi Arabic. Preferred value: aec.
pattern ExtAec :: Extlang
pattern ExtAec = Extlang (Subtag 14093759947158323219)

 -- | @aed@. Description: Argentine Sign Language. Preferred value: aed.
pattern ExtAed :: Extlang
pattern ExtAed = Extlang (Subtag 14093768743251345427)

 -- | @aen@. Description: Armenian Sign Language. Preferred value: aen.
pattern ExtAen :: Extlang
pattern ExtAen = Extlang (Subtag 14093856704181567507)

 -- | @afb@. Description: Gulf Arabic. Preferred value: afb.
pattern ExtAfb :: Extlang
pattern ExtAfb = Extlang (Subtag 14094877050972143635)

 -- | @afg@. Description: Afghan Sign Language. Preferred value: afg.
pattern ExtAfg :: Extlang
pattern ExtAfg = Extlang (Subtag 14094921031437254675)

 -- | @ajp@. Description: South Levantine Arabic. Preferred value: ajp.
pattern ExtAjp :: Extlang
pattern ExtAjp = Extlang (Subtag 14099503795901825043)

 -- | @apc@. Description: North Levantine Arabic. Preferred value: apc.
pattern ExtApc :: Extlang
pattern ExtApc = Extlang (Subtag 14106144846133592083)

 -- | @apd@. Description: Sudanese Arabic. Preferred value: apd.
pattern ExtApd :: Extlang
pattern ExtApd = Extlang (Subtag 14106153642226614291)

 -- | @arb@. Description: Standard Arabic. Preferred value: arb.
pattern ExtArb :: Extlang
pattern ExtArb = Extlang (Subtag 14108387849854255123)

 -- | @arq@. Description: Algerian Arabic. Preferred value: arq.
pattern ExtArq :: Extlang
pattern ExtArq = Extlang (Subtag 14108519791249588243)

 -- | @ars@. Description: Najdi Arabic. Preferred value: ars.
pattern ExtArs :: Extlang
pattern ExtArs = Extlang (Subtag 14108537383435632659)

 -- | @ary@. Description: Moroccan Arabic. Preferred value: ary.
pattern ExtAry :: Extlang
pattern ExtAry = Extlang (Subtag 14108590159993765907)

 -- | @arz@. Description: Egyptian Arabic. Preferred value: arz.
pattern ExtArz :: Extlang
pattern ExtArz = Extlang (Subtag 14108598956086788115)

 -- | @ase@. Description: American Sign Language. Preferred value: ase.
pattern ExtAse :: Extlang
pattern ExtAse = Extlang (Subtag 14109540138040164371)

 -- | @asf@. Description: Auslan; Australian Sign Language. Preferred value: asf.
pattern ExtAsf :: Extlang
pattern ExtAsf = Extlang (Subtag 14109548934133186579)

 -- | @asp@. Description: Algerian Sign Language. Preferred value: asp.
pattern ExtAsp :: Extlang
pattern ExtAsp = Extlang (Subtag 14109636895063408659)

 -- | @asq@. Description: Austrian Sign Language. Preferred value: asq.
pattern ExtAsq :: Extlang
pattern ExtAsq = Extlang (Subtag 14109645691156430867)

 -- | @asw@. Description: Australian Aborigines Sign Language. Preferred value: asw.
pattern ExtAsw :: Extlang
pattern ExtAsw = Extlang (Subtag 14109698467714564115)

 -- | @auz@. Description: Uzbeki Arabic. Preferred value: auz.
pattern ExtAuz :: Extlang
pattern ExtAuz = Extlang (Subtag 14111976655807315987)

 -- | @avl@. Description: Eastern Egyptian Bedawi Arabic. Preferred value: avl.
pattern ExtAvl :: Extlang
pattern ExtAvl = Extlang (Subtag 14112979410411847699)

 -- | @ayh@. Description: Hadrami Arabic. Preferred value: ayh.
pattern ExtAyh :: Extlang
pattern ExtAyh = Extlang (Subtag 14116321925760286739)

 -- | @ayl@. Description: Libyan Arabic. Preferred value: ayl.
pattern ExtAyl :: Extlang
pattern ExtAyl = Extlang (Subtag 14116357110132375571)

 -- | @ayn@. Description: Sanaani Arabic. Preferred value: ayn.
pattern ExtAyn :: Extlang
pattern ExtAyn = Extlang (Subtag 14116374702318419987)

 -- | @ayp@. Description: North Mesopotamian Arabic. Preferred value: ayp.
pattern ExtAyp :: Extlang
pattern ExtAyp = Extlang (Subtag 14116392294504464403)

 -- | @bbz@. Description: Babalia Creole Arabic. Deprecated. Preferred value: bbz.
pattern ExtBbz :: Extlang
pattern ExtBbz = Extlang (Subtag 14234699745653162003)

 -- | @bfi@. Description: British Sign Language. Preferred value: bfi.
pattern ExtBfi :: Extlang
pattern ExtBfi = Extlang (Subtag 14239053811699154963)

 -- | @bfk@. Description: Ban Khor Sign Language. Preferred value: bfk.
pattern ExtBfk :: Extlang
pattern ExtBfk = Extlang (Subtag 14239071403885199379)

 -- | @bjn@. Description: Banjar. Preferred value: bjn.
pattern ExtBjn :: Extlang
pattern ExtBjn = Extlang (Subtag 14243601391791636499)

 -- | @bog@. Description: Bamako Sign Language. Preferred value: bog.
pattern ExtBog :: Extlang
pattern ExtBog = Extlang (Subtag 14249169318674694163)

 -- | @bqn@. Description: Bulgarian Sign Language. Preferred value: bqn.
pattern ExtBqn :: Extlang
pattern ExtBqn = Extlang (Subtag 14251482691139534867)

 -- | @bqy@. Description: Bengkala Sign Language. Preferred value: bqy.
pattern ExtBqy :: Extlang
pattern ExtBqy = Extlang (Subtag 14251579448162779155)

 -- | @btj@. Description: Bacanese Malay. Preferred value: btj.
pattern ExtBtj :: Extlang
pattern ExtBtj = Extlang (Subtag 14254825206487973907)

 -- | @bve@. Description: Berau Malay. Preferred value: bve.
pattern ExtBve :: Extlang
pattern ExtBve = Extlang (Subtag 14257033025836548115)

 -- | @bvl@. Description: Bolivian Sign Language. Preferred value: bvl.
pattern ExtBvl :: Extlang
pattern ExtBvl = Extlang (Subtag 14257094598487703571)

 -- | @bvu@. Description: Bukit Malay. Preferred value: bvu.
pattern ExtBvu :: Extlang
pattern ExtBvu = Extlang (Subtag 14257173763324903443)

 -- | @bzs@. Description: Brazilian Sign Language. Preferred value: bzs.
pattern ExtBzs :: Extlang
pattern ExtBzs = Extlang (Subtag 14261659770766229523)

 -- | @cdo@. Description: Min Dong Chinese. Preferred value: cdo.
pattern ExtCdo :: Extlang
pattern ExtCdo = Extlang (Subtag 14380969976519458835)

 -- | @cds@. Description: Chadian Sign Language. Preferred value: cds.
pattern ExtCds :: Extlang
pattern ExtCds = Extlang (Subtag 14381005160891547667)

 -- | @cjy@. Description: Jinyu Chinese. Preferred value: cjy.
pattern ExtCjy :: Extlang
pattern ExtCjy = Extlang (Subtag 14387813336890736659)

 -- | @cmn@. Description: Mandarin Chinese. Preferred value: cmn.
pattern ExtCmn :: Extlang
pattern ExtCmn = Extlang (Subtag 14391094279588020243)

 -- | @cnp@. Description: Northern Ping Chinese; Northern Pinghua. Preferred value: cnp.
pattern ExtCnp :: Extlang
pattern ExtCnp = Extlang (Subtag 14392237771680907283)

 -- | @coa@. Description: Cocos Islands Malay. Preferred value: coa.
pattern ExtCoa :: Extlang
pattern ExtCoa = Extlang (Subtag 14393231730192416787)

 -- | @cpx@. Description: Pu-Xian Chinese. Preferred value: cpx.
pattern ExtCpx :: Extlang
pattern ExtCpx = Extlang (Subtag 14394559940238770195)

 -- | @csc@. Description: Catalan Sign Language; Lengua de señas catalana; Llengua de Signes Catalana. Preferred value: csc.
pattern ExtCsc :: Extlang
pattern ExtCsc = Extlang (Subtag 14397752922005831699)

 -- | @csd@. Description: Chiangmai Sign Language. Preferred value: csd.
pattern ExtCsd :: Extlang
pattern ExtCsd = Extlang (Subtag 14397761718098853907)

 -- | @cse@. Description: Czech Sign Language. Preferred value: cse.
pattern ExtCse :: Extlang
pattern ExtCse = Extlang (Subtag 14397770514191876115)

 -- | @csf@. Description: Cuba Sign Language. Preferred value: csf.
pattern ExtCsf :: Extlang
pattern ExtCsf = Extlang (Subtag 14397779310284898323)

 -- | @csg@. Description: Chilean Sign Language. Preferred value: csg.
pattern ExtCsg :: Extlang
pattern ExtCsg = Extlang (Subtag 14397788106377920531)

 -- | @csl@. Description: Chinese Sign Language. Preferred value: csl.
pattern ExtCsl :: Extlang
pattern ExtCsl = Extlang (Subtag 14397832086843031571)

 -- | @csn@. Description: Colombian Sign Language. Preferred value: csn.
pattern ExtCsn :: Extlang
pattern ExtCsn = Extlang (Subtag 14397849679029075987)

 -- | @csp@. Description: Southern Ping Chinese; Southern Pinghua. Preferred value: csp.
pattern ExtCsp :: Extlang
pattern ExtCsp = Extlang (Subtag 14397867271215120403)

 -- | @csq@. Description: Croatia Sign Language. Preferred value: csq.
pattern ExtCsq :: Extlang
pattern ExtCsq = Extlang (Subtag 14397876067308142611)

 -- | @csr@. Description: Costa Rican Sign Language. Preferred value: csr.
pattern ExtCsr :: Extlang
pattern ExtCsr = Extlang (Subtag 14397884863401164819)

 -- | @csx@. Description: Cambodian Sign Language. Preferred value: csx.
pattern ExtCsx :: Extlang
pattern ExtCsx = Extlang (Subtag 14397937639959298067)

 -- | @czh@. Description: Huizhou Chinese. Preferred value: czh.
pattern ExtCzh :: Extlang
pattern ExtCzh = Extlang (Subtag 14405678201818841107)

 -- | @czo@. Description: Min Zhong Chinese. Preferred value: czo.
pattern ExtCzo :: Extlang
pattern ExtCzo = Extlang (Subtag 14405739774469996563)

 -- | @doq@. Description: Dominican Sign Language. Preferred value: doq.
pattern ExtDoq :: Extlang
pattern ExtDoq = Extlang (Subtag 14537487655756627987)

 -- | @dse@. Description: Dutch Sign Language. Preferred value: dse.
pattern ExtDse :: Extlang
pattern ExtDse = Extlang (Subtag 14541885702267731987)

 -- | @dsl@. Description: Danish Sign Language. Preferred value: dsl.
pattern ExtDsl :: Extlang
pattern ExtDsl = Extlang (Subtag 14541947274918887443)

 -- | @dup@. Description: Duano. Preferred value: dup.
pattern ExtDup :: Extlang
pattern ExtDup = Extlang (Subtag 14544234259104661523)

 -- | @ecs@. Description: Ecuadorian Sign Language. Preferred value: ecs.
pattern ExtEcs :: Extlang
pattern ExtEcs = Extlang (Subtag 14668109637136416787)

 -- | @ehs@. Description: Miyakubo Sign Language. Preferred value: ehs.
pattern ExtEhs :: Extlang
pattern ExtEhs = Extlang (Subtag 14673739136670629907)

 -- | @esl@. Description: Egypt Sign Language. Preferred value: esl.
pattern ExtEsl :: Extlang
pattern ExtEsl = Extlang (Subtag 14686062462994743315)

 -- | @esn@. Description: Salvadoran Sign Language. Preferred value: esn.
pattern ExtEsn :: Extlang
pattern ExtEsn = Extlang (Subtag 14686080055180787731)

 -- | @eso@. Description: Estonian Sign Language. Preferred value: eso.
pattern ExtEso :: Extlang
pattern ExtEso = Extlang (Subtag 14686088851273809939)

 -- | @eth@. Description: Ethiopian Sign Language. Preferred value: eth.
pattern ExtEth :: Extlang
pattern ExtEth = Extlang (Subtag 14687153178529497107)

 -- | @fcs@. Description: Quebec Sign Language. Preferred value: fcs.
pattern ExtFcs :: Extlang
pattern ExtFcs = Extlang (Subtag 14812224825212272659)

 -- | @fse@. Description: Finnish Sign Language. Preferred value: fse.
pattern ExtFse :: Extlang
pattern ExtFse = Extlang (Subtag 14830116078419443731)

 -- | @fsl@. Description: French Sign Language. Preferred value: fsl.
pattern ExtFsl :: Extlang
pattern ExtFsl = Extlang (Subtag 14830177651070599187)

 -- | @fss@. Description: Finland-Swedish Sign Language; finlandssvenskt teckenspråk; suomenruotsalainen viittomakieli. Preferred value: fss.
pattern ExtFss :: Extlang
pattern ExtFss = Extlang (Subtag 14830239223721754643)

 -- | @gan@. Description: Gan Chinese. Preferred value: gan.
pattern ExtGan :: Extlang
pattern ExtGan = Extlang (Subtag 14954044233009332243)

 -- | @gds@. Description: Ghandruk Sign Language. Preferred value: gds.
pattern ExtGds :: Extlang
pattern ExtGds = Extlang (Subtag 14957465913194971155)

 -- | @gom@. Description: Goan Konkani. Preferred value: gom.
pattern ExtGom :: Extlang
pattern ExtGom = Extlang (Subtag 14969798035612106771)

 -- | @gse@. Description: Ghanaian Sign Language. Preferred value: gse.
pattern ExtGse :: Extlang
pattern ExtGse = Extlang (Subtag 14974231266495299603)

 -- | @gsg@. Description: German Sign Language. Preferred value: gsg.
pattern ExtGsg :: Extlang
pattern ExtGsg = Extlang (Subtag 14974248858681344019)

 -- | @gsm@. Description: Guatemalan Sign Language. Preferred value: gsm.
pattern ExtGsm :: Extlang
pattern ExtGsm = Extlang (Subtag 14974301635239477267)

 -- | @gss@. Description: Greek Sign Language. Preferred value: gss.
pattern ExtGss :: Extlang
pattern ExtGss = Extlang (Subtag 14974354411797610515)

 -- | @gus@. Description: Guinean Sign Language. Preferred value: gus.
pattern ExtGus :: Extlang
pattern ExtGus = Extlang (Subtag 14976606211611295763)

 -- | @hab@. Description: Hanoi Sign Language. Preferred value: hab.
pattern ExtHab :: Extlang
pattern ExtHab = Extlang (Subtag 15098053867968921619)

 -- | @haf@. Description: Haiphong Sign Language. Preferred value: haf.
pattern ExtHaf :: Extlang
pattern ExtHaf = Extlang (Subtag 15098089052341010451)

 -- | @hak@. Description: Hakka Chinese. Preferred value: hak.
pattern ExtHak :: Extlang
pattern ExtHak = Extlang (Subtag 15098133032806121491)

 -- | @hds@. Description: Honduras Sign Language. Preferred value: hds.
pattern ExtHds :: Extlang
pattern ExtHds = Extlang (Subtag 15101581101270827027)

 -- | @hji@. Description: Haji. Preferred value: hji.
pattern ExtHji :: Extlang
pattern ExtHji = Extlang (Subtag 15108248539781660691)

 -- | @hks@. Description: Hong Kong Sign Language; Heung Kong Sau Yue. Preferred value: hks.
pattern ExtHks :: Extlang
pattern ExtHks = Extlang (Subtag 15109462400618725395)

 -- | @hos@. Description: Ho Chi Minh City Sign Language. Preferred value: hos.
pattern ExtHos :: Extlang
pattern ExtHos = Extlang (Subtag 15113966000246095891)

 -- | @hps@. Description: Hawai\'i Sign Language (HSL); Hawai\'i Pidgin Sign Language. Preferred value: hps.
pattern ExtHps :: Extlang
pattern ExtHps = Extlang (Subtag 15115091900152938515)

 -- | @hsh@. Description: Hungarian Sign Language. Preferred value: hsh.
pattern ExtHsh :: Extlang
pattern ExtHsh = Extlang (Subtag 15118372842850222099)

 -- | @hsl@. Description: Hausa Sign Language. Preferred value: hsl.
pattern ExtHsl :: Extlang
pattern ExtHsl = Extlang (Subtag 15118408027222310931)

 -- | @hsn@. Description: Xiang Chinese. Preferred value: hsn.
pattern ExtHsn :: Extlang
pattern ExtHsn = Extlang (Subtag 15118425619408355347)

 -- | @icl@. Description: Icelandic Sign Language. Preferred value: icl.
pattern ExtIcl :: Extlang
pattern ExtIcl = Extlang (Subtag 15244508816788684819)

 -- | @iks@. Description: Inuit Sign Language. Preferred value: iks.
pattern ExtIks :: Extlang
pattern ExtIks = Extlang (Subtag 15253577588694581267)

 -- | @ils@. Description: International Sign. Preferred value: ils.
pattern ExtIls :: Extlang
pattern ExtIls = Extlang (Subtag 15254703488601423891)

 -- | @inl@. Description: Indonesian Sign Language. Preferred value: inl.
pattern ExtInl :: Extlang
pattern ExtInl = Extlang (Subtag 15256893715763953683)

 -- | @ins@. Description: Indian Sign Language. Preferred value: ins.
pattern ExtIns :: Extlang
pattern ExtIns = Extlang (Subtag 15256955288415109139)

 -- | @ise@. Description: Italian Sign Language. Preferred value: ise.
pattern ExtIse :: Extlang
pattern ExtIse = Extlang (Subtag 15262461642647011347)

 -- | @isg@. Description: Irish Sign Language. Preferred value: isg.
pattern ExtIsg :: Extlang
pattern ExtIsg = Extlang (Subtag 15262479234833055763)

 -- | @isr@. Description: Israeli Sign Language. Preferred value: isr.
pattern ExtIsr :: Extlang
pattern ExtIsr = Extlang (Subtag 15262575991856300051)

 -- | @jak@. Description: Jakun. Preferred value: jak.
pattern ExtJak :: Extlang
pattern ExtJak = Extlang (Subtag 15386363408957833235)

 -- | @jax@. Description: Jambi Malay. Preferred value: jax.
pattern ExtJax :: Extlang
pattern ExtJax = Extlang (Subtag 15386477758167121939)

 -- | @jcs@. Description: Jamaican Country Sign Language. Preferred value: jcs.
pattern ExtJcs :: Extlang
pattern ExtJcs = Extlang (Subtag 15388685577515696147)

 -- | @jhs@. Description: Jhankot Sign Language. Preferred value: jhs.
pattern ExtJhs :: Extlang
pattern ExtJhs = Extlang (Subtag 15394315077049909267)

 -- | @jks@. Description: Amami Koniya Sign Language. Preferred value: jks.
pattern ExtJks :: Extlang
pattern ExtJks = Extlang (Subtag 15397692776770437139)

 -- | @jls@. Description: Jamaican Sign Language. Preferred value: jls.
pattern ExtJls :: Extlang
pattern ExtJls = Extlang (Subtag 15398818676677279763)

 -- | @jos@. Description: Jordanian Sign Language. Preferred value: jos.
pattern ExtJos :: Extlang
pattern ExtJos = Extlang (Subtag 15402196376397807635)

 -- | @jsl@. Description: Japanese Sign Language. Preferred value: jsl.
pattern ExtJsl :: Extlang
pattern ExtJsl = Extlang (Subtag 15406638403374022675)

 -- | @jus@. Description: Jumla Sign Language. Preferred value: jus.
pattern ExtJus :: Extlang
pattern ExtJus = Extlang (Subtag 15408951775838863379)

 -- | @kgi@. Description: Selangor Sign Language. Preferred value: kgi.
pattern ExtKgi :: Extlang
pattern ExtKgi = Extlang (Subtag 15537216404288700435)

 -- | @knn@. Description: Konkani (individual language). Preferred value: knn.
pattern ExtKnn :: Extlang
pattern ExtKnn = Extlang (Subtag 15545141684101709843)

 -- | @kvb@. Description: Kubu. Preferred value: kvb.
pattern ExtKvb :: Extlang
pattern ExtKvb = Extlang (Subtag 15554043330240184339)

 -- | @kvk@. Description: Korean Sign Language. Preferred value: kvk.
pattern ExtKvk :: Extlang
pattern ExtKvk = Extlang (Subtag 15554122495077384211)

 -- | @kvr@. Description: Kerinci. Preferred value: kvr.
pattern ExtKvr :: Extlang
pattern ExtKvr = Extlang (Subtag 15554184067728539667)

 -- | @kxd@. Description: Brunei. Preferred value: kxd.
pattern ExtKxd :: Extlang
pattern ExtKxd = Extlang (Subtag 15556312722239914003)

 -- | @lbs@. Description: Libyan Sign Language. Preferred value: lbs.
pattern ExtLbs :: Extlang
pattern ExtLbs = Extlang (Subtag 15675790053760565267)

 -- | @lce@. Description: Loncong; Sekak. Preferred value: lce.
pattern ExtLce :: Extlang
pattern ExtLce = Extlang (Subtag 15676792808365096979)

 -- | @lcf@. Description: Lubu. Preferred value: lcf.
pattern ExtLcf :: Extlang
pattern ExtLcf = Extlang (Subtag 15676801604458119187)

 -- | @liw@. Description: Col. Preferred value: liw.
pattern ExtLiw :: Extlang
pattern ExtLiw = Extlang (Subtag 15683706537480552467)

 -- | @lls@. Description: Lithuanian Sign Language. Preferred value: lls.
pattern ExtLls :: Extlang
pattern ExtLls = Extlang (Subtag 15687049052828991507)

 -- | @lsb@. Description: Burundian Sign Language; Langue des Signes Burundaise. Preferred value: lsb.
pattern ExtLsb :: Extlang
pattern ExtLsb = Extlang (Subtag 15694780818595512339)

 -- | @lsg@. Description: Lyons Sign Language. Deprecated. Preferred value: lsg.
pattern ExtLsg :: Extlang
pattern ExtLsg = Extlang (Subtag 15694824799060623379)

 -- | @lsl@. Description: Latvian Sign Language. Preferred value: lsl.
pattern ExtLsl :: Extlang
pattern ExtLsl = Extlang (Subtag 15694868779525734419)

 -- | @lsn@. Description: Tibetan Sign Language. Preferred value: lsn.
pattern ExtLsn :: Extlang
pattern ExtLsn = Extlang (Subtag 15694886371711778835)

 -- | @lso@. Description: Laos Sign Language. Preferred value: lso.
pattern ExtLso :: Extlang
pattern ExtLso = Extlang (Subtag 15694895167804801043)

 -- | @lsp@. Description: Panamanian Sign Language; Lengua de Señas Panameñas. Preferred value: lsp.
pattern ExtLsp :: Extlang
pattern ExtLsp = Extlang (Subtag 15694903963897823251)

 -- | @lst@. Description: Trinidad and Tobago Sign Language. Preferred value: lst.
pattern ExtLst :: Extlang
pattern ExtLst = Extlang (Subtag 15694939148269912083)

 -- | @lsv@. Description: Sivia Sign Language. Preferred value: lsv.
pattern ExtLsv :: Extlang
pattern ExtLsv = Extlang (Subtag 15694956740455956499)

 -- | @lsy@. Description: Mauritian Sign Language. Preferred value: lsy.
pattern ExtLsy :: Extlang
pattern ExtLsy = Extlang (Subtag 15694983128735023123)

 -- | @ltg@. Description: Latgalian. Preferred value: ltg.
pattern ExtLtg :: Extlang
pattern ExtLtg = Extlang (Subtag 15695950698967466003)

 -- | @lvs@. Description: Standard Latvian. Preferred value: lvs.
pattern ExtLvs :: Extlang
pattern ExtLvs = Extlang (Subtag 15698308051897417747)

 -- | @lws@. Description: Malawian Sign Language. Preferred value: lws.
pattern ExtLws :: Extlang
pattern ExtLws = Extlang (Subtag 15699433951804260371)

 -- | @lzh@. Description: Literary Chinese. Preferred value: lzh.
pattern ExtLzh :: Extlang
pattern ExtLzh = Extlang (Subtag 15702714894501543955)

 -- | @max@. Description: North Moluccan Malay. Preferred value: max.
pattern ExtMax :: Extlang
pattern ExtMax = Extlang (Subtag 15818823322394689555)

 -- | @mdl@. Description: Maltese Sign Language. Preferred value: mdl.
pattern ExtMdl :: Extlang
pattern ExtMdl = Extlang (Subtag 15822095468998950931)

 -- | @meo@. Description: Kedah Malay. Preferred value: meo.
pattern ExtMeo :: Extlang
pattern ExtMeo = Extlang (Subtag 15823247757184860179)

 -- | @mfa@. Description: Pattani Malay. Preferred value: mfa.
pattern ExtMfa :: Extlang
pattern ExtMfa = Extlang (Subtag 15824250511789391891)

 -- | @mfb@. Description: Bangka. Preferred value: mfb.
pattern ExtMfb :: Extlang
pattern ExtMfb = Extlang (Subtag 15824259307882414099)

 -- | @mfs@. Description: Mexican Sign Language. Preferred value: mfs.
pattern ExtMfs :: Extlang
pattern ExtMfs = Extlang (Subtag 15824408841463791635)

 -- | @min@. Description: Minangkabau. Preferred value: min.
pattern ExtMin :: Extlang
pattern ExtMin = Extlang (Subtag 15827742560719208467)

 -- | @mnp@. Description: Min Bei Chinese. Preferred value: mnp.
pattern ExtMnp :: Extlang
pattern ExtMnp = Extlang (Subtag 15833389652439466003)

 -- | @mqg@. Description: Kota Bangun Kutai Malay. Preferred value: mqg.
pattern ExtMqg :: Extlang
pattern ExtMqg = Extlang (Subtag 15836688187322794003)

 -- | @mre@. Description: Martha\'s Vineyard Sign Language. Preferred value: mre.
pattern ExtMre :: Extlang
pattern ExtMre = Extlang (Subtag 15837796495043592211)

 -- | @msd@. Description: Yucatec Maya Sign Language. Preferred value: msd.
pattern ExtMsd :: Extlang
pattern ExtMsd = Extlang (Subtag 15838913598857412627)

 -- | @msi@. Description: Sabah Malay. Preferred value: msi.
pattern ExtMsi :: Extlang
pattern ExtMsi = Extlang (Subtag 15838957579322523667)

 -- | @msr@. Description: Mongolian Sign Language. Preferred value: msr.
pattern ExtMsr :: Extlang
pattern ExtMsr = Extlang (Subtag 15839036744159723539)

 -- | @mui@. Description: Musi. Preferred value: mui.
pattern ExtMui :: Extlang
pattern ExtMui = Extlang (Subtag 15841209379136208915)

 -- | @mzc@. Description: Madagascar Sign Language. Preferred value: mzc.
pattern ExtMzc :: Extlang
pattern ExtMzc = Extlang (Subtag 15846786102112288787)

 -- | @mzg@. Description: Monastic Sign Language. Preferred value: mzg.
pattern ExtMzg :: Extlang
pattern ExtMzg = Extlang (Subtag 15846821286484377619)

 -- | @mzy@. Description: Mozambican Sign Language. Preferred value: mzy.
pattern ExtMzy :: Extlang
pattern ExtMzy = Extlang (Subtag 15846979616158777363)

 -- | @nan@. Description: Min Nan Chinese. Preferred value: nan.
pattern ExtNan :: Extlang
pattern ExtNan = Extlang (Subtag 15962850549540323347)

 -- | @nbs@. Description: Namibian Sign Language. Preferred value: nbs.
pattern ExtNbs :: Extlang
pattern ExtNbs = Extlang (Subtag 15964020429912277011)

 -- | @ncs@. Description: Nicaraguan Sign Language. Preferred value: ncs.
pattern ExtNcs :: Extlang
pattern ExtNcs = Extlang (Subtag 15965146329819119635)

 -- | @nsi@. Description: Nigerian Sign Language. Preferred value: nsi.
pattern ExtNsi :: Extlang
pattern ExtNsi = Extlang (Subtag 15983072767398379539)

 -- | @nsl@. Description: Norwegian Sign Language. Preferred value: nsl.
pattern ExtNsl :: Extlang
pattern ExtNsl = Extlang (Subtag 15983099155677446163)

 -- | @nsp@. Description: Nepalese Sign Language. Preferred value: nsp.
pattern ExtNsp :: Extlang
pattern ExtNsp = Extlang (Subtag 15983134340049534995)

 -- | @nsr@. Description: Maritime Sign Language. Preferred value: nsr.
pattern ExtNsr :: Extlang
pattern ExtNsr = Extlang (Subtag 15983151932235579411)

 -- | @nzs@. Description: New Zealand Sign Language. Preferred value: nzs.
pattern ExtNzs :: Extlang
pattern ExtNzs = Extlang (Subtag 15991042027676499987)

 -- | @okl@. Description: Old Kentish Sign Language. Preferred value: okl.
pattern ExtOkl :: Extlang
pattern ExtOkl = Extlang (Subtag 16118207144498561043)

 -- | @orn@. Description: Orang Kanaq. Preferred value: orn.
pattern ExtOrn :: Extlang
pattern ExtOrn = Extlang (Subtag 16126106036032503827)

 -- | @ors@. Description: Orang Seletar. Preferred value: ors.
pattern ExtOrs :: Extlang
pattern ExtOrs = Extlang (Subtag 16126150016497614867)

 -- | @pel@. Description: Pekal. Preferred value: pel.
pattern ExtPel :: Extlang
pattern ExtPel = Extlang (Subtag 16255566933133361171)

 -- | @pga@. Description: Sudanese Creole Arabic. Preferred value: pga.
pattern ExtPga :: Extlang
pattern ExtPga = Extlang (Subtag 16257721975923802131)

 -- | @pgz@. Description: Papua New Guinean Sign Language. Preferred value: pgz.
pattern ExtPgz :: Extlang
pattern ExtPgz = Extlang (Subtag 16257941878249357331)

 -- | @pks@. Description: Pakistan Sign Language. Preferred value: pks.
pattern ExtPks :: Extlang
pattern ExtPks = Extlang (Subtag 16262383905225572371)

 -- | @prl@. Description: Peruvian Sign Language. Preferred value: prl.
pattern ExtPrl :: Extlang
pattern ExtPrl = Extlang (Subtag 16270203631922315283)

 -- | @prz@. Description: Providencia Sign Language. Preferred value: prz.
pattern ExtPrz :: Extlang
pattern ExtPrz = Extlang (Subtag 16270326777224626195)

 -- | @psc@. Description: Persian Sign Language. Preferred value: psc.
pattern ExtPsc :: Extlang
pattern ExtPsc = Extlang (Subtag 16271250366991958035)

 -- | @psd@. Description: Plains Indian Sign Language. Preferred value: psd.
pattern ExtPsd :: Extlang
pattern ExtPsd = Extlang (Subtag 16271259163084980243)

 -- | @pse@. Description: Central Malay. Preferred value: pse.
pattern ExtPse :: Extlang
pattern ExtPse = Extlang (Subtag 16271267959178002451)

 -- | @psg@. Description: Penang Sign Language. Preferred value: psg.
pattern ExtPsg :: Extlang
pattern ExtPsg = Extlang (Subtag 16271285551364046867)

 -- | @psl@. Description: Puerto Rican Sign Language. Preferred value: psl.
pattern ExtPsl :: Extlang
pattern ExtPsl = Extlang (Subtag 16271329531829157907)

 -- | @pso@. Description: Polish Sign Language. Preferred value: pso.
pattern ExtPso :: Extlang
pattern ExtPso = Extlang (Subtag 16271355920108224531)

 -- | @psp@. Description: Philippine Sign Language. Preferred value: psp.
pattern ExtPsp :: Extlang
pattern ExtPsp = Extlang (Subtag 16271364716201246739)

 -- | @psr@. Description: Portuguese Sign Language. Preferred value: psr.
pattern ExtPsr :: Extlang
pattern ExtPsr = Extlang (Subtag 16271382308387291155)

 -- | @pys@. Description: Paraguayan Sign Language; Lengua de Señas del Paraguay. Preferred value: pys.
pattern ExtPys :: Extlang
pattern ExtPys = Extlang (Subtag 16278146503921369107)

 -- | @rms@. Description: Romanian Sign Language. Preferred value: rms.
pattern ExtRms :: Extlang
pattern ExtRms = Extlang (Subtag 16552866081190969363)

 -- | @rsi@. Description: Rennellese Sign Language. Deprecated. Preferred value: rsi.
pattern ExtRsi :: Extlang
pattern ExtRsi = Extlang (Subtag 16559533519701803027)

 -- | @rsl@. Description: Russian Sign Language. Preferred value: rsl.
pattern ExtRsl :: Extlang
pattern ExtRsl = Extlang (Subtag 16559559907980869651)

 -- | @rsm@. Description: Miriwoong Sign Language. Preferred value: rsm.
pattern ExtRsm :: Extlang
pattern ExtRsm = Extlang (Subtag 16559568704073891859)

 -- | @sdl@. Description: Saudi Arabian Sign Language. Preferred value: sdl.
pattern ExtSdl :: Extlang
pattern ExtSdl = Extlang (Subtag 16686786597454086163)

 -- | @sfb@. Description: Langue des signes de Belgique Francophone; French Belgian Sign Language. Preferred value: sfb.
pattern ExtSfb :: Extlang
pattern ExtSfb = Extlang (Subtag 16688950436337549331)

 -- | @sfs@. Description: South African Sign Language. Preferred value: sfs.
pattern ExtSfs :: Extlang
pattern ExtSfs = Extlang (Subtag 16689099969918926867)

 -- | @sgg@. Description: Swiss-German Sign Language. Preferred value: sgg.
pattern ExtSgg :: Extlang
pattern ExtSgg = Extlang (Subtag 16690120316709502995)

 -- | @sgx@. Description: Sierra Leone Sign Language. Preferred value: sgx.
pattern ExtSgx :: Extlang
pattern ExtSgx = Extlang (Subtag 16690269850290880531)

 -- | @shu@. Description: Chadian Arabic. Preferred value: shu.
pattern ExtShu :: Extlang
pattern ExtShu = Extlang (Subtag 16691369361918656531)

 -- | @slf@. Description: Swiss-Italian Sign Language. Preferred value: slf.
pattern ExtSlf :: Extlang
pattern ExtSlf = Extlang (Subtag 16695741020150693907)

 -- | @sls@. Description: Singapore Sign Language. Preferred value: sls.
pattern ExtSls :: Extlang
pattern ExtSls = Extlang (Subtag 16695855369359982611)

 -- | @sqk@. Description: Albanian Sign Language. Preferred value: sqk.
pattern ExtSqk :: Extlang
pattern ExtSqk = Extlang (Subtag 16701414500150018067)

 -- | @sqs@. Description: Sri Lankan Sign Language. Preferred value: sqs.
pattern ExtSqs :: Extlang
pattern ExtSqs = Extlang (Subtag 16701484868894195731)

 -- | @sqx@. Description: Kufr Qassem Sign Language (KQSL). Preferred value: sqx.
pattern ExtSqx :: Extlang
pattern ExtSqx = Extlang (Subtag 16701528849359306771)

 -- | @ssh@. Description: Shihhi Arabic. Preferred value: ssh.
pattern ExtSsh :: Extlang
pattern ExtSsh = Extlang (Subtag 16703639911684636691)

 -- | @ssp@. Description: Spanish Sign Language. Preferred value: ssp.
pattern ExtSsp :: Extlang
pattern ExtSsp = Extlang (Subtag 16703710280428814355)

 -- | @ssr@. Description: Swiss-French Sign Language. Preferred value: ssr.
pattern ExtSsr :: Extlang
pattern ExtSsr = Extlang (Subtag 16703727872614858771)

 -- | @svk@. Description: Slovakian Sign Language. Preferred value: svk.
pattern ExtSvk :: Extlang
pattern ExtSvk = Extlang (Subtag 16707043999684231187)

 -- | @swc@. Description: Congo Swahili. Preferred value: swc.
pattern ExtSwc :: Extlang
pattern ExtSwc = Extlang (Subtag 16708099530846896147)

 -- | @swh@. Description: Swahili (individual language); Kiswahili. Preferred value: swh.
pattern ExtSwh :: Extlang
pattern ExtSwh = Extlang (Subtag 16708143511312007187)

 -- | @swl@. Description: Swedish Sign Language. Preferred value: swl.
pattern ExtSwl :: Extlang
pattern ExtSwl = Extlang (Subtag 16708178695684096019)

 -- | @syy@. Description: Al-Sayyid Bedouin Sign Language. Preferred value: syy.
pattern ExtSyy :: Extlang
pattern ExtSyy = Extlang (Subtag 16710544844707069971)

 -- | @szs@. Description: Solomon Islands Sign Language. Preferred value: szs.
pattern ExtSzs :: Extlang
pattern ExtSzs = Extlang (Subtag 16711617968055779347)

 -- | @tmw@. Description: Temuan. Preferred value: tmw.
pattern ExtTmw :: Extlang
pattern ExtTmw = Extlang (Subtag 16841131641714769939)

 -- | @tse@. Description: Tunisian Sign Language. Preferred value: tse.
pattern ExtTse :: Extlang
pattern ExtTse = Extlang (Subtag 16847728711481425939)

 -- | @tsm@. Description: Turkish Sign Language; Türk İşaret Dili. Preferred value: tsm.
pattern ExtTsm :: Extlang
pattern ExtTsm = Extlang (Subtag 16847799080225603603)

 -- | @tsq@. Description: Thai Sign Language. Preferred value: tsq.
pattern ExtTsq :: Extlang
pattern ExtTsq = Extlang (Subtag 16847834264597692435)

 -- | @tss@. Description: Taiwan Sign Language. Preferred value: tss.
pattern ExtTss :: Extlang
pattern ExtTss = Extlang (Subtag 16847851856783736851)

 -- | @tsy@. Description: Tebul Sign Language. Preferred value: tsy.
pattern ExtTsy :: Extlang
pattern ExtTsy = Extlang (Subtag 16847904633341870099)

 -- | @tza@. Description: Tanzanian Sign Language. Preferred value: tza.
pattern ExtTza :: Extlang
pattern ExtTza = Extlang (Subtag 16855574826457235475)

 -- | @ugn@. Description: Ugandan Sign Language. Preferred value: ugn.
pattern ExtUgn :: Extlang
pattern ExtUgn = Extlang (Subtag 16978412265512370195)

 -- | @ugy@. Description: Uruguayan Sign Language. Preferred value: ugy.
pattern ExtUgy :: Extlang
pattern ExtUgy = Extlang (Subtag 16978509022535614483)

 -- | @ukl@. Description: Ukrainian Sign Language. Preferred value: ukl.
pattern ExtUkl :: Extlang
pattern ExtUkl = Extlang (Subtag 16982898272953696275)

 -- | @uks@. Description: Urubú-Kaapor Sign Language; Kaapor Sign Language. Preferred value: uks.
pattern ExtUks :: Extlang
pattern ExtUks = Extlang (Subtag 16982959845604851731)

 -- | @urk@. Description: Urak Lawoi\'. Preferred value: urk.
pattern ExtUrk :: Extlang
pattern ExtUrk = Extlang (Subtag 16990770776208572435)

 -- | @uzn@. Description: Northern Uzbek. Preferred value: uzn.
pattern ExtUzn :: Extlang
pattern ExtUzn = Extlang (Subtag 16999804363742380051)

 -- | @uzs@. Description: Southern Uzbek. Preferred value: uzs.
pattern ExtUzs :: Extlang
pattern ExtUzs = Extlang (Subtag 16999848344207491091)

 -- | @vgt@. Description: Vlaamse Gebarentaal; Flemish Sign Language. Preferred value: vgt.
pattern ExtVgt :: Extlang
pattern ExtVgt = Extlang (Subtag 17122580230146359315)

 -- | @vkk@. Description: Kaur. Preferred value: vkk.
pattern ExtVkk :: Extlang
pattern ExtVkk = Extlang (Subtag 17127004664936529939)

 -- | @vkt@. Description: Tenggarong Kutai Malay. Preferred value: vkt.
pattern ExtVkt :: Extlang
pattern ExtVkt = Extlang (Subtag 17127083829773729811)

 -- | @vsi@. Description: Moldova Sign Language. Preferred value: vsi.
pattern ExtVsi :: Extlang
pattern ExtVsi = Extlang (Subtag 17135994272005226515)

 -- | @vsl@. Description: Venezuelan Sign Language. Preferred value: vsl.
pattern ExtVsl :: Extlang
pattern ExtVsl = Extlang (Subtag 17136020660284293139)

 -- | @vsv@. Description: Valencian Sign Language; Llengua de signes valenciana. Preferred value: vsv.
pattern ExtVsv :: Extlang
pattern ExtVsv = Extlang (Subtag 17136108621214515219)

 -- | @wbs@. Description: West Bengal Sign Language. Preferred value: wbs.
pattern ExtWbs :: Extlang
pattern ExtWbs = Extlang (Subtag 17261057122594979859)

 -- | @wuu@. Description: Wu Chinese. Preferred value: wuu.
pattern ExtWuu :: Extlang
pattern ExtWuu = Extlang (Subtag 17282466813011034131)

 -- | @xki@. Description: Kenyan Sign Language. Preferred value: xki.
pattern ExtXki :: Extlang
pattern ExtXki = Extlang (Subtag 17415217448902197267)

 -- | @xml@. Description: Malaysian Sign Language. Preferred value: xml.
pattern ExtXml :: Extlang
pattern ExtXml = Extlang (Subtag 17417495636994949139)

 -- | @xmm@. Description: Manado Malay. Preferred value: xmm.
pattern ExtXmm :: Extlang
pattern ExtXmm = Extlang (Subtag 17417504433087971347)

 -- | @xms@. Description: Moroccan Sign Language. Preferred value: xms.
pattern ExtXms :: Extlang
pattern ExtXms = Extlang (Subtag 17417557209646104595)

 -- | @yds@. Description: Yiddish Sign Language. Deprecated. Preferred value: yds.
pattern ExtYds :: Extlang
pattern ExtYds = Extlang (Subtag 17551539298560376851)

 -- | @ygs@. Description: Yolŋu Sign Language. Preferred value: ygs.
pattern ExtYgs :: Extlang
pattern ExtYgs = Extlang (Subtag 17554916998280904723)

 -- | @yhs@. Description: Yan-nhaŋu Sign Language. Preferred value: yhs.
pattern ExtYhs :: Extlang
pattern ExtYhs = Extlang (Subtag 17556042898187747347)

 -- | @ysl@. Description: Yugoslavian Sign Language. Preferred value: ysl.
pattern ExtYsl :: Extlang
pattern ExtYsl = Extlang (Subtag 17568366224511860755)

 -- | @ysm@. Description: Myanmar Sign Language. Preferred value: ysm.
pattern ExtYsm :: Extlang
pattern ExtYsm = Extlang (Subtag 17568375020604882963)

 -- | @yue@. Description: Yue Chinese; Cantonese. Preferred value: yue.
pattern ExtYue :: Extlang
pattern ExtYue = Extlang (Subtag 17570556451674390547)

 -- | @zib@. Description: Zimbabwe Sign Language. Preferred value: zib.
pattern ExtZib :: Extlang
pattern ExtZib = Extlang (Subtag 17701134452589068307)

 -- | @zlm@. Description: Malay (individual language). Preferred value: zlm.
pattern ExtZlm :: Extlang
pattern ExtZlm = Extlang (Subtag 17704608909332840467)

 -- | @zmi@. Description: Negeri Sembilan Malay. Preferred value: zmi.
pattern ExtZmi :: Extlang
pattern ExtZmi = Extlang (Subtag 17705699624867594259)

 -- | @zsl@. Description: Zambian Sign Language. Preferred value: zsl.
pattern ExtZsl :: Extlang
pattern ExtZsl = Extlang (Subtag 17712481412587716627)

 -- | @zsm@. Description: Standard Malay. Preferred value: zsm.
pattern ExtZsm :: Extlang
pattern ExtZsm = Extlang (Subtag 17712490208680738835)
