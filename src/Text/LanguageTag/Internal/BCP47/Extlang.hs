-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}

module Text.LanguageTag.Internal.BCP47.Extlang where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..), hashUsing)
import Text.LanguageTag.Internal.BCP47.Syntax (Subtag(..), unwrapSubtag)
import qualified Data.HashMap.Strict as HM

-- | The BCP47 extended language tags as of 2021-02-23. These are prefixed with "Ext" because they may overlap with primary language subtags. Note that if extended language subtags have a preferred value, then it refers to a primary subtag.
data Extlang
  = ExtAao -- ^ @aao@. Description: Algerian Saharan Arabic. Preferred value: aao.
  | ExtAbh -- ^ @abh@. Description: Tajiki Arabic. Preferred value: abh.
  | ExtAbv -- ^ @abv@. Description: Baharna Arabic. Preferred value: abv.
  | ExtAcm -- ^ @acm@. Description: Mesopotamian Arabic. Preferred value: acm.
  | ExtAcq -- ^ @acq@. Description: Ta\'izzi-Adeni Arabic. Preferred value: acq.
  | ExtAcw -- ^ @acw@. Description: Hijazi Arabic. Preferred value: acw.
  | ExtAcx -- ^ @acx@. Description: Omani Arabic. Preferred value: acx.
  | ExtAcy -- ^ @acy@. Description: Cypriot Arabic. Preferred value: acy.
  | ExtAdf -- ^ @adf@. Description: Dhofari Arabic. Preferred value: adf.
  | ExtAds -- ^ @ads@. Description: Adamorobe Sign Language. Preferred value: ads.
  | ExtAeb -- ^ @aeb@. Description: Tunisian Arabic. Preferred value: aeb.
  | ExtAec -- ^ @aec@. Description: Saidi Arabic. Preferred value: aec.
  | ExtAed -- ^ @aed@. Description: Argentine Sign Language. Preferred value: aed.
  | ExtAen -- ^ @aen@. Description: Armenian Sign Language. Preferred value: aen.
  | ExtAfb -- ^ @afb@. Description: Gulf Arabic. Preferred value: afb.
  | ExtAfg -- ^ @afg@. Description: Afghan Sign Language. Preferred value: afg.
  | ExtAjp -- ^ @ajp@. Description: South Levantine Arabic. Preferred value: ajp.
  | ExtApc -- ^ @apc@. Description: North Levantine Arabic. Preferred value: apc.
  | ExtApd -- ^ @apd@. Description: Sudanese Arabic. Preferred value: apd.
  | ExtArb -- ^ @arb@. Description: Standard Arabic. Preferred value: arb.
  | ExtArq -- ^ @arq@. Description: Algerian Arabic. Preferred value: arq.
  | ExtArs -- ^ @ars@. Description: Najdi Arabic. Preferred value: ars.
  | ExtAry -- ^ @ary@. Description: Moroccan Arabic. Preferred value: ary.
  | ExtArz -- ^ @arz@. Description: Egyptian Arabic. Preferred value: arz.
  | ExtAse -- ^ @ase@. Description: American Sign Language. Preferred value: ase.
  | ExtAsf -- ^ @asf@. Description: Auslan; Australian Sign Language. Preferred value: asf.
  | ExtAsp -- ^ @asp@. Description: Algerian Sign Language. Preferred value: asp.
  | ExtAsq -- ^ @asq@. Description: Austrian Sign Language. Preferred value: asq.
  | ExtAsw -- ^ @asw@. Description: Australian Aborigines Sign Language. Preferred value: asw.
  | ExtAuz -- ^ @auz@. Description: Uzbeki Arabic. Preferred value: auz.
  | ExtAvl -- ^ @avl@. Description: Eastern Egyptian Bedawi Arabic. Preferred value: avl.
  | ExtAyh -- ^ @ayh@. Description: Hadrami Arabic. Preferred value: ayh.
  | ExtAyl -- ^ @ayl@. Description: Libyan Arabic. Preferred value: ayl.
  | ExtAyn -- ^ @ayn@. Description: Sanaani Arabic. Preferred value: ayn.
  | ExtAyp -- ^ @ayp@. Description: North Mesopotamian Arabic. Preferred value: ayp.
  | ExtBbz -- ^ @bbz@. Description: Babalia Creole Arabic. Deprecated. Preferred value: bbz.
  | ExtBfi -- ^ @bfi@. Description: British Sign Language. Preferred value: bfi.
  | ExtBfk -- ^ @bfk@. Description: Ban Khor Sign Language. Preferred value: bfk.
  | ExtBjn -- ^ @bjn@. Description: Banjar. Preferred value: bjn.
  | ExtBog -- ^ @bog@. Description: Bamako Sign Language. Preferred value: bog.
  | ExtBqn -- ^ @bqn@. Description: Bulgarian Sign Language. Preferred value: bqn.
  | ExtBqy -- ^ @bqy@. Description: Bengkala Sign Language. Preferred value: bqy.
  | ExtBtj -- ^ @btj@. Description: Bacanese Malay. Preferred value: btj.
  | ExtBve -- ^ @bve@. Description: Berau Malay. Preferred value: bve.
  | ExtBvl -- ^ @bvl@. Description: Bolivian Sign Language. Preferred value: bvl.
  | ExtBvu -- ^ @bvu@. Description: Bukit Malay. Preferred value: bvu.
  | ExtBzs -- ^ @bzs@. Description: Brazilian Sign Language. Preferred value: bzs.
  | ExtCdo -- ^ @cdo@. Description: Min Dong Chinese. Preferred value: cdo.
  | ExtCds -- ^ @cds@. Description: Chadian Sign Language. Preferred value: cds.
  | ExtCjy -- ^ @cjy@. Description: Jinyu Chinese. Preferred value: cjy.
  | ExtCmn -- ^ @cmn@. Description: Mandarin Chinese. Preferred value: cmn.
  | ExtCnp -- ^ @cnp@. Description: Northern Ping Chinese; Northern Pinghua. Preferred value: cnp.
  | ExtCoa -- ^ @coa@. Description: Cocos Islands Malay. Preferred value: coa.
  | ExtCpx -- ^ @cpx@. Description: Pu-Xian Chinese. Preferred value: cpx.
  | ExtCsc -- ^ @csc@. Description: Catalan Sign Language; Lengua de señas catalana; Llengua de Signes Catalana. Preferred value: csc.
  | ExtCsd -- ^ @csd@. Description: Chiangmai Sign Language. Preferred value: csd.
  | ExtCse -- ^ @cse@. Description: Czech Sign Language. Preferred value: cse.
  | ExtCsf -- ^ @csf@. Description: Cuba Sign Language. Preferred value: csf.
  | ExtCsg -- ^ @csg@. Description: Chilean Sign Language. Preferred value: csg.
  | ExtCsl -- ^ @csl@. Description: Chinese Sign Language. Preferred value: csl.
  | ExtCsn -- ^ @csn@. Description: Colombian Sign Language. Preferred value: csn.
  | ExtCsp -- ^ @csp@. Description: Southern Ping Chinese; Southern Pinghua. Preferred value: csp.
  | ExtCsq -- ^ @csq@. Description: Croatia Sign Language. Preferred value: csq.
  | ExtCsr -- ^ @csr@. Description: Costa Rican Sign Language. Preferred value: csr.
  | ExtCsx -- ^ @csx@. Description: Cambodian Sign Language. Preferred value: csx.
  | ExtCzh -- ^ @czh@. Description: Huizhou Chinese. Preferred value: czh.
  | ExtCzo -- ^ @czo@. Description: Min Zhong Chinese. Preferred value: czo.
  | ExtDoq -- ^ @doq@. Description: Dominican Sign Language. Preferred value: doq.
  | ExtDse -- ^ @dse@. Description: Dutch Sign Language. Preferred value: dse.
  | ExtDsl -- ^ @dsl@. Description: Danish Sign Language. Preferred value: dsl.
  | ExtDup -- ^ @dup@. Description: Duano. Preferred value: dup.
  | ExtEcs -- ^ @ecs@. Description: Ecuadorian Sign Language. Preferred value: ecs.
  | ExtEhs -- ^ @ehs@. Description: Miyakubo Sign Language. Preferred value: ehs.
  | ExtEsl -- ^ @esl@. Description: Egypt Sign Language. Preferred value: esl.
  | ExtEsn -- ^ @esn@. Description: Salvadoran Sign Language. Preferred value: esn.
  | ExtEso -- ^ @eso@. Description: Estonian Sign Language. Preferred value: eso.
  | ExtEth -- ^ @eth@. Description: Ethiopian Sign Language. Preferred value: eth.
  | ExtFcs -- ^ @fcs@. Description: Quebec Sign Language. Preferred value: fcs.
  | ExtFse -- ^ @fse@. Description: Finnish Sign Language. Preferred value: fse.
  | ExtFsl -- ^ @fsl@. Description: French Sign Language. Preferred value: fsl.
  | ExtFss -- ^ @fss@. Description: Finland-Swedish Sign Language; finlandssvenskt teckenspråk; suomenruotsalainen viittomakieli. Preferred value: fss.
  | ExtGan -- ^ @gan@. Description: Gan Chinese. Preferred value: gan.
  | ExtGds -- ^ @gds@. Description: Ghandruk Sign Language. Preferred value: gds.
  | ExtGom -- ^ @gom@. Description: Goan Konkani. Preferred value: gom.
  | ExtGse -- ^ @gse@. Description: Ghanaian Sign Language. Preferred value: gse.
  | ExtGsg -- ^ @gsg@. Description: German Sign Language. Preferred value: gsg.
  | ExtGsm -- ^ @gsm@. Description: Guatemalan Sign Language. Preferred value: gsm.
  | ExtGss -- ^ @gss@. Description: Greek Sign Language. Preferred value: gss.
  | ExtGus -- ^ @gus@. Description: Guinean Sign Language. Preferred value: gus.
  | ExtHab -- ^ @hab@. Description: Hanoi Sign Language. Preferred value: hab.
  | ExtHaf -- ^ @haf@. Description: Haiphong Sign Language. Preferred value: haf.
  | ExtHak -- ^ @hak@. Description: Hakka Chinese. Preferred value: hak.
  | ExtHds -- ^ @hds@. Description: Honduras Sign Language. Preferred value: hds.
  | ExtHji -- ^ @hji@. Description: Haji. Preferred value: hji.
  | ExtHks -- ^ @hks@. Description: Hong Kong Sign Language; Heung Kong Sau Yue. Preferred value: hks.
  | ExtHos -- ^ @hos@. Description: Ho Chi Minh City Sign Language. Preferred value: hos.
  | ExtHps -- ^ @hps@. Description: Hawai\'i Sign Language (HSL); Hawai\'i Pidgin Sign Language. Preferred value: hps.
  | ExtHsh -- ^ @hsh@. Description: Hungarian Sign Language. Preferred value: hsh.
  | ExtHsl -- ^ @hsl@. Description: Hausa Sign Language. Preferred value: hsl.
  | ExtHsn -- ^ @hsn@. Description: Xiang Chinese. Preferred value: hsn.
  | ExtIcl -- ^ @icl@. Description: Icelandic Sign Language. Preferred value: icl.
  | ExtIks -- ^ @iks@. Description: Inuit Sign Language. Preferred value: iks.
  | ExtIls -- ^ @ils@. Description: International Sign. Preferred value: ils.
  | ExtInl -- ^ @inl@. Description: Indonesian Sign Language. Preferred value: inl.
  | ExtIns -- ^ @ins@. Description: Indian Sign Language. Preferred value: ins.
  | ExtIse -- ^ @ise@. Description: Italian Sign Language. Preferred value: ise.
  | ExtIsg -- ^ @isg@. Description: Irish Sign Language. Preferred value: isg.
  | ExtIsr -- ^ @isr@. Description: Israeli Sign Language. Preferred value: isr.
  | ExtJak -- ^ @jak@. Description: Jakun. Preferred value: jak.
  | ExtJax -- ^ @jax@. Description: Jambi Malay. Preferred value: jax.
  | ExtJcs -- ^ @jcs@. Description: Jamaican Country Sign Language. Preferred value: jcs.
  | ExtJhs -- ^ @jhs@. Description: Jhankot Sign Language. Preferred value: jhs.
  | ExtJks -- ^ @jks@. Description: Amami Koniya Sign Language. Preferred value: jks.
  | ExtJls -- ^ @jls@. Description: Jamaican Sign Language. Preferred value: jls.
  | ExtJos -- ^ @jos@. Description: Jordanian Sign Language. Preferred value: jos.
  | ExtJsl -- ^ @jsl@. Description: Japanese Sign Language. Preferred value: jsl.
  | ExtJus -- ^ @jus@. Description: Jumla Sign Language. Preferred value: jus.
  | ExtKgi -- ^ @kgi@. Description: Selangor Sign Language. Preferred value: kgi.
  | ExtKnn -- ^ @knn@. Description: Konkani (individual language). Preferred value: knn.
  | ExtKvb -- ^ @kvb@. Description: Kubu. Preferred value: kvb.
  | ExtKvk -- ^ @kvk@. Description: Korean Sign Language. Preferred value: kvk.
  | ExtKvr -- ^ @kvr@. Description: Kerinci. Preferred value: kvr.
  | ExtKxd -- ^ @kxd@. Description: Brunei. Preferred value: kxd.
  | ExtLbs -- ^ @lbs@. Description: Libyan Sign Language. Preferred value: lbs.
  | ExtLce -- ^ @lce@. Description: Loncong; Sekak. Preferred value: lce.
  | ExtLcf -- ^ @lcf@. Description: Lubu. Preferred value: lcf.
  | ExtLiw -- ^ @liw@. Description: Col. Preferred value: liw.
  | ExtLls -- ^ @lls@. Description: Lithuanian Sign Language. Preferred value: lls.
  | ExtLsb -- ^ @lsb@. Description: Burundian Sign Language; Langue des Signes Burundaise. Preferred value: lsb.
  | ExtLsg -- ^ @lsg@. Description: Lyons Sign Language. Deprecated. Preferred value: lsg.
  | ExtLsl -- ^ @lsl@. Description: Latvian Sign Language. Preferred value: lsl.
  | ExtLsn -- ^ @lsn@. Description: Tibetan Sign Language. Preferred value: lsn.
  | ExtLso -- ^ @lso@. Description: Laos Sign Language. Preferred value: lso.
  | ExtLsp -- ^ @lsp@. Description: Panamanian Sign Language; Lengua de Señas Panameñas. Preferred value: lsp.
  | ExtLst -- ^ @lst@. Description: Trinidad and Tobago Sign Language. Preferred value: lst.
  | ExtLsv -- ^ @lsv@. Description: Sivia Sign Language. Preferred value: lsv.
  | ExtLsy -- ^ @lsy@. Description: Mauritian Sign Language. Preferred value: lsy.
  | ExtLtg -- ^ @ltg@. Description: Latgalian. Preferred value: ltg.
  | ExtLvs -- ^ @lvs@. Description: Standard Latvian. Preferred value: lvs.
  | ExtLws -- ^ @lws@. Description: Malawian Sign Language. Preferred value: lws.
  | ExtLzh -- ^ @lzh@. Description: Literary Chinese. Preferred value: lzh.
  | ExtMax -- ^ @max@. Description: North Moluccan Malay. Preferred value: max.
  | ExtMdl -- ^ @mdl@. Description: Maltese Sign Language. Preferred value: mdl.
  | ExtMeo -- ^ @meo@. Description: Kedah Malay. Preferred value: meo.
  | ExtMfa -- ^ @mfa@. Description: Pattani Malay. Preferred value: mfa.
  | ExtMfb -- ^ @mfb@. Description: Bangka. Preferred value: mfb.
  | ExtMfs -- ^ @mfs@. Description: Mexican Sign Language. Preferred value: mfs.
  | ExtMin -- ^ @min@. Description: Minangkabau. Preferred value: min.
  | ExtMnp -- ^ @mnp@. Description: Min Bei Chinese. Preferred value: mnp.
  | ExtMqg -- ^ @mqg@. Description: Kota Bangun Kutai Malay. Preferred value: mqg.
  | ExtMre -- ^ @mre@. Description: Martha\'s Vineyard Sign Language. Preferred value: mre.
  | ExtMsd -- ^ @msd@. Description: Yucatec Maya Sign Language. Preferred value: msd.
  | ExtMsi -- ^ @msi@. Description: Sabah Malay. Preferred value: msi.
  | ExtMsr -- ^ @msr@. Description: Mongolian Sign Language. Preferred value: msr.
  | ExtMui -- ^ @mui@. Description: Musi. Preferred value: mui.
  | ExtMzc -- ^ @mzc@. Description: Madagascar Sign Language. Preferred value: mzc.
  | ExtMzg -- ^ @mzg@. Description: Monastic Sign Language. Preferred value: mzg.
  | ExtMzy -- ^ @mzy@. Description: Mozambican Sign Language. Preferred value: mzy.
  | ExtNan -- ^ @nan@. Description: Min Nan Chinese. Preferred value: nan.
  | ExtNbs -- ^ @nbs@. Description: Namibian Sign Language. Preferred value: nbs.
  | ExtNcs -- ^ @ncs@. Description: Nicaraguan Sign Language. Preferred value: ncs.
  | ExtNsi -- ^ @nsi@. Description: Nigerian Sign Language. Preferred value: nsi.
  | ExtNsl -- ^ @nsl@. Description: Norwegian Sign Language. Preferred value: nsl.
  | ExtNsp -- ^ @nsp@. Description: Nepalese Sign Language. Preferred value: nsp.
  | ExtNsr -- ^ @nsr@. Description: Maritime Sign Language. Preferred value: nsr.
  | ExtNzs -- ^ @nzs@. Description: New Zealand Sign Language. Preferred value: nzs.
  | ExtOkl -- ^ @okl@. Description: Old Kentish Sign Language. Preferred value: okl.
  | ExtOrn -- ^ @orn@. Description: Orang Kanaq. Preferred value: orn.
  | ExtOrs -- ^ @ors@. Description: Orang Seletar. Preferred value: ors.
  | ExtPel -- ^ @pel@. Description: Pekal. Preferred value: pel.
  | ExtPga -- ^ @pga@. Description: Sudanese Creole Arabic. Preferred value: pga.
  | ExtPgz -- ^ @pgz@. Description: Papua New Guinean Sign Language. Preferred value: pgz.
  | ExtPks -- ^ @pks@. Description: Pakistan Sign Language. Preferred value: pks.
  | ExtPrl -- ^ @prl@. Description: Peruvian Sign Language. Preferred value: prl.
  | ExtPrz -- ^ @prz@. Description: Providencia Sign Language. Preferred value: prz.
  | ExtPsc -- ^ @psc@. Description: Persian Sign Language. Preferred value: psc.
  | ExtPsd -- ^ @psd@. Description: Plains Indian Sign Language. Preferred value: psd.
  | ExtPse -- ^ @pse@. Description: Central Malay. Preferred value: pse.
  | ExtPsg -- ^ @psg@. Description: Penang Sign Language. Preferred value: psg.
  | ExtPsl -- ^ @psl@. Description: Puerto Rican Sign Language. Preferred value: psl.
  | ExtPso -- ^ @pso@. Description: Polish Sign Language. Preferred value: pso.
  | ExtPsp -- ^ @psp@. Description: Philippine Sign Language. Preferred value: psp.
  | ExtPsr -- ^ @psr@. Description: Portuguese Sign Language. Preferred value: psr.
  | ExtPys -- ^ @pys@. Description: Paraguayan Sign Language; Lengua de Señas del Paraguay. Preferred value: pys.
  | ExtRms -- ^ @rms@. Description: Romanian Sign Language. Preferred value: rms.
  | ExtRsi -- ^ @rsi@. Description: Rennellese Sign Language. Deprecated. Preferred value: rsi.
  | ExtRsl -- ^ @rsl@. Description: Russian Sign Language. Preferred value: rsl.
  | ExtRsm -- ^ @rsm@. Description: Miriwoong Sign Language. Preferred value: rsm.
  | ExtSdl -- ^ @sdl@. Description: Saudi Arabian Sign Language. Preferred value: sdl.
  | ExtSfb -- ^ @sfb@. Description: Langue des signes de Belgique Francophone; French Belgian Sign Language. Preferred value: sfb.
  | ExtSfs -- ^ @sfs@. Description: South African Sign Language. Preferred value: sfs.
  | ExtSgg -- ^ @sgg@. Description: Swiss-German Sign Language. Preferred value: sgg.
  | ExtSgx -- ^ @sgx@. Description: Sierra Leone Sign Language. Preferred value: sgx.
  | ExtShu -- ^ @shu@. Description: Chadian Arabic. Preferred value: shu.
  | ExtSlf -- ^ @slf@. Description: Swiss-Italian Sign Language. Preferred value: slf.
  | ExtSls -- ^ @sls@. Description: Singapore Sign Language. Preferred value: sls.
  | ExtSqk -- ^ @sqk@. Description: Albanian Sign Language. Preferred value: sqk.
  | ExtSqs -- ^ @sqs@. Description: Sri Lankan Sign Language. Preferred value: sqs.
  | ExtSqx -- ^ @sqx@. Description: Kufr Qassem Sign Language (KQSL). Preferred value: sqx.
  | ExtSsh -- ^ @ssh@. Description: Shihhi Arabic. Preferred value: ssh.
  | ExtSsp -- ^ @ssp@. Description: Spanish Sign Language. Preferred value: ssp.
  | ExtSsr -- ^ @ssr@. Description: Swiss-French Sign Language. Preferred value: ssr.
  | ExtSvk -- ^ @svk@. Description: Slovakian Sign Language. Preferred value: svk.
  | ExtSwc -- ^ @swc@. Description: Congo Swahili. Preferred value: swc.
  | ExtSwh -- ^ @swh@. Description: Swahili (individual language); Kiswahili. Preferred value: swh.
  | ExtSwl -- ^ @swl@. Description: Swedish Sign Language. Preferred value: swl.
  | ExtSyy -- ^ @syy@. Description: Al-Sayyid Bedouin Sign Language. Preferred value: syy.
  | ExtSzs -- ^ @szs@. Description: Solomon Islands Sign Language. Preferred value: szs.
  | ExtTmw -- ^ @tmw@. Description: Temuan. Preferred value: tmw.
  | ExtTse -- ^ @tse@. Description: Tunisian Sign Language. Preferred value: tse.
  | ExtTsm -- ^ @tsm@. Description: Turkish Sign Language; Türk İşaret Dili. Preferred value: tsm.
  | ExtTsq -- ^ @tsq@. Description: Thai Sign Language. Preferred value: tsq.
  | ExtTss -- ^ @tss@. Description: Taiwan Sign Language. Preferred value: tss.
  | ExtTsy -- ^ @tsy@. Description: Tebul Sign Language. Preferred value: tsy.
  | ExtTza -- ^ @tza@. Description: Tanzanian Sign Language. Preferred value: tza.
  | ExtUgn -- ^ @ugn@. Description: Ugandan Sign Language. Preferred value: ugn.
  | ExtUgy -- ^ @ugy@. Description: Uruguayan Sign Language. Preferred value: ugy.
  | ExtUkl -- ^ @ukl@. Description: Ukrainian Sign Language. Preferred value: ukl.
  | ExtUks -- ^ @uks@. Description: Urubú-Kaapor Sign Language; Kaapor Sign Language. Preferred value: uks.
  | ExtUrk -- ^ @urk@. Description: Urak Lawoi\'. Preferred value: urk.
  | ExtUzn -- ^ @uzn@. Description: Northern Uzbek. Preferred value: uzn.
  | ExtUzs -- ^ @uzs@. Description: Southern Uzbek. Preferred value: uzs.
  | ExtVgt -- ^ @vgt@. Description: Vlaamse Gebarentaal; Flemish Sign Language. Preferred value: vgt.
  | ExtVkk -- ^ @vkk@. Description: Kaur. Preferred value: vkk.
  | ExtVkt -- ^ @vkt@. Description: Tenggarong Kutai Malay. Preferred value: vkt.
  | ExtVsi -- ^ @vsi@. Description: Moldova Sign Language. Preferred value: vsi.
  | ExtVsl -- ^ @vsl@. Description: Venezuelan Sign Language. Preferred value: vsl.
  | ExtVsv -- ^ @vsv@. Description: Valencian Sign Language; Llengua de signes valenciana. Preferred value: vsv.
  | ExtWbs -- ^ @wbs@. Description: West Bengal Sign Language. Preferred value: wbs.
  | ExtWuu -- ^ @wuu@. Description: Wu Chinese. Preferred value: wuu.
  | ExtXki -- ^ @xki@. Description: Kenyan Sign Language. Preferred value: xki.
  | ExtXml -- ^ @xml@. Description: Malaysian Sign Language. Preferred value: xml.
  | ExtXmm -- ^ @xmm@. Description: Manado Malay. Preferred value: xmm.
  | ExtXms -- ^ @xms@. Description: Moroccan Sign Language. Preferred value: xms.
  | ExtYds -- ^ @yds@. Description: Yiddish Sign Language. Deprecated. Preferred value: yds.
  | ExtYgs -- ^ @ygs@. Description: Yolŋu Sign Language. Preferred value: ygs.
  | ExtYhs -- ^ @yhs@. Description: Yan-nhaŋu Sign Language. Preferred value: yhs.
  | ExtYsl -- ^ @ysl@. Description: Yugoslavian Sign Language. Preferred value: ysl.
  | ExtYsm -- ^ @ysm@. Description: Myanmar Sign Language. Preferred value: ysm.
  | ExtYue -- ^ @yue@. Description: Yue Chinese; Cantonese. Preferred value: yue.
  | ExtZib -- ^ @zib@. Description: Zimbabwe Sign Language. Preferred value: zib.
  | ExtZlm -- ^ @zlm@. Description: Malay (individual language). Preferred value: zlm.
  | ExtZmi -- ^ @zmi@. Description: Negeri Sembilan Malay. Preferred value: zmi.
  | ExtZsl -- ^ @zsl@. Description: Zambian Sign Language. Preferred value: zsl.
  | ExtZsm -- ^ @zsm@. Description: Standard Malay. Preferred value: zsm.

  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData Extlang where
  rnf a = seq a ()

instance Hashable Extlang where
  hashWithSalt = hashUsing fromEnum

parseExtlang :: Subtag -> Maybe Extlang
parseExtlang = flip HM.lookup table . unwrapSubtag
  where
    table = HM.fromList
      [ (14089361900647219203, ExtAao)
      , (14090426227902906371, ExtAbh)
      , (14090549373205217283, ExtAbv)
      , (14091596108274860035, ExtAcm)
      , (14091631292646948867, ExtAcq)
      , (14091684069205082115, ExtAcw)
      , (14091692865298104323, ExtAcx)
      , (14091701661391126531, ExtAcy)
      , (14092660435530547203, ExtAdf)
      , (14092774784739835907, ExtAds)
      , (14093751151065300995, ExtAeb)
      , (14093759947158323203, ExtAec)
      , (14093768743251345411, ExtAed)
      , (14093856704181567491, ExtAen)
      , (14094877050972143619, ExtAfb)
      , (14094921031437254659, ExtAfg)
      , (14099503795901825027, ExtAjp)
      , (14106144846133592067, ExtApc)
      , (14106153642226614275, ExtApd)
      , (14108387849854255107, ExtArb)
      , (14108519791249588227, ExtArq)
      , (14108537383435632643, ExtArs)
      , (14108590159993765891, ExtAry)
      , (14108598956086788099, ExtArz)
      , (14109540138040164355, ExtAse)
      , (14109548934133186563, ExtAsf)
      , (14109636895063408643, ExtAsp)
      , (14109645691156430851, ExtAsq)
      , (14109698467714564099, ExtAsw)
      , (14111976655807315971, ExtAuz)
      , (14112979410411847683, ExtAvl)
      , (14116321925760286723, ExtAyh)
      , (14116357110132375555, ExtAyl)
      , (14116374702318419971, ExtAyn)
      , (14116392294504464387, ExtAyp)
      , (14234699745653161987, ExtBbz)
      , (14239053811699154947, ExtBfi)
      , (14239071403885199363, ExtBfk)
      , (14243601391791636483, ExtBjn)
      , (14249169318674694147, ExtBog)
      , (14251482691139534851, ExtBqn)
      , (14251579448162779139, ExtBqy)
      , (14254825206487973891, ExtBtj)
      , (14257033025836548099, ExtBve)
      , (14257094598487703555, ExtBvl)
      , (14257173763324903427, ExtBvu)
      , (14261659770766229507, ExtBzs)
      , (14380969976519458819, ExtCdo)
      , (14381005160891547651, ExtCds)
      , (14387813336890736643, ExtCjy)
      , (14391094279588020227, ExtCmn)
      , (14392237771680907267, ExtCnp)
      , (14393231730192416771, ExtCoa)
      , (14394559940238770179, ExtCpx)
      , (14397752922005831683, ExtCsc)
      , (14397761718098853891, ExtCsd)
      , (14397770514191876099, ExtCse)
      , (14397779310284898307, ExtCsf)
      , (14397788106377920515, ExtCsg)
      , (14397832086843031555, ExtCsl)
      , (14397849679029075971, ExtCsn)
      , (14397867271215120387, ExtCsp)
      , (14397876067308142595, ExtCsq)
      , (14397884863401164803, ExtCsr)
      , (14397937639959298051, ExtCsx)
      , (14405678201818841091, ExtCzh)
      , (14405739774469996547, ExtCzo)
      , (14537487655756627971, ExtDoq)
      , (14541885702267731971, ExtDse)
      , (14541947274918887427, ExtDsl)
      , (14544234259104661507, ExtDup)
      , (14668109637136416771, ExtEcs)
      , (14673739136670629891, ExtEhs)
      , (14686062462994743299, ExtEsl)
      , (14686080055180787715, ExtEsn)
      , (14686088851273809923, ExtEso)
      , (14687153178529497091, ExtEth)
      , (14812224825212272643, ExtFcs)
      , (14830116078419443715, ExtFse)
      , (14830177651070599171, ExtFsl)
      , (14830239223721754627, ExtFss)
      , (14954044233009332227, ExtGan)
      , (14957465913194971139, ExtGds)
      , (14969798035612106755, ExtGom)
      , (14974231266495299587, ExtGse)
      , (14974248858681344003, ExtGsg)
      , (14974301635239477251, ExtGsm)
      , (14974354411797610499, ExtGss)
      , (14976606211611295747, ExtGus)
      , (15098053867968921603, ExtHab)
      , (15098089052341010435, ExtHaf)
      , (15098133032806121475, ExtHak)
      , (15101581101270827011, ExtHds)
      , (15108248539781660675, ExtHji)
      , (15109462400618725379, ExtHks)
      , (15113966000246095875, ExtHos)
      , (15115091900152938499, ExtHps)
      , (15118372842850222083, ExtHsh)
      , (15118408027222310915, ExtHsl)
      , (15118425619408355331, ExtHsn)
      , (15244508816788684803, ExtIcl)
      , (15253577588694581251, ExtIks)
      , (15254703488601423875, ExtIls)
      , (15256893715763953667, ExtInl)
      , (15256955288415109123, ExtIns)
      , (15262461642647011331, ExtIse)
      , (15262479234833055747, ExtIsg)
      , (15262575991856300035, ExtIsr)
      , (15386363408957833219, ExtJak)
      , (15386477758167121923, ExtJax)
      , (15388685577515696131, ExtJcs)
      , (15394315077049909251, ExtJhs)
      , (15397692776770437123, ExtJks)
      , (15398818676677279747, ExtJls)
      , (15402196376397807619, ExtJos)
      , (15406638403374022659, ExtJsl)
      , (15408951775838863363, ExtJus)
      , (15537216404288700419, ExtKgi)
      , (15545141684101709827, ExtKnn)
      , (15554043330240184323, ExtKvb)
      , (15554122495077384195, ExtKvk)
      , (15554184067728539651, ExtKvr)
      , (15556312722239913987, ExtKxd)
      , (15675790053760565251, ExtLbs)
      , (15676792808365096963, ExtLce)
      , (15676801604458119171, ExtLcf)
      , (15683706537480552451, ExtLiw)
      , (15687049052828991491, ExtLls)
      , (15694780818595512323, ExtLsb)
      , (15694824799060623363, ExtLsg)
      , (15694868779525734403, ExtLsl)
      , (15694886371711778819, ExtLsn)
      , (15694895167804801027, ExtLso)
      , (15694903963897823235, ExtLsp)
      , (15694939148269912067, ExtLst)
      , (15694956740455956483, ExtLsv)
      , (15694983128735023107, ExtLsy)
      , (15695950698967465987, ExtLtg)
      , (15698308051897417731, ExtLvs)
      , (15699433951804260355, ExtLws)
      , (15702714894501543939, ExtLzh)
      , (15818823322394689539, ExtMax)
      , (15822095468998950915, ExtMdl)
      , (15823247757184860163, ExtMeo)
      , (15824250511789391875, ExtMfa)
      , (15824259307882414083, ExtMfb)
      , (15824408841463791619, ExtMfs)
      , (15827742560719208451, ExtMin)
      , (15833389652439465987, ExtMnp)
      , (15836688187322793987, ExtMqg)
      , (15837796495043592195, ExtMre)
      , (15838913598857412611, ExtMsd)
      , (15838957579322523651, ExtMsi)
      , (15839036744159723523, ExtMsr)
      , (15841209379136208899, ExtMui)
      , (15846786102112288771, ExtMzc)
      , (15846821286484377603, ExtMzg)
      , (15846979616158777347, ExtMzy)
      , (15962850549540323331, ExtNan)
      , (15964020429912276995, ExtNbs)
      , (15965146329819119619, ExtNcs)
      , (15983072767398379523, ExtNsi)
      , (15983099155677446147, ExtNsl)
      , (15983134340049534979, ExtNsp)
      , (15983151932235579395, ExtNsr)
      , (15991042027676499971, ExtNzs)
      , (16118207144498561027, ExtOkl)
      , (16126106036032503811, ExtOrn)
      , (16126150016497614851, ExtOrs)
      , (16255566933133361155, ExtPel)
      , (16257721975923802115, ExtPga)
      , (16257941878249357315, ExtPgz)
      , (16262383905225572355, ExtPks)
      , (16270203631922315267, ExtPrl)
      , (16270326777224626179, ExtPrz)
      , (16271250366991958019, ExtPsc)
      , (16271259163084980227, ExtPsd)
      , (16271267959178002435, ExtPse)
      , (16271285551364046851, ExtPsg)
      , (16271329531829157891, ExtPsl)
      , (16271355920108224515, ExtPso)
      , (16271364716201246723, ExtPsp)
      , (16271382308387291139, ExtPsr)
      , (16278146503921369091, ExtPys)
      , (16552866081190969347, ExtRms)
      , (16559533519701803011, ExtRsi)
      , (16559559907980869635, ExtRsl)
      , (16559568704073891843, ExtRsm)
      , (16686786597454086147, ExtSdl)
      , (16688950436337549315, ExtSfb)
      , (16689099969918926851, ExtSfs)
      , (16690120316709502979, ExtSgg)
      , (16690269850290880515, ExtSgx)
      , (16691369361918656515, ExtShu)
      , (16695741020150693891, ExtSlf)
      , (16695855369359982595, ExtSls)
      , (16701414500150018051, ExtSqk)
      , (16701484868894195715, ExtSqs)
      , (16701528849359306755, ExtSqx)
      , (16703639911684636675, ExtSsh)
      , (16703710280428814339, ExtSsp)
      , (16703727872614858755, ExtSsr)
      , (16707043999684231171, ExtSvk)
      , (16708099530846896131, ExtSwc)
      , (16708143511312007171, ExtSwh)
      , (16708178695684096003, ExtSwl)
      , (16710544844707069955, ExtSyy)
      , (16711617968055779331, ExtSzs)
      , (16841131641714769923, ExtTmw)
      , (16847728711481425923, ExtTse)
      , (16847799080225603587, ExtTsm)
      , (16847834264597692419, ExtTsq)
      , (16847851856783736835, ExtTss)
      , (16847904633341870083, ExtTsy)
      , (16855574826457235459, ExtTza)
      , (16978412265512370179, ExtUgn)
      , (16978509022535614467, ExtUgy)
      , (16982898272953696259, ExtUkl)
      , (16982959845604851715, ExtUks)
      , (16990770776208572419, ExtUrk)
      , (16999804363742380035, ExtUzn)
      , (16999848344207491075, ExtUzs)
      , (17122580230146359299, ExtVgt)
      , (17127004664936529923, ExtVkk)
      , (17127083829773729795, ExtVkt)
      , (17135994272005226499, ExtVsi)
      , (17136020660284293123, ExtVsl)
      , (17136108621214515203, ExtVsv)
      , (17261057122594979843, ExtWbs)
      , (17282466813011034115, ExtWuu)
      , (17415217448902197251, ExtXki)
      , (17417495636994949123, ExtXml)
      , (17417504433087971331, ExtXmm)
      , (17417557209646104579, ExtXms)
      , (17551539298560376835, ExtYds)
      , (17554916998280904707, ExtYgs)
      , (17556042898187747331, ExtYhs)
      , (17568366224511860739, ExtYsl)
      , (17568375020604882947, ExtYsm)
      , (17570556451674390531, ExtYue)
      , (17701134452589068291, ExtZib)
      , (17704608909332840451, ExtZlm)
      , (17705699624867594243, ExtZmi)
      , (17712481412587716611, ExtZsl)
      , (17712490208680738819, ExtZsm)]
extlangToSubtag :: Extlang -> Subtag
extlangToSubtag x = case x of
  ExtAao -> Subtag 14089361900647219203
  ExtAbh -> Subtag 14090426227902906371
  ExtAbv -> Subtag 14090549373205217283
  ExtAcm -> Subtag 14091596108274860035
  ExtAcq -> Subtag 14091631292646948867
  ExtAcw -> Subtag 14091684069205082115
  ExtAcx -> Subtag 14091692865298104323
  ExtAcy -> Subtag 14091701661391126531
  ExtAdf -> Subtag 14092660435530547203
  ExtAds -> Subtag 14092774784739835907
  ExtAeb -> Subtag 14093751151065300995
  ExtAec -> Subtag 14093759947158323203
  ExtAed -> Subtag 14093768743251345411
  ExtAen -> Subtag 14093856704181567491
  ExtAfb -> Subtag 14094877050972143619
  ExtAfg -> Subtag 14094921031437254659
  ExtAjp -> Subtag 14099503795901825027
  ExtApc -> Subtag 14106144846133592067
  ExtApd -> Subtag 14106153642226614275
  ExtArb -> Subtag 14108387849854255107
  ExtArq -> Subtag 14108519791249588227
  ExtArs -> Subtag 14108537383435632643
  ExtAry -> Subtag 14108590159993765891
  ExtArz -> Subtag 14108598956086788099
  ExtAse -> Subtag 14109540138040164355
  ExtAsf -> Subtag 14109548934133186563
  ExtAsp -> Subtag 14109636895063408643
  ExtAsq -> Subtag 14109645691156430851
  ExtAsw -> Subtag 14109698467714564099
  ExtAuz -> Subtag 14111976655807315971
  ExtAvl -> Subtag 14112979410411847683
  ExtAyh -> Subtag 14116321925760286723
  ExtAyl -> Subtag 14116357110132375555
  ExtAyn -> Subtag 14116374702318419971
  ExtAyp -> Subtag 14116392294504464387
  ExtBbz -> Subtag 14234699745653161987
  ExtBfi -> Subtag 14239053811699154947
  ExtBfk -> Subtag 14239071403885199363
  ExtBjn -> Subtag 14243601391791636483
  ExtBog -> Subtag 14249169318674694147
  ExtBqn -> Subtag 14251482691139534851
  ExtBqy -> Subtag 14251579448162779139
  ExtBtj -> Subtag 14254825206487973891
  ExtBve -> Subtag 14257033025836548099
  ExtBvl -> Subtag 14257094598487703555
  ExtBvu -> Subtag 14257173763324903427
  ExtBzs -> Subtag 14261659770766229507
  ExtCdo -> Subtag 14380969976519458819
  ExtCds -> Subtag 14381005160891547651
  ExtCjy -> Subtag 14387813336890736643
  ExtCmn -> Subtag 14391094279588020227
  ExtCnp -> Subtag 14392237771680907267
  ExtCoa -> Subtag 14393231730192416771
  ExtCpx -> Subtag 14394559940238770179
  ExtCsc -> Subtag 14397752922005831683
  ExtCsd -> Subtag 14397761718098853891
  ExtCse -> Subtag 14397770514191876099
  ExtCsf -> Subtag 14397779310284898307
  ExtCsg -> Subtag 14397788106377920515
  ExtCsl -> Subtag 14397832086843031555
  ExtCsn -> Subtag 14397849679029075971
  ExtCsp -> Subtag 14397867271215120387
  ExtCsq -> Subtag 14397876067308142595
  ExtCsr -> Subtag 14397884863401164803
  ExtCsx -> Subtag 14397937639959298051
  ExtCzh -> Subtag 14405678201818841091
  ExtCzo -> Subtag 14405739774469996547
  ExtDoq -> Subtag 14537487655756627971
  ExtDse -> Subtag 14541885702267731971
  ExtDsl -> Subtag 14541947274918887427
  ExtDup -> Subtag 14544234259104661507
  ExtEcs -> Subtag 14668109637136416771
  ExtEhs -> Subtag 14673739136670629891
  ExtEsl -> Subtag 14686062462994743299
  ExtEsn -> Subtag 14686080055180787715
  ExtEso -> Subtag 14686088851273809923
  ExtEth -> Subtag 14687153178529497091
  ExtFcs -> Subtag 14812224825212272643
  ExtFse -> Subtag 14830116078419443715
  ExtFsl -> Subtag 14830177651070599171
  ExtFss -> Subtag 14830239223721754627
  ExtGan -> Subtag 14954044233009332227
  ExtGds -> Subtag 14957465913194971139
  ExtGom -> Subtag 14969798035612106755
  ExtGse -> Subtag 14974231266495299587
  ExtGsg -> Subtag 14974248858681344003
  ExtGsm -> Subtag 14974301635239477251
  ExtGss -> Subtag 14974354411797610499
  ExtGus -> Subtag 14976606211611295747
  ExtHab -> Subtag 15098053867968921603
  ExtHaf -> Subtag 15098089052341010435
  ExtHak -> Subtag 15098133032806121475
  ExtHds -> Subtag 15101581101270827011
  ExtHji -> Subtag 15108248539781660675
  ExtHks -> Subtag 15109462400618725379
  ExtHos -> Subtag 15113966000246095875
  ExtHps -> Subtag 15115091900152938499
  ExtHsh -> Subtag 15118372842850222083
  ExtHsl -> Subtag 15118408027222310915
  ExtHsn -> Subtag 15118425619408355331
  ExtIcl -> Subtag 15244508816788684803
  ExtIks -> Subtag 15253577588694581251
  ExtIls -> Subtag 15254703488601423875
  ExtInl -> Subtag 15256893715763953667
  ExtIns -> Subtag 15256955288415109123
  ExtIse -> Subtag 15262461642647011331
  ExtIsg -> Subtag 15262479234833055747
  ExtIsr -> Subtag 15262575991856300035
  ExtJak -> Subtag 15386363408957833219
  ExtJax -> Subtag 15386477758167121923
  ExtJcs -> Subtag 15388685577515696131
  ExtJhs -> Subtag 15394315077049909251
  ExtJks -> Subtag 15397692776770437123
  ExtJls -> Subtag 15398818676677279747
  ExtJos -> Subtag 15402196376397807619
  ExtJsl -> Subtag 15406638403374022659
  ExtJus -> Subtag 15408951775838863363
  ExtKgi -> Subtag 15537216404288700419
  ExtKnn -> Subtag 15545141684101709827
  ExtKvb -> Subtag 15554043330240184323
  ExtKvk -> Subtag 15554122495077384195
  ExtKvr -> Subtag 15554184067728539651
  ExtKxd -> Subtag 15556312722239913987
  ExtLbs -> Subtag 15675790053760565251
  ExtLce -> Subtag 15676792808365096963
  ExtLcf -> Subtag 15676801604458119171
  ExtLiw -> Subtag 15683706537480552451
  ExtLls -> Subtag 15687049052828991491
  ExtLsb -> Subtag 15694780818595512323
  ExtLsg -> Subtag 15694824799060623363
  ExtLsl -> Subtag 15694868779525734403
  ExtLsn -> Subtag 15694886371711778819
  ExtLso -> Subtag 15694895167804801027
  ExtLsp -> Subtag 15694903963897823235
  ExtLst -> Subtag 15694939148269912067
  ExtLsv -> Subtag 15694956740455956483
  ExtLsy -> Subtag 15694983128735023107
  ExtLtg -> Subtag 15695950698967465987
  ExtLvs -> Subtag 15698308051897417731
  ExtLws -> Subtag 15699433951804260355
  ExtLzh -> Subtag 15702714894501543939
  ExtMax -> Subtag 15818823322394689539
  ExtMdl -> Subtag 15822095468998950915
  ExtMeo -> Subtag 15823247757184860163
  ExtMfa -> Subtag 15824250511789391875
  ExtMfb -> Subtag 15824259307882414083
  ExtMfs -> Subtag 15824408841463791619
  ExtMin -> Subtag 15827742560719208451
  ExtMnp -> Subtag 15833389652439465987
  ExtMqg -> Subtag 15836688187322793987
  ExtMre -> Subtag 15837796495043592195
  ExtMsd -> Subtag 15838913598857412611
  ExtMsi -> Subtag 15838957579322523651
  ExtMsr -> Subtag 15839036744159723523
  ExtMui -> Subtag 15841209379136208899
  ExtMzc -> Subtag 15846786102112288771
  ExtMzg -> Subtag 15846821286484377603
  ExtMzy -> Subtag 15846979616158777347
  ExtNan -> Subtag 15962850549540323331
  ExtNbs -> Subtag 15964020429912276995
  ExtNcs -> Subtag 15965146329819119619
  ExtNsi -> Subtag 15983072767398379523
  ExtNsl -> Subtag 15983099155677446147
  ExtNsp -> Subtag 15983134340049534979
  ExtNsr -> Subtag 15983151932235579395
  ExtNzs -> Subtag 15991042027676499971
  ExtOkl -> Subtag 16118207144498561027
  ExtOrn -> Subtag 16126106036032503811
  ExtOrs -> Subtag 16126150016497614851
  ExtPel -> Subtag 16255566933133361155
  ExtPga -> Subtag 16257721975923802115
  ExtPgz -> Subtag 16257941878249357315
  ExtPks -> Subtag 16262383905225572355
  ExtPrl -> Subtag 16270203631922315267
  ExtPrz -> Subtag 16270326777224626179
  ExtPsc -> Subtag 16271250366991958019
  ExtPsd -> Subtag 16271259163084980227
  ExtPse -> Subtag 16271267959178002435
  ExtPsg -> Subtag 16271285551364046851
  ExtPsl -> Subtag 16271329531829157891
  ExtPso -> Subtag 16271355920108224515
  ExtPsp -> Subtag 16271364716201246723
  ExtPsr -> Subtag 16271382308387291139
  ExtPys -> Subtag 16278146503921369091
  ExtRms -> Subtag 16552866081190969347
  ExtRsi -> Subtag 16559533519701803011
  ExtRsl -> Subtag 16559559907980869635
  ExtRsm -> Subtag 16559568704073891843
  ExtSdl -> Subtag 16686786597454086147
  ExtSfb -> Subtag 16688950436337549315
  ExtSfs -> Subtag 16689099969918926851
  ExtSgg -> Subtag 16690120316709502979
  ExtSgx -> Subtag 16690269850290880515
  ExtShu -> Subtag 16691369361918656515
  ExtSlf -> Subtag 16695741020150693891
  ExtSls -> Subtag 16695855369359982595
  ExtSqk -> Subtag 16701414500150018051
  ExtSqs -> Subtag 16701484868894195715
  ExtSqx -> Subtag 16701528849359306755
  ExtSsh -> Subtag 16703639911684636675
  ExtSsp -> Subtag 16703710280428814339
  ExtSsr -> Subtag 16703727872614858755
  ExtSvk -> Subtag 16707043999684231171
  ExtSwc -> Subtag 16708099530846896131
  ExtSwh -> Subtag 16708143511312007171
  ExtSwl -> Subtag 16708178695684096003
  ExtSyy -> Subtag 16710544844707069955
  ExtSzs -> Subtag 16711617968055779331
  ExtTmw -> Subtag 16841131641714769923
  ExtTse -> Subtag 16847728711481425923
  ExtTsm -> Subtag 16847799080225603587
  ExtTsq -> Subtag 16847834264597692419
  ExtTss -> Subtag 16847851856783736835
  ExtTsy -> Subtag 16847904633341870083
  ExtTza -> Subtag 16855574826457235459
  ExtUgn -> Subtag 16978412265512370179
  ExtUgy -> Subtag 16978509022535614467
  ExtUkl -> Subtag 16982898272953696259
  ExtUks -> Subtag 16982959845604851715
  ExtUrk -> Subtag 16990770776208572419
  ExtUzn -> Subtag 16999804363742380035
  ExtUzs -> Subtag 16999848344207491075
  ExtVgt -> Subtag 17122580230146359299
  ExtVkk -> Subtag 17127004664936529923
  ExtVkt -> Subtag 17127083829773729795
  ExtVsi -> Subtag 17135994272005226499
  ExtVsl -> Subtag 17136020660284293123
  ExtVsv -> Subtag 17136108621214515203
  ExtWbs -> Subtag 17261057122594979843
  ExtWuu -> Subtag 17282466813011034115
  ExtXki -> Subtag 17415217448902197251
  ExtXml -> Subtag 17417495636994949123
  ExtXmm -> Subtag 17417504433087971331
  ExtXms -> Subtag 17417557209646104579
  ExtYds -> Subtag 17551539298560376835
  ExtYgs -> Subtag 17554916998280904707
  ExtYhs -> Subtag 17556042898187747331
  ExtYsl -> Subtag 17568366224511860739
  ExtYsm -> Subtag 17568375020604882947
  ExtYue -> Subtag 17570556451674390531
  ExtZib -> Subtag 17701134452589068291
  ExtZlm -> Subtag 17704608909332840451
  ExtZmi -> Subtag 17705699624867594243
  ExtZsl -> Subtag 17712481412587716611
  ExtZsm -> Subtag 17712490208680738819
