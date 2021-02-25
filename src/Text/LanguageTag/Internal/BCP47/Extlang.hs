-- This is an auto-generated file. Do not edit by hand

module Text.LanguageTag.Internal.BCP47.Extlang where

import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..), hashUsing)

-- | The BCP47 extended language tags as of 2021-02-23. These are prefixed with "Ext" because they may overlap with primary language subtags. Note that if extended language subtags have a preferred value, then it refers to a primary subtag.
data Extlang
  = ExtAao -- ^ @aao@. Description: Algerian Saharan Arabic.
  | ExtAbh -- ^ @abh@. Description: Tajiki Arabic.
  | ExtAbv -- ^ @abv@. Description: Baharna Arabic.
  | ExtAcm -- ^ @acm@. Description: Mesopotamian Arabic.
  | ExtAcq -- ^ @acq@. Description: Ta\'izzi-Adeni Arabic.
  | ExtAcw -- ^ @acw@. Description: Hijazi Arabic.
  | ExtAcx -- ^ @acx@. Description: Omani Arabic.
  | ExtAcy -- ^ @acy@. Description: Cypriot Arabic.
  | ExtAdf -- ^ @adf@. Description: Dhofari Arabic.
  | ExtAds -- ^ @ads@. Description: Adamorobe Sign Language.
  | ExtAeb -- ^ @aeb@. Description: Tunisian Arabic.
  | ExtAec -- ^ @aec@. Description: Saidi Arabic.
  | ExtAed -- ^ @aed@. Description: Argentine Sign Language.
  | ExtAen -- ^ @aen@. Description: Armenian Sign Language.
  | ExtAfb -- ^ @afb@. Description: Gulf Arabic.
  | ExtAfg -- ^ @afg@. Description: Afghan Sign Language.
  | ExtAjp -- ^ @ajp@. Description: South Levantine Arabic.
  | ExtApc -- ^ @apc@. Description: North Levantine Arabic.
  | ExtApd -- ^ @apd@. Description: Sudanese Arabic.
  | ExtArb -- ^ @arb@. Description: Standard Arabic.
  | ExtArq -- ^ @arq@. Description: Algerian Arabic.
  | ExtArs -- ^ @ars@. Description: Najdi Arabic.
  | ExtAry -- ^ @ary@. Description: Moroccan Arabic.
  | ExtArz -- ^ @arz@. Description: Egyptian Arabic.
  | ExtAse -- ^ @ase@. Description: American Sign Language.
  | ExtAsf -- ^ @asf@. Description: Auslan; Australian Sign Language.
  | ExtAsp -- ^ @asp@. Description: Algerian Sign Language.
  | ExtAsq -- ^ @asq@. Description: Austrian Sign Language.
  | ExtAsw -- ^ @asw@. Description: Australian Aborigines Sign Language.
  | ExtAuz -- ^ @auz@. Description: Uzbeki Arabic.
  | ExtAvl -- ^ @avl@. Description: Eastern Egyptian Bedawi Arabic.
  | ExtAyh -- ^ @ayh@. Description: Hadrami Arabic.
  | ExtAyl -- ^ @ayl@. Description: Libyan Arabic.
  | ExtAyn -- ^ @ayn@. Description: Sanaani Arabic.
  | ExtAyp -- ^ @ayp@. Description: North Mesopotamian Arabic.
  | ExtBbz -- ^ @bbz@. Description: Babalia Creole Arabic. Deprecated. Preferred value: bbz.
  | ExtBfi -- ^ @bfi@. Description: British Sign Language.
  | ExtBfk -- ^ @bfk@. Description: Ban Khor Sign Language.
  | ExtBjn -- ^ @bjn@. Description: Banjar.
  | ExtBog -- ^ @bog@. Description: Bamako Sign Language.
  | ExtBqn -- ^ @bqn@. Description: Bulgarian Sign Language.
  | ExtBqy -- ^ @bqy@. Description: Bengkala Sign Language.
  | ExtBtj -- ^ @btj@. Description: Bacanese Malay.
  | ExtBve -- ^ @bve@. Description: Berau Malay.
  | ExtBvl -- ^ @bvl@. Description: Bolivian Sign Language.
  | ExtBvu -- ^ @bvu@. Description: Bukit Malay.
  | ExtBzs -- ^ @bzs@. Description: Brazilian Sign Language.
  | ExtCdo -- ^ @cdo@. Description: Min Dong Chinese.
  | ExtCds -- ^ @cds@. Description: Chadian Sign Language.
  | ExtCjy -- ^ @cjy@. Description: Jinyu Chinese.
  | ExtCmn -- ^ @cmn@. Description: Mandarin Chinese.
  | ExtCnp -- ^ @cnp@. Description: Northern Ping Chinese; Northern Pinghua.
  | ExtCoa -- ^ @coa@. Description: Cocos Islands Malay.
  | ExtCpx -- ^ @cpx@. Description: Pu-Xian Chinese.
  | ExtCsc -- ^ @csc@. Description: Catalan Sign Language; Lengua de señas catalana; Llengua de Signes Catalana.
  | ExtCsd -- ^ @csd@. Description: Chiangmai Sign Language.
  | ExtCse -- ^ @cse@. Description: Czech Sign Language.
  | ExtCsf -- ^ @csf@. Description: Cuba Sign Language.
  | ExtCsg -- ^ @csg@. Description: Chilean Sign Language.
  | ExtCsl -- ^ @csl@. Description: Chinese Sign Language.
  | ExtCsn -- ^ @csn@. Description: Colombian Sign Language.
  | ExtCsp -- ^ @csp@. Description: Southern Ping Chinese; Southern Pinghua.
  | ExtCsq -- ^ @csq@. Description: Croatia Sign Language.
  | ExtCsr -- ^ @csr@. Description: Costa Rican Sign Language.
  | ExtCsx -- ^ @csx@. Description: Cambodian Sign Language.
  | ExtCzh -- ^ @czh@. Description: Huizhou Chinese.
  | ExtCzo -- ^ @czo@. Description: Min Zhong Chinese.
  | ExtDoq -- ^ @doq@. Description: Dominican Sign Language.
  | ExtDse -- ^ @dse@. Description: Dutch Sign Language.
  | ExtDsl -- ^ @dsl@. Description: Danish Sign Language.
  | ExtDup -- ^ @dup@. Description: Duano.
  | ExtEcs -- ^ @ecs@. Description: Ecuadorian Sign Language.
  | ExtEhs -- ^ @ehs@. Description: Miyakubo Sign Language.
  | ExtEsl -- ^ @esl@. Description: Egypt Sign Language.
  | ExtEsn -- ^ @esn@. Description: Salvadoran Sign Language.
  | ExtEso -- ^ @eso@. Description: Estonian Sign Language.
  | ExtEth -- ^ @eth@. Description: Ethiopian Sign Language.
  | ExtFcs -- ^ @fcs@. Description: Quebec Sign Language.
  | ExtFse -- ^ @fse@. Description: Finnish Sign Language.
  | ExtFsl -- ^ @fsl@. Description: French Sign Language.
  | ExtFss -- ^ @fss@. Description: Finland-Swedish Sign Language; finlandssvenskt teckenspråk; suomenruotsalainen viittomakieli.
  | ExtGan -- ^ @gan@. Description: Gan Chinese.
  | ExtGds -- ^ @gds@. Description: Ghandruk Sign Language.
  | ExtGom -- ^ @gom@. Description: Goan Konkani.
  | ExtGse -- ^ @gse@. Description: Ghanaian Sign Language.
  | ExtGsg -- ^ @gsg@. Description: German Sign Language.
  | ExtGsm -- ^ @gsm@. Description: Guatemalan Sign Language.
  | ExtGss -- ^ @gss@. Description: Greek Sign Language.
  | ExtGus -- ^ @gus@. Description: Guinean Sign Language.
  | ExtHab -- ^ @hab@. Description: Hanoi Sign Language.
  | ExtHaf -- ^ @haf@. Description: Haiphong Sign Language.
  | ExtHak -- ^ @hak@. Description: Hakka Chinese.
  | ExtHds -- ^ @hds@. Description: Honduras Sign Language.
  | ExtHji -- ^ @hji@. Description: Haji.
  | ExtHks -- ^ @hks@. Description: Hong Kong Sign Language; Heung Kong Sau Yue.
  | ExtHos -- ^ @hos@. Description: Ho Chi Minh City Sign Language.
  | ExtHps -- ^ @hps@. Description: Hawai\'i Sign Language (HSL); Hawai\'i Pidgin Sign Language.
  | ExtHsh -- ^ @hsh@. Description: Hungarian Sign Language.
  | ExtHsl -- ^ @hsl@. Description: Hausa Sign Language.
  | ExtHsn -- ^ @hsn@. Description: Xiang Chinese.
  | ExtIcl -- ^ @icl@. Description: Icelandic Sign Language.
  | ExtIks -- ^ @iks@. Description: Inuit Sign Language.
  | ExtIls -- ^ @ils@. Description: International Sign.
  | ExtInl -- ^ @inl@. Description: Indonesian Sign Language.
  | ExtIns -- ^ @ins@. Description: Indian Sign Language.
  | ExtIse -- ^ @ise@. Description: Italian Sign Language.
  | ExtIsg -- ^ @isg@. Description: Irish Sign Language.
  | ExtIsr -- ^ @isr@. Description: Israeli Sign Language.
  | ExtJak -- ^ @jak@. Description: Jakun.
  | ExtJax -- ^ @jax@. Description: Jambi Malay.
  | ExtJcs -- ^ @jcs@. Description: Jamaican Country Sign Language.
  | ExtJhs -- ^ @jhs@. Description: Jhankot Sign Language.
  | ExtJks -- ^ @jks@. Description: Amami Koniya Sign Language.
  | ExtJls -- ^ @jls@. Description: Jamaican Sign Language.
  | ExtJos -- ^ @jos@. Description: Jordanian Sign Language.
  | ExtJsl -- ^ @jsl@. Description: Japanese Sign Language.
  | ExtJus -- ^ @jus@. Description: Jumla Sign Language.
  | ExtKgi -- ^ @kgi@. Description: Selangor Sign Language.
  | ExtKnn -- ^ @knn@. Description: Konkani (individual language).
  | ExtKvb -- ^ @kvb@. Description: Kubu.
  | ExtKvk -- ^ @kvk@. Description: Korean Sign Language.
  | ExtKvr -- ^ @kvr@. Description: Kerinci.
  | ExtKxd -- ^ @kxd@. Description: Brunei.
  | ExtLbs -- ^ @lbs@. Description: Libyan Sign Language.
  | ExtLce -- ^ @lce@. Description: Loncong; Sekak.
  | ExtLcf -- ^ @lcf@. Description: Lubu.
  | ExtLiw -- ^ @liw@. Description: Col.
  | ExtLls -- ^ @lls@. Description: Lithuanian Sign Language.
  | ExtLsb -- ^ @lsb@. Description: Burundian Sign Language; Langue des Signes Burundaise.
  | ExtLsg -- ^ @lsg@. Description: Lyons Sign Language. Deprecated. Preferred value: lsg.
  | ExtLsl -- ^ @lsl@. Description: Latvian Sign Language.
  | ExtLsn -- ^ @lsn@. Description: Tibetan Sign Language.
  | ExtLso -- ^ @lso@. Description: Laos Sign Language.
  | ExtLsp -- ^ @lsp@. Description: Panamanian Sign Language; Lengua de Señas Panameñas.
  | ExtLst -- ^ @lst@. Description: Trinidad and Tobago Sign Language.
  | ExtLsv -- ^ @lsv@. Description: Sivia Sign Language.
  | ExtLsy -- ^ @lsy@. Description: Mauritian Sign Language.
  | ExtLtg -- ^ @ltg@. Description: Latgalian.
  | ExtLvs -- ^ @lvs@. Description: Standard Latvian.
  | ExtLws -- ^ @lws@. Description: Malawian Sign Language.
  | ExtLzh -- ^ @lzh@. Description: Literary Chinese.
  | ExtMax -- ^ @max@. Description: North Moluccan Malay.
  | ExtMdl -- ^ @mdl@. Description: Maltese Sign Language.
  | ExtMeo -- ^ @meo@. Description: Kedah Malay.
  | ExtMfa -- ^ @mfa@. Description: Pattani Malay.
  | ExtMfb -- ^ @mfb@. Description: Bangka.
  | ExtMfs -- ^ @mfs@. Description: Mexican Sign Language.
  | ExtMin -- ^ @min@. Description: Minangkabau.
  | ExtMnp -- ^ @mnp@. Description: Min Bei Chinese.
  | ExtMqg -- ^ @mqg@. Description: Kota Bangun Kutai Malay.
  | ExtMre -- ^ @mre@. Description: Martha\'s Vineyard Sign Language.
  | ExtMsd -- ^ @msd@. Description: Yucatec Maya Sign Language.
  | ExtMsi -- ^ @msi@. Description: Sabah Malay.
  | ExtMsr -- ^ @msr@. Description: Mongolian Sign Language.
  | ExtMui -- ^ @mui@. Description: Musi.
  | ExtMzc -- ^ @mzc@. Description: Madagascar Sign Language.
  | ExtMzg -- ^ @mzg@. Description: Monastic Sign Language.
  | ExtMzy -- ^ @mzy@. Description: Mozambican Sign Language.
  | ExtNan -- ^ @nan@. Description: Min Nan Chinese.
  | ExtNbs -- ^ @nbs@. Description: Namibian Sign Language.
  | ExtNcs -- ^ @ncs@. Description: Nicaraguan Sign Language.
  | ExtNsi -- ^ @nsi@. Description: Nigerian Sign Language.
  | ExtNsl -- ^ @nsl@. Description: Norwegian Sign Language.
  | ExtNsp -- ^ @nsp@. Description: Nepalese Sign Language.
  | ExtNsr -- ^ @nsr@. Description: Maritime Sign Language.
  | ExtNzs -- ^ @nzs@. Description: New Zealand Sign Language.
  | ExtOkl -- ^ @okl@. Description: Old Kentish Sign Language.
  | ExtOrn -- ^ @orn@. Description: Orang Kanaq.
  | ExtOrs -- ^ @ors@. Description: Orang Seletar.
  | ExtPel -- ^ @pel@. Description: Pekal.
  | ExtPga -- ^ @pga@. Description: Sudanese Creole Arabic.
  | ExtPgz -- ^ @pgz@. Description: Papua New Guinean Sign Language.
  | ExtPks -- ^ @pks@. Description: Pakistan Sign Language.
  | ExtPrl -- ^ @prl@. Description: Peruvian Sign Language.
  | ExtPrz -- ^ @prz@. Description: Providencia Sign Language.
  | ExtPsc -- ^ @psc@. Description: Persian Sign Language.
  | ExtPsd -- ^ @psd@. Description: Plains Indian Sign Language.
  | ExtPse -- ^ @pse@. Description: Central Malay.
  | ExtPsg -- ^ @psg@. Description: Penang Sign Language.
  | ExtPsl -- ^ @psl@. Description: Puerto Rican Sign Language.
  | ExtPso -- ^ @pso@. Description: Polish Sign Language.
  | ExtPsp -- ^ @psp@. Description: Philippine Sign Language.
  | ExtPsr -- ^ @psr@. Description: Portuguese Sign Language.
  | ExtPys -- ^ @pys@. Description: Paraguayan Sign Language; Lengua de Señas del Paraguay.
  | ExtRms -- ^ @rms@. Description: Romanian Sign Language.
  | ExtRsi -- ^ @rsi@. Description: Rennellese Sign Language. Deprecated. Preferred value: rsi.
  | ExtRsl -- ^ @rsl@. Description: Russian Sign Language.
  | ExtRsm -- ^ @rsm@. Description: Miriwoong Sign Language.
  | ExtSdl -- ^ @sdl@. Description: Saudi Arabian Sign Language.
  | ExtSfb -- ^ @sfb@. Description: Langue des signes de Belgique Francophone; French Belgian Sign Language.
  | ExtSfs -- ^ @sfs@. Description: South African Sign Language.
  | ExtSgg -- ^ @sgg@. Description: Swiss-German Sign Language.
  | ExtSgx -- ^ @sgx@. Description: Sierra Leone Sign Language.
  | ExtShu -- ^ @shu@. Description: Chadian Arabic.
  | ExtSlf -- ^ @slf@. Description: Swiss-Italian Sign Language.
  | ExtSls -- ^ @sls@. Description: Singapore Sign Language.
  | ExtSqk -- ^ @sqk@. Description: Albanian Sign Language.
  | ExtSqs -- ^ @sqs@. Description: Sri Lankan Sign Language.
  | ExtSqx -- ^ @sqx@. Description: Kufr Qassem Sign Language (KQSL).
  | ExtSsh -- ^ @ssh@. Description: Shihhi Arabic.
  | ExtSsp -- ^ @ssp@. Description: Spanish Sign Language.
  | ExtSsr -- ^ @ssr@. Description: Swiss-French Sign Language.
  | ExtSvk -- ^ @svk@. Description: Slovakian Sign Language.
  | ExtSwc -- ^ @swc@. Description: Congo Swahili.
  | ExtSwh -- ^ @swh@. Description: Swahili (individual language); Kiswahili.
  | ExtSwl -- ^ @swl@. Description: Swedish Sign Language.
  | ExtSyy -- ^ @syy@. Description: Al-Sayyid Bedouin Sign Language.
  | ExtSzs -- ^ @szs@. Description: Solomon Islands Sign Language.
  | ExtTmw -- ^ @tmw@. Description: Temuan.
  | ExtTse -- ^ @tse@. Description: Tunisian Sign Language.
  | ExtTsm -- ^ @tsm@. Description: Turkish Sign Language; Türk İşaret Dili.
  | ExtTsq -- ^ @tsq@. Description: Thai Sign Language.
  | ExtTss -- ^ @tss@. Description: Taiwan Sign Language.
  | ExtTsy -- ^ @tsy@. Description: Tebul Sign Language.
  | ExtTza -- ^ @tza@. Description: Tanzanian Sign Language.
  | ExtUgn -- ^ @ugn@. Description: Ugandan Sign Language.
  | ExtUgy -- ^ @ugy@. Description: Uruguayan Sign Language.
  | ExtUkl -- ^ @ukl@. Description: Ukrainian Sign Language.
  | ExtUks -- ^ @uks@. Description: Urubú-Kaapor Sign Language; Kaapor Sign Language.
  | ExtUrk -- ^ @urk@. Description: Urak Lawoi\'.
  | ExtUzn -- ^ @uzn@. Description: Northern Uzbek.
  | ExtUzs -- ^ @uzs@. Description: Southern Uzbek.
  | ExtVgt -- ^ @vgt@. Description: Vlaamse Gebarentaal; Flemish Sign Language.
  | ExtVkk -- ^ @vkk@. Description: Kaur.
  | ExtVkt -- ^ @vkt@. Description: Tenggarong Kutai Malay.
  | ExtVsi -- ^ @vsi@. Description: Moldova Sign Language.
  | ExtVsl -- ^ @vsl@. Description: Venezuelan Sign Language.
  | ExtVsv -- ^ @vsv@. Description: Valencian Sign Language; Llengua de signes valenciana.
  | ExtWbs -- ^ @wbs@. Description: West Bengal Sign Language.
  | ExtWuu -- ^ @wuu@. Description: Wu Chinese.
  | ExtXki -- ^ @xki@. Description: Kenyan Sign Language.
  | ExtXml -- ^ @xml@. Description: Malaysian Sign Language.
  | ExtXmm -- ^ @xmm@. Description: Manado Malay.
  | ExtXms -- ^ @xms@. Description: Moroccan Sign Language.
  | ExtYds -- ^ @yds@. Description: Yiddish Sign Language. Deprecated. Preferred value: yds.
  | ExtYgs -- ^ @ygs@. Description: Yolŋu Sign Language.
  | ExtYhs -- ^ @yhs@. Description: Yan-nhaŋu Sign Language.
  | ExtYsl -- ^ @ysl@. Description: Yugoslavian Sign Language.
  | ExtYsm -- ^ @ysm@. Description: Myanmar Sign Language.
  | ExtYue -- ^ @yue@. Description: Yue Chinese; Cantonese.
  | ExtZib -- ^ @zib@. Description: Zimbabwe Sign Language.
  | ExtZlm -- ^ @zlm@. Description: Malay (individual language).
  | ExtZmi -- ^ @zmi@. Description: Negeri Sembilan Malay.
  | ExtZsl -- ^ @zsl@. Description: Zambian Sign Language.
  | ExtZsm -- ^ @zsm@. Description: Standard Malay.

  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData Extlang where
  rnf a = seq a ()

instance Hashable Extlang where
  hashWithSalt = hashUsing fromEnum
