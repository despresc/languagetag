-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}

module Text.LanguageTag.Internal.BCP47.Script where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..), hashUsing)
import Text.LanguageTag.Internal.BCP47.Syntax (Subtag(..), unwrapSubtag)
import qualified Data.HashMap.Strict as HM

-- | The BCP47 script tags as of 2021-02-23.
data Script
  = Adlm -- ^ @Adlm@. Description: Adlam.
  | Afak -- ^ @Afak@. Description: Afaka.
  | Aghb -- ^ @Aghb@. Description: Caucasian Albanian.
  | Ahom -- ^ @Ahom@. Description: Ahom; Tai Ahom.
  | Arab -- ^ @Arab@. Description: Arabic.
  | Aran -- ^ @Aran@. Description: Arabic (Nastaliq variant).
  | Armi -- ^ @Armi@. Description: Imperial Aramaic.
  | Armn -- ^ @Armn@. Description: Armenian.
  | Avst -- ^ @Avst@. Description: Avestan.
  | Bali -- ^ @Bali@. Description: Balinese.
  | Bamu -- ^ @Bamu@. Description: Bamum.
  | Bass -- ^ @Bass@. Description: Bassa Vah.
  | Batk -- ^ @Batk@. Description: Batak.
  | Beng -- ^ @Beng@. Description: Bengali; Bangla.
  | Bhks -- ^ @Bhks@. Description: Bhaiksuki.
  | Blis -- ^ @Blis@. Description: Blissymbols.
  | Bopo -- ^ @Bopo@. Description: Bopomofo.
  | Brah -- ^ @Brah@. Description: Brahmi.
  | Brai -- ^ @Brai@. Description: Braille.
  | Bugi -- ^ @Bugi@. Description: Buginese.
  | Buhd -- ^ @Buhd@. Description: Buhid.
  | Cakm -- ^ @Cakm@. Description: Chakma.
  | Cans -- ^ @Cans@. Description: Unified Canadian Aboriginal Syllabics.
  | Cari -- ^ @Cari@. Description: Carian.
  | Cham -- ^ @Cham@. Description: Cham.
  | Cher -- ^ @Cher@. Description: Cherokee.
  | Chrs -- ^ @Chrs@. Description: Chorasmian.
  | Cirt -- ^ @Cirt@. Description: Cirth.
  | Copt -- ^ @Copt@. Description: Coptic.
  | Cpmn -- ^ @Cpmn@. Description: Cypro-Minoan.
  | Cprt -- ^ @Cprt@. Description: Cypriot syllabary.
  | Cyrl -- ^ @Cyrl@. Description: Cyrillic.
  | Cyrs -- ^ @Cyrs@. Description: Cyrillic (Old Church Slavonic variant).
  | Deva -- ^ @Deva@. Description: Devanagari; Nagari.
  | Diak -- ^ @Diak@. Description: Dives Akuru.
  | Dogr -- ^ @Dogr@. Description: Dogra.
  | Dsrt -- ^ @Dsrt@. Description: Deseret; Mormon.
  | Dupl -- ^ @Dupl@. Description: Duployan shorthand; Duployan stenography.
  | Egyd -- ^ @Egyd@. Description: Egyptian demotic.
  | Egyh -- ^ @Egyh@. Description: Egyptian hieratic.
  | Egyp -- ^ @Egyp@. Description: Egyptian hieroglyphs.
  | Elba -- ^ @Elba@. Description: Elbasan.
  | Elym -- ^ @Elym@. Description: Elymaic.
  | Ethi -- ^ @Ethi@. Description: Ethiopic; Geʻez; Ge\'ez.
  | Geok -- ^ @Geok@. Description: Khutsuri (Asomtavruli and Nuskhuri).
  | Geor -- ^ @Geor@. Description: Georgian (Mkhedruli and Mtavruli).
  | Glag -- ^ @Glag@. Description: Glagolitic.
  | Gong -- ^ @Gong@. Description: Gunjala Gondi.
  | Gonm -- ^ @Gonm@. Description: Masaram Gondi.
  | Goth -- ^ @Goth@. Description: Gothic.
  | Gran -- ^ @Gran@. Description: Grantha.
  | Grek -- ^ @Grek@. Description: Greek.
  | Gujr -- ^ @Gujr@. Description: Gujarati.
  | Guru -- ^ @Guru@. Description: Gurmukhi.
  | Hanb -- ^ @Hanb@. Description: Han with Bopomofo (alias for Han + Bopomofo).
  | Hang -- ^ @Hang@. Description: Hangul; Hangŭl; Hangeul.
  | Hani -- ^ @Hani@. Description: Han; Hanzi; Kanji; Hanja.
  | Hano -- ^ @Hano@. Description: Hanunoo; Hanunóo.
  | Hans -- ^ @Hans@. Description: Han (Simplified variant).
  | Hant -- ^ @Hant@. Description: Han (Traditional variant).
  | Hatr -- ^ @Hatr@. Description: Hatran.
  | Hebr -- ^ @Hebr@. Description: Hebrew.
  | Hira -- ^ @Hira@. Description: Hiragana.
  | Hluw -- ^ @Hluw@. Description: Anatolian Hieroglyphs; Luwian Hieroglyphs; Hittite Hieroglyphs.
  | Hmng -- ^ @Hmng@. Description: Pahawh Hmong.
  | Hmnp -- ^ @Hmnp@. Description: Nyiakeng Puachue Hmong.
  | Hrkt -- ^ @Hrkt@. Description: Japanese syllabaries (alias for Hiragana + Katakana).
  | Hung -- ^ @Hung@. Description: Old Hungarian; Hungarian Runic.
  | Inds -- ^ @Inds@. Description: Indus; Harappan.
  | Ital -- ^ @Ital@. Description: Old Italic (Etruscan, Oscan, etc.).
  | Jamo -- ^ @Jamo@. Description: Jamo (alias for Jamo subset of Hangul).
  | Java -- ^ @Java@. Description: Javanese.
  | Jpan -- ^ @Jpan@. Description: Japanese (alias for Han + Hiragana + Katakana).
  | Jurc -- ^ @Jurc@. Description: Jurchen.
  | Kali -- ^ @Kali@. Description: Kayah Li.
  | Kana -- ^ @Kana@. Description: Katakana.
  | Khar -- ^ @Khar@. Description: Kharoshthi.
  | Khmr -- ^ @Khmr@. Description: Khmer.
  | Khoj -- ^ @Khoj@. Description: Khojki.
  | Kitl -- ^ @Kitl@. Description: Khitan large script.
  | Kits -- ^ @Kits@. Description: Khitan small script.
  | Knda -- ^ @Knda@. Description: Kannada.
  | Kore -- ^ @Kore@. Description: Korean (alias for Hangul + Han).
  | Kpel -- ^ @Kpel@. Description: Kpelle.
  | Kthi -- ^ @Kthi@. Description: Kaithi.
  | Lana -- ^ @Lana@. Description: Tai Tham; Lanna.
  | Laoo -- ^ @Laoo@. Description: Lao.
  | Latf -- ^ @Latf@. Description: Latin (Fraktur variant).
  | Latg -- ^ @Latg@. Description: Latin (Gaelic variant).
  | Latn -- ^ @Latn@. Description: Latin.
  | Leke -- ^ @Leke@. Description: Leke.
  | Lepc -- ^ @Lepc@. Description: Lepcha; Róng.
  | Limb -- ^ @Limb@. Description: Limbu.
  | Lina -- ^ @Lina@. Description: Linear A.
  | Linb -- ^ @Linb@. Description: Linear B.
  | Lisu -- ^ @Lisu@. Description: Lisu; Fraser.
  | Loma -- ^ @Loma@. Description: Loma.
  | Lyci -- ^ @Lyci@. Description: Lycian.
  | Lydi -- ^ @Lydi@. Description: Lydian.
  | Mahj -- ^ @Mahj@. Description: Mahajani.
  | Maka -- ^ @Maka@. Description: Makasar.
  | Mand -- ^ @Mand@. Description: Mandaic; Mandaean.
  | Mani -- ^ @Mani@. Description: Manichaean.
  | Marc -- ^ @Marc@. Description: Marchen.
  | Maya -- ^ @Maya@. Description: Mayan hieroglyphs.
  | Medf -- ^ @Medf@. Description: Medefaidrin; Oberi Okaime; Oberi Ɔkaimɛ.
  | Mend -- ^ @Mend@. Description: Mende Kikakui.
  | Merc -- ^ @Merc@. Description: Meroitic Cursive.
  | Mero -- ^ @Mero@. Description: Meroitic Hieroglyphs.
  | Mlym -- ^ @Mlym@. Description: Malayalam.
  | Modi -- ^ @Modi@. Description: Modi; Moḍī.
  | Mong -- ^ @Mong@. Description: Mongolian.
  | Moon -- ^ @Moon@. Description: Moon; Moon code; Moon script; Moon type.
  | Mroo -- ^ @Mroo@. Description: Mro; Mru.
  | Mtei -- ^ @Mtei@. Description: Meitei Mayek; Meithei; Meetei.
  | Mult -- ^ @Mult@. Description: Multani.
  | Mymr -- ^ @Mymr@. Description: Myanmar; Burmese.
  | Nand -- ^ @Nand@. Description: Nandinagari.
  | Narb -- ^ @Narb@. Description: Old North Arabian; Ancient North Arabian.
  | Nbat -- ^ @Nbat@. Description: Nabataean.
  | Newa -- ^ @Newa@. Description: Newa; Newar; Newari; Nepāla lipi.
  | Nkdb -- ^ @Nkdb@. Description: Naxi Dongba; na²¹ɕi³³ to³³ba²¹; Nakhi Tomba.
  | Nkgb -- ^ @Nkgb@. Description: Naxi Geba; na²¹ɕi³³ gʌ²¹ba²¹; \'Na-\'Khi ²Ggŏ-¹baw; Nakhi Geba.
  | Nkoo -- ^ @Nkoo@. Description: N’Ko; N\'Ko.
  | Nshu -- ^ @Nshu@. Description: Nüshu.
  | Ogam -- ^ @Ogam@. Description: Ogham.
  | Olck -- ^ @Olck@. Description: Ol Chiki; Ol Cemet\'; Ol; Santali.
  | Orkh -- ^ @Orkh@. Description: Old Turkic; Orkhon Runic.
  | Orya -- ^ @Orya@. Description: Oriya; Odia.
  | Osge -- ^ @Osge@. Description: Osage.
  | Osma -- ^ @Osma@. Description: Osmanya.
  | Ougr -- ^ @Ougr@. Description: Old Uyghur.
  | Palm -- ^ @Palm@. Description: Palmyrene.
  | Pauc -- ^ @Pauc@. Description: Pau Cin Hau.
  | Pcun -- ^ @Pcun@. Description: Proto-Cuneiform.
  | Pelm -- ^ @Pelm@. Description: Proto-Elamite.
  | Perm -- ^ @Perm@. Description: Old Permic.
  | Phag -- ^ @Phag@. Description: Phags-pa.
  | Phli -- ^ @Phli@. Description: Inscriptional Pahlavi.
  | Phlp -- ^ @Phlp@. Description: Psalter Pahlavi.
  | Phlv -- ^ @Phlv@. Description: Book Pahlavi.
  | Phnx -- ^ @Phnx@. Description: Phoenician.
  | Piqd -- ^ @Piqd@. Description: Klingon (KLI pIqaD).
  | Plrd -- ^ @Plrd@. Description: Miao; Pollard.
  | Prti -- ^ @Prti@. Description: Inscriptional Parthian.
  | Psin -- ^ @Psin@. Description: Proto-Sinaitic.
  | Qaaa -- ^ @Qaaa@. Description: Private use.
  | Qaab -- ^ @Qaab@. Description: Private use.
  | Qaac -- ^ @Qaac@. Description: Private use.
  | Qaad -- ^ @Qaad@. Description: Private use.
  | Qaae -- ^ @Qaae@. Description: Private use.
  | Qaaf -- ^ @Qaaf@. Description: Private use.
  | Qaag -- ^ @Qaag@. Description: Private use.
  | Qaah -- ^ @Qaah@. Description: Private use.
  | Qaai -- ^ @Qaai@. Description: Private use.
  | Qaaj -- ^ @Qaaj@. Description: Private use.
  | Qaak -- ^ @Qaak@. Description: Private use.
  | Qaal -- ^ @Qaal@. Description: Private use.
  | Qaam -- ^ @Qaam@. Description: Private use.
  | Qaan -- ^ @Qaan@. Description: Private use.
  | Qaao -- ^ @Qaao@. Description: Private use.
  | Qaap -- ^ @Qaap@. Description: Private use.
  | Qaaq -- ^ @Qaaq@. Description: Private use.
  | Qaar -- ^ @Qaar@. Description: Private use.
  | Qaas -- ^ @Qaas@. Description: Private use.
  | Qaat -- ^ @Qaat@. Description: Private use.
  | Qaau -- ^ @Qaau@. Description: Private use.
  | Qaav -- ^ @Qaav@. Description: Private use.
  | Qaaw -- ^ @Qaaw@. Description: Private use.
  | Qaax -- ^ @Qaax@. Description: Private use.
  | Qaay -- ^ @Qaay@. Description: Private use.
  | Qaaz -- ^ @Qaaz@. Description: Private use.
  | Qaba -- ^ @Qaba@. Description: Private use.
  | Qabb -- ^ @Qabb@. Description: Private use.
  | Qabc -- ^ @Qabc@. Description: Private use.
  | Qabd -- ^ @Qabd@. Description: Private use.
  | Qabe -- ^ @Qabe@. Description: Private use.
  | Qabf -- ^ @Qabf@. Description: Private use.
  | Qabg -- ^ @Qabg@. Description: Private use.
  | Qabh -- ^ @Qabh@. Description: Private use.
  | Qabi -- ^ @Qabi@. Description: Private use.
  | Qabj -- ^ @Qabj@. Description: Private use.
  | Qabk -- ^ @Qabk@. Description: Private use.
  | Qabl -- ^ @Qabl@. Description: Private use.
  | Qabm -- ^ @Qabm@. Description: Private use.
  | Qabn -- ^ @Qabn@. Description: Private use.
  | Qabo -- ^ @Qabo@. Description: Private use.
  | Qabp -- ^ @Qabp@. Description: Private use.
  | Qabq -- ^ @Qabq@. Description: Private use.
  | Qabr -- ^ @Qabr@. Description: Private use.
  | Qabs -- ^ @Qabs@. Description: Private use.
  | Qabt -- ^ @Qabt@. Description: Private use.
  | Qabu -- ^ @Qabu@. Description: Private use.
  | Qabv -- ^ @Qabv@. Description: Private use.
  | Qabw -- ^ @Qabw@. Description: Private use.
  | Qabx -- ^ @Qabx@. Description: Private use.
  | Ranj -- ^ @Ranj@. Description: Ranjana.
  | Rjng -- ^ @Rjng@. Description: Rejang; Redjang; Kaganga.
  | Rohg -- ^ @Rohg@. Description: Hanifi Rohingya.
  | Roro -- ^ @Roro@. Description: Rongorongo.
  | Runr -- ^ @Runr@. Description: Runic.
  | Samr -- ^ @Samr@. Description: Samaritan.
  | Sara -- ^ @Sara@. Description: Sarati.
  | Sarb -- ^ @Sarb@. Description: Old South Arabian.
  | Saur -- ^ @Saur@. Description: Saurashtra.
  | Sgnw -- ^ @Sgnw@. Description: SignWriting.
  | Shaw -- ^ @Shaw@. Description: Shavian; Shaw.
  | Shrd -- ^ @Shrd@. Description: Sharada; Śāradā.
  | Shui -- ^ @Shui@. Description: Shuishu.
  | Sidd -- ^ @Sidd@. Description: Siddham; Siddhaṃ; Siddhamātṛkā.
  | Sind -- ^ @Sind@. Description: Khudawadi; Sindhi.
  | Sinh -- ^ @Sinh@. Description: Sinhala.
  | Sogd -- ^ @Sogd@. Description: Sogdian.
  | Sogo -- ^ @Sogo@. Description: Old Sogdian.
  | Sora -- ^ @Sora@. Description: Sora Sompeng.
  | Soyo -- ^ @Soyo@. Description: Soyombo.
  | Sund -- ^ @Sund@. Description: Sundanese.
  | Sylo -- ^ @Sylo@. Description: Syloti Nagri.
  | Syrc -- ^ @Syrc@. Description: Syriac.
  | Syre -- ^ @Syre@. Description: Syriac (Estrangelo variant).
  | Syrj -- ^ @Syrj@. Description: Syriac (Western variant).
  | Syrn -- ^ @Syrn@. Description: Syriac (Eastern variant).
  | Tagb -- ^ @Tagb@. Description: Tagbanwa.
  | Takr -- ^ @Takr@. Description: Takri; Ṭākrī; Ṭāṅkrī.
  | Tale -- ^ @Tale@. Description: Tai Le.
  | Talu -- ^ @Talu@. Description: New Tai Lue.
  | Taml -- ^ @Taml@. Description: Tamil.
  | Tang -- ^ @Tang@. Description: Tangut.
  | Tavt -- ^ @Tavt@. Description: Tai Viet.
  | Telu -- ^ @Telu@. Description: Telugu.
  | Teng -- ^ @Teng@. Description: Tengwar.
  | Tfng -- ^ @Tfng@. Description: Tifinagh; Berber.
  | Tglg -- ^ @Tglg@. Description: Tagalog; Baybayin; Alibata.
  | Thaa -- ^ @Thaa@. Description: Thaana.
  | Thai -- ^ @Thai@. Description: Thai.
  | Tibt -- ^ @Tibt@. Description: Tibetan.
  | Tirh -- ^ @Tirh@. Description: Tirhuta.
  | Toto -- ^ @Toto@. Description: Toto.
  | Ugar -- ^ @Ugar@. Description: Ugaritic.
  | Vaii -- ^ @Vaii@. Description: Vai.
  | Visp -- ^ @Visp@. Description: Visible Speech.
  | Wara -- ^ @Wara@. Description: Warang Citi; Varang Kshiti.
  | Wcho -- ^ @Wcho@. Description: Wancho.
  | Wole -- ^ @Wole@. Description: Woleai.
  | Xpeo -- ^ @Xpeo@. Description: Old Persian.
  | Xsux -- ^ @Xsux@. Description: Sumero-Akkadian cuneiform.
  | Yezi -- ^ @Yezi@. Description: Yezidi.
  | Yiii -- ^ @Yiii@. Description: Yi.
  | Zanb -- ^ @Zanb@. Description: Zanabazar Square; Zanabazarin Dörböljin Useg; Xewtee Dörböljin Bicig; Horizontal Square Script.
  | Zinh -- ^ @Zinh@. Description: Code for inherited script.
  | Zmth -- ^ @Zmth@. Description: Mathematical notation.
  | Zsye -- ^ @Zsye@. Description: Symbols (Emoji variant).
  | Zsym -- ^ @Zsym@. Description: Symbols.
  | Zxxx -- ^ @Zxxx@. Description: Code for unwritten documents.
  | Zyyy -- ^ @Zyyy@. Description: Code for undetermined script.
  | Zzzz -- ^ @Zzzz@. Description: Code for uncoded script.

  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData Script where
  rnf a = seq a ()

instance Hashable Script where
  hashWithSalt = hashUsing fromEnum

parseScript :: Subtag -> Maybe Script
parseScript = flip HM.lookup table . unwrapSubtag
  where
    table = HM.fromList
      [ (14092720702511644676, Adlm)
      , (14094875607863132164, Afak)
      , (14096062461945839620, Aghb)
      , (14097250690418081796, Ahom)
      , (14108385788269953028, Arab)
      , (14108386612903673860, Aran)
      , (14108491822422556676, Armi)
      , (14108492166019940356, Armn)
      , (14113048954522304516, Avst)
      , (14233457915989065732, Bali)
      , (14233467536715808772, Bamu)
      , (14233520175834988548, Bass)
      , (14233528422172196868, Batk)
      , (14237978970363527172, Beng)
      , (14241331106438709252, Bhks)
      , (14245817113880035332, Blis)
      , (14249256111373811716, Bopo)
      , (14252501388662669316, Brah)
      , (14252501457382146052, Brai)
      , (14255931933660807172, Bugi)
      , (14255940386156445700, Buhd)
      , (14377564582849806340, Cakm)
      , (14377591383445733380, Cans)
      , (14377625880623054852, Cari)
      , (14385357921267482628, Cham)
      , (14385393449236955140, Cher)
      , (14385507867165720580, Chrs)
      , (14386633835792039940, Cirt)
      , (14393371643047051268, Copt)
      , (14394470742357966852, Cpmn)
      , (14394515135139938308, Cprt)
      , (14404647684545708036, Cyrl)
      , (14404648165582045188, Cyrs)
      , (14526279302942556164, Deva)
      , (14530598871811227652, Diak)
      , (14537407528846753796, Dogr)
      , (14542008022936322052, Dsrt)
      , (14544241680808148996, Dupl)
      , (14672672885269594116, Egyd)
      , (14672673160147501060, Egyh)
      , (14672673709903314948, Egyp)
      , (14678099868505866244, Elba)
      , (14678303003279097860, Elym)
      , (14687160394074554372, Ethi)
      , (14958563981713735684, Geok)
      , (14958564462750072836, Geor)
      , (14966321860881416196, Glag)
      , (14969813909811232772, Gong)
      , (14969814322128093188, Gonm)
      , (14969866755088842756, Goth)
      , (14973077741358809092, Gran)
      , (14973112719572467716, Grek)
      , (14976534880794443780, Gujr)
      , (14976605455697051652, Guru)
      , (15098166155593908228, Hanb)
      , (15098166499191291908, Hang)
      , (15098166636630245380, Hani)
      , (15098167048947105796, Hano)
      , (15098167323825012740, Hans)
      , (15098167392544489476, Hant)
      , (15098220031663669252, Hatr)
      , (15102565301616640004, Hebr)
      , (15107208470501261316, Hira)
      , (15110614070329344004, Hluw)
      , (15111677298073403396, Hmng)
      , (15111677916548694020, Hmnp)
      , (15117281302681747460, Hrkt)
      , (15120684497328144388, Hung)
      , (15256831249759600644, Inds)
      , (15263559779885252612, Ital)
      , (15386388629005795332, Jamo)
      , (15386466831770320900, Java)
      , (15403171505772691460, Jpan)
      , (15408949782974038020, Jurc)
      , (15530494608671768580, Kali)
      , (15530511651101999108, Kana)
      , (15538279769471713284, Khar)
      , (15538385322587979780, Khmr)
      , (15538402365018210308, Khoj)
      , (15539572382829117444, Kitl)
      , (15539572863865454596, Kits)
      , (15545060388960731140, Knda)
      , (15546309709047791620, Kore)
      , (15547321740781682692, Kpel)
      , (15551851522529689604, Kthi)
      , (15674626839177854980, Lana)
      , (15674636597343551492, Laoo)
      , (15674679959333371908, Latf)
      , (15674680028052848644, Latg)
      , (15674680509089185796, Latn)
      , (15679104325404065796, Leke)
      , (15679148168430223364, Lepc)
      , (15683625311059050500, Limb)
      , (15683634038432595972, Lina)
      , (15683634107152072708, Linb)
      , (15683679393287241732, Lisu)
      , (15690380641780629508, Loma)
      , (15701552229674647556, Lyci)
      , (15701561025767669764, Lydi)
      , (15818689869170868228, Mahj)
      , (15818715638974644228, Maka)
      , (15818742233412141060, Mand)
      , (15818742577009524740, Mani)
      , (15818777349064753156, Marc)
      , (15818838784276955140, Maya)
      , (15823158009548242948, Medf)
      , (15823245833039511556, Mend)
      , (15823280948692123652, Merc)
      , (15823281773325844484, Mero)
      , (15831224507885944836, Mlym)
      , (15834417214775099396, Modi)
      , (15834505038266368004, Mong)
      , (15834514315395727364, Moon)
      , (15837892083835731972, Mroo)
      , (15840055510402334724, Mtei)
      , (15841243738874576900, Mult)
      , (15845755997156016132, Mymr)
      , (15962857421487996932, Nand)
      , (15962892468421132292, Narb)
      , (15963870071697178628, Nbat)
      , (15967439979794137092, Newa)
      , (15974028322187247620, Nkdb)
      , (15974054710466314244, Nkgb)
      , (15974125972563689476, Nkoo)
      , (15983072011484135428, Nshu)
      , (16113614278270910468, Ogam)
      , (16119261232552214532, Olck)
      , (16126086794579017732, Orkh)
      , (16126209458844991492, Orya)
      , (16127177303955341316, Osge)
      , (16127229805635567620, Osma)
      , (16129429997122224132, Ougr)
      , (16251070823928954884, Palm)
      , (16251149301571387396, Pauc)
      , (16253401857299316740, Pcun)
      , (16255574423556325380, Pelm)
      , (16255627200114458628, Perm)
      , (16258854953936748548, Phag)
      , (16258951848398946308, Phli)
      , (16258952329435283460, Phlp)
      , (16258952741752143876, Phlv)
      , (16258970471377141764, Phnx)
      , (16260121385173516292, Piqd)
      , (16263507880987066372, Plrd)
      , (16270281216211550212, Prti)
      , (16271310702692532228, Psin)
      , (16395088430347845636, Qaaa)
      , (16395088499067322372, Qaab)
      , (16395088567786799108, Qaac)
      , (16395088636506275844, Qaad)
      , (16395088705225752580, Qaae)
      , (16395088773945229316, Qaaf)
      , (16395088842664706052, Qaag)
      , (16395088911384182788, Qaah)
      , (16395088980103659524, Qaai)
      , (16395089048823136260, Qaaj)
      , (16395089117542612996, Qaak)
      , (16395089186262089732, Qaal)
      , (16395089254981566468, Qaam)
      , (16395089323701043204, Qaan)
      , (16395089392420519940, Qaao)
      , (16395089461139996676, Qaap)
      , (16395089529859473412, Qaaq)
      , (16395089598578950148, Qaar)
      , (16395089667298426884, Qaas)
      , (16395089736017903620, Qaat)
      , (16395089804737380356, Qaau)
      , (16395089873456857092, Qaav)
      , (16395089942176333828, Qaaw)
      , (16395090010895810564, Qaax)
      , (16395090079615287300, Qaay)
      , (16395090148334764036, Qaaz)
      , (16395097226440867844, Qaba)
      , (16395097295160344580, Qabb)
      , (16395097363879821316, Qabc)
      , (16395097432599298052, Qabd)
      , (16395097501318774788, Qabe)
      , (16395097570038251524, Qabf)
      , (16395097638757728260, Qabg)
      , (16395097707477204996, Qabh)
      , (16395097776196681732, Qabi)
      , (16395097844916158468, Qabj)
      , (16395097913635635204, Qabk)
      , (16395097982355111940, Qabl)
      , (16395098051074588676, Qabm)
      , (16395098119794065412, Qabn)
      , (16395098188513542148, Qabo)
      , (16395098257233018884, Qabp)
      , (16395098325952495620, Qabq)
      , (16395098394671972356, Qabr)
      , (16395098463391449092, Qabs)
      , (16395098532110925828, Qabt)
      , (16395098600830402564, Qabu)
      , (16395098669549879300, Qabv)
      , (16395098738269356036, Qabw)
      , (16395098806988832772, Qabx)
      , (16539318586108280836, Ranj)
      , (16549451479111434244, Rjng)
      , (16555028202087514116, Rohg)
      , (16555116712773550084, Roro)
      , (16561837134000947204, Runr)
      , (16683425527846928388, Samr)
      , (16683468340080934916, Sara)
      , (16683468408800411652, Sarb)
      , (16683495896591106052, Saur)
      , (16690190066978390020, Sgnw)
      , (16691201617675943940, Shaw)
      , (16691349845587263492, Shrd)
      , (16691376577463713796, Shui)
      , (16692352600191795204, Sidd)
      , (16692440561122017284, Sind)
      , (16692440835999924228, Sinh)
      , (16699134387911917572, Sogd)
      , (16699135143826161668, Sogo)
      , (16699230938776731652, Sora)
      , (16699293473500561412, Soyo)
      , (16705951360004128772, Sund)
      , (16710438123359698948, Sylo)
      , (16710490075284111364, Syrc)
      , (16710490212723064836, Syre)
      , (16710490556320448516, Syrj)
      , (16710490831198355460, Syrn)
      , (16827486839853023236, Tagb)
      , (16827523123736739844, Takr)
      , (16827531026476564484, Tale)
      , (16827532125988192260, Talu)
      , (16827540303605923844, Taml)
      , (16827548756101562372, Tang)
      , (16827620018198937604, Tavt)
      , (16832035725615562756, Telu)
      , (16832052355728932868, Teng)
      , (16833178255635775492, Tfng)
      , (16834286563356573700, Tglg)
      , (16835315293923311620, Thaa)
      , (16835315843679125508, Thai)
      , (16836451295593234436, Tibt)
      , (16836591208447868932, Tirh)
      , (16843364681111306244, Toto)
      , (16978305750323429380, Ugar)
      , (17115735289227116548, Vaii)
      , (17124830930448416772, Visp)
      , (17259929092384358404, Wara)
      , (17262093893340495876, Wcho)
      , (17275639189399928836, Wole)
      , (17420819391926239236, Xpeo)
      , (17424338447610413060, Xsux)
      , (17552733986663432196, Yezi)
      , (17557088052709425156, Yiii)
      , (17692239540959313924, Zanb)
      , (17701247152530915332, Zinh)
      , (17705803528716419076, Zmth)
      , (17712602702464155652, Zsye)
      , (17712603252219969540, Zsym)
      , (17718224711575404548, Zxxx)
      , (17719359476294746116, Zyyy)
      , (17720494241014087684, Zzzz)]
scriptToSubtag :: Script -> Subtag
scriptToSubtag x = case x of
  Adlm -> Subtag 14092720702511644676
  Afak -> Subtag 14094875607863132164
  Aghb -> Subtag 14096062461945839620
  Ahom -> Subtag 14097250690418081796
  Arab -> Subtag 14108385788269953028
  Aran -> Subtag 14108386612903673860
  Armi -> Subtag 14108491822422556676
  Armn -> Subtag 14108492166019940356
  Avst -> Subtag 14113048954522304516
  Bali -> Subtag 14233457915989065732
  Bamu -> Subtag 14233467536715808772
  Bass -> Subtag 14233520175834988548
  Batk -> Subtag 14233528422172196868
  Beng -> Subtag 14237978970363527172
  Bhks -> Subtag 14241331106438709252
  Blis -> Subtag 14245817113880035332
  Bopo -> Subtag 14249256111373811716
  Brah -> Subtag 14252501388662669316
  Brai -> Subtag 14252501457382146052
  Bugi -> Subtag 14255931933660807172
  Buhd -> Subtag 14255940386156445700
  Cakm -> Subtag 14377564582849806340
  Cans -> Subtag 14377591383445733380
  Cari -> Subtag 14377625880623054852
  Cham -> Subtag 14385357921267482628
  Cher -> Subtag 14385393449236955140
  Chrs -> Subtag 14385507867165720580
  Cirt -> Subtag 14386633835792039940
  Copt -> Subtag 14393371643047051268
  Cpmn -> Subtag 14394470742357966852
  Cprt -> Subtag 14394515135139938308
  Cyrl -> Subtag 14404647684545708036
  Cyrs -> Subtag 14404648165582045188
  Deva -> Subtag 14526279302942556164
  Diak -> Subtag 14530598871811227652
  Dogr -> Subtag 14537407528846753796
  Dsrt -> Subtag 14542008022936322052
  Dupl -> Subtag 14544241680808148996
  Egyd -> Subtag 14672672885269594116
  Egyh -> Subtag 14672673160147501060
  Egyp -> Subtag 14672673709903314948
  Elba -> Subtag 14678099868505866244
  Elym -> Subtag 14678303003279097860
  Ethi -> Subtag 14687160394074554372
  Geok -> Subtag 14958563981713735684
  Geor -> Subtag 14958564462750072836
  Glag -> Subtag 14966321860881416196
  Gong -> Subtag 14969813909811232772
  Gonm -> Subtag 14969814322128093188
  Goth -> Subtag 14969866755088842756
  Gran -> Subtag 14973077741358809092
  Grek -> Subtag 14973112719572467716
  Gujr -> Subtag 14976534880794443780
  Guru -> Subtag 14976605455697051652
  Hanb -> Subtag 15098166155593908228
  Hang -> Subtag 15098166499191291908
  Hani -> Subtag 15098166636630245380
  Hano -> Subtag 15098167048947105796
  Hans -> Subtag 15098167323825012740
  Hant -> Subtag 15098167392544489476
  Hatr -> Subtag 15098220031663669252
  Hebr -> Subtag 15102565301616640004
  Hira -> Subtag 15107208470501261316
  Hluw -> Subtag 15110614070329344004
  Hmng -> Subtag 15111677298073403396
  Hmnp -> Subtag 15111677916548694020
  Hrkt -> Subtag 15117281302681747460
  Hung -> Subtag 15120684497328144388
  Inds -> Subtag 15256831249759600644
  Ital -> Subtag 15263559779885252612
  Jamo -> Subtag 15386388629005795332
  Java -> Subtag 15386466831770320900
  Jpan -> Subtag 15403171505772691460
  Jurc -> Subtag 15408949782974038020
  Kali -> Subtag 15530494608671768580
  Kana -> Subtag 15530511651101999108
  Khar -> Subtag 15538279769471713284
  Khmr -> Subtag 15538385322587979780
  Khoj -> Subtag 15538402365018210308
  Kitl -> Subtag 15539572382829117444
  Kits -> Subtag 15539572863865454596
  Knda -> Subtag 15545060388960731140
  Kore -> Subtag 15546309709047791620
  Kpel -> Subtag 15547321740781682692
  Kthi -> Subtag 15551851522529689604
  Lana -> Subtag 15674626839177854980
  Laoo -> Subtag 15674636597343551492
  Latf -> Subtag 15674679959333371908
  Latg -> Subtag 15674680028052848644
  Latn -> Subtag 15674680509089185796
  Leke -> Subtag 15679104325404065796
  Lepc -> Subtag 15679148168430223364
  Limb -> Subtag 15683625311059050500
  Lina -> Subtag 15683634038432595972
  Linb -> Subtag 15683634107152072708
  Lisu -> Subtag 15683679393287241732
  Loma -> Subtag 15690380641780629508
  Lyci -> Subtag 15701552229674647556
  Lydi -> Subtag 15701561025767669764
  Mahj -> Subtag 15818689869170868228
  Maka -> Subtag 15818715638974644228
  Mand -> Subtag 15818742233412141060
  Mani -> Subtag 15818742577009524740
  Marc -> Subtag 15818777349064753156
  Maya -> Subtag 15818838784276955140
  Medf -> Subtag 15823158009548242948
  Mend -> Subtag 15823245833039511556
  Merc -> Subtag 15823280948692123652
  Mero -> Subtag 15823281773325844484
  Mlym -> Subtag 15831224507885944836
  Modi -> Subtag 15834417214775099396
  Mong -> Subtag 15834505038266368004
  Moon -> Subtag 15834514315395727364
  Mroo -> Subtag 15837892083835731972
  Mtei -> Subtag 15840055510402334724
  Mult -> Subtag 15841243738874576900
  Mymr -> Subtag 15845755997156016132
  Nand -> Subtag 15962857421487996932
  Narb -> Subtag 15962892468421132292
  Nbat -> Subtag 15963870071697178628
  Newa -> Subtag 15967439979794137092
  Nkdb -> Subtag 15974028322187247620
  Nkgb -> Subtag 15974054710466314244
  Nkoo -> Subtag 15974125972563689476
  Nshu -> Subtag 15983072011484135428
  Ogam -> Subtag 16113614278270910468
  Olck -> Subtag 16119261232552214532
  Orkh -> Subtag 16126086794579017732
  Orya -> Subtag 16126209458844991492
  Osge -> Subtag 16127177303955341316
  Osma -> Subtag 16127229805635567620
  Ougr -> Subtag 16129429997122224132
  Palm -> Subtag 16251070823928954884
  Pauc -> Subtag 16251149301571387396
  Pcun -> Subtag 16253401857299316740
  Pelm -> Subtag 16255574423556325380
  Perm -> Subtag 16255627200114458628
  Phag -> Subtag 16258854953936748548
  Phli -> Subtag 16258951848398946308
  Phlp -> Subtag 16258952329435283460
  Phlv -> Subtag 16258952741752143876
  Phnx -> Subtag 16258970471377141764
  Piqd -> Subtag 16260121385173516292
  Plrd -> Subtag 16263507880987066372
  Prti -> Subtag 16270281216211550212
  Psin -> Subtag 16271310702692532228
  Qaaa -> Subtag 16395088430347845636
  Qaab -> Subtag 16395088499067322372
  Qaac -> Subtag 16395088567786799108
  Qaad -> Subtag 16395088636506275844
  Qaae -> Subtag 16395088705225752580
  Qaaf -> Subtag 16395088773945229316
  Qaag -> Subtag 16395088842664706052
  Qaah -> Subtag 16395088911384182788
  Qaai -> Subtag 16395088980103659524
  Qaaj -> Subtag 16395089048823136260
  Qaak -> Subtag 16395089117542612996
  Qaal -> Subtag 16395089186262089732
  Qaam -> Subtag 16395089254981566468
  Qaan -> Subtag 16395089323701043204
  Qaao -> Subtag 16395089392420519940
  Qaap -> Subtag 16395089461139996676
  Qaaq -> Subtag 16395089529859473412
  Qaar -> Subtag 16395089598578950148
  Qaas -> Subtag 16395089667298426884
  Qaat -> Subtag 16395089736017903620
  Qaau -> Subtag 16395089804737380356
  Qaav -> Subtag 16395089873456857092
  Qaaw -> Subtag 16395089942176333828
  Qaax -> Subtag 16395090010895810564
  Qaay -> Subtag 16395090079615287300
  Qaaz -> Subtag 16395090148334764036
  Qaba -> Subtag 16395097226440867844
  Qabb -> Subtag 16395097295160344580
  Qabc -> Subtag 16395097363879821316
  Qabd -> Subtag 16395097432599298052
  Qabe -> Subtag 16395097501318774788
  Qabf -> Subtag 16395097570038251524
  Qabg -> Subtag 16395097638757728260
  Qabh -> Subtag 16395097707477204996
  Qabi -> Subtag 16395097776196681732
  Qabj -> Subtag 16395097844916158468
  Qabk -> Subtag 16395097913635635204
  Qabl -> Subtag 16395097982355111940
  Qabm -> Subtag 16395098051074588676
  Qabn -> Subtag 16395098119794065412
  Qabo -> Subtag 16395098188513542148
  Qabp -> Subtag 16395098257233018884
  Qabq -> Subtag 16395098325952495620
  Qabr -> Subtag 16395098394671972356
  Qabs -> Subtag 16395098463391449092
  Qabt -> Subtag 16395098532110925828
  Qabu -> Subtag 16395098600830402564
  Qabv -> Subtag 16395098669549879300
  Qabw -> Subtag 16395098738269356036
  Qabx -> Subtag 16395098806988832772
  Ranj -> Subtag 16539318586108280836
  Rjng -> Subtag 16549451479111434244
  Rohg -> Subtag 16555028202087514116
  Roro -> Subtag 16555116712773550084
  Runr -> Subtag 16561837134000947204
  Samr -> Subtag 16683425527846928388
  Sara -> Subtag 16683468340080934916
  Sarb -> Subtag 16683468408800411652
  Saur -> Subtag 16683495896591106052
  Sgnw -> Subtag 16690190066978390020
  Shaw -> Subtag 16691201617675943940
  Shrd -> Subtag 16691349845587263492
  Shui -> Subtag 16691376577463713796
  Sidd -> Subtag 16692352600191795204
  Sind -> Subtag 16692440561122017284
  Sinh -> Subtag 16692440835999924228
  Sogd -> Subtag 16699134387911917572
  Sogo -> Subtag 16699135143826161668
  Sora -> Subtag 16699230938776731652
  Soyo -> Subtag 16699293473500561412
  Sund -> Subtag 16705951360004128772
  Sylo -> Subtag 16710438123359698948
  Syrc -> Subtag 16710490075284111364
  Syre -> Subtag 16710490212723064836
  Syrj -> Subtag 16710490556320448516
  Syrn -> Subtag 16710490831198355460
  Tagb -> Subtag 16827486839853023236
  Takr -> Subtag 16827523123736739844
  Tale -> Subtag 16827531026476564484
  Talu -> Subtag 16827532125988192260
  Taml -> Subtag 16827540303605923844
  Tang -> Subtag 16827548756101562372
  Tavt -> Subtag 16827620018198937604
  Telu -> Subtag 16832035725615562756
  Teng -> Subtag 16832052355728932868
  Tfng -> Subtag 16833178255635775492
  Tglg -> Subtag 16834286563356573700
  Thaa -> Subtag 16835315293923311620
  Thai -> Subtag 16835315843679125508
  Tibt -> Subtag 16836451295593234436
  Tirh -> Subtag 16836591208447868932
  Toto -> Subtag 16843364681111306244
  Ugar -> Subtag 16978305750323429380
  Vaii -> Subtag 17115735289227116548
  Visp -> Subtag 17124830930448416772
  Wara -> Subtag 17259929092384358404
  Wcho -> Subtag 17262093893340495876
  Wole -> Subtag 17275639189399928836
  Xpeo -> Subtag 17420819391926239236
  Xsux -> Subtag 17424338447610413060
  Yezi -> Subtag 17552733986663432196
  Yiii -> Subtag 17557088052709425156
  Zanb -> Subtag 17692239540959313924
  Zinh -> Subtag 17701247152530915332
  Zmth -> Subtag 17705803528716419076
  Zsye -> Subtag 17712602702464155652
  Zsym -> Subtag 17712603252219969540
  Zxxx -> Subtag 17718224711575404548
  Zyyy -> Subtag 17719359476294746116
  Zzzz -> Subtag 17720494241014087684
