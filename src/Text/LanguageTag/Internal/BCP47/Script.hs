-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.LanguageTag.Internal.BCP47.Script where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import Text.LanguageTag.Subtag (renderSubtag)

-- | A valid BCP47 script tag as of 2021-02-23
newtype Script = Script Subtag
  deriving (Eq, Ord, Hashable, NFData)

instance Show Script where
  show (Script t) = T.unpack $ renderSubtag t

 -- | @Adlm@. Description: Adlam.
pattern Adlm :: Script
pattern Adlm = Script (Subtag 14092720702511644692)

 -- | @Afak@. Description: Afaka.
pattern Afak :: Script
pattern Afak = Script (Subtag 14094875607863132180)

 -- | @Aghb@. Description: Caucasian Albanian.
pattern Aghb :: Script
pattern Aghb = Script (Subtag 14096062461945839636)

 -- | @Ahom@. Description: Ahom; Tai Ahom.
pattern Ahom :: Script
pattern Ahom = Script (Subtag 14097250690418081812)

 -- | @Arab@. Description: Arabic.
pattern Arab :: Script
pattern Arab = Script (Subtag 14108385788269953044)

 -- | @Aran@. Description: Arabic (Nastaliq variant).
pattern Aran :: Script
pattern Aran = Script (Subtag 14108386612903673876)

 -- | @Armi@. Description: Imperial Aramaic.
pattern Armi :: Script
pattern Armi = Script (Subtag 14108491822422556692)

 -- | @Armn@. Description: Armenian.
pattern Armn :: Script
pattern Armn = Script (Subtag 14108492166019940372)

 -- | @Avst@. Description: Avestan.
pattern Avst :: Script
pattern Avst = Script (Subtag 14113048954522304532)

 -- | @Bali@. Description: Balinese.
pattern Bali :: Script
pattern Bali = Script (Subtag 14233457915989065748)

 -- | @Bamu@. Description: Bamum.
pattern Bamu :: Script
pattern Bamu = Script (Subtag 14233467536715808788)

 -- | @Bass@. Description: Bassa Vah.
pattern Bass :: Script
pattern Bass = Script (Subtag 14233520175834988564)

 -- | @Batk@. Description: Batak.
pattern Batk :: Script
pattern Batk = Script (Subtag 14233528422172196884)

 -- | @Beng@. Description: Bengali; Bangla.
pattern Beng :: Script
pattern Beng = Script (Subtag 14237978970363527188)

 -- | @Bhks@. Description: Bhaiksuki.
pattern Bhks :: Script
pattern Bhks = Script (Subtag 14241331106438709268)

 -- | @Blis@. Description: Blissymbols.
pattern Blis :: Script
pattern Blis = Script (Subtag 14245817113880035348)

 -- | @Bopo@. Description: Bopomofo.
pattern Bopo :: Script
pattern Bopo = Script (Subtag 14249256111373811732)

 -- | @Brah@. Description: Brahmi.
pattern Brah :: Script
pattern Brah = Script (Subtag 14252501388662669332)

 -- | @Brai@. Description: Braille.
pattern Brai :: Script
pattern Brai = Script (Subtag 14252501457382146068)

 -- | @Bugi@. Description: Buginese.
pattern Bugi :: Script
pattern Bugi = Script (Subtag 14255931933660807188)

 -- | @Buhd@. Description: Buhid.
pattern Buhd :: Script
pattern Buhd = Script (Subtag 14255940386156445716)

 -- | @Cakm@. Description: Chakma.
pattern Cakm :: Script
pattern Cakm = Script (Subtag 14377564582849806356)

 -- | @Cans@. Description: Unified Canadian Aboriginal Syllabics.
pattern Cans :: Script
pattern Cans = Script (Subtag 14377591383445733396)

 -- | @Cari@. Description: Carian.
pattern Cari :: Script
pattern Cari = Script (Subtag 14377625880623054868)

 -- | @Cham@. Description: Cham.
pattern Cham :: Script
pattern Cham = Script (Subtag 14385357921267482644)

 -- | @Cher@. Description: Cherokee.
pattern Cher :: Script
pattern Cher = Script (Subtag 14385393449236955156)

 -- | @Chrs@. Description: Chorasmian.
pattern Chrs :: Script
pattern Chrs = Script (Subtag 14385507867165720596)

 -- | @Cirt@. Description: Cirth.
pattern Cirt :: Script
pattern Cirt = Script (Subtag 14386633835792039956)

 -- | @Copt@. Description: Coptic.
pattern Copt :: Script
pattern Copt = Script (Subtag 14393371643047051284)

 -- | @Cpmn@. Description: Cypro-Minoan.
pattern Cpmn :: Script
pattern Cpmn = Script (Subtag 14394470742357966868)

 -- | @Cprt@. Description: Cypriot syllabary.
pattern Cprt :: Script
pattern Cprt = Script (Subtag 14394515135139938324)

 -- | @Cyrl@. Description: Cyrillic.
pattern Cyrl :: Script
pattern Cyrl = Script (Subtag 14404647684545708052)

 -- | @Cyrs@. Description: Cyrillic (Old Church Slavonic variant).
pattern Cyrs :: Script
pattern Cyrs = Script (Subtag 14404648165582045204)

 -- | @Deva@. Description: Devanagari; Nagari.
pattern Deva :: Script
pattern Deva = Script (Subtag 14526279302942556180)

 -- | @Diak@. Description: Dives Akuru.
pattern Diak :: Script
pattern Diak = Script (Subtag 14530598871811227668)

 -- | @Dogr@. Description: Dogra.
pattern Dogr :: Script
pattern Dogr = Script (Subtag 14537407528846753812)

 -- | @Dsrt@. Description: Deseret; Mormon.
pattern Dsrt :: Script
pattern Dsrt = Script (Subtag 14542008022936322068)

 -- | @Dupl@. Description: Duployan shorthand; Duployan stenography.
pattern Dupl :: Script
pattern Dupl = Script (Subtag 14544241680808149012)

 -- | @Egyd@. Description: Egyptian demotic.
pattern Egyd :: Script
pattern Egyd = Script (Subtag 14672672885269594132)

 -- | @Egyh@. Description: Egyptian hieratic.
pattern Egyh :: Script
pattern Egyh = Script (Subtag 14672673160147501076)

 -- | @Egyp@. Description: Egyptian hieroglyphs.
pattern Egyp :: Script
pattern Egyp = Script (Subtag 14672673709903314964)

 -- | @Elba@. Description: Elbasan.
pattern Elba :: Script
pattern Elba = Script (Subtag 14678099868505866260)

 -- | @Elym@. Description: Elymaic.
pattern Elym :: Script
pattern Elym = Script (Subtag 14678303003279097876)

 -- | @Ethi@. Description: Ethiopic; Geʻez; Ge\'ez.
pattern Ethi :: Script
pattern Ethi = Script (Subtag 14687160394074554388)

 -- | @Geok@. Description: Khutsuri (Asomtavruli and Nuskhuri).
pattern Geok :: Script
pattern Geok = Script (Subtag 14958563981713735700)

 -- | @Geor@. Description: Georgian (Mkhedruli and Mtavruli).
pattern Geor :: Script
pattern Geor = Script (Subtag 14958564462750072852)

 -- | @Glag@. Description: Glagolitic.
pattern Glag :: Script
pattern Glag = Script (Subtag 14966321860881416212)

 -- | @Gong@. Description: Gunjala Gondi.
pattern Gong :: Script
pattern Gong = Script (Subtag 14969813909811232788)

 -- | @Gonm@. Description: Masaram Gondi.
pattern Gonm :: Script
pattern Gonm = Script (Subtag 14969814322128093204)

 -- | @Goth@. Description: Gothic.
pattern Goth :: Script
pattern Goth = Script (Subtag 14969866755088842772)

 -- | @Gran@. Description: Grantha.
pattern Gran :: Script
pattern Gran = Script (Subtag 14973077741358809108)

 -- | @Grek@. Description: Greek.
pattern Grek :: Script
pattern Grek = Script (Subtag 14973112719572467732)

 -- | @Gujr@. Description: Gujarati.
pattern Gujr :: Script
pattern Gujr = Script (Subtag 14976534880794443796)

 -- | @Guru@. Description: Gurmukhi.
pattern Guru :: Script
pattern Guru = Script (Subtag 14976605455697051668)

 -- | @Hanb@. Description: Han with Bopomofo (alias for Han + Bopomofo).
pattern Hanb :: Script
pattern Hanb = Script (Subtag 15098166155593908244)

 -- | @Hang@. Description: Hangul; Hangŭl; Hangeul.
pattern Hang :: Script
pattern Hang = Script (Subtag 15098166499191291924)

 -- | @Hani@. Description: Han; Hanzi; Kanji; Hanja.
pattern Hani :: Script
pattern Hani = Script (Subtag 15098166636630245396)

 -- | @Hano@. Description: Hanunoo; Hanunóo.
pattern Hano :: Script
pattern Hano = Script (Subtag 15098167048947105812)

 -- | @Hans@. Description: Han (Simplified variant).
pattern Hans :: Script
pattern Hans = Script (Subtag 15098167323825012756)

 -- | @Hant@. Description: Han (Traditional variant).
pattern Hant :: Script
pattern Hant = Script (Subtag 15098167392544489492)

 -- | @Hatr@. Description: Hatran.
pattern Hatr :: Script
pattern Hatr = Script (Subtag 15098220031663669268)

 -- | @Hebr@. Description: Hebrew.
pattern Hebr :: Script
pattern Hebr = Script (Subtag 15102565301616640020)

 -- | @Hira@. Description: Hiragana.
pattern Hira :: Script
pattern Hira = Script (Subtag 15107208470501261332)

 -- | @Hluw@. Description: Anatolian Hieroglyphs; Luwian Hieroglyphs; Hittite Hieroglyphs.
pattern Hluw :: Script
pattern Hluw = Script (Subtag 15110614070329344020)

 -- | @Hmng@. Description: Pahawh Hmong.
pattern Hmng :: Script
pattern Hmng = Script (Subtag 15111677298073403412)

 -- | @Hmnp@. Description: Nyiakeng Puachue Hmong.
pattern Hmnp :: Script
pattern Hmnp = Script (Subtag 15111677916548694036)

 -- | @Hrkt@. Description: Japanese syllabaries (alias for Hiragana + Katakana).
pattern Hrkt :: Script
pattern Hrkt = Script (Subtag 15117281302681747476)

 -- | @Hung@. Description: Old Hungarian; Hungarian Runic.
pattern Hung :: Script
pattern Hung = Script (Subtag 15120684497328144404)

 -- | @Inds@. Description: Indus; Harappan.
pattern Inds :: Script
pattern Inds = Script (Subtag 15256831249759600660)

 -- | @Ital@. Description: Old Italic (Etruscan, Oscan, etc.).
pattern Ital :: Script
pattern Ital = Script (Subtag 15263559779885252628)

 -- | @Jamo@. Description: Jamo (alias for Jamo subset of Hangul).
pattern Jamo :: Script
pattern Jamo = Script (Subtag 15386388629005795348)

 -- | @Java@. Description: Javanese.
pattern Java :: Script
pattern Java = Script (Subtag 15386466831770320916)

 -- | @Jpan@. Description: Japanese (alias for Han + Hiragana + Katakana).
pattern Jpan :: Script
pattern Jpan = Script (Subtag 15403171505772691476)

 -- | @Jurc@. Description: Jurchen.
pattern Jurc :: Script
pattern Jurc = Script (Subtag 15408949782974038036)

 -- | @Kali@. Description: Kayah Li.
pattern Kali :: Script
pattern Kali = Script (Subtag 15530494608671768596)

 -- | @Kana@. Description: Katakana.
pattern Kana :: Script
pattern Kana = Script (Subtag 15530511651101999124)

 -- | @Khar@. Description: Kharoshthi.
pattern Khar :: Script
pattern Khar = Script (Subtag 15538279769471713300)

 -- | @Khmr@. Description: Khmer.
pattern Khmr :: Script
pattern Khmr = Script (Subtag 15538385322587979796)

 -- | @Khoj@. Description: Khojki.
pattern Khoj :: Script
pattern Khoj = Script (Subtag 15538402365018210324)

 -- | @Kitl@. Description: Khitan large script.
pattern Kitl :: Script
pattern Kitl = Script (Subtag 15539572382829117460)

 -- | @Kits@. Description: Khitan small script.
pattern Kits :: Script
pattern Kits = Script (Subtag 15539572863865454612)

 -- | @Knda@. Description: Kannada.
pattern Knda :: Script
pattern Knda = Script (Subtag 15545060388960731156)

 -- | @Kore@. Description: Korean (alias for Hangul + Han).
pattern Kore :: Script
pattern Kore = Script (Subtag 15546309709047791636)

 -- | @Kpel@. Description: Kpelle.
pattern Kpel :: Script
pattern Kpel = Script (Subtag 15547321740781682708)

 -- | @Kthi@. Description: Kaithi.
pattern Kthi :: Script
pattern Kthi = Script (Subtag 15551851522529689620)

 -- | @Lana@. Description: Tai Tham; Lanna.
pattern Lana :: Script
pattern Lana = Script (Subtag 15674626839177854996)

 -- | @Laoo@. Description: Lao.
pattern Laoo :: Script
pattern Laoo = Script (Subtag 15674636597343551508)

 -- | @Latf@. Description: Latin (Fraktur variant).
pattern Latf :: Script
pattern Latf = Script (Subtag 15674679959333371924)

 -- | @Latg@. Description: Latin (Gaelic variant).
pattern Latg :: Script
pattern Latg = Script (Subtag 15674680028052848660)

 -- | @Latn@. Description: Latin.
pattern Latn :: Script
pattern Latn = Script (Subtag 15674680509089185812)

 -- | @Leke@. Description: Leke.
pattern Leke :: Script
pattern Leke = Script (Subtag 15679104325404065812)

 -- | @Lepc@. Description: Lepcha; Róng.
pattern Lepc :: Script
pattern Lepc = Script (Subtag 15679148168430223380)

 -- | @Limb@. Description: Limbu.
pattern Limb :: Script
pattern Limb = Script (Subtag 15683625311059050516)

 -- | @Lina@. Description: Linear A.
pattern Lina :: Script
pattern Lina = Script (Subtag 15683634038432595988)

 -- | @Linb@. Description: Linear B.
pattern Linb :: Script
pattern Linb = Script (Subtag 15683634107152072724)

 -- | @Lisu@. Description: Lisu; Fraser.
pattern Lisu :: Script
pattern Lisu = Script (Subtag 15683679393287241748)

 -- | @Loma@. Description: Loma.
pattern Loma :: Script
pattern Loma = Script (Subtag 15690380641780629524)

 -- | @Lyci@. Description: Lycian.
pattern Lyci :: Script
pattern Lyci = Script (Subtag 15701552229674647572)

 -- | @Lydi@. Description: Lydian.
pattern Lydi :: Script
pattern Lydi = Script (Subtag 15701561025767669780)

 -- | @Mahj@. Description: Mahajani.
pattern Mahj :: Script
pattern Mahj = Script (Subtag 15818689869170868244)

 -- | @Maka@. Description: Makasar.
pattern Maka :: Script
pattern Maka = Script (Subtag 15818715638974644244)

 -- | @Mand@. Description: Mandaic; Mandaean.
pattern Mand :: Script
pattern Mand = Script (Subtag 15818742233412141076)

 -- | @Mani@. Description: Manichaean.
pattern Mani :: Script
pattern Mani = Script (Subtag 15818742577009524756)

 -- | @Marc@. Description: Marchen.
pattern Marc :: Script
pattern Marc = Script (Subtag 15818777349064753172)

 -- | @Maya@. Description: Mayan hieroglyphs.
pattern Maya :: Script
pattern Maya = Script (Subtag 15818838784276955156)

 -- | @Medf@. Description: Medefaidrin; Oberi Okaime; Oberi Ɔkaimɛ.
pattern Medf :: Script
pattern Medf = Script (Subtag 15823158009548242964)

 -- | @Mend@. Description: Mende Kikakui.
pattern Mend :: Script
pattern Mend = Script (Subtag 15823245833039511572)

 -- | @Merc@. Description: Meroitic Cursive.
pattern Merc :: Script
pattern Merc = Script (Subtag 15823280948692123668)

 -- | @Mero@. Description: Meroitic Hieroglyphs.
pattern Mero :: Script
pattern Mero = Script (Subtag 15823281773325844500)

 -- | @Mlym@. Description: Malayalam.
pattern Mlym :: Script
pattern Mlym = Script (Subtag 15831224507885944852)

 -- | @Modi@. Description: Modi; Moḍī.
pattern Modi :: Script
pattern Modi = Script (Subtag 15834417214775099412)

 -- | @Mong@. Description: Mongolian.
pattern Mong :: Script
pattern Mong = Script (Subtag 15834505038266368020)

 -- | @Moon@. Description: Moon; Moon code; Moon script; Moon type.
pattern Moon :: Script
pattern Moon = Script (Subtag 15834514315395727380)

 -- | @Mroo@. Description: Mro; Mru.
pattern Mroo :: Script
pattern Mroo = Script (Subtag 15837892083835731988)

 -- | @Mtei@. Description: Meitei Mayek; Meithei; Meetei.
pattern Mtei :: Script
pattern Mtei = Script (Subtag 15840055510402334740)

 -- | @Mult@. Description: Multani.
pattern Mult :: Script
pattern Mult = Script (Subtag 15841243738874576916)

 -- | @Mymr@. Description: Myanmar; Burmese.
pattern Mymr :: Script
pattern Mymr = Script (Subtag 15845755997156016148)

 -- | @Nand@. Description: Nandinagari.
pattern Nand :: Script
pattern Nand = Script (Subtag 15962857421487996948)

 -- | @Narb@. Description: Old North Arabian; Ancient North Arabian.
pattern Narb :: Script
pattern Narb = Script (Subtag 15962892468421132308)

 -- | @Nbat@. Description: Nabataean.
pattern Nbat :: Script
pattern Nbat = Script (Subtag 15963870071697178644)

 -- | @Newa@. Description: Newa; Newar; Newari; Nepāla lipi.
pattern Newa :: Script
pattern Newa = Script (Subtag 15967439979794137108)

 -- | @Nkdb@. Description: Naxi Dongba; na²¹ɕi³³ to³³ba²¹; Nakhi Tomba.
pattern Nkdb :: Script
pattern Nkdb = Script (Subtag 15974028322187247636)

 -- | @Nkgb@. Description: Naxi Geba; na²¹ɕi³³ gʌ²¹ba²¹; \'Na-\'Khi ²Ggŏ-¹baw; Nakhi Geba.
pattern Nkgb :: Script
pattern Nkgb = Script (Subtag 15974054710466314260)

 -- | @Nkoo@. Description: N’Ko; N\'Ko.
pattern Nkoo :: Script
pattern Nkoo = Script (Subtag 15974125972563689492)

 -- | @Nshu@. Description: Nüshu.
pattern Nshu :: Script
pattern Nshu = Script (Subtag 15983072011484135444)

 -- | @Ogam@. Description: Ogham.
pattern Ogam :: Script
pattern Ogam = Script (Subtag 16113614278270910484)

 -- | @Olck@. Description: Ol Chiki; Ol Cemet\'; Ol; Santali.
pattern Olck :: Script
pattern Olck = Script (Subtag 16119261232552214548)

 -- | @Orkh@. Description: Old Turkic; Orkhon Runic.
pattern Orkh :: Script
pattern Orkh = Script (Subtag 16126086794579017748)

 -- | @Orya@. Description: Oriya; Odia.
pattern Orya :: Script
pattern Orya = Script (Subtag 16126209458844991508)

 -- | @Osge@. Description: Osage.
pattern Osge :: Script
pattern Osge = Script (Subtag 16127177303955341332)

 -- | @Osma@. Description: Osmanya.
pattern Osma :: Script
pattern Osma = Script (Subtag 16127229805635567636)

 -- | @Ougr@. Description: Old Uyghur.
pattern Ougr :: Script
pattern Ougr = Script (Subtag 16129429997122224148)

 -- | @Palm@. Description: Palmyrene.
pattern Palm :: Script
pattern Palm = Script (Subtag 16251070823928954900)

 -- | @Pauc@. Description: Pau Cin Hau.
pattern Pauc :: Script
pattern Pauc = Script (Subtag 16251149301571387412)

 -- | @Pcun@. Description: Proto-Cuneiform.
pattern Pcun :: Script
pattern Pcun = Script (Subtag 16253401857299316756)

 -- | @Pelm@. Description: Proto-Elamite.
pattern Pelm :: Script
pattern Pelm = Script (Subtag 16255574423556325396)

 -- | @Perm@. Description: Old Permic.
pattern Perm :: Script
pattern Perm = Script (Subtag 16255627200114458644)

 -- | @Phag@. Description: Phags-pa.
pattern Phag :: Script
pattern Phag = Script (Subtag 16258854953936748564)

 -- | @Phli@. Description: Inscriptional Pahlavi.
pattern Phli :: Script
pattern Phli = Script (Subtag 16258951848398946324)

 -- | @Phlp@. Description: Psalter Pahlavi.
pattern Phlp :: Script
pattern Phlp = Script (Subtag 16258952329435283476)

 -- | @Phlv@. Description: Book Pahlavi.
pattern Phlv :: Script
pattern Phlv = Script (Subtag 16258952741752143892)

 -- | @Phnx@. Description: Phoenician.
pattern Phnx :: Script
pattern Phnx = Script (Subtag 16258970471377141780)

 -- | @Piqd@. Description: Klingon (KLI pIqaD).
pattern Piqd :: Script
pattern Piqd = Script (Subtag 16260121385173516308)

 -- | @Plrd@. Description: Miao; Pollard.
pattern Plrd :: Script
pattern Plrd = Script (Subtag 16263507880987066388)

 -- | @Prti@. Description: Inscriptional Parthian.
pattern Prti :: Script
pattern Prti = Script (Subtag 16270281216211550228)

 -- | @Psin@. Description: Proto-Sinaitic.
pattern Psin :: Script
pattern Psin = Script (Subtag 16271310702692532244)

 -- | @Qaaa@. Description: Private use.
pattern Qaaa :: Script
pattern Qaaa = Script (Subtag 16395088430347845652)

 -- | @Qaab@. Description: Private use.
pattern Qaab :: Script
pattern Qaab = Script (Subtag 16395088499067322388)

 -- | @Qaac@. Description: Private use.
pattern Qaac :: Script
pattern Qaac = Script (Subtag 16395088567786799124)

 -- | @Qaad@. Description: Private use.
pattern Qaad :: Script
pattern Qaad = Script (Subtag 16395088636506275860)

 -- | @Qaae@. Description: Private use.
pattern Qaae :: Script
pattern Qaae = Script (Subtag 16395088705225752596)

 -- | @Qaaf@. Description: Private use.
pattern Qaaf :: Script
pattern Qaaf = Script (Subtag 16395088773945229332)

 -- | @Qaag@. Description: Private use.
pattern Qaag :: Script
pattern Qaag = Script (Subtag 16395088842664706068)

 -- | @Qaah@. Description: Private use.
pattern Qaah :: Script
pattern Qaah = Script (Subtag 16395088911384182804)

 -- | @Qaai@. Description: Private use.
pattern Qaai :: Script
pattern Qaai = Script (Subtag 16395088980103659540)

 -- | @Qaaj@. Description: Private use.
pattern Qaaj :: Script
pattern Qaaj = Script (Subtag 16395089048823136276)

 -- | @Qaak@. Description: Private use.
pattern Qaak :: Script
pattern Qaak = Script (Subtag 16395089117542613012)

 -- | @Qaal@. Description: Private use.
pattern Qaal :: Script
pattern Qaal = Script (Subtag 16395089186262089748)

 -- | @Qaam@. Description: Private use.
pattern Qaam :: Script
pattern Qaam = Script (Subtag 16395089254981566484)

 -- | @Qaan@. Description: Private use.
pattern Qaan :: Script
pattern Qaan = Script (Subtag 16395089323701043220)

 -- | @Qaao@. Description: Private use.
pattern Qaao :: Script
pattern Qaao = Script (Subtag 16395089392420519956)

 -- | @Qaap@. Description: Private use.
pattern Qaap :: Script
pattern Qaap = Script (Subtag 16395089461139996692)

 -- | @Qaaq@. Description: Private use.
pattern Qaaq :: Script
pattern Qaaq = Script (Subtag 16395089529859473428)

 -- | @Qaar@. Description: Private use.
pattern Qaar :: Script
pattern Qaar = Script (Subtag 16395089598578950164)

 -- | @Qaas@. Description: Private use.
pattern Qaas :: Script
pattern Qaas = Script (Subtag 16395089667298426900)

 -- | @Qaat@. Description: Private use.
pattern Qaat :: Script
pattern Qaat = Script (Subtag 16395089736017903636)

 -- | @Qaau@. Description: Private use.
pattern Qaau :: Script
pattern Qaau = Script (Subtag 16395089804737380372)

 -- | @Qaav@. Description: Private use.
pattern Qaav :: Script
pattern Qaav = Script (Subtag 16395089873456857108)

 -- | @Qaaw@. Description: Private use.
pattern Qaaw :: Script
pattern Qaaw = Script (Subtag 16395089942176333844)

 -- | @Qaax@. Description: Private use.
pattern Qaax :: Script
pattern Qaax = Script (Subtag 16395090010895810580)

 -- | @Qaay@. Description: Private use.
pattern Qaay :: Script
pattern Qaay = Script (Subtag 16395090079615287316)

 -- | @Qaaz@. Description: Private use.
pattern Qaaz :: Script
pattern Qaaz = Script (Subtag 16395090148334764052)

 -- | @Qaba@. Description: Private use.
pattern Qaba :: Script
pattern Qaba = Script (Subtag 16395097226440867860)

 -- | @Qabb@. Description: Private use.
pattern Qabb :: Script
pattern Qabb = Script (Subtag 16395097295160344596)

 -- | @Qabc@. Description: Private use.
pattern Qabc :: Script
pattern Qabc = Script (Subtag 16395097363879821332)

 -- | @Qabd@. Description: Private use.
pattern Qabd :: Script
pattern Qabd = Script (Subtag 16395097432599298068)

 -- | @Qabe@. Description: Private use.
pattern Qabe :: Script
pattern Qabe = Script (Subtag 16395097501318774804)

 -- | @Qabf@. Description: Private use.
pattern Qabf :: Script
pattern Qabf = Script (Subtag 16395097570038251540)

 -- | @Qabg@. Description: Private use.
pattern Qabg :: Script
pattern Qabg = Script (Subtag 16395097638757728276)

 -- | @Qabh@. Description: Private use.
pattern Qabh :: Script
pattern Qabh = Script (Subtag 16395097707477205012)

 -- | @Qabi@. Description: Private use.
pattern Qabi :: Script
pattern Qabi = Script (Subtag 16395097776196681748)

 -- | @Qabj@. Description: Private use.
pattern Qabj :: Script
pattern Qabj = Script (Subtag 16395097844916158484)

 -- | @Qabk@. Description: Private use.
pattern Qabk :: Script
pattern Qabk = Script (Subtag 16395097913635635220)

 -- | @Qabl@. Description: Private use.
pattern Qabl :: Script
pattern Qabl = Script (Subtag 16395097982355111956)

 -- | @Qabm@. Description: Private use.
pattern Qabm :: Script
pattern Qabm = Script (Subtag 16395098051074588692)

 -- | @Qabn@. Description: Private use.
pattern Qabn :: Script
pattern Qabn = Script (Subtag 16395098119794065428)

 -- | @Qabo@. Description: Private use.
pattern Qabo :: Script
pattern Qabo = Script (Subtag 16395098188513542164)

 -- | @Qabp@. Description: Private use.
pattern Qabp :: Script
pattern Qabp = Script (Subtag 16395098257233018900)

 -- | @Qabq@. Description: Private use.
pattern Qabq :: Script
pattern Qabq = Script (Subtag 16395098325952495636)

 -- | @Qabr@. Description: Private use.
pattern Qabr :: Script
pattern Qabr = Script (Subtag 16395098394671972372)

 -- | @Qabs@. Description: Private use.
pattern Qabs :: Script
pattern Qabs = Script (Subtag 16395098463391449108)

 -- | @Qabt@. Description: Private use.
pattern Qabt :: Script
pattern Qabt = Script (Subtag 16395098532110925844)

 -- | @Qabu@. Description: Private use.
pattern Qabu :: Script
pattern Qabu = Script (Subtag 16395098600830402580)

 -- | @Qabv@. Description: Private use.
pattern Qabv :: Script
pattern Qabv = Script (Subtag 16395098669549879316)

 -- | @Qabw@. Description: Private use.
pattern Qabw :: Script
pattern Qabw = Script (Subtag 16395098738269356052)

 -- | @Qabx@. Description: Private use.
pattern Qabx :: Script
pattern Qabx = Script (Subtag 16395098806988832788)

 -- | @Ranj@. Description: Ranjana.
pattern Ranj :: Script
pattern Ranj = Script (Subtag 16539318586108280852)

 -- | @Rjng@. Description: Rejang; Redjang; Kaganga.
pattern Rjng :: Script
pattern Rjng = Script (Subtag 16549451479111434260)

 -- | @Rohg@. Description: Hanifi Rohingya.
pattern Rohg :: Script
pattern Rohg = Script (Subtag 16555028202087514132)

 -- | @Roro@. Description: Rongorongo.
pattern Roro :: Script
pattern Roro = Script (Subtag 16555116712773550100)

 -- | @Runr@. Description: Runic.
pattern Runr :: Script
pattern Runr = Script (Subtag 16561837134000947220)

 -- | @Samr@. Description: Samaritan.
pattern Samr :: Script
pattern Samr = Script (Subtag 16683425527846928404)

 -- | @Sara@. Description: Sarati.
pattern Sara :: Script
pattern Sara = Script (Subtag 16683468340080934932)

 -- | @Sarb@. Description: Old South Arabian.
pattern Sarb :: Script
pattern Sarb = Script (Subtag 16683468408800411668)

 -- | @Saur@. Description: Saurashtra.
pattern Saur :: Script
pattern Saur = Script (Subtag 16683495896591106068)

 -- | @Sgnw@. Description: SignWriting.
pattern Sgnw :: Script
pattern Sgnw = Script (Subtag 16690190066978390036)

 -- | @Shaw@. Description: Shavian; Shaw.
pattern Shaw :: Script
pattern Shaw = Script (Subtag 16691201617675943956)

 -- | @Shrd@. Description: Sharada; Śāradā.
pattern Shrd :: Script
pattern Shrd = Script (Subtag 16691349845587263508)

 -- | @Shui@. Description: Shuishu.
pattern Shui :: Script
pattern Shui = Script (Subtag 16691376577463713812)

 -- | @Sidd@. Description: Siddham; Siddhaṃ; Siddhamātṛkā.
pattern Sidd :: Script
pattern Sidd = Script (Subtag 16692352600191795220)

 -- | @Sind@. Description: Khudawadi; Sindhi.
pattern Sind :: Script
pattern Sind = Script (Subtag 16692440561122017300)

 -- | @Sinh@. Description: Sinhala.
pattern Sinh :: Script
pattern Sinh = Script (Subtag 16692440835999924244)

 -- | @Sogd@. Description: Sogdian.
pattern Sogd :: Script
pattern Sogd = Script (Subtag 16699134387911917588)

 -- | @Sogo@. Description: Old Sogdian.
pattern Sogo :: Script
pattern Sogo = Script (Subtag 16699135143826161684)

 -- | @Sora@. Description: Sora Sompeng.
pattern Sora :: Script
pattern Sora = Script (Subtag 16699230938776731668)

 -- | @Soyo@. Description: Soyombo.
pattern Soyo :: Script
pattern Soyo = Script (Subtag 16699293473500561428)

 -- | @Sund@. Description: Sundanese.
pattern Sund :: Script
pattern Sund = Script (Subtag 16705951360004128788)

 -- | @Sylo@. Description: Syloti Nagri.
pattern Sylo :: Script
pattern Sylo = Script (Subtag 16710438123359698964)

 -- | @Syrc@. Description: Syriac.
pattern Syrc :: Script
pattern Syrc = Script (Subtag 16710490075284111380)

 -- | @Syre@. Description: Syriac (Estrangelo variant).
pattern Syre :: Script
pattern Syre = Script (Subtag 16710490212723064852)

 -- | @Syrj@. Description: Syriac (Western variant).
pattern Syrj :: Script
pattern Syrj = Script (Subtag 16710490556320448532)

 -- | @Syrn@. Description: Syriac (Eastern variant).
pattern Syrn :: Script
pattern Syrn = Script (Subtag 16710490831198355476)

 -- | @Tagb@. Description: Tagbanwa.
pattern Tagb :: Script
pattern Tagb = Script (Subtag 16827486839853023252)

 -- | @Takr@. Description: Takri; Ṭākrī; Ṭāṅkrī.
pattern Takr :: Script
pattern Takr = Script (Subtag 16827523123736739860)

 -- | @Tale@. Description: Tai Le.
pattern Tale :: Script
pattern Tale = Script (Subtag 16827531026476564500)

 -- | @Talu@. Description: New Tai Lue.
pattern Talu :: Script
pattern Talu = Script (Subtag 16827532125988192276)

 -- | @Taml@. Description: Tamil.
pattern Taml :: Script
pattern Taml = Script (Subtag 16827540303605923860)

 -- | @Tang@. Description: Tangut.
pattern Tang :: Script
pattern Tang = Script (Subtag 16827548756101562388)

 -- | @Tavt@. Description: Tai Viet.
pattern Tavt :: Script
pattern Tavt = Script (Subtag 16827620018198937620)

 -- | @Telu@. Description: Telugu.
pattern Telu :: Script
pattern Telu = Script (Subtag 16832035725615562772)

 -- | @Teng@. Description: Tengwar.
pattern Teng :: Script
pattern Teng = Script (Subtag 16832052355728932884)

 -- | @Tfng@. Description: Tifinagh; Berber.
pattern Tfng :: Script
pattern Tfng = Script (Subtag 16833178255635775508)

 -- | @Tglg@. Description: Tagalog; Baybayin; Alibata.
pattern Tglg :: Script
pattern Tglg = Script (Subtag 16834286563356573716)

 -- | @Thaa@. Description: Thaana.
pattern Thaa :: Script
pattern Thaa = Script (Subtag 16835315293923311636)

 -- | @Thai@. Description: Thai.
pattern Thai :: Script
pattern Thai = Script (Subtag 16835315843679125524)

 -- | @Tibt@. Description: Tibetan.
pattern Tibt :: Script
pattern Tibt = Script (Subtag 16836451295593234452)

 -- | @Tirh@. Description: Tirhuta.
pattern Tirh :: Script
pattern Tirh = Script (Subtag 16836591208447868948)

 -- | @Toto@. Description: Toto.
pattern Toto :: Script
pattern Toto = Script (Subtag 16843364681111306260)

 -- | @Ugar@. Description: Ugaritic.
pattern Ugar :: Script
pattern Ugar = Script (Subtag 16978305750323429396)

 -- | @Vaii@. Description: Vai.
pattern Vaii :: Script
pattern Vaii = Script (Subtag 17115735289227116564)

 -- | @Visp@. Description: Visible Speech.
pattern Visp :: Script
pattern Visp = Script (Subtag 17124830930448416788)

 -- | @Wara@. Description: Warang Citi; Varang Kshiti.
pattern Wara :: Script
pattern Wara = Script (Subtag 17259929092384358420)

 -- | @Wcho@. Description: Wancho.
pattern Wcho :: Script
pattern Wcho = Script (Subtag 17262093893340495892)

 -- | @Wole@. Description: Woleai.
pattern Wole :: Script
pattern Wole = Script (Subtag 17275639189399928852)

 -- | @Xpeo@. Description: Old Persian.
pattern Xpeo :: Script
pattern Xpeo = Script (Subtag 17420819391926239252)

 -- | @Xsux@. Description: Sumero-Akkadian cuneiform.
pattern Xsux :: Script
pattern Xsux = Script (Subtag 17424338447610413076)

 -- | @Yezi@. Description: Yezidi.
pattern Yezi :: Script
pattern Yezi = Script (Subtag 17552733986663432212)

 -- | @Yiii@. Description: Yi.
pattern Yiii :: Script
pattern Yiii = Script (Subtag 17557088052709425172)

 -- | @Zanb@. Description: Zanabazar Square; Zanabazarin Dörböljin Useg; Xewtee Dörböljin Bicig; Horizontal Square Script.
pattern Zanb :: Script
pattern Zanb = Script (Subtag 17692239540959313940)

 -- | @Zinh@. Description: Code for inherited script.
pattern Zinh :: Script
pattern Zinh = Script (Subtag 17701247152530915348)

 -- | @Zmth@. Description: Mathematical notation.
pattern Zmth :: Script
pattern Zmth = Script (Subtag 17705803528716419092)

 -- | @Zsye@. Description: Symbols (Emoji variant).
pattern Zsye :: Script
pattern Zsye = Script (Subtag 17712602702464155668)

 -- | @Zsym@. Description: Symbols.
pattern Zsym :: Script
pattern Zsym = Script (Subtag 17712603252219969556)

 -- | @Zxxx@. Description: Code for unwritten documents.
pattern Zxxx :: Script
pattern Zxxx = Script (Subtag 17718224711575404564)

 -- | @Zyyy@. Description: Code for undetermined script.
pattern Zyyy :: Script
pattern Zyyy = Script (Subtag 17719359476294746132)

 -- | @Zzzz@. Description: Code for uncoded script.
pattern Zzzz :: Script
pattern Zzzz = Script (Subtag 17720494241014087700)
