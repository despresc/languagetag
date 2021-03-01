-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.ScriptRecords 
  (lookupScriptDetails, lookupSubtagScript) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Script
import Text.LanguageTag.Internal.BCP47.Validate
import Data.List.NonEmpty (NonEmpty(..))
import Text.LanguageTag.Internal.BCP47.Syntax (Subtag(..))
import qualified Data.HashMap.Strict as HM

scriptTable :: [(Script, Subtag, ScriptRecord)]
scriptTable =
  [(Adlm, Subtag 14092720702511644676, ScriptRecord ("Adlam" :| []) NotDeprecated)
  ,(Afak, Subtag 14094875607863132164, ScriptRecord ("Afaka" :| []) NotDeprecated)
  ,(Aghb, Subtag 14096062461945839620, ScriptRecord ("Caucasian Albanian" :| []) NotDeprecated)
  ,(Ahom, Subtag 14097250690418081796, ScriptRecord ("Ahom" :| ["Tai Ahom"]) NotDeprecated)
  ,(Arab, Subtag 14108385788269953028, ScriptRecord ("Arabic" :| []) NotDeprecated)
  ,(Aran, Subtag 14108386612903673860, ScriptRecord ("Arabic (Nastaliq variant)" :| []) NotDeprecated)
  ,(Armi, Subtag 14108491822422556676, ScriptRecord ("Imperial Aramaic" :| []) NotDeprecated)
  ,(Armn, Subtag 14108492166019940356, ScriptRecord ("Armenian" :| []) NotDeprecated)
  ,(Avst, Subtag 14113048954522304516, ScriptRecord ("Avestan" :| []) NotDeprecated)
  ,(Bali, Subtag 14233457915989065732, ScriptRecord ("Balinese" :| []) NotDeprecated)
  ,(Bamu, Subtag 14233467536715808772, ScriptRecord ("Bamum" :| []) NotDeprecated)
  ,(Bass, Subtag 14233520175834988548, ScriptRecord ("Bassa Vah" :| []) NotDeprecated)
  ,(Batk, Subtag 14233528422172196868, ScriptRecord ("Batak" :| []) NotDeprecated)
  ,(Beng, Subtag 14237978970363527172, ScriptRecord ("Bengali" :| ["Bangla"]) NotDeprecated)
  ,(Bhks, Subtag 14241331106438709252, ScriptRecord ("Bhaiksuki" :| []) NotDeprecated)
  ,(Blis, Subtag 14245817113880035332, ScriptRecord ("Blissymbols" :| []) NotDeprecated)
  ,(Bopo, Subtag 14249256111373811716, ScriptRecord ("Bopomofo" :| []) NotDeprecated)
  ,(Brah, Subtag 14252501388662669316, ScriptRecord ("Brahmi" :| []) NotDeprecated)
  ,(Brai, Subtag 14252501457382146052, ScriptRecord ("Braille" :| []) NotDeprecated)
  ,(Bugi, Subtag 14255931933660807172, ScriptRecord ("Buginese" :| []) NotDeprecated)
  ,(Buhd, Subtag 14255940386156445700, ScriptRecord ("Buhid" :| []) NotDeprecated)
  ,(Cakm, Subtag 14377564582849806340, ScriptRecord ("Chakma" :| []) NotDeprecated)
  ,(Cans, Subtag 14377591383445733380, ScriptRecord ("Unified Canadian Aboriginal Syllabics" :| []) NotDeprecated)
  ,(Cari, Subtag 14377625880623054852, ScriptRecord ("Carian" :| []) NotDeprecated)
  ,(Cham, Subtag 14385357921267482628, ScriptRecord ("Cham" :| []) NotDeprecated)
  ,(Cher, Subtag 14385393449236955140, ScriptRecord ("Cherokee" :| []) NotDeprecated)
  ,(Chrs, Subtag 14385507867165720580, ScriptRecord ("Chorasmian" :| []) NotDeprecated)
  ,(Cirt, Subtag 14386633835792039940, ScriptRecord ("Cirth" :| []) NotDeprecated)
  ,(Copt, Subtag 14393371643047051268, ScriptRecord ("Coptic" :| []) NotDeprecated)
  ,(Cpmn, Subtag 14394470742357966852, ScriptRecord ("Cypro-Minoan" :| []) NotDeprecated)
  ,(Cprt, Subtag 14394515135139938308, ScriptRecord ("Cypriot syllabary" :| []) NotDeprecated)
  ,(Cyrl, Subtag 14404647684545708036, ScriptRecord ("Cyrillic" :| []) NotDeprecated)
  ,(Cyrs, Subtag 14404648165582045188, ScriptRecord ("Cyrillic (Old Church Slavonic variant)" :| []) NotDeprecated)
  ,(Deva, Subtag 14526279302942556164, ScriptRecord ("Devanagari" :| ["Nagari"]) NotDeprecated)
  ,(Diak, Subtag 14530598871811227652, ScriptRecord ("Dives Akuru" :| []) NotDeprecated)
  ,(Dogr, Subtag 14537407528846753796, ScriptRecord ("Dogra" :| []) NotDeprecated)
  ,(Dsrt, Subtag 14542008022936322052, ScriptRecord ("Deseret" :| ["Mormon"]) NotDeprecated)
  ,(Dupl, Subtag 14544241680808148996, ScriptRecord ("Duployan shorthand" :| ["Duployan stenography"]) NotDeprecated)
  ,(Egyd, Subtag 14672672885269594116, ScriptRecord ("Egyptian demotic" :| []) NotDeprecated)
  ,(Egyh, Subtag 14672673160147501060, ScriptRecord ("Egyptian hieratic" :| []) NotDeprecated)
  ,(Egyp, Subtag 14672673709903314948, ScriptRecord ("Egyptian hieroglyphs" :| []) NotDeprecated)
  ,(Elba, Subtag 14678099868505866244, ScriptRecord ("Elbasan" :| []) NotDeprecated)
  ,(Elym, Subtag 14678303003279097860, ScriptRecord ("Elymaic" :| []) NotDeprecated)
  ,(Ethi, Subtag 14687160394074554372, ScriptRecord ("Ethiopic" :| ["Ge\699ez","Ge'ez"]) NotDeprecated)
  ,(Geok, Subtag 14958563981713735684, ScriptRecord ("Khutsuri (Asomtavruli and Nuskhuri)" :| []) NotDeprecated)
  ,(Geor, Subtag 14958564462750072836, ScriptRecord ("Georgian (Mkhedruli and Mtavruli)" :| []) NotDeprecated)
  ,(Glag, Subtag 14966321860881416196, ScriptRecord ("Glagolitic" :| []) NotDeprecated)
  ,(Gong, Subtag 14969813909811232772, ScriptRecord ("Gunjala Gondi" :| []) NotDeprecated)
  ,(Gonm, Subtag 14969814322128093188, ScriptRecord ("Masaram Gondi" :| []) NotDeprecated)
  ,(Goth, Subtag 14969866755088842756, ScriptRecord ("Gothic" :| []) NotDeprecated)
  ,(Gran, Subtag 14973077741358809092, ScriptRecord ("Grantha" :| []) NotDeprecated)
  ,(Grek, Subtag 14973112719572467716, ScriptRecord ("Greek" :| []) NotDeprecated)
  ,(Gujr, Subtag 14976534880794443780, ScriptRecord ("Gujarati" :| []) NotDeprecated)
  ,(Guru, Subtag 14976605455697051652, ScriptRecord ("Gurmukhi" :| []) NotDeprecated)
  ,(Hanb, Subtag 15098166155593908228, ScriptRecord ("Han with Bopomofo (alias for Han + Bopomofo)" :| []) NotDeprecated)
  ,(Hang, Subtag 15098166499191291908, ScriptRecord ("Hangul" :| ["Hang\365l","Hangeul"]) NotDeprecated)
  ,(Hani, Subtag 15098166636630245380, ScriptRecord ("Han" :| ["Hanzi","Kanji","Hanja"]) NotDeprecated)
  ,(Hano, Subtag 15098167048947105796, ScriptRecord ("Hanunoo" :| ["Hanun\243o"]) NotDeprecated)
  ,(Hans, Subtag 15098167323825012740, ScriptRecord ("Han (Simplified variant)" :| []) NotDeprecated)
  ,(Hant, Subtag 15098167392544489476, ScriptRecord ("Han (Traditional variant)" :| []) NotDeprecated)
  ,(Hatr, Subtag 15098220031663669252, ScriptRecord ("Hatran" :| []) NotDeprecated)
  ,(Hebr, Subtag 15102565301616640004, ScriptRecord ("Hebrew" :| []) NotDeprecated)
  ,(Hira, Subtag 15107208470501261316, ScriptRecord ("Hiragana" :| []) NotDeprecated)
  ,(Hluw, Subtag 15110614070329344004, ScriptRecord ("Anatolian Hieroglyphs" :| ["Luwian Hieroglyphs","Hittite Hieroglyphs"]) NotDeprecated)
  ,(Hmng, Subtag 15111677298073403396, ScriptRecord ("Pahawh Hmong" :| []) NotDeprecated)
  ,(Hmnp, Subtag 15111677916548694020, ScriptRecord ("Nyiakeng Puachue Hmong" :| []) NotDeprecated)
  ,(Hrkt, Subtag 15117281302681747460, ScriptRecord ("Japanese syllabaries (alias for Hiragana + Katakana)" :| []) NotDeprecated)
  ,(Hung, Subtag 15120684497328144388, ScriptRecord ("Old Hungarian" :| ["Hungarian Runic"]) NotDeprecated)
  ,(Inds, Subtag 15256831249759600644, ScriptRecord ("Indus" :| ["Harappan"]) NotDeprecated)
  ,(Ital, Subtag 15263559779885252612, ScriptRecord ("Old Italic (Etruscan, Oscan, etc.)" :| []) NotDeprecated)
  ,(Jamo, Subtag 15386388629005795332, ScriptRecord ("Jamo (alias for Jamo subset of Hangul)" :| []) NotDeprecated)
  ,(Java, Subtag 15386466831770320900, ScriptRecord ("Javanese" :| []) NotDeprecated)
  ,(Jpan, Subtag 15403171505772691460, ScriptRecord ("Japanese (alias for Han + Hiragana + Katakana)" :| []) NotDeprecated)
  ,(Jurc, Subtag 15408949782974038020, ScriptRecord ("Jurchen" :| []) NotDeprecated)
  ,(Kali, Subtag 15530494608671768580, ScriptRecord ("Kayah Li" :| []) NotDeprecated)
  ,(Kana, Subtag 15530511651101999108, ScriptRecord ("Katakana" :| []) NotDeprecated)
  ,(Khar, Subtag 15538279769471713284, ScriptRecord ("Kharoshthi" :| []) NotDeprecated)
  ,(Khmr, Subtag 15538385322587979780, ScriptRecord ("Khmer" :| []) NotDeprecated)
  ,(Khoj, Subtag 15538402365018210308, ScriptRecord ("Khojki" :| []) NotDeprecated)
  ,(Kitl, Subtag 15539572382829117444, ScriptRecord ("Khitan large script" :| []) NotDeprecated)
  ,(Kits, Subtag 15539572863865454596, ScriptRecord ("Khitan small script" :| []) NotDeprecated)
  ,(Knda, Subtag 15545060388960731140, ScriptRecord ("Kannada" :| []) NotDeprecated)
  ,(Kore, Subtag 15546309709047791620, ScriptRecord ("Korean (alias for Hangul + Han)" :| []) NotDeprecated)
  ,(Kpel, Subtag 15547321740781682692, ScriptRecord ("Kpelle" :| []) NotDeprecated)
  ,(Kthi, Subtag 15551851522529689604, ScriptRecord ("Kaithi" :| []) NotDeprecated)
  ,(Lana, Subtag 15674626839177854980, ScriptRecord ("Tai Tham" :| ["Lanna"]) NotDeprecated)
  ,(Laoo, Subtag 15674636597343551492, ScriptRecord ("Lao" :| []) NotDeprecated)
  ,(Latf, Subtag 15674679959333371908, ScriptRecord ("Latin (Fraktur variant)" :| []) NotDeprecated)
  ,(Latg, Subtag 15674680028052848644, ScriptRecord ("Latin (Gaelic variant)" :| []) NotDeprecated)
  ,(Latn, Subtag 15674680509089185796, ScriptRecord ("Latin" :| []) NotDeprecated)
  ,(Leke, Subtag 15679104325404065796, ScriptRecord ("Leke" :| []) NotDeprecated)
  ,(Lepc, Subtag 15679148168430223364, ScriptRecord ("Lepcha" :| ["R\243ng"]) NotDeprecated)
  ,(Limb, Subtag 15683625311059050500, ScriptRecord ("Limbu" :| []) NotDeprecated)
  ,(Lina, Subtag 15683634038432595972, ScriptRecord ("Linear A" :| []) NotDeprecated)
  ,(Linb, Subtag 15683634107152072708, ScriptRecord ("Linear B" :| []) NotDeprecated)
  ,(Lisu, Subtag 15683679393287241732, ScriptRecord ("Lisu" :| ["Fraser"]) NotDeprecated)
  ,(Loma, Subtag 15690380641780629508, ScriptRecord ("Loma" :| []) NotDeprecated)
  ,(Lyci, Subtag 15701552229674647556, ScriptRecord ("Lycian" :| []) NotDeprecated)
  ,(Lydi, Subtag 15701561025767669764, ScriptRecord ("Lydian" :| []) NotDeprecated)
  ,(Mahj, Subtag 15818689869170868228, ScriptRecord ("Mahajani" :| []) NotDeprecated)
  ,(Maka, Subtag 15818715638974644228, ScriptRecord ("Makasar" :| []) NotDeprecated)
  ,(Mand, Subtag 15818742233412141060, ScriptRecord ("Mandaic" :| ["Mandaean"]) NotDeprecated)
  ,(Mani, Subtag 15818742577009524740, ScriptRecord ("Manichaean" :| []) NotDeprecated)
  ,(Marc, Subtag 15818777349064753156, ScriptRecord ("Marchen" :| []) NotDeprecated)
  ,(Maya, Subtag 15818838784276955140, ScriptRecord ("Mayan hieroglyphs" :| []) NotDeprecated)
  ,(Medf, Subtag 15823158009548242948, ScriptRecord ("Medefaidrin" :| ["Oberi Okaime","Oberi \390kaim\603"]) NotDeprecated)
  ,(Mend, Subtag 15823245833039511556, ScriptRecord ("Mende Kikakui" :| []) NotDeprecated)
  ,(Merc, Subtag 15823280948692123652, ScriptRecord ("Meroitic Cursive" :| []) NotDeprecated)
  ,(Mero, Subtag 15823281773325844484, ScriptRecord ("Meroitic Hieroglyphs" :| []) NotDeprecated)
  ,(Mlym, Subtag 15831224507885944836, ScriptRecord ("Malayalam" :| []) NotDeprecated)
  ,(Modi, Subtag 15834417214775099396, ScriptRecord ("Modi" :| ["Mo\7693\299"]) NotDeprecated)
  ,(Mong, Subtag 15834505038266368004, ScriptRecord ("Mongolian" :| []) NotDeprecated)
  ,(Moon, Subtag 15834514315395727364, ScriptRecord ("Moon" :| ["Moon code","Moon script","Moon type"]) NotDeprecated)
  ,(Mroo, Subtag 15837892083835731972, ScriptRecord ("Mro" :| ["Mru"]) NotDeprecated)
  ,(Mtei, Subtag 15840055510402334724, ScriptRecord ("Meitei Mayek" :| ["Meithei","Meetei"]) NotDeprecated)
  ,(Mult, Subtag 15841243738874576900, ScriptRecord ("Multani" :| []) NotDeprecated)
  ,(Mymr, Subtag 15845755997156016132, ScriptRecord ("Myanmar" :| ["Burmese"]) NotDeprecated)
  ,(Nand, Subtag 15962857421487996932, ScriptRecord ("Nandinagari" :| []) NotDeprecated)
  ,(Narb, Subtag 15962892468421132292, ScriptRecord ("Old North Arabian" :| ["Ancient North Arabian"]) NotDeprecated)
  ,(Nbat, Subtag 15963870071697178628, ScriptRecord ("Nabataean" :| []) NotDeprecated)
  ,(Newa, Subtag 15967439979794137092, ScriptRecord ("Newa" :| ["Newar","Newari","Nep\257la lipi"]) NotDeprecated)
  ,(Nkdb, Subtag 15974028322187247620, ScriptRecord ("Naxi Dongba" :| ["na\178\185\597i\179\179 to\179\179ba\178\185","Nakhi Tomba"]) NotDeprecated)
  ,(Nkgb, Subtag 15974054710466314244, ScriptRecord ("Naxi Geba" :| ["na\178\185\597i\179\179 g\652\178\185ba\178\185","'Na-'Khi \178Gg\335-\185baw","Nakhi Geba"]) NotDeprecated)
  ,(Nkoo, Subtag 15974125972563689476, ScriptRecord ("N\8217Ko" :| ["N'Ko"]) NotDeprecated)
  ,(Nshu, Subtag 15983072011484135428, ScriptRecord ("N\252shu" :| []) NotDeprecated)
  ,(Ogam, Subtag 16113614278270910468, ScriptRecord ("Ogham" :| []) NotDeprecated)
  ,(Olck, Subtag 16119261232552214532, ScriptRecord ("Ol Chiki" :| ["Ol Cemet'","Ol","Santali"]) NotDeprecated)
  ,(Orkh, Subtag 16126086794579017732, ScriptRecord ("Old Turkic" :| ["Orkhon Runic"]) NotDeprecated)
  ,(Orya, Subtag 16126209458844991492, ScriptRecord ("Oriya" :| ["Odia"]) NotDeprecated)
  ,(Osge, Subtag 16127177303955341316, ScriptRecord ("Osage" :| []) NotDeprecated)
  ,(Osma, Subtag 16127229805635567620, ScriptRecord ("Osmanya" :| []) NotDeprecated)
  ,(Ougr, Subtag 16129429997122224132, ScriptRecord ("Old Uyghur" :| []) NotDeprecated)
  ,(Palm, Subtag 16251070823928954884, ScriptRecord ("Palmyrene" :| []) NotDeprecated)
  ,(Pauc, Subtag 16251149301571387396, ScriptRecord ("Pau Cin Hau" :| []) NotDeprecated)
  ,(Pcun, Subtag 16253401857299316740, ScriptRecord ("Proto-Cuneiform" :| []) NotDeprecated)
  ,(Pelm, Subtag 16255574423556325380, ScriptRecord ("Proto-Elamite" :| []) NotDeprecated)
  ,(Perm, Subtag 16255627200114458628, ScriptRecord ("Old Permic" :| []) NotDeprecated)
  ,(Phag, Subtag 16258854953936748548, ScriptRecord ("Phags-pa" :| []) NotDeprecated)
  ,(Phli, Subtag 16258951848398946308, ScriptRecord ("Inscriptional Pahlavi" :| []) NotDeprecated)
  ,(Phlp, Subtag 16258952329435283460, ScriptRecord ("Psalter Pahlavi" :| []) NotDeprecated)
  ,(Phlv, Subtag 16258952741752143876, ScriptRecord ("Book Pahlavi" :| []) NotDeprecated)
  ,(Phnx, Subtag 16258970471377141764, ScriptRecord ("Phoenician" :| []) NotDeprecated)
  ,(Piqd, Subtag 16260121385173516292, ScriptRecord ("Klingon (KLI pIqaD)" :| []) NotDeprecated)
  ,(Plrd, Subtag 16263507880987066372, ScriptRecord ("Miao" :| ["Pollard"]) NotDeprecated)
  ,(Prti, Subtag 16270281216211550212, ScriptRecord ("Inscriptional Parthian" :| []) NotDeprecated)
  ,(Psin, Subtag 16271310702692532228, ScriptRecord ("Proto-Sinaitic" :| []) NotDeprecated)
  ,(Qaaa, Subtag 16395088430347845636, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaab, Subtag 16395088499067322372, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaac, Subtag 16395088567786799108, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaad, Subtag 16395088636506275844, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaae, Subtag 16395088705225752580, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaaf, Subtag 16395088773945229316, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaag, Subtag 16395088842664706052, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaah, Subtag 16395088911384182788, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaai, Subtag 16395088980103659524, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaaj, Subtag 16395089048823136260, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaak, Subtag 16395089117542612996, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaal, Subtag 16395089186262089732, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaam, Subtag 16395089254981566468, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaan, Subtag 16395089323701043204, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaao, Subtag 16395089392420519940, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaap, Subtag 16395089461139996676, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaaq, Subtag 16395089529859473412, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaar, Subtag 16395089598578950148, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaas, Subtag 16395089667298426884, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaat, Subtag 16395089736017903620, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaau, Subtag 16395089804737380356, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaav, Subtag 16395089873456857092, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaaw, Subtag 16395089942176333828, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaax, Subtag 16395090010895810564, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaay, Subtag 16395090079615287300, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaaz, Subtag 16395090148334764036, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qaba, Subtag 16395097226440867844, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabb, Subtag 16395097295160344580, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabc, Subtag 16395097363879821316, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabd, Subtag 16395097432599298052, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabe, Subtag 16395097501318774788, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabf, Subtag 16395097570038251524, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabg, Subtag 16395097638757728260, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabh, Subtag 16395097707477204996, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabi, Subtag 16395097776196681732, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabj, Subtag 16395097844916158468, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabk, Subtag 16395097913635635204, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabl, Subtag 16395097982355111940, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabm, Subtag 16395098051074588676, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabn, Subtag 16395098119794065412, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabo, Subtag 16395098188513542148, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabp, Subtag 16395098257233018884, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabq, Subtag 16395098325952495620, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabr, Subtag 16395098394671972356, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabs, Subtag 16395098463391449092, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabt, Subtag 16395098532110925828, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabu, Subtag 16395098600830402564, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabv, Subtag 16395098669549879300, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabw, Subtag 16395098738269356036, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Qabx, Subtag 16395098806988832772, ScriptRecord ("Private use" :| []) NotDeprecated)
  ,(Ranj, Subtag 16539318586108280836, ScriptRecord ("Ranjana" :| []) NotDeprecated)
  ,(Rjng, Subtag 16549451479111434244, ScriptRecord ("Rejang" :| ["Redjang","Kaganga"]) NotDeprecated)
  ,(Rohg, Subtag 16555028202087514116, ScriptRecord ("Hanifi Rohingya" :| []) NotDeprecated)
  ,(Roro, Subtag 16555116712773550084, ScriptRecord ("Rongorongo" :| []) NotDeprecated)
  ,(Runr, Subtag 16561837134000947204, ScriptRecord ("Runic" :| []) NotDeprecated)
  ,(Samr, Subtag 16683425527846928388, ScriptRecord ("Samaritan" :| []) NotDeprecated)
  ,(Sara, Subtag 16683468340080934916, ScriptRecord ("Sarati" :| []) NotDeprecated)
  ,(Sarb, Subtag 16683468408800411652, ScriptRecord ("Old South Arabian" :| []) NotDeprecated)
  ,(Saur, Subtag 16683495896591106052, ScriptRecord ("Saurashtra" :| []) NotDeprecated)
  ,(Sgnw, Subtag 16690190066978390020, ScriptRecord ("SignWriting" :| []) NotDeprecated)
  ,(Shaw, Subtag 16691201617675943940, ScriptRecord ("Shavian" :| ["Shaw"]) NotDeprecated)
  ,(Shrd, Subtag 16691349845587263492, ScriptRecord ("Sharada" :| ["\346\257rad\257"]) NotDeprecated)
  ,(Shui, Subtag 16691376577463713796, ScriptRecord ("Shuishu" :| []) NotDeprecated)
  ,(Sidd, Subtag 16692352600191795204, ScriptRecord ("Siddham" :| ["Siddha\7747","Siddham\257t\7771k\257"]) NotDeprecated)
  ,(Sind, Subtag 16692440561122017284, ScriptRecord ("Khudawadi" :| ["Sindhi"]) NotDeprecated)
  ,(Sinh, Subtag 16692440835999924228, ScriptRecord ("Sinhala" :| []) NotDeprecated)
  ,(Sogd, Subtag 16699134387911917572, ScriptRecord ("Sogdian" :| []) NotDeprecated)
  ,(Sogo, Subtag 16699135143826161668, ScriptRecord ("Old Sogdian" :| []) NotDeprecated)
  ,(Sora, Subtag 16699230938776731652, ScriptRecord ("Sora Sompeng" :| []) NotDeprecated)
  ,(Soyo, Subtag 16699293473500561412, ScriptRecord ("Soyombo" :| []) NotDeprecated)
  ,(Sund, Subtag 16705951360004128772, ScriptRecord ("Sundanese" :| []) NotDeprecated)
  ,(Sylo, Subtag 16710438123359698948, ScriptRecord ("Syloti Nagri" :| []) NotDeprecated)
  ,(Syrc, Subtag 16710490075284111364, ScriptRecord ("Syriac" :| []) NotDeprecated)
  ,(Syre, Subtag 16710490212723064836, ScriptRecord ("Syriac (Estrangelo variant)" :| []) NotDeprecated)
  ,(Syrj, Subtag 16710490556320448516, ScriptRecord ("Syriac (Western variant)" :| []) NotDeprecated)
  ,(Syrn, Subtag 16710490831198355460, ScriptRecord ("Syriac (Eastern variant)" :| []) NotDeprecated)
  ,(Tagb, Subtag 16827486839853023236, ScriptRecord ("Tagbanwa" :| []) NotDeprecated)
  ,(Takr, Subtag 16827523123736739844, ScriptRecord ("Takri" :| ["\7788\257kr\299","\7788\257\7749kr\299"]) NotDeprecated)
  ,(Tale, Subtag 16827531026476564484, ScriptRecord ("Tai Le" :| []) NotDeprecated)
  ,(Talu, Subtag 16827532125988192260, ScriptRecord ("New Tai Lue" :| []) NotDeprecated)
  ,(Taml, Subtag 16827540303605923844, ScriptRecord ("Tamil" :| []) NotDeprecated)
  ,(Tang, Subtag 16827548756101562372, ScriptRecord ("Tangut" :| []) NotDeprecated)
  ,(Tavt, Subtag 16827620018198937604, ScriptRecord ("Tai Viet" :| []) NotDeprecated)
  ,(Telu, Subtag 16832035725615562756, ScriptRecord ("Telugu" :| []) NotDeprecated)
  ,(Teng, Subtag 16832052355728932868, ScriptRecord ("Tengwar" :| []) NotDeprecated)
  ,(Tfng, Subtag 16833178255635775492, ScriptRecord ("Tifinagh" :| ["Berber"]) NotDeprecated)
  ,(Tglg, Subtag 16834286563356573700, ScriptRecord ("Tagalog" :| ["Baybayin","Alibata"]) NotDeprecated)
  ,(Thaa, Subtag 16835315293923311620, ScriptRecord ("Thaana" :| []) NotDeprecated)
  ,(Thai, Subtag 16835315843679125508, ScriptRecord ("Thai" :| []) NotDeprecated)
  ,(Tibt, Subtag 16836451295593234436, ScriptRecord ("Tibetan" :| []) NotDeprecated)
  ,(Tirh, Subtag 16836591208447868932, ScriptRecord ("Tirhuta" :| []) NotDeprecated)
  ,(Toto, Subtag 16843364681111306244, ScriptRecord ("Toto" :| []) NotDeprecated)
  ,(Ugar, Subtag 16978305750323429380, ScriptRecord ("Ugaritic" :| []) NotDeprecated)
  ,(Vaii, Subtag 17115735289227116548, ScriptRecord ("Vai" :| []) NotDeprecated)
  ,(Visp, Subtag 17124830930448416772, ScriptRecord ("Visible Speech" :| []) NotDeprecated)
  ,(Wara, Subtag 17259929092384358404, ScriptRecord ("Warang Citi" :| ["Varang Kshiti"]) NotDeprecated)
  ,(Wcho, Subtag 17262093893340495876, ScriptRecord ("Wancho" :| []) NotDeprecated)
  ,(Wole, Subtag 17275639189399928836, ScriptRecord ("Woleai" :| []) NotDeprecated)
  ,(Xpeo, Subtag 17420819391926239236, ScriptRecord ("Old Persian" :| []) NotDeprecated)
  ,(Xsux, Subtag 17424338447610413060, ScriptRecord ("Sumero-Akkadian cuneiform" :| []) NotDeprecated)
  ,(Yezi, Subtag 17552733986663432196, ScriptRecord ("Yezidi" :| []) NotDeprecated)
  ,(Yiii, Subtag 17557088052709425156, ScriptRecord ("Yi" :| []) NotDeprecated)
  ,(Zanb, Subtag 17692239540959313924, ScriptRecord ("Zanabazar Square" :| ["Zanabazarin D\246rb\246ljin Useg","Xewtee D\246rb\246ljin Bicig","Horizontal Square Script"]) NotDeprecated)
  ,(Zinh, Subtag 17701247152530915332, ScriptRecord ("Code for inherited script" :| []) NotDeprecated)
  ,(Zmth, Subtag 17705803528716419076, ScriptRecord ("Mathematical notation" :| []) NotDeprecated)
  ,(Zsye, Subtag 17712602702464155652, ScriptRecord ("Symbols (Emoji variant)" :| []) NotDeprecated)
  ,(Zsym, Subtag 17712603252219969540, ScriptRecord ("Symbols" :| []) NotDeprecated)
  ,(Zxxx, Subtag 17718224711575404548, ScriptRecord ("Code for unwritten documents" :| []) NotDeprecated)
  ,(Zyyy, Subtag 17719359476294746116, ScriptRecord ("Code for undetermined script" :| []) NotDeprecated)
  ,(Zzzz, Subtag 17720494241014087684, ScriptRecord ("Code for uncoded script" :| []) NotDeprecated)]

lookupScriptDetails :: Script -> (Subtag, ScriptRecord)
lookupScriptDetails x = case HM.lookup x tab of
  Nothing -> error $ "internal invariant violated: subtag " <> show x <> " does not have an associated record"
  Just r -> r
  where
    tab = HM.fromList $ (\(a, b, c) -> (a, (b, c))) <$> scriptTable

lookupSubtagScript :: Subtag -> Maybe Script
lookupSubtagScript = flip HM.lookup tab
  where
    tab = HM.fromList $ (\(a, b, _) -> (b, a)) <$> scriptTable