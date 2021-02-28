-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}

module Text.LanguageTag.Internal.BCP47.Region where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..), hashUsing)
import Text.LanguageTag.Internal.BCP47.Syntax (Subtag(..), unwrapSubtag)
import qualified Data.HashMap.Strict as HM

-- | The BCP47 region tags as of 2021-02-23.
data Region
  = R001 -- ^ @001@. Description: World.
  | R002 -- ^ @002@. Description: Africa.
  | R003 -- ^ @003@. Description: North America.
  | R005 -- ^ @005@. Description: South America.
  | R009 -- ^ @009@. Description: Oceania.
  | R011 -- ^ @011@. Description: Western Africa.
  | R013 -- ^ @013@. Description: Central America.
  | R014 -- ^ @014@. Description: Eastern Africa.
  | R015 -- ^ @015@. Description: Northern Africa.
  | R017 -- ^ @017@. Description: Middle Africa.
  | R018 -- ^ @018@. Description: Southern Africa.
  | R019 -- ^ @019@. Description: Americas.
  | R021 -- ^ @021@. Description: Northern America.
  | R029 -- ^ @029@. Description: Caribbean.
  | R030 -- ^ @030@. Description: Eastern Asia.
  | R034 -- ^ @034@. Description: Southern Asia.
  | R035 -- ^ @035@. Description: South-Eastern Asia.
  | R039 -- ^ @039@. Description: Southern Europe.
  | R053 -- ^ @053@. Description: Australia and New Zealand.
  | R054 -- ^ @054@. Description: Melanesia.
  | R057 -- ^ @057@. Description: Micronesia.
  | R061 -- ^ @061@. Description: Polynesia.
  | R142 -- ^ @142@. Description: Asia.
  | R143 -- ^ @143@. Description: Central Asia.
  | R145 -- ^ @145@. Description: Western Asia.
  | R150 -- ^ @150@. Description: Europe.
  | R151 -- ^ @151@. Description: Eastern Europe.
  | R154 -- ^ @154@. Description: Northern Europe.
  | R155 -- ^ @155@. Description: Western Europe.
  | R202 -- ^ @202@. Description: Sub-Saharan Africa.
  | R419 -- ^ @419@. Description: Latin America and the Caribbean.
  | AA -- ^ @AA@. Description: Private use.
  | AC -- ^ @AC@. Description: Ascension Island.
  | AD -- ^ @AD@. Description: Andorra.
  | AE -- ^ @AE@. Description: United Arab Emirates.
  | AF -- ^ @AF@. Description: Afghanistan.
  | AG -- ^ @AG@. Description: Antigua and Barbuda.
  | AI -- ^ @AI@. Description: Anguilla.
  | AL -- ^ @AL@. Description: Albania.
  | AM -- ^ @AM@. Description: Armenia.
  | AN -- ^ @AN@. Description: Netherlands Antilles. Deprecated.
  | AO -- ^ @AO@. Description: Angola.
  | AQ -- ^ @AQ@. Description: Antarctica.
  | AR -- ^ @AR@. Description: Argentina.
  | AS -- ^ @AS@. Description: American Samoa.
  | AT -- ^ @AT@. Description: Austria.
  | AU -- ^ @AU@. Description: Australia.
  | AW -- ^ @AW@. Description: Aruba.
  | AX -- ^ @AX@. Description: Åland Islands.
  | AZ -- ^ @AZ@. Description: Azerbaijan.
  | BA -- ^ @BA@. Description: Bosnia and Herzegovina.
  | BB -- ^ @BB@. Description: Barbados.
  | BD -- ^ @BD@. Description: Bangladesh.
  | BE -- ^ @BE@. Description: Belgium.
  | BF -- ^ @BF@. Description: Burkina Faso.
  | BG -- ^ @BG@. Description: Bulgaria.
  | BH -- ^ @BH@. Description: Bahrain.
  | BI -- ^ @BI@. Description: Burundi.
  | BJ -- ^ @BJ@. Description: Benin.
  | BL -- ^ @BL@. Description: Saint Barthélemy.
  | BM -- ^ @BM@. Description: Bermuda.
  | BN -- ^ @BN@. Description: Brunei Darussalam.
  | BO -- ^ @BO@. Description: Bolivia.
  | BQ -- ^ @BQ@. Description: Bonaire, Sint Eustatius and Saba.
  | BR -- ^ @BR@. Description: Brazil.
  | BS -- ^ @BS@. Description: Bahamas.
  | BT -- ^ @BT@. Description: Bhutan.
  | BU -- ^ @BU@. Description: Burma. Deprecated. Preferred value: MM.
  | BV -- ^ @BV@. Description: Bouvet Island.
  | BW -- ^ @BW@. Description: Botswana.
  | BY -- ^ @BY@. Description: Belarus.
  | BZ -- ^ @BZ@. Description: Belize.
  | CA -- ^ @CA@. Description: Canada.
  | CC -- ^ @CC@. Description: Cocos (Keeling) Islands.
  | CD -- ^ @CD@. Description: The Democratic Republic of the Congo.
  | CF -- ^ @CF@. Description: Central African Republic.
  | CG -- ^ @CG@. Description: Congo.
  | CH -- ^ @CH@. Description: Switzerland.
  | CI -- ^ @CI@. Description: Côte d\'Ivoire.
  | CK -- ^ @CK@. Description: Cook Islands.
  | CL -- ^ @CL@. Description: Chile.
  | CM -- ^ @CM@. Description: Cameroon.
  | CN -- ^ @CN@. Description: China.
  | CO -- ^ @CO@. Description: Colombia.
  | CP -- ^ @CP@. Description: Clipperton Island.
  | CR -- ^ @CR@. Description: Costa Rica.
  | CS -- ^ @CS@. Description: Serbia and Montenegro. Deprecated.
  | CU -- ^ @CU@. Description: Cuba.
  | CV -- ^ @CV@. Description: Cabo Verde; Cape Verde.
  | CW -- ^ @CW@. Description: Curaçao.
  | CX -- ^ @CX@. Description: Christmas Island.
  | CY -- ^ @CY@. Description: Cyprus.
  | CZ -- ^ @CZ@. Description: Czechia; Czech Republic.
  | DD -- ^ @DD@. Description: German Democratic Republic. Deprecated. Preferred value: DE.
  | DE -- ^ @DE@. Description: Germany.
  | DG -- ^ @DG@. Description: Diego Garcia.
  | DJ -- ^ @DJ@. Description: Djibouti.
  | DK -- ^ @DK@. Description: Denmark.
  | DM -- ^ @DM@. Description: Dominica.
  | DO -- ^ @DO@. Description: Dominican Republic.
  | DZ -- ^ @DZ@. Description: Algeria.
  | EA -- ^ @EA@. Description: Ceuta, Melilla.
  | EC -- ^ @EC@. Description: Ecuador.
  | EE -- ^ @EE@. Description: Estonia.
  | EG -- ^ @EG@. Description: Egypt.
  | EH -- ^ @EH@. Description: Western Sahara.
  | ER -- ^ @ER@. Description: Eritrea.
  | ES -- ^ @ES@. Description: Spain.
  | ET -- ^ @ET@. Description: Ethiopia.
  | EU -- ^ @EU@. Description: European Union.
  | EZ -- ^ @EZ@. Description: Eurozone.
  | FI -- ^ @FI@. Description: Finland.
  | FJ -- ^ @FJ@. Description: Fiji.
  | FK -- ^ @FK@. Description: Falkland Islands (Malvinas).
  | FM -- ^ @FM@. Description: Federated States of Micronesia.
  | FO -- ^ @FO@. Description: Faroe Islands.
  | FR -- ^ @FR@. Description: France.
  | FX -- ^ @FX@. Description: Metropolitan France. Deprecated. Preferred value: FR.
  | GA -- ^ @GA@. Description: Gabon.
  | GB -- ^ @GB@. Description: United Kingdom.
  | GD -- ^ @GD@. Description: Grenada.
  | GE -- ^ @GE@. Description: Georgia.
  | GF -- ^ @GF@. Description: French Guiana.
  | GG -- ^ @GG@. Description: Guernsey.
  | GH -- ^ @GH@. Description: Ghana.
  | GI -- ^ @GI@. Description: Gibraltar.
  | GL -- ^ @GL@. Description: Greenland.
  | GM -- ^ @GM@. Description: Gambia.
  | GN -- ^ @GN@. Description: Guinea.
  | GP -- ^ @GP@. Description: Guadeloupe.
  | GQ -- ^ @GQ@. Description: Equatorial Guinea.
  | GR -- ^ @GR@. Description: Greece.
  | GS -- ^ @GS@. Description: South Georgia and the South Sandwich Islands.
  | GT -- ^ @GT@. Description: Guatemala.
  | GU -- ^ @GU@. Description: Guam.
  | GW -- ^ @GW@. Description: Guinea-Bissau.
  | GY -- ^ @GY@. Description: Guyana.
  | HK -- ^ @HK@. Description: Hong Kong.
  | HM -- ^ @HM@. Description: Heard Island and McDonald Islands.
  | HN -- ^ @HN@. Description: Honduras.
  | HR -- ^ @HR@. Description: Croatia.
  | HT -- ^ @HT@. Description: Haiti.
  | HU -- ^ @HU@. Description: Hungary.
  | IC -- ^ @IC@. Description: Canary Islands.
  | ID -- ^ @ID@. Description: Indonesia.
  | IE -- ^ @IE@. Description: Ireland.
  | IL -- ^ @IL@. Description: Israel.
  | IM -- ^ @IM@. Description: Isle of Man.
  | IN -- ^ @IN@. Description: India.
  | IO -- ^ @IO@. Description: British Indian Ocean Territory.
  | IQ -- ^ @IQ@. Description: Iraq.
  | IR -- ^ @IR@. Description: Islamic Republic of Iran.
  | IS -- ^ @IS@. Description: Iceland.
  | IT -- ^ @IT@. Description: Italy.
  | JE -- ^ @JE@. Description: Jersey.
  | JM -- ^ @JM@. Description: Jamaica.
  | JO -- ^ @JO@. Description: Jordan.
  | JP -- ^ @JP@. Description: Japan.
  | KE -- ^ @KE@. Description: Kenya.
  | KG -- ^ @KG@. Description: Kyrgyzstan.
  | KH -- ^ @KH@. Description: Cambodia.
  | KI -- ^ @KI@. Description: Kiribati.
  | KM -- ^ @KM@. Description: Comoros.
  | KN -- ^ @KN@. Description: Saint Kitts and Nevis.
  | KP -- ^ @KP@. Description: Democratic People\'s Republic of Korea.
  | KR -- ^ @KR@. Description: Republic of Korea.
  | KW -- ^ @KW@. Description: Kuwait.
  | KY -- ^ @KY@. Description: Cayman Islands.
  | KZ -- ^ @KZ@. Description: Kazakhstan.
  | LA -- ^ @LA@. Description: Lao People\'s Democratic Republic.
  | LB -- ^ @LB@. Description: Lebanon.
  | LC -- ^ @LC@. Description: Saint Lucia.
  | LI -- ^ @LI@. Description: Liechtenstein.
  | LK -- ^ @LK@. Description: Sri Lanka.
  | LR -- ^ @LR@. Description: Liberia.
  | LS -- ^ @LS@. Description: Lesotho.
  | LT -- ^ @LT@. Description: Lithuania.
  | LU -- ^ @LU@. Description: Luxembourg.
  | LV -- ^ @LV@. Description: Latvia.
  | LY -- ^ @LY@. Description: Libya.
  | MA -- ^ @MA@. Description: Morocco.
  | MC -- ^ @MC@. Description: Monaco.
  | MD -- ^ @MD@. Description: Moldova.
  | ME -- ^ @ME@. Description: Montenegro.
  | MF -- ^ @MF@. Description: Saint Martin (French part).
  | MG -- ^ @MG@. Description: Madagascar.
  | MH -- ^ @MH@. Description: Marshall Islands.
  | MK -- ^ @MK@. Description: North Macedonia.
  | ML -- ^ @ML@. Description: Mali.
  | MM -- ^ @MM@. Description: Myanmar.
  | MN -- ^ @MN@. Description: Mongolia.
  | MO -- ^ @MO@. Description: Macao.
  | MP -- ^ @MP@. Description: Northern Mariana Islands.
  | MQ -- ^ @MQ@. Description: Martinique.
  | MR -- ^ @MR@. Description: Mauritania.
  | MS -- ^ @MS@. Description: Montserrat.
  | MT -- ^ @MT@. Description: Malta.
  | MU -- ^ @MU@. Description: Mauritius.
  | MV -- ^ @MV@. Description: Maldives.
  | MW -- ^ @MW@. Description: Malawi.
  | MX -- ^ @MX@. Description: Mexico.
  | MY -- ^ @MY@. Description: Malaysia.
  | MZ -- ^ @MZ@. Description: Mozambique.
  | NA -- ^ @NA@. Description: Namibia.
  | NC -- ^ @NC@. Description: New Caledonia.
  | NE -- ^ @NE@. Description: Niger.
  | NF -- ^ @NF@. Description: Norfolk Island.
  | NG -- ^ @NG@. Description: Nigeria.
  | NI -- ^ @NI@. Description: Nicaragua.
  | NL -- ^ @NL@. Description: Netherlands.
  | NO -- ^ @NO@. Description: Norway.
  | NP -- ^ @NP@. Description: Nepal.
  | NR -- ^ @NR@. Description: Nauru.
  | NT -- ^ @NT@. Description: Neutral Zone. Deprecated.
  | NU -- ^ @NU@. Description: Niue.
  | NZ -- ^ @NZ@. Description: New Zealand.
  | OM -- ^ @OM@. Description: Oman.
  | PA -- ^ @PA@. Description: Panama.
  | PE -- ^ @PE@. Description: Peru.
  | PF -- ^ @PF@. Description: French Polynesia.
  | PG -- ^ @PG@. Description: Papua New Guinea.
  | PH -- ^ @PH@. Description: Philippines.
  | PK -- ^ @PK@. Description: Pakistan.
  | PL -- ^ @PL@. Description: Poland.
  | PM -- ^ @PM@. Description: Saint Pierre and Miquelon.
  | PN -- ^ @PN@. Description: Pitcairn.
  | PR -- ^ @PR@. Description: Puerto Rico.
  | PS -- ^ @PS@. Description: State of Palestine.
  | PT -- ^ @PT@. Description: Portugal.
  | PW -- ^ @PW@. Description: Palau.
  | PY -- ^ @PY@. Description: Paraguay.
  | QA -- ^ @QA@. Description: Qatar.
  | QM -- ^ @QM@. Description: Private use.
  | QN -- ^ @QN@. Description: Private use.
  | QO -- ^ @QO@. Description: Private use.
  | QP -- ^ @QP@. Description: Private use.
  | QQ -- ^ @QQ@. Description: Private use.
  | QR -- ^ @QR@. Description: Private use.
  | QS -- ^ @QS@. Description: Private use.
  | QT -- ^ @QT@. Description: Private use.
  | QU -- ^ @QU@. Description: Private use.
  | QV -- ^ @QV@. Description: Private use.
  | QW -- ^ @QW@. Description: Private use.
  | QX -- ^ @QX@. Description: Private use.
  | QY -- ^ @QY@. Description: Private use.
  | QZ -- ^ @QZ@. Description: Private use.
  | RE -- ^ @RE@. Description: Réunion.
  | RO -- ^ @RO@. Description: Romania.
  | RS -- ^ @RS@. Description: Serbia.
  | RU -- ^ @RU@. Description: Russian Federation.
  | RW -- ^ @RW@. Description: Rwanda.
  | SA -- ^ @SA@. Description: Saudi Arabia.
  | SB -- ^ @SB@. Description: Solomon Islands.
  | SC -- ^ @SC@. Description: Seychelles.
  | SD -- ^ @SD@. Description: Sudan.
  | SE -- ^ @SE@. Description: Sweden.
  | SG -- ^ @SG@. Description: Singapore.
  | SH -- ^ @SH@. Description: Saint Helena, Ascension and Tristan da Cunha.
  | SI -- ^ @SI@. Description: Slovenia.
  | SJ -- ^ @SJ@. Description: Svalbard and Jan Mayen.
  | SK -- ^ @SK@. Description: Slovakia.
  | SL -- ^ @SL@. Description: Sierra Leone.
  | SM -- ^ @SM@. Description: San Marino.
  | SN -- ^ @SN@. Description: Senegal.
  | SO -- ^ @SO@. Description: Somalia.
  | SR -- ^ @SR@. Description: Suriname.
  | SS -- ^ @SS@. Description: South Sudan.
  | ST -- ^ @ST@. Description: Sao Tome and Principe.
  | SU -- ^ @SU@. Description: Union of Soviet Socialist Republics. Deprecated.
  | SV -- ^ @SV@. Description: El Salvador.
  | SX -- ^ @SX@. Description: Sint Maarten (Dutch part).
  | SY -- ^ @SY@. Description: Syrian Arab Republic.
  | SZ -- ^ @SZ@. Description: Eswatini; eSwatini; Swaziland.
  | TA -- ^ @TA@. Description: Tristan da Cunha.
  | TC -- ^ @TC@. Description: Turks and Caicos Islands.
  | TD -- ^ @TD@. Description: Chad.
  | TF -- ^ @TF@. Description: French Southern Territories.
  | TG -- ^ @TG@. Description: Togo.
  | TH -- ^ @TH@. Description: Thailand.
  | TJ -- ^ @TJ@. Description: Tajikistan.
  | TK -- ^ @TK@. Description: Tokelau.
  | TL -- ^ @TL@. Description: Timor-Leste.
  | TM -- ^ @TM@. Description: Turkmenistan.
  | TN -- ^ @TN@. Description: Tunisia.
  | TO -- ^ @TO@. Description: Tonga.
  | TP -- ^ @TP@. Description: East Timor. Deprecated. Preferred value: TL.
  | TR -- ^ @TR@. Description: Turkey.
  | TT -- ^ @TT@. Description: Trinidad and Tobago.
  | TV -- ^ @TV@. Description: Tuvalu.
  | TW -- ^ @TW@. Description: Taiwan, Province of China.
  | TZ -- ^ @TZ@. Description: United Republic of Tanzania.
  | UA -- ^ @UA@. Description: Ukraine.
  | UG -- ^ @UG@. Description: Uganda.
  | UM -- ^ @UM@. Description: United States Minor Outlying Islands.
  | UN -- ^ @UN@. Description: United Nations.
  | US -- ^ @US@. Description: United States.
  | UY -- ^ @UY@. Description: Uruguay.
  | UZ -- ^ @UZ@. Description: Uzbekistan.
  | VA -- ^ @VA@. Description: Holy See (Vatican City State).
  | VC -- ^ @VC@. Description: Saint Vincent and the Grenadines.
  | VE -- ^ @VE@. Description: Venezuela.
  | VG -- ^ @VG@. Description: British Virgin Islands.
  | VI -- ^ @VI@. Description: U.S. Virgin Islands.
  | VN -- ^ @VN@. Description: Viet Nam.
  | VU -- ^ @VU@. Description: Vanuatu.
  | WF -- ^ @WF@. Description: Wallis and Futuna.
  | WS -- ^ @WS@. Description: Samoa.
  | XA -- ^ @XA@. Description: Private use.
  | XB -- ^ @XB@. Description: Private use.
  | XC -- ^ @XC@. Description: Private use.
  | XD -- ^ @XD@. Description: Private use.
  | XE -- ^ @XE@. Description: Private use.
  | XF -- ^ @XF@. Description: Private use.
  | XG -- ^ @XG@. Description: Private use.
  | XH -- ^ @XH@. Description: Private use.
  | XI -- ^ @XI@. Description: Private use.
  | XJ -- ^ @XJ@. Description: Private use.
  | XK -- ^ @XK@. Description: Private use.
  | XL -- ^ @XL@. Description: Private use.
  | XM -- ^ @XM@. Description: Private use.
  | XN -- ^ @XN@. Description: Private use.
  | XO -- ^ @XO@. Description: Private use.
  | XP -- ^ @XP@. Description: Private use.
  | XQ -- ^ @XQ@. Description: Private use.
  | XR -- ^ @XR@. Description: Private use.
  | XS -- ^ @XS@. Description: Private use.
  | XT -- ^ @XT@. Description: Private use.
  | XU -- ^ @XU@. Description: Private use.
  | XV -- ^ @XV@. Description: Private use.
  | XW -- ^ @XW@. Description: Private use.
  | XX -- ^ @XX@. Description: Private use.
  | XY -- ^ @XY@. Description: Private use.
  | XZ -- ^ @XZ@. Description: Private use.
  | YD -- ^ @YD@. Description: Democratic Yemen. Deprecated. Preferred value: YE.
  | YE -- ^ @YE@. Description: Yemen.
  | YT -- ^ @YT@. Description: Mayotte.
  | YU -- ^ @YU@. Description: Yugoslavia. Deprecated.
  | ZA -- ^ @ZA@. Description: South Africa.
  | ZM -- ^ @ZM@. Description: Zambia.
  | ZR -- ^ @ZR@. Description: Zaire. Deprecated. Preferred value: CD.
  | ZW -- ^ @ZW@. Description: Zimbabwe.
  | ZZ -- ^ @ZZ@. Description: Private use.

  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData Region where
  rnf a = seq a ()

instance Hashable Region where
  hashWithSalt = hashUsing fromEnum

parseRegion :: Subtag -> Maybe Region
parseRegion = flip HM.lookup table . unwrapSubtag
  where
    table = HM.fromList
      [ (6972003231727616003, R001)
      , (6972012027820638211, R002)
      , (6972020823913660419, R003)
      , (6972038416099704835, R005)
      , (6972073600471793667, R009)
      , (6973129131634458627, R011)
      , (6973146723820503043, R013)
      , (6973155519913525251, R014)
      , (6973164316006547459, R015)
      , (6973181908192591875, R017)
      , (6973190704285614083, R018)
      , (6973199500378636291, R019)
      , (6974255031541301251, R021)
      , (6974325400285478915, R029)
      , (6975372135355121667, R030)
      , (6975407319727210499, R034)
      , (6975416115820232707, R035)
      , (6975451300192321539, R039)
      , (6977650323447873539, R053)
      , (6977659119540895747, R054)
      , (6977685507819962371, R057)
      , (6978758631168671747, R061)
      , (7120630815523864579, R142)
      , (7120639611616886787, R143)
      , (7120657203802931203, R145)
      , (7121739123244662787, R150)
      , (7121747919337684995, R151)
      , (7121774307616751619, R154)
      , (7121783103709773827, R155)
      , (7260242403972349955, R202)
      , (7549660252682059779, R419)
      , (14088385534321754114, AA)
      , (14090637334135439362, AC)
      , (14091763234042281986, AD)
      , (14092889133949124610, AE)
      , (14094015033855967234, AF)
      , (14095140933762809858, AG)
      , (14097392733576495106, AI)
      , (14100770433297022978, AL)
      , (14101896333203865602, AM)
      , (14103022233110708226, AN)
      , (14104148133017550850, AO)
      , (14106399932831236098, AQ)
      , (14107525832738078722, AR)
      , (14108651732644921346, AS)
      , (14109777632551763970, AT)
      , (14110903532458606594, AU)
      , (14113155332272291842, AW)
      , (14114281232179134466, AX)
      , (14116533031992819714, AZ)
      , (14232500722397609986, BA)
      , (14233626622304452610, BB)
      , (14235878422118137858, BD)
      , (14237004322024980482, BE)
      , (14238130221931823106, BF)
      , (14239256121838665730, BG)
      , (14240382021745508354, BH)
      , (14241507921652350978, BI)
      , (14242633821559193602, BJ)
      , (14244885621372878850, BL)
      , (14246011521279721474, BM)
      , (14247137421186564098, BN)
      , (14248263321093406722, BO)
      , (14250515120907091970, BQ)
      , (14251641020813934594, BR)
      , (14252766920720777218, BS)
      , (14253892820627619842, BT)
      , (14255018720534462466, BU)
      , (14256144620441305090, BV)
      , (14257270520348147714, BW)
      , (14259522320161832962, BY)
      , (14260648220068675586, BZ)
      , (14376615910473465858, CA)
      , (14378867710287151106, CC)
      , (14379993610193993730, CD)
      , (14382245410007678978, CF)
      , (14383371309914521602, CG)
      , (14384497209821364226, CH)
      , (14385623109728206850, CI)
      , (14387874909541892098, CK)
      , (14389000809448734722, CL)
      , (14390126709355577346, CM)
      , (14391252609262419970, CN)
      , (14392378509169262594, CO)
      , (14393504409076105218, CP)
      , (14395756208889790466, CR)
      , (14396882108796633090, CS)
      , (14399133908610318338, CU)
      , (14400259808517160962, CV)
      , (14401385708424003586, CW)
      , (14402511608330846210, CX)
      , (14403637508237688834, CY)
      , (14404763408144531458, CZ)
      , (14524108798269849602, DD)
      , (14525234698176692226, DE)
      , (14527486497990377474, DG)
      , (14530864197710905346, DJ)
      , (14531990097617747970, DK)
      , (14534241897431433218, DM)
      , (14536493697245118466, DO)
      , (14548878596220387330, DZ)
      , (14664846286625177602, EA)
      , (14667098086438862850, EC)
      , (14669349886252548098, EE)
      , (14671601686066233346, EG)
      , (14672727585973075970, EH)
      , (14683986585041502210, ER)
      , (14685112484948344834, ES)
      , (14686238384855187458, ET)
      , (14687364284762030082, EU)
      , (14692993784296243202, EZ)
      , (14817968673955774466, FI)
      , (14819094573862617090, FJ)
      , (14820220473769459714, FK)
      , (14822472273583144962, FM)
      , (14824724073396830210, FO)
      , (14828101773117358082, FR)
      , (14834857172558413826, FX)
      , (14953076662776889346, GA)
      , (14954202562683731970, GB)
      , (14956454362497417218, GD)
      , (14957580262404259842, GE)
      , (14958706162311102466, GF)
      , (14959832062217945090, GG)
      , (14960957962124787714, GH)
      , (14962083862031630338, GI)
      , (14965461561752158210, GL)
      , (14966587461659000834, GM)
      , (14967713361565843458, GN)
      , (14969965161379528706, GP)
      , (14971091061286371330, GQ)
      , (14972216961193213954, GR)
      , (14973342861100056578, GS)
      , (14974468761006899202, GT)
      , (14975594660913741826, GU)
      , (14977846460727427074, GW)
      , (14980098260541112322, GY)
      , (15108450849921171458, HK)
      , (15110702649734856706, HM)
      , (15111828549641699330, HN)
      , (15116332149269069826, HR)
      , (15118583949082755074, HT)
      , (15119709848989597698, HU)
      , (15243558838742286338, IC)
      , (15244684738649128962, ID)
      , (15245810638555971586, IE)
      , (15253691937903869954, IL)
      , (15254817837810712578, IM)
      , (15255943737717555202, IN)
      , (15257069637624397826, IO)
      , (15259321437438083074, IQ)
      , (15260447337344925698, IR)
      , (15261573237251768322, IS)
      , (15262699137158610946, IT)
      , (15389925826631827458, JE)
      , (15398933025886568450, JM)
      , (15401184825700253698, JO)
      , (15402310725607096322, JP)
      , (15534041014707683330, KE)
      , (15536292814521368578, KG)
      , (15537418714428211202, KH)
      , (15538544614335053826, KI)
      , (15543048213962424322, KM)
      , (15544174113869266946, KN)
      , (15546425913682952194, KP)
      , (15548677713496637442, KR)
      , (15554307213030850562, KW)
      , (15556559012844535810, KY)
      , (15557684912751378434, KZ)
      , (15673652603156168706, LA)
      , (15674778503063011330, LB)
      , (15675904402969853954, LC)
      , (15682659802410909698, LI)
      , (15684911602224594946, LK)
      , (15692792901572493314, LR)
      , (15693918801479335938, LS)
      , (15695044701386178562, LT)
      , (15696170601293021186, LU)
      , (15697296501199863810, LV)
      , (15700674200920391682, LY)
      , (15817767791232024578, MA)
      , (15820019591045709826, MC)
      , (15821145490952552450, MD)
      , (15822271390859395074, ME)
      , (15823397290766237698, MF)
      , (15824523190673080322, MG)
      , (15825649090579922946, MH)
      , (15829026790300450818, MK)
      , (15830152690207293442, ML)
      , (15831278590114136066, MM)
      , (15832404490020978690, MN)
      , (15833530389927821314, MO)
      , (15834656289834663938, MP)
      , (15835782189741506562, MQ)
      , (15836908089648349186, MR)
      , (15838033989555191810, MS)
      , (15839159889462034434, MT)
      , (15840285789368877058, MU)
      , (15841411689275719682, MV)
      , (15842537589182562306, MW)
      , (15843663489089404930, MX)
      , (15844789388996247554, MY)
      , (15845915288903090178, MZ)
      , (15961882979307880450, NA)
      , (15964134779121565698, NC)
      , (15966386578935250946, NE)
      , (15967512478842093570, NF)
      , (15968638378748936194, NG)
      , (15970890178562621442, NI)
      , (15974267878283149314, NL)
      , (15977645578003677186, NO)
      , (15978771477910519810, NP)
      , (15981023277724205058, NR)
      , (15983275077537890306, NT)
      , (15984400977444732930, NU)
      , (15990030476978946050, NZ)
      , (16119508966265847810, OM)
      , (16250113355459592194, PA)
      , (16254616955086962690, PE)
      , (16255742854993805314, PF)
      , (16256868754900647938, PG)
      , (16257994654807490562, PH)
      , (16261372354528018434, PK)
      , (16262498254434861058, PL)
      , (16263624154341703682, PM)
      , (16264750054248546306, PN)
      , (16269253653875916802, PR)
      , (16270379553782759426, PS)
      , (16271505453689602050, PT)
      , (16274883153410129922, PW)
      , (16277134953223815170, PY)
      , (16394228543535448066, QA)
      , (16407739342417559554, QM)
      , (16408865242324402178, QN)
      , (16409991142231244802, QO)
      , (16411117042138087426, QP)
      , (16412242942044930050, QQ)
      , (16413368841951772674, QR)
      , (16414494741858615298, QS)
      , (16415620641765457922, QT)
      , (16416746541672300546, QU)
      , (16417872441579143170, QV)
      , (16418998341485985794, QW)
      , (16420124241392828418, QX)
      , (16421250141299671042, QY)
      , (16422376041206513666, QZ)
      , (16542847331238674434, RE)
      , (16554106330307100674, RO)
      , (16558609929934471170, RS)
      , (16560861729748156418, RU)
      , (16563113529561841666, RW)
      , (16682458919687159810, SA)
      , (16683584819594002434, SB)
      , (16684710719500845058, SC)
      , (16685836619407687682, SD)
      , (16686962519314530306, SE)
      , (16689214319128215554, SG)
      , (16690340219035058178, SH)
      , (16691466118941900802, SI)
      , (16692592018848743426, SJ)
      , (16693717918755586050, SK)
      , (16694843818662428674, SL)
      , (16695969718569271298, SM)
      , (16697095618476113922, SN)
      , (16698221518382956546, SO)
      , (16701599218103484418, SR)
      , (16702725118010327042, SS)
      , (16703851017917169666, ST)
      , (16704976917824012290, SU)
      , (16706102817730854914, SV)
      , (16708354617544540162, SX)
      , (16709480517451382786, SY)
      , (16710606417358225410, SZ)
      , (16826574107763015682, TA)
      , (16828825907576700930, TC)
      , (16829951807483543554, TD)
      , (16832203607297228802, TF)
      , (16833329507204071426, TG)
      , (16834455407110914050, TH)
      , (16836707206924599298, TJ)
      , (16837833106831441922, TK)
      , (16838959006738284546, TL)
      , (16840084906645127170, TM)
      , (16841210806551969794, TN)
      , (16842336706458812418, TO)
      , (16843462606365655042, TP)
      , (16845714406179340290, TR)
      , (16847966205993025538, TT)
      , (16850218005806710786, TV)
      , (16851343905713553410, TW)
      , (16854721605434081282, TZ)
      , (16970689295838871554, UA)
      , (16977444695279927298, UG)
      , (16984200094720983042, UM)
      , (16985325994627825666, UN)
      , (16990955494162038786, US)
      , (16997710893603094530, UY)
      , (16998836793509937154, UZ)
      , (17114804483914727426, VA)
      , (17117056283728412674, VC)
      , (17119308083542097922, VE)
      , (17121559883355783170, VG)
      , (17123811683169468418, VI)
      , (17129441182703681538, VN)
      , (17137322482051579906, VU)
      , (17264549171524796418, WF)
      , (17279185870313750530, WS)
      , (17403034860066439170, XA)
      , (17404160759973281794, XB)
      , (17405286659880124418, XC)
      , (17406412559786967042, XD)
      , (17407538459693809666, XE)
      , (17408664359600652290, XF)
      , (17409790259507494914, XG)
      , (17410916159414337538, XH)
      , (17412042059321180162, XI)
      , (17413167959228022786, XJ)
      , (17414293859134865410, XK)
      , (17415419759041708034, XL)
      , (17416545658948550658, XM)
      , (17417671558855393282, XN)
      , (17418797458762235906, XO)
      , (17419923358669078530, XP)
      , (17421049258575921154, XQ)
      , (17422175158482763778, XR)
      , (17423301058389606402, XS)
      , (17424426958296449026, XT)
      , (17425552858203291650, XU)
      , (17426678758110134274, XV)
      , (17427804658016976898, XW)
      , (17428930557923819522, XX)
      , (17430056457830662146, XY)
      , (17431182357737504770, XZ)
      , (17550527747862822914, YD)
      , (17551653647769665538, YE)
      , (17568542146372304898, YT)
      , (17569668046279147522, YU)
      , (17691265236218150914, ZA)
      , (17704776035100262402, ZM)
      , (17710405534634475522, ZR)
      , (17716035034168688642, ZW)
      , (17719412733889216514, ZZ)]
regionToSubtag :: Region -> Subtag
regionToSubtag x = case x of
  R001 -> Subtag 6972003231727616003
  R002 -> Subtag 6972012027820638211
  R003 -> Subtag 6972020823913660419
  R005 -> Subtag 6972038416099704835
  R009 -> Subtag 6972073600471793667
  R011 -> Subtag 6973129131634458627
  R013 -> Subtag 6973146723820503043
  R014 -> Subtag 6973155519913525251
  R015 -> Subtag 6973164316006547459
  R017 -> Subtag 6973181908192591875
  R018 -> Subtag 6973190704285614083
  R019 -> Subtag 6973199500378636291
  R021 -> Subtag 6974255031541301251
  R029 -> Subtag 6974325400285478915
  R030 -> Subtag 6975372135355121667
  R034 -> Subtag 6975407319727210499
  R035 -> Subtag 6975416115820232707
  R039 -> Subtag 6975451300192321539
  R053 -> Subtag 6977650323447873539
  R054 -> Subtag 6977659119540895747
  R057 -> Subtag 6977685507819962371
  R061 -> Subtag 6978758631168671747
  R142 -> Subtag 7120630815523864579
  R143 -> Subtag 7120639611616886787
  R145 -> Subtag 7120657203802931203
  R150 -> Subtag 7121739123244662787
  R151 -> Subtag 7121747919337684995
  R154 -> Subtag 7121774307616751619
  R155 -> Subtag 7121783103709773827
  R202 -> Subtag 7260242403972349955
  R419 -> Subtag 7549660252682059779
  AA -> Subtag 14088385534321754114
  AC -> Subtag 14090637334135439362
  AD -> Subtag 14091763234042281986
  AE -> Subtag 14092889133949124610
  AF -> Subtag 14094015033855967234
  AG -> Subtag 14095140933762809858
  AI -> Subtag 14097392733576495106
  AL -> Subtag 14100770433297022978
  AM -> Subtag 14101896333203865602
  AN -> Subtag 14103022233110708226
  AO -> Subtag 14104148133017550850
  AQ -> Subtag 14106399932831236098
  AR -> Subtag 14107525832738078722
  AS -> Subtag 14108651732644921346
  AT -> Subtag 14109777632551763970
  AU -> Subtag 14110903532458606594
  AW -> Subtag 14113155332272291842
  AX -> Subtag 14114281232179134466
  AZ -> Subtag 14116533031992819714
  BA -> Subtag 14232500722397609986
  BB -> Subtag 14233626622304452610
  BD -> Subtag 14235878422118137858
  BE -> Subtag 14237004322024980482
  BF -> Subtag 14238130221931823106
  BG -> Subtag 14239256121838665730
  BH -> Subtag 14240382021745508354
  BI -> Subtag 14241507921652350978
  BJ -> Subtag 14242633821559193602
  BL -> Subtag 14244885621372878850
  BM -> Subtag 14246011521279721474
  BN -> Subtag 14247137421186564098
  BO -> Subtag 14248263321093406722
  BQ -> Subtag 14250515120907091970
  BR -> Subtag 14251641020813934594
  BS -> Subtag 14252766920720777218
  BT -> Subtag 14253892820627619842
  BU -> Subtag 14255018720534462466
  BV -> Subtag 14256144620441305090
  BW -> Subtag 14257270520348147714
  BY -> Subtag 14259522320161832962
  BZ -> Subtag 14260648220068675586
  CA -> Subtag 14376615910473465858
  CC -> Subtag 14378867710287151106
  CD -> Subtag 14379993610193993730
  CF -> Subtag 14382245410007678978
  CG -> Subtag 14383371309914521602
  CH -> Subtag 14384497209821364226
  CI -> Subtag 14385623109728206850
  CK -> Subtag 14387874909541892098
  CL -> Subtag 14389000809448734722
  CM -> Subtag 14390126709355577346
  CN -> Subtag 14391252609262419970
  CO -> Subtag 14392378509169262594
  CP -> Subtag 14393504409076105218
  CR -> Subtag 14395756208889790466
  CS -> Subtag 14396882108796633090
  CU -> Subtag 14399133908610318338
  CV -> Subtag 14400259808517160962
  CW -> Subtag 14401385708424003586
  CX -> Subtag 14402511608330846210
  CY -> Subtag 14403637508237688834
  CZ -> Subtag 14404763408144531458
  DD -> Subtag 14524108798269849602
  DE -> Subtag 14525234698176692226
  DG -> Subtag 14527486497990377474
  DJ -> Subtag 14530864197710905346
  DK -> Subtag 14531990097617747970
  DM -> Subtag 14534241897431433218
  DO -> Subtag 14536493697245118466
  DZ -> Subtag 14548878596220387330
  EA -> Subtag 14664846286625177602
  EC -> Subtag 14667098086438862850
  EE -> Subtag 14669349886252548098
  EG -> Subtag 14671601686066233346
  EH -> Subtag 14672727585973075970
  ER -> Subtag 14683986585041502210
  ES -> Subtag 14685112484948344834
  ET -> Subtag 14686238384855187458
  EU -> Subtag 14687364284762030082
  EZ -> Subtag 14692993784296243202
  FI -> Subtag 14817968673955774466
  FJ -> Subtag 14819094573862617090
  FK -> Subtag 14820220473769459714
  FM -> Subtag 14822472273583144962
  FO -> Subtag 14824724073396830210
  FR -> Subtag 14828101773117358082
  FX -> Subtag 14834857172558413826
  GA -> Subtag 14953076662776889346
  GB -> Subtag 14954202562683731970
  GD -> Subtag 14956454362497417218
  GE -> Subtag 14957580262404259842
  GF -> Subtag 14958706162311102466
  GG -> Subtag 14959832062217945090
  GH -> Subtag 14960957962124787714
  GI -> Subtag 14962083862031630338
  GL -> Subtag 14965461561752158210
  GM -> Subtag 14966587461659000834
  GN -> Subtag 14967713361565843458
  GP -> Subtag 14969965161379528706
  GQ -> Subtag 14971091061286371330
  GR -> Subtag 14972216961193213954
  GS -> Subtag 14973342861100056578
  GT -> Subtag 14974468761006899202
  GU -> Subtag 14975594660913741826
  GW -> Subtag 14977846460727427074
  GY -> Subtag 14980098260541112322
  HK -> Subtag 15108450849921171458
  HM -> Subtag 15110702649734856706
  HN -> Subtag 15111828549641699330
  HR -> Subtag 15116332149269069826
  HT -> Subtag 15118583949082755074
  HU -> Subtag 15119709848989597698
  IC -> Subtag 15243558838742286338
  ID -> Subtag 15244684738649128962
  IE -> Subtag 15245810638555971586
  IL -> Subtag 15253691937903869954
  IM -> Subtag 15254817837810712578
  IN -> Subtag 15255943737717555202
  IO -> Subtag 15257069637624397826
  IQ -> Subtag 15259321437438083074
  IR -> Subtag 15260447337344925698
  IS -> Subtag 15261573237251768322
  IT -> Subtag 15262699137158610946
  JE -> Subtag 15389925826631827458
  JM -> Subtag 15398933025886568450
  JO -> Subtag 15401184825700253698
  JP -> Subtag 15402310725607096322
  KE -> Subtag 15534041014707683330
  KG -> Subtag 15536292814521368578
  KH -> Subtag 15537418714428211202
  KI -> Subtag 15538544614335053826
  KM -> Subtag 15543048213962424322
  KN -> Subtag 15544174113869266946
  KP -> Subtag 15546425913682952194
  KR -> Subtag 15548677713496637442
  KW -> Subtag 15554307213030850562
  KY -> Subtag 15556559012844535810
  KZ -> Subtag 15557684912751378434
  LA -> Subtag 15673652603156168706
  LB -> Subtag 15674778503063011330
  LC -> Subtag 15675904402969853954
  LI -> Subtag 15682659802410909698
  LK -> Subtag 15684911602224594946
  LR -> Subtag 15692792901572493314
  LS -> Subtag 15693918801479335938
  LT -> Subtag 15695044701386178562
  LU -> Subtag 15696170601293021186
  LV -> Subtag 15697296501199863810
  LY -> Subtag 15700674200920391682
  MA -> Subtag 15817767791232024578
  MC -> Subtag 15820019591045709826
  MD -> Subtag 15821145490952552450
  ME -> Subtag 15822271390859395074
  MF -> Subtag 15823397290766237698
  MG -> Subtag 15824523190673080322
  MH -> Subtag 15825649090579922946
  MK -> Subtag 15829026790300450818
  ML -> Subtag 15830152690207293442
  MM -> Subtag 15831278590114136066
  MN -> Subtag 15832404490020978690
  MO -> Subtag 15833530389927821314
  MP -> Subtag 15834656289834663938
  MQ -> Subtag 15835782189741506562
  MR -> Subtag 15836908089648349186
  MS -> Subtag 15838033989555191810
  MT -> Subtag 15839159889462034434
  MU -> Subtag 15840285789368877058
  MV -> Subtag 15841411689275719682
  MW -> Subtag 15842537589182562306
  MX -> Subtag 15843663489089404930
  MY -> Subtag 15844789388996247554
  MZ -> Subtag 15845915288903090178
  NA -> Subtag 15961882979307880450
  NC -> Subtag 15964134779121565698
  NE -> Subtag 15966386578935250946
  NF -> Subtag 15967512478842093570
  NG -> Subtag 15968638378748936194
  NI -> Subtag 15970890178562621442
  NL -> Subtag 15974267878283149314
  NO -> Subtag 15977645578003677186
  NP -> Subtag 15978771477910519810
  NR -> Subtag 15981023277724205058
  NT -> Subtag 15983275077537890306
  NU -> Subtag 15984400977444732930
  NZ -> Subtag 15990030476978946050
  OM -> Subtag 16119508966265847810
  PA -> Subtag 16250113355459592194
  PE -> Subtag 16254616955086962690
  PF -> Subtag 16255742854993805314
  PG -> Subtag 16256868754900647938
  PH -> Subtag 16257994654807490562
  PK -> Subtag 16261372354528018434
  PL -> Subtag 16262498254434861058
  PM -> Subtag 16263624154341703682
  PN -> Subtag 16264750054248546306
  PR -> Subtag 16269253653875916802
  PS -> Subtag 16270379553782759426
  PT -> Subtag 16271505453689602050
  PW -> Subtag 16274883153410129922
  PY -> Subtag 16277134953223815170
  QA -> Subtag 16394228543535448066
  QM -> Subtag 16407739342417559554
  QN -> Subtag 16408865242324402178
  QO -> Subtag 16409991142231244802
  QP -> Subtag 16411117042138087426
  QQ -> Subtag 16412242942044930050
  QR -> Subtag 16413368841951772674
  QS -> Subtag 16414494741858615298
  QT -> Subtag 16415620641765457922
  QU -> Subtag 16416746541672300546
  QV -> Subtag 16417872441579143170
  QW -> Subtag 16418998341485985794
  QX -> Subtag 16420124241392828418
  QY -> Subtag 16421250141299671042
  QZ -> Subtag 16422376041206513666
  RE -> Subtag 16542847331238674434
  RO -> Subtag 16554106330307100674
  RS -> Subtag 16558609929934471170
  RU -> Subtag 16560861729748156418
  RW -> Subtag 16563113529561841666
  SA -> Subtag 16682458919687159810
  SB -> Subtag 16683584819594002434
  SC -> Subtag 16684710719500845058
  SD -> Subtag 16685836619407687682
  SE -> Subtag 16686962519314530306
  SG -> Subtag 16689214319128215554
  SH -> Subtag 16690340219035058178
  SI -> Subtag 16691466118941900802
  SJ -> Subtag 16692592018848743426
  SK -> Subtag 16693717918755586050
  SL -> Subtag 16694843818662428674
  SM -> Subtag 16695969718569271298
  SN -> Subtag 16697095618476113922
  SO -> Subtag 16698221518382956546
  SR -> Subtag 16701599218103484418
  SS -> Subtag 16702725118010327042
  ST -> Subtag 16703851017917169666
  SU -> Subtag 16704976917824012290
  SV -> Subtag 16706102817730854914
  SX -> Subtag 16708354617544540162
  SY -> Subtag 16709480517451382786
  SZ -> Subtag 16710606417358225410
  TA -> Subtag 16826574107763015682
  TC -> Subtag 16828825907576700930
  TD -> Subtag 16829951807483543554
  TF -> Subtag 16832203607297228802
  TG -> Subtag 16833329507204071426
  TH -> Subtag 16834455407110914050
  TJ -> Subtag 16836707206924599298
  TK -> Subtag 16837833106831441922
  TL -> Subtag 16838959006738284546
  TM -> Subtag 16840084906645127170
  TN -> Subtag 16841210806551969794
  TO -> Subtag 16842336706458812418
  TP -> Subtag 16843462606365655042
  TR -> Subtag 16845714406179340290
  TT -> Subtag 16847966205993025538
  TV -> Subtag 16850218005806710786
  TW -> Subtag 16851343905713553410
  TZ -> Subtag 16854721605434081282
  UA -> Subtag 16970689295838871554
  UG -> Subtag 16977444695279927298
  UM -> Subtag 16984200094720983042
  UN -> Subtag 16985325994627825666
  US -> Subtag 16990955494162038786
  UY -> Subtag 16997710893603094530
  UZ -> Subtag 16998836793509937154
  VA -> Subtag 17114804483914727426
  VC -> Subtag 17117056283728412674
  VE -> Subtag 17119308083542097922
  VG -> Subtag 17121559883355783170
  VI -> Subtag 17123811683169468418
  VN -> Subtag 17129441182703681538
  VU -> Subtag 17137322482051579906
  WF -> Subtag 17264549171524796418
  WS -> Subtag 17279185870313750530
  XA -> Subtag 17403034860066439170
  XB -> Subtag 17404160759973281794
  XC -> Subtag 17405286659880124418
  XD -> Subtag 17406412559786967042
  XE -> Subtag 17407538459693809666
  XF -> Subtag 17408664359600652290
  XG -> Subtag 17409790259507494914
  XH -> Subtag 17410916159414337538
  XI -> Subtag 17412042059321180162
  XJ -> Subtag 17413167959228022786
  XK -> Subtag 17414293859134865410
  XL -> Subtag 17415419759041708034
  XM -> Subtag 17416545658948550658
  XN -> Subtag 17417671558855393282
  XO -> Subtag 17418797458762235906
  XP -> Subtag 17419923358669078530
  XQ -> Subtag 17421049258575921154
  XR -> Subtag 17422175158482763778
  XS -> Subtag 17423301058389606402
  XT -> Subtag 17424426958296449026
  XU -> Subtag 17425552858203291650
  XV -> Subtag 17426678758110134274
  XW -> Subtag 17427804658016976898
  XX -> Subtag 17428930557923819522
  XY -> Subtag 17430056457830662146
  XZ -> Subtag 17431182357737504770
  YD -> Subtag 17550527747862822914
  YE -> Subtag 17551653647769665538
  YT -> Subtag 17568542146372304898
  YU -> Subtag 17569668046279147522
  ZA -> Subtag 17691265236218150914
  ZM -> Subtag 17704776035100262402
  ZR -> Subtag 17710405534634475522
  ZW -> Subtag 17716035034168688642
  ZZ -> Subtag 17719412733889216514
