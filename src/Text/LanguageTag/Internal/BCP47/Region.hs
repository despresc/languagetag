-- This is an auto-generated file. Do not edit by hand

module Text.LanguageTag.Internal.BCP47.Region where

import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..), hashUsing)

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
  | Aa -- ^ @AA@. Description: Private use.
  | Ac -- ^ @AC@. Description: Ascension Island.
  | Ad -- ^ @AD@. Description: Andorra.
  | Ae -- ^ @AE@. Description: United Arab Emirates.
  | Af -- ^ @AF@. Description: Afghanistan.
  | Ag -- ^ @AG@. Description: Antigua and Barbuda.
  | Ai -- ^ @AI@. Description: Anguilla.
  | Al -- ^ @AL@. Description: Albania.
  | Am -- ^ @AM@. Description: Armenia.
  | An -- ^ @AN@. Description: Netherlands Antilles. Deprecated.
  | Ao -- ^ @AO@. Description: Angola.
  | Aq -- ^ @AQ@. Description: Antarctica.
  | Ar -- ^ @AR@. Description: Argentina.
  | As -- ^ @AS@. Description: American Samoa.
  | At -- ^ @AT@. Description: Austria.
  | Au -- ^ @AU@. Description: Australia.
  | Aw -- ^ @AW@. Description: Aruba.
  | Ax -- ^ @AX@. Description: Åland Islands.
  | Az -- ^ @AZ@. Description: Azerbaijan.
  | Ba -- ^ @BA@. Description: Bosnia and Herzegovina.
  | Bb -- ^ @BB@. Description: Barbados.
  | Bd -- ^ @BD@. Description: Bangladesh.
  | Be -- ^ @BE@. Description: Belgium.
  | Bf -- ^ @BF@. Description: Burkina Faso.
  | Bg -- ^ @BG@. Description: Bulgaria.
  | Bh -- ^ @BH@. Description: Bahrain.
  | Bi -- ^ @BI@. Description: Burundi.
  | Bj -- ^ @BJ@. Description: Benin.
  | Bl -- ^ @BL@. Description: Saint Barthélemy.
  | Bm -- ^ @BM@. Description: Bermuda.
  | Bn -- ^ @BN@. Description: Brunei Darussalam.
  | Bo -- ^ @BO@. Description: Bolivia.
  | Bq -- ^ @BQ@. Description: Bonaire, Sint Eustatius and Saba.
  | Br -- ^ @BR@. Description: Brazil.
  | Bs -- ^ @BS@. Description: Bahamas.
  | Bt -- ^ @BT@. Description: Bhutan.
  | Bu -- ^ @BU@. Description: Burma. Deprecated. Preferred value: MM.
  | Bv -- ^ @BV@. Description: Bouvet Island.
  | Bw -- ^ @BW@. Description: Botswana.
  | By -- ^ @BY@. Description: Belarus.
  | Bz -- ^ @BZ@. Description: Belize.
  | Ca -- ^ @CA@. Description: Canada.
  | Cc -- ^ @CC@. Description: Cocos (Keeling) Islands.
  | Cd -- ^ @CD@. Description: The Democratic Republic of the Congo.
  | Cf -- ^ @CF@. Description: Central African Republic.
  | Cg -- ^ @CG@. Description: Congo.
  | Ch -- ^ @CH@. Description: Switzerland.
  | Ci -- ^ @CI@. Description: Côte d\'Ivoire.
  | Ck -- ^ @CK@. Description: Cook Islands.
  | Cl -- ^ @CL@. Description: Chile.
  | Cm -- ^ @CM@. Description: Cameroon.
  | Cn -- ^ @CN@. Description: China.
  | Co -- ^ @CO@. Description: Colombia.
  | Cp -- ^ @CP@. Description: Clipperton Island.
  | Cr -- ^ @CR@. Description: Costa Rica.
  | Cs -- ^ @CS@. Description: Serbia and Montenegro. Deprecated.
  | Cu -- ^ @CU@. Description: Cuba.
  | Cv -- ^ @CV@. Description: Cabo Verde; Cape Verde.
  | Cw -- ^ @CW@. Description: Curaçao.
  | Cx -- ^ @CX@. Description: Christmas Island.
  | Cy -- ^ @CY@. Description: Cyprus.
  | Cz -- ^ @CZ@. Description: Czechia; Czech Republic.
  | Dd -- ^ @DD@. Description: German Democratic Republic. Deprecated. Preferred value: DE.
  | De -- ^ @DE@. Description: Germany.
  | Dg -- ^ @DG@. Description: Diego Garcia.
  | Dj -- ^ @DJ@. Description: Djibouti.
  | Dk -- ^ @DK@. Description: Denmark.
  | Dm -- ^ @DM@. Description: Dominica.
  | Do -- ^ @DO@. Description: Dominican Republic.
  | Dz -- ^ @DZ@. Description: Algeria.
  | Ea -- ^ @EA@. Description: Ceuta, Melilla.
  | Ec -- ^ @EC@. Description: Ecuador.
  | Ee -- ^ @EE@. Description: Estonia.
  | Eg -- ^ @EG@. Description: Egypt.
  | Eh -- ^ @EH@. Description: Western Sahara.
  | Er -- ^ @ER@. Description: Eritrea.
  | Es -- ^ @ES@. Description: Spain.
  | Et -- ^ @ET@. Description: Ethiopia.
  | Eu -- ^ @EU@. Description: European Union.
  | Ez -- ^ @EZ@. Description: Eurozone.
  | Fi -- ^ @FI@. Description: Finland.
  | Fj -- ^ @FJ@. Description: Fiji.
  | Fk -- ^ @FK@. Description: Falkland Islands (Malvinas).
  | Fm -- ^ @FM@. Description: Federated States of Micronesia.
  | Fo -- ^ @FO@. Description: Faroe Islands.
  | Fr -- ^ @FR@. Description: France.
  | Fx -- ^ @FX@. Description: Metropolitan France. Deprecated. Preferred value: FR.
  | Ga -- ^ @GA@. Description: Gabon.
  | Gb -- ^ @GB@. Description: United Kingdom.
  | Gd -- ^ @GD@. Description: Grenada.
  | Ge -- ^ @GE@. Description: Georgia.
  | Gf -- ^ @GF@. Description: French Guiana.
  | Gg -- ^ @GG@. Description: Guernsey.
  | Gh -- ^ @GH@. Description: Ghana.
  | Gi -- ^ @GI@. Description: Gibraltar.
  | Gl -- ^ @GL@. Description: Greenland.
  | Gm -- ^ @GM@. Description: Gambia.
  | Gn -- ^ @GN@. Description: Guinea.
  | Gp -- ^ @GP@. Description: Guadeloupe.
  | Gq -- ^ @GQ@. Description: Equatorial Guinea.
  | Gr -- ^ @GR@. Description: Greece.
  | Gs -- ^ @GS@. Description: South Georgia and the South Sandwich Islands.
  | Gt -- ^ @GT@. Description: Guatemala.
  | Gu -- ^ @GU@. Description: Guam.
  | Gw -- ^ @GW@. Description: Guinea-Bissau.
  | Gy -- ^ @GY@. Description: Guyana.
  | Hk -- ^ @HK@. Description: Hong Kong.
  | Hm -- ^ @HM@. Description: Heard Island and McDonald Islands.
  | Hn -- ^ @HN@. Description: Honduras.
  | Hr -- ^ @HR@. Description: Croatia.
  | Ht -- ^ @HT@. Description: Haiti.
  | Hu -- ^ @HU@. Description: Hungary.
  | Ic -- ^ @IC@. Description: Canary Islands.
  | Id -- ^ @ID@. Description: Indonesia.
  | Ie -- ^ @IE@. Description: Ireland.
  | Il -- ^ @IL@. Description: Israel.
  | Im -- ^ @IM@. Description: Isle of Man.
  | In -- ^ @IN@. Description: India.
  | Io -- ^ @IO@. Description: British Indian Ocean Territory.
  | Iq -- ^ @IQ@. Description: Iraq.
  | Ir -- ^ @IR@. Description: Islamic Republic of Iran.
  | Is -- ^ @IS@. Description: Iceland.
  | It -- ^ @IT@. Description: Italy.
  | Je -- ^ @JE@. Description: Jersey.
  | Jm -- ^ @JM@. Description: Jamaica.
  | Jo -- ^ @JO@. Description: Jordan.
  | Jp -- ^ @JP@. Description: Japan.
  | Ke -- ^ @KE@. Description: Kenya.
  | Kg -- ^ @KG@. Description: Kyrgyzstan.
  | Kh -- ^ @KH@. Description: Cambodia.
  | Ki -- ^ @KI@. Description: Kiribati.
  | Km -- ^ @KM@. Description: Comoros.
  | Kn -- ^ @KN@. Description: Saint Kitts and Nevis.
  | Kp -- ^ @KP@. Description: Democratic People\'s Republic of Korea.
  | Kr -- ^ @KR@. Description: Republic of Korea.
  | Kw -- ^ @KW@. Description: Kuwait.
  | Ky -- ^ @KY@. Description: Cayman Islands.
  | Kz -- ^ @KZ@. Description: Kazakhstan.
  | La -- ^ @LA@. Description: Lao People\'s Democratic Republic.
  | Lb -- ^ @LB@. Description: Lebanon.
  | Lc -- ^ @LC@. Description: Saint Lucia.
  | Li -- ^ @LI@. Description: Liechtenstein.
  | Lk -- ^ @LK@. Description: Sri Lanka.
  | Lr -- ^ @LR@. Description: Liberia.
  | Ls -- ^ @LS@. Description: Lesotho.
  | Lt -- ^ @LT@. Description: Lithuania.
  | Lu -- ^ @LU@. Description: Luxembourg.
  | Lv -- ^ @LV@. Description: Latvia.
  | Ly -- ^ @LY@. Description: Libya.
  | Ma -- ^ @MA@. Description: Morocco.
  | Mc -- ^ @MC@. Description: Monaco.
  | Md -- ^ @MD@. Description: Moldova.
  | Me -- ^ @ME@. Description: Montenegro.
  | Mf -- ^ @MF@. Description: Saint Martin (French part).
  | Mg -- ^ @MG@. Description: Madagascar.
  | Mh -- ^ @MH@. Description: Marshall Islands.
  | Mk -- ^ @MK@. Description: North Macedonia.
  | Ml -- ^ @ML@. Description: Mali.
  | Mm -- ^ @MM@. Description: Myanmar.
  | Mn -- ^ @MN@. Description: Mongolia.
  | Mo -- ^ @MO@. Description: Macao.
  | Mp -- ^ @MP@. Description: Northern Mariana Islands.
  | Mq -- ^ @MQ@. Description: Martinique.
  | Mr -- ^ @MR@. Description: Mauritania.
  | Ms -- ^ @MS@. Description: Montserrat.
  | Mt -- ^ @MT@. Description: Malta.
  | Mu -- ^ @MU@. Description: Mauritius.
  | Mv -- ^ @MV@. Description: Maldives.
  | Mw -- ^ @MW@. Description: Malawi.
  | Mx -- ^ @MX@. Description: Mexico.
  | My -- ^ @MY@. Description: Malaysia.
  | Mz -- ^ @MZ@. Description: Mozambique.
  | Na -- ^ @NA@. Description: Namibia.
  | Nc -- ^ @NC@. Description: New Caledonia.
  | Ne -- ^ @NE@. Description: Niger.
  | Nf -- ^ @NF@. Description: Norfolk Island.
  | Ng -- ^ @NG@. Description: Nigeria.
  | Ni -- ^ @NI@. Description: Nicaragua.
  | Nl -- ^ @NL@. Description: Netherlands.
  | No -- ^ @NO@. Description: Norway.
  | Np -- ^ @NP@. Description: Nepal.
  | Nr -- ^ @NR@. Description: Nauru.
  | Nt -- ^ @NT@. Description: Neutral Zone. Deprecated.
  | Nu -- ^ @NU@. Description: Niue.
  | Nz -- ^ @NZ@. Description: New Zealand.
  | Om -- ^ @OM@. Description: Oman.
  | Pa -- ^ @PA@. Description: Panama.
  | Pe -- ^ @PE@. Description: Peru.
  | Pf -- ^ @PF@. Description: French Polynesia.
  | Pg -- ^ @PG@. Description: Papua New Guinea.
  | Ph -- ^ @PH@. Description: Philippines.
  | Pk -- ^ @PK@. Description: Pakistan.
  | Pl -- ^ @PL@. Description: Poland.
  | Pm -- ^ @PM@. Description: Saint Pierre and Miquelon.
  | Pn -- ^ @PN@. Description: Pitcairn.
  | Pr -- ^ @PR@. Description: Puerto Rico.
  | Ps -- ^ @PS@. Description: State of Palestine.
  | Pt -- ^ @PT@. Description: Portugal.
  | Pw -- ^ @PW@. Description: Palau.
  | Py -- ^ @PY@. Description: Paraguay.
  | Qa -- ^ @QA@. Description: Qatar.
  | Qm -- ^ @QM@. Description: Private use.
  | Qn -- ^ @QN@. Description: Private use.
  | Qo -- ^ @QO@. Description: Private use.
  | Qp -- ^ @QP@. Description: Private use.
  | Qq -- ^ @QQ@. Description: Private use.
  | Qr -- ^ @QR@. Description: Private use.
  | Qs -- ^ @QS@. Description: Private use.
  | Qt -- ^ @QT@. Description: Private use.
  | Qu -- ^ @QU@. Description: Private use.
  | Qv -- ^ @QV@. Description: Private use.
  | Qw -- ^ @QW@. Description: Private use.
  | Qx -- ^ @QX@. Description: Private use.
  | Qy -- ^ @QY@. Description: Private use.
  | Qz -- ^ @QZ@. Description: Private use.
  | Re -- ^ @RE@. Description: Réunion.
  | Ro -- ^ @RO@. Description: Romania.
  | Rs -- ^ @RS@. Description: Serbia.
  | Ru -- ^ @RU@. Description: Russian Federation.
  | Rw -- ^ @RW@. Description: Rwanda.
  | Sa -- ^ @SA@. Description: Saudi Arabia.
  | Sb -- ^ @SB@. Description: Solomon Islands.
  | Sc -- ^ @SC@. Description: Seychelles.
  | Sd -- ^ @SD@. Description: Sudan.
  | Se -- ^ @SE@. Description: Sweden.
  | Sg -- ^ @SG@. Description: Singapore.
  | Sh -- ^ @SH@. Description: Saint Helena, Ascension and Tristan da Cunha.
  | Si -- ^ @SI@. Description: Slovenia.
  | Sj -- ^ @SJ@. Description: Svalbard and Jan Mayen.
  | Sk -- ^ @SK@. Description: Slovakia.
  | Sl -- ^ @SL@. Description: Sierra Leone.
  | Sm -- ^ @SM@. Description: San Marino.
  | Sn -- ^ @SN@. Description: Senegal.
  | So -- ^ @SO@. Description: Somalia.
  | Sr -- ^ @SR@. Description: Suriname.
  | Ss -- ^ @SS@. Description: South Sudan.
  | St -- ^ @ST@. Description: Sao Tome and Principe.
  | Su -- ^ @SU@. Description: Union of Soviet Socialist Republics. Deprecated.
  | Sv -- ^ @SV@. Description: El Salvador.
  | Sx -- ^ @SX@. Description: Sint Maarten (Dutch part).
  | Sy -- ^ @SY@. Description: Syrian Arab Republic.
  | Sz -- ^ @SZ@. Description: Eswatini; eSwatini; Swaziland.
  | Ta -- ^ @TA@. Description: Tristan da Cunha.
  | Tc -- ^ @TC@. Description: Turks and Caicos Islands.
  | Td -- ^ @TD@. Description: Chad.
  | Tf -- ^ @TF@. Description: French Southern Territories.
  | Tg -- ^ @TG@. Description: Togo.
  | Th -- ^ @TH@. Description: Thailand.
  | Tj -- ^ @TJ@. Description: Tajikistan.
  | Tk -- ^ @TK@. Description: Tokelau.
  | Tl -- ^ @TL@. Description: Timor-Leste.
  | Tm -- ^ @TM@. Description: Turkmenistan.
  | Tn -- ^ @TN@. Description: Tunisia.
  | To -- ^ @TO@. Description: Tonga.
  | Tp -- ^ @TP@. Description: East Timor. Deprecated. Preferred value: TL.
  | Tr -- ^ @TR@. Description: Turkey.
  | Tt -- ^ @TT@. Description: Trinidad and Tobago.
  | Tv -- ^ @TV@. Description: Tuvalu.
  | Tw -- ^ @TW@. Description: Taiwan, Province of China.
  | Tz -- ^ @TZ@. Description: United Republic of Tanzania.
  | Ua -- ^ @UA@. Description: Ukraine.
  | Ug -- ^ @UG@. Description: Uganda.
  | Um -- ^ @UM@. Description: United States Minor Outlying Islands.
  | Un -- ^ @UN@. Description: United Nations.
  | Us -- ^ @US@. Description: United States.
  | Uy -- ^ @UY@. Description: Uruguay.
  | Uz -- ^ @UZ@. Description: Uzbekistan.
  | Va -- ^ @VA@. Description: Holy See (Vatican City State).
  | Vc -- ^ @VC@. Description: Saint Vincent and the Grenadines.
  | Ve -- ^ @VE@. Description: Venezuela.
  | Vg -- ^ @VG@. Description: British Virgin Islands.
  | Vi -- ^ @VI@. Description: U.S. Virgin Islands.
  | Vn -- ^ @VN@. Description: Viet Nam.
  | Vu -- ^ @VU@. Description: Vanuatu.
  | Wf -- ^ @WF@. Description: Wallis and Futuna.
  | Ws -- ^ @WS@. Description: Samoa.
  | Xa -- ^ @XA@. Description: Private use.
  | Xb -- ^ @XB@. Description: Private use.
  | Xc -- ^ @XC@. Description: Private use.
  | Xd -- ^ @XD@. Description: Private use.
  | Xe -- ^ @XE@. Description: Private use.
  | Xf -- ^ @XF@. Description: Private use.
  | Xg -- ^ @XG@. Description: Private use.
  | Xh -- ^ @XH@. Description: Private use.
  | Xi -- ^ @XI@. Description: Private use.
  | Xj -- ^ @XJ@. Description: Private use.
  | Xk -- ^ @XK@. Description: Private use.
  | Xl -- ^ @XL@. Description: Private use.
  | Xm -- ^ @XM@. Description: Private use.
  | Xn -- ^ @XN@. Description: Private use.
  | Xo -- ^ @XO@. Description: Private use.
  | Xp -- ^ @XP@. Description: Private use.
  | Xq -- ^ @XQ@. Description: Private use.
  | Xr -- ^ @XR@. Description: Private use.
  | Xs -- ^ @XS@. Description: Private use.
  | Xt -- ^ @XT@. Description: Private use.
  | Xu -- ^ @XU@. Description: Private use.
  | Xv -- ^ @XV@. Description: Private use.
  | Xw -- ^ @XW@. Description: Private use.
  | Xx -- ^ @XX@. Description: Private use.
  | Xy -- ^ @XY@. Description: Private use.
  | Xz -- ^ @XZ@. Description: Private use.
  | Yd -- ^ @YD@. Description: Democratic Yemen. Deprecated. Preferred value: YE.
  | Ye -- ^ @YE@. Description: Yemen.
  | Yt -- ^ @YT@. Description: Mayotte.
  | Yu -- ^ @YU@. Description: Yugoslavia. Deprecated.
  | Za -- ^ @ZA@. Description: South Africa.
  | Zm -- ^ @ZM@. Description: Zambia.
  | Zr -- ^ @ZR@. Description: Zaire. Deprecated. Preferred value: CD.
  | Zw -- ^ @ZW@. Description: Zimbabwe.
  | Zz -- ^ @ZZ@. Description: Private use.

  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData Region where
  rnf a = seq a ()

instance Hashable Region where
  hashWithSalt = hashUsing fromEnum
