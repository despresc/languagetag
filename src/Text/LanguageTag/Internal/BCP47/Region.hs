-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.LanguageTag.Internal.BCP47.Region where

import Prelude hiding (LT, GT)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Text.LanguageTag.Internal.Subtag (Subtag(..))
import Text.LanguageTag.Subtag (renderSubtagUpper)

-- | A valid BCP47 region tag as of 2021-02-23
newtype Region = Region Subtag
  deriving (Eq, Ord, Hashable, NFData)

instance Show Region where
  show (Region t) = T.unpack $ renderSubtagUpper t

 -- | @001@. Description: World.
pattern REG001 :: Region
pattern REG001 = Region (Subtag 6972003231727616035)

 -- | @002@. Description: Africa.
pattern REG002 :: Region
pattern REG002 = Region (Subtag 6972012027820638243)

 -- | @003@. Description: North America.
pattern REG003 :: Region
pattern REG003 = Region (Subtag 6972020823913660451)

 -- | @005@. Description: South America.
pattern REG005 :: Region
pattern REG005 = Region (Subtag 6972038416099704867)

 -- | @009@. Description: Oceania.
pattern REG009 :: Region
pattern REG009 = Region (Subtag 6972073600471793699)

 -- | @011@. Description: Western Africa.
pattern REG011 :: Region
pattern REG011 = Region (Subtag 6973129131634458659)

 -- | @013@. Description: Central America.
pattern REG013 :: Region
pattern REG013 = Region (Subtag 6973146723820503075)

 -- | @014@. Description: Eastern Africa.
pattern REG014 :: Region
pattern REG014 = Region (Subtag 6973155519913525283)

 -- | @015@. Description: Northern Africa.
pattern REG015 :: Region
pattern REG015 = Region (Subtag 6973164316006547491)

 -- | @017@. Description: Middle Africa.
pattern REG017 :: Region
pattern REG017 = Region (Subtag 6973181908192591907)

 -- | @018@. Description: Southern Africa.
pattern REG018 :: Region
pattern REG018 = Region (Subtag 6973190704285614115)

 -- | @019@. Description: Americas.
pattern REG019 :: Region
pattern REG019 = Region (Subtag 6973199500378636323)

 -- | @021@. Description: Northern America.
pattern REG021 :: Region
pattern REG021 = Region (Subtag 6974255031541301283)

 -- | @029@. Description: Caribbean.
pattern REG029 :: Region
pattern REG029 = Region (Subtag 6974325400285478947)

 -- | @030@. Description: Eastern Asia.
pattern REG030 :: Region
pattern REG030 = Region (Subtag 6975372135355121699)

 -- | @034@. Description: Southern Asia.
pattern REG034 :: Region
pattern REG034 = Region (Subtag 6975407319727210531)

 -- | @035@. Description: South-Eastern Asia.
pattern REG035 :: Region
pattern REG035 = Region (Subtag 6975416115820232739)

 -- | @039@. Description: Southern Europe.
pattern REG039 :: Region
pattern REG039 = Region (Subtag 6975451300192321571)

 -- | @053@. Description: Australia and New Zealand.
pattern REG053 :: Region
pattern REG053 = Region (Subtag 6977650323447873571)

 -- | @054@. Description: Melanesia.
pattern REG054 :: Region
pattern REG054 = Region (Subtag 6977659119540895779)

 -- | @057@. Description: Micronesia.
pattern REG057 :: Region
pattern REG057 = Region (Subtag 6977685507819962403)

 -- | @061@. Description: Polynesia.
pattern REG061 :: Region
pattern REG061 = Region (Subtag 6978758631168671779)

 -- | @142@. Description: Asia.
pattern REG142 :: Region
pattern REG142 = Region (Subtag 7120630815523864611)

 -- | @143@. Description: Central Asia.
pattern REG143 :: Region
pattern REG143 = Region (Subtag 7120639611616886819)

 -- | @145@. Description: Western Asia.
pattern REG145 :: Region
pattern REG145 = Region (Subtag 7120657203802931235)

 -- | @150@. Description: Europe.
pattern REG150 :: Region
pattern REG150 = Region (Subtag 7121739123244662819)

 -- | @151@. Description: Eastern Europe.
pattern REG151 :: Region
pattern REG151 = Region (Subtag 7121747919337685027)

 -- | @154@. Description: Northern Europe.
pattern REG154 :: Region
pattern REG154 = Region (Subtag 7121774307616751651)

 -- | @155@. Description: Western Europe.
pattern REG155 :: Region
pattern REG155 = Region (Subtag 7121783103709773859)

 -- | @202@. Description: Sub-Saharan Africa.
pattern REG202 :: Region
pattern REG202 = Region (Subtag 7260242403972349987)

 -- | @419@. Description: Latin America and the Caribbean.
pattern REG419 :: Region
pattern REG419 = Region (Subtag 7549660252682059811)

 -- | @AA@. Description: Private use.
pattern AA :: Region
pattern AA = Region (Subtag 14088385534321754130)

 -- | @AC@. Description: Ascension Island.
pattern AC :: Region
pattern AC = Region (Subtag 14090637334135439378)

 -- | @AD@. Description: Andorra.
pattern AD :: Region
pattern AD = Region (Subtag 14091763234042282002)

 -- | @AE@. Description: United Arab Emirates.
pattern AE :: Region
pattern AE = Region (Subtag 14092889133949124626)

 -- | @AF@. Description: Afghanistan.
pattern AF :: Region
pattern AF = Region (Subtag 14094015033855967250)

 -- | @AG@. Description: Antigua and Barbuda.
pattern AG :: Region
pattern AG = Region (Subtag 14095140933762809874)

 -- | @AI@. Description: Anguilla.
pattern AI :: Region
pattern AI = Region (Subtag 14097392733576495122)

 -- | @AL@. Description: Albania.
pattern AL :: Region
pattern AL = Region (Subtag 14100770433297022994)

 -- | @AM@. Description: Armenia.
pattern AM :: Region
pattern AM = Region (Subtag 14101896333203865618)

 -- | @AN@. Description: Netherlands Antilles. Deprecated.
pattern AN :: Region
pattern AN = Region (Subtag 14103022233110708242)

 -- | @AO@. Description: Angola.
pattern AO :: Region
pattern AO = Region (Subtag 14104148133017550866)

 -- | @AQ@. Description: Antarctica.
pattern AQ :: Region
pattern AQ = Region (Subtag 14106399932831236114)

 -- | @AR@. Description: Argentina.
pattern AR :: Region
pattern AR = Region (Subtag 14107525832738078738)

 -- | @AS@. Description: American Samoa.
pattern AS :: Region
pattern AS = Region (Subtag 14108651732644921362)

 -- | @AT@. Description: Austria.
pattern AT :: Region
pattern AT = Region (Subtag 14109777632551763986)

 -- | @AU@. Description: Australia.
pattern AU :: Region
pattern AU = Region (Subtag 14110903532458606610)

 -- | @AW@. Description: Aruba.
pattern AW :: Region
pattern AW = Region (Subtag 14113155332272291858)

 -- | @AX@. Description: Åland Islands.
pattern AX :: Region
pattern AX = Region (Subtag 14114281232179134482)

 -- | @AZ@. Description: Azerbaijan.
pattern AZ :: Region
pattern AZ = Region (Subtag 14116533031992819730)

 -- | @BA@. Description: Bosnia and Herzegovina.
pattern BA :: Region
pattern BA = Region (Subtag 14232500722397610002)

 -- | @BB@. Description: Barbados.
pattern BB :: Region
pattern BB = Region (Subtag 14233626622304452626)

 -- | @BD@. Description: Bangladesh.
pattern BD :: Region
pattern BD = Region (Subtag 14235878422118137874)

 -- | @BE@. Description: Belgium.
pattern BE :: Region
pattern BE = Region (Subtag 14237004322024980498)

 -- | @BF@. Description: Burkina Faso.
pattern BF :: Region
pattern BF = Region (Subtag 14238130221931823122)

 -- | @BG@. Description: Bulgaria.
pattern BG :: Region
pattern BG = Region (Subtag 14239256121838665746)

 -- | @BH@. Description: Bahrain.
pattern BH :: Region
pattern BH = Region (Subtag 14240382021745508370)

 -- | @BI@. Description: Burundi.
pattern BI :: Region
pattern BI = Region (Subtag 14241507921652350994)

 -- | @BJ@. Description: Benin.
pattern BJ :: Region
pattern BJ = Region (Subtag 14242633821559193618)

 -- | @BL@. Description: Saint Barthélemy.
pattern BL :: Region
pattern BL = Region (Subtag 14244885621372878866)

 -- | @BM@. Description: Bermuda.
pattern BM :: Region
pattern BM = Region (Subtag 14246011521279721490)

 -- | @BN@. Description: Brunei Darussalam.
pattern BN :: Region
pattern BN = Region (Subtag 14247137421186564114)

 -- | @BO@. Description: Bolivia.
pattern BO :: Region
pattern BO = Region (Subtag 14248263321093406738)

 -- | @BQ@. Description: Bonaire, Sint Eustatius and Saba.
pattern BQ :: Region
pattern BQ = Region (Subtag 14250515120907091986)

 -- | @BR@. Description: Brazil.
pattern BR :: Region
pattern BR = Region (Subtag 14251641020813934610)

 -- | @BS@. Description: Bahamas.
pattern BS :: Region
pattern BS = Region (Subtag 14252766920720777234)

 -- | @BT@. Description: Bhutan.
pattern BT :: Region
pattern BT = Region (Subtag 14253892820627619858)

 -- | @BU@. Description: Burma. Deprecated. Preferred value: MM.
pattern BU :: Region
pattern BU = Region (Subtag 14255018720534462482)

 -- | @BV@. Description: Bouvet Island.
pattern BV :: Region
pattern BV = Region (Subtag 14256144620441305106)

 -- | @BW@. Description: Botswana.
pattern BW :: Region
pattern BW = Region (Subtag 14257270520348147730)

 -- | @BY@. Description: Belarus.
pattern BY :: Region
pattern BY = Region (Subtag 14259522320161832978)

 -- | @BZ@. Description: Belize.
pattern BZ :: Region
pattern BZ = Region (Subtag 14260648220068675602)

 -- | @CA@. Description: Canada.
pattern CA :: Region
pattern CA = Region (Subtag 14376615910473465874)

 -- | @CC@. Description: Cocos (Keeling) Islands.
pattern CC :: Region
pattern CC = Region (Subtag 14378867710287151122)

 -- | @CD@. Description: The Democratic Republic of the Congo.
pattern CD :: Region
pattern CD = Region (Subtag 14379993610193993746)

 -- | @CF@. Description: Central African Republic.
pattern CF :: Region
pattern CF = Region (Subtag 14382245410007678994)

 -- | @CG@. Description: Congo.
pattern CG :: Region
pattern CG = Region (Subtag 14383371309914521618)

 -- | @CH@. Description: Switzerland.
pattern CH :: Region
pattern CH = Region (Subtag 14384497209821364242)

 -- | @CI@. Description: Côte d\'Ivoire.
pattern CI :: Region
pattern CI = Region (Subtag 14385623109728206866)

 -- | @CK@. Description: Cook Islands.
pattern CK :: Region
pattern CK = Region (Subtag 14387874909541892114)

 -- | @CL@. Description: Chile.
pattern CL :: Region
pattern CL = Region (Subtag 14389000809448734738)

 -- | @CM@. Description: Cameroon.
pattern CM :: Region
pattern CM = Region (Subtag 14390126709355577362)

 -- | @CN@. Description: China.
pattern CN :: Region
pattern CN = Region (Subtag 14391252609262419986)

 -- | @CO@. Description: Colombia.
pattern CO :: Region
pattern CO = Region (Subtag 14392378509169262610)

 -- | @CP@. Description: Clipperton Island.
pattern CP :: Region
pattern CP = Region (Subtag 14393504409076105234)

 -- | @CR@. Description: Costa Rica.
pattern CR :: Region
pattern CR = Region (Subtag 14395756208889790482)

 -- | @CS@. Description: Serbia and Montenegro. Deprecated.
pattern CS :: Region
pattern CS = Region (Subtag 14396882108796633106)

 -- | @CU@. Description: Cuba.
pattern CU :: Region
pattern CU = Region (Subtag 14399133908610318354)

 -- | @CV@. Description: Cabo Verde; Cape Verde.
pattern CV :: Region
pattern CV = Region (Subtag 14400259808517160978)

 -- | @CW@. Description: Curaçao.
pattern CW :: Region
pattern CW = Region (Subtag 14401385708424003602)

 -- | @CX@. Description: Christmas Island.
pattern CX :: Region
pattern CX = Region (Subtag 14402511608330846226)

 -- | @CY@. Description: Cyprus.
pattern CY :: Region
pattern CY = Region (Subtag 14403637508237688850)

 -- | @CZ@. Description: Czechia; Czech Republic.
pattern CZ :: Region
pattern CZ = Region (Subtag 14404763408144531474)

 -- | @DD@. Description: German Democratic Republic. Deprecated. Preferred value: DE.
pattern DD :: Region
pattern DD = Region (Subtag 14524108798269849618)

 -- | @DE@. Description: Germany.
pattern DE :: Region
pattern DE = Region (Subtag 14525234698176692242)

 -- | @DG@. Description: Diego Garcia.
pattern DG :: Region
pattern DG = Region (Subtag 14527486497990377490)

 -- | @DJ@. Description: Djibouti.
pattern DJ :: Region
pattern DJ = Region (Subtag 14530864197710905362)

 -- | @DK@. Description: Denmark.
pattern DK :: Region
pattern DK = Region (Subtag 14531990097617747986)

 -- | @DM@. Description: Dominica.
pattern DM :: Region
pattern DM = Region (Subtag 14534241897431433234)

 -- | @DO@. Description: Dominican Republic.
pattern DO :: Region
pattern DO = Region (Subtag 14536493697245118482)

 -- | @DZ@. Description: Algeria.
pattern DZ :: Region
pattern DZ = Region (Subtag 14548878596220387346)

 -- | @EA@. Description: Ceuta, Melilla.
pattern EA :: Region
pattern EA = Region (Subtag 14664846286625177618)

 -- | @EC@. Description: Ecuador.
pattern EC :: Region
pattern EC = Region (Subtag 14667098086438862866)

 -- | @EE@. Description: Estonia.
pattern EE :: Region
pattern EE = Region (Subtag 14669349886252548114)

 -- | @EG@. Description: Egypt.
pattern EG :: Region
pattern EG = Region (Subtag 14671601686066233362)

 -- | @EH@. Description: Western Sahara.
pattern EH :: Region
pattern EH = Region (Subtag 14672727585973075986)

 -- | @ER@. Description: Eritrea.
pattern ER :: Region
pattern ER = Region (Subtag 14683986585041502226)

 -- | @ES@. Description: Spain.
pattern ES :: Region
pattern ES = Region (Subtag 14685112484948344850)

 -- | @ET@. Description: Ethiopia.
pattern ET :: Region
pattern ET = Region (Subtag 14686238384855187474)

 -- | @EU@. Description: European Union.
pattern EU :: Region
pattern EU = Region (Subtag 14687364284762030098)

 -- | @EZ@. Description: Eurozone.
pattern EZ :: Region
pattern EZ = Region (Subtag 14692993784296243218)

 -- | @FI@. Description: Finland.
pattern FI :: Region
pattern FI = Region (Subtag 14817968673955774482)

 -- | @FJ@. Description: Fiji.
pattern FJ :: Region
pattern FJ = Region (Subtag 14819094573862617106)

 -- | @FK@. Description: Falkland Islands (Malvinas).
pattern FK :: Region
pattern FK = Region (Subtag 14820220473769459730)

 -- | @FM@. Description: Federated States of Micronesia.
pattern FM :: Region
pattern FM = Region (Subtag 14822472273583144978)

 -- | @FO@. Description: Faroe Islands.
pattern FO :: Region
pattern FO = Region (Subtag 14824724073396830226)

 -- | @FR@. Description: France.
pattern FR :: Region
pattern FR = Region (Subtag 14828101773117358098)

 -- | @FX@. Description: Metropolitan France. Deprecated. Preferred value: FR.
pattern FX :: Region
pattern FX = Region (Subtag 14834857172558413842)

 -- | @GA@. Description: Gabon.
pattern GA :: Region
pattern GA = Region (Subtag 14953076662776889362)

 -- | @GB@. Description: United Kingdom.
pattern GB :: Region
pattern GB = Region (Subtag 14954202562683731986)

 -- | @GD@. Description: Grenada.
pattern GD :: Region
pattern GD = Region (Subtag 14956454362497417234)

 -- | @GE@. Description: Georgia.
pattern GE :: Region
pattern GE = Region (Subtag 14957580262404259858)

 -- | @GF@. Description: French Guiana.
pattern GF :: Region
pattern GF = Region (Subtag 14958706162311102482)

 -- | @GG@. Description: Guernsey.
pattern GG :: Region
pattern GG = Region (Subtag 14959832062217945106)

 -- | @GH@. Description: Ghana.
pattern GH :: Region
pattern GH = Region (Subtag 14960957962124787730)

 -- | @GI@. Description: Gibraltar.
pattern GI :: Region
pattern GI = Region (Subtag 14962083862031630354)

 -- | @GL@. Description: Greenland.
pattern GL :: Region
pattern GL = Region (Subtag 14965461561752158226)

 -- | @GM@. Description: Gambia.
pattern GM :: Region
pattern GM = Region (Subtag 14966587461659000850)

 -- | @GN@. Description: Guinea.
pattern GN :: Region
pattern GN = Region (Subtag 14967713361565843474)

 -- | @GP@. Description: Guadeloupe.
pattern GP :: Region
pattern GP = Region (Subtag 14969965161379528722)

 -- | @GQ@. Description: Equatorial Guinea.
pattern GQ :: Region
pattern GQ = Region (Subtag 14971091061286371346)

 -- | @GR@. Description: Greece.
pattern GR :: Region
pattern GR = Region (Subtag 14972216961193213970)

 -- | @GS@. Description: South Georgia and the South Sandwich Islands.
pattern GS :: Region
pattern GS = Region (Subtag 14973342861100056594)

 -- | @GT@. Description: Guatemala.
pattern GT :: Region
pattern GT = Region (Subtag 14974468761006899218)

 -- | @GU@. Description: Guam.
pattern GU :: Region
pattern GU = Region (Subtag 14975594660913741842)

 -- | @GW@. Description: Guinea-Bissau.
pattern GW :: Region
pattern GW = Region (Subtag 14977846460727427090)

 -- | @GY@. Description: Guyana.
pattern GY :: Region
pattern GY = Region (Subtag 14980098260541112338)

 -- | @HK@. Description: Hong Kong.
pattern HK :: Region
pattern HK = Region (Subtag 15108450849921171474)

 -- | @HM@. Description: Heard Island and McDonald Islands.
pattern HM :: Region
pattern HM = Region (Subtag 15110702649734856722)

 -- | @HN@. Description: Honduras.
pattern HN :: Region
pattern HN = Region (Subtag 15111828549641699346)

 -- | @HR@. Description: Croatia.
pattern HR :: Region
pattern HR = Region (Subtag 15116332149269069842)

 -- | @HT@. Description: Haiti.
pattern HT :: Region
pattern HT = Region (Subtag 15118583949082755090)

 -- | @HU@. Description: Hungary.
pattern HU :: Region
pattern HU = Region (Subtag 15119709848989597714)

 -- | @IC@. Description: Canary Islands.
pattern IC :: Region
pattern IC = Region (Subtag 15243558838742286354)

 -- | @ID@. Description: Indonesia.
pattern ID :: Region
pattern ID = Region (Subtag 15244684738649128978)

 -- | @IE@. Description: Ireland.
pattern IE :: Region
pattern IE = Region (Subtag 15245810638555971602)

 -- | @IL@. Description: Israel.
pattern IL :: Region
pattern IL = Region (Subtag 15253691937903869970)

 -- | @IM@. Description: Isle of Man.
pattern IM :: Region
pattern IM = Region (Subtag 15254817837810712594)

 -- | @IN@. Description: India.
pattern IN :: Region
pattern IN = Region (Subtag 15255943737717555218)

 -- | @IO@. Description: British Indian Ocean Territory.
pattern IO :: Region
pattern IO = Region (Subtag 15257069637624397842)

 -- | @IQ@. Description: Iraq.
pattern IQ :: Region
pattern IQ = Region (Subtag 15259321437438083090)

 -- | @IR@. Description: Islamic Republic of Iran.
pattern IR :: Region
pattern IR = Region (Subtag 15260447337344925714)

 -- | @IS@. Description: Iceland.
pattern IS :: Region
pattern IS = Region (Subtag 15261573237251768338)

 -- | @IT@. Description: Italy.
pattern IT :: Region
pattern IT = Region (Subtag 15262699137158610962)

 -- | @JE@. Description: Jersey.
pattern JE :: Region
pattern JE = Region (Subtag 15389925826631827474)

 -- | @JM@. Description: Jamaica.
pattern JM :: Region
pattern JM = Region (Subtag 15398933025886568466)

 -- | @JO@. Description: Jordan.
pattern JO :: Region
pattern JO = Region (Subtag 15401184825700253714)

 -- | @JP@. Description: Japan.
pattern JP :: Region
pattern JP = Region (Subtag 15402310725607096338)

 -- | @KE@. Description: Kenya.
pattern KE :: Region
pattern KE = Region (Subtag 15534041014707683346)

 -- | @KG@. Description: Kyrgyzstan.
pattern KG :: Region
pattern KG = Region (Subtag 15536292814521368594)

 -- | @KH@. Description: Cambodia.
pattern KH :: Region
pattern KH = Region (Subtag 15537418714428211218)

 -- | @KI@. Description: Kiribati.
pattern KI :: Region
pattern KI = Region (Subtag 15538544614335053842)

 -- | @KM@. Description: Comoros.
pattern KM :: Region
pattern KM = Region (Subtag 15543048213962424338)

 -- | @KN@. Description: Saint Kitts and Nevis.
pattern KN :: Region
pattern KN = Region (Subtag 15544174113869266962)

 -- | @KP@. Description: Democratic People\'s Republic of Korea.
pattern KP :: Region
pattern KP = Region (Subtag 15546425913682952210)

 -- | @KR@. Description: Republic of Korea.
pattern KR :: Region
pattern KR = Region (Subtag 15548677713496637458)

 -- | @KW@. Description: Kuwait.
pattern KW :: Region
pattern KW = Region (Subtag 15554307213030850578)

 -- | @KY@. Description: Cayman Islands.
pattern KY :: Region
pattern KY = Region (Subtag 15556559012844535826)

 -- | @KZ@. Description: Kazakhstan.
pattern KZ :: Region
pattern KZ = Region (Subtag 15557684912751378450)

 -- | @LA@. Description: Lao People\'s Democratic Republic.
pattern LA :: Region
pattern LA = Region (Subtag 15673652603156168722)

 -- | @LB@. Description: Lebanon.
pattern LB :: Region
pattern LB = Region (Subtag 15674778503063011346)

 -- | @LC@. Description: Saint Lucia.
pattern LC :: Region
pattern LC = Region (Subtag 15675904402969853970)

 -- | @LI@. Description: Liechtenstein.
pattern LI :: Region
pattern LI = Region (Subtag 15682659802410909714)

 -- | @LK@. Description: Sri Lanka.
pattern LK :: Region
pattern LK = Region (Subtag 15684911602224594962)

 -- | @LR@. Description: Liberia.
pattern LR :: Region
pattern LR = Region (Subtag 15692792901572493330)

 -- | @LS@. Description: Lesotho.
pattern LS :: Region
pattern LS = Region (Subtag 15693918801479335954)

 -- | @LT@. Description: Lithuania.
pattern LT :: Region
pattern LT = Region (Subtag 15695044701386178578)

 -- | @LU@. Description: Luxembourg.
pattern LU :: Region
pattern LU = Region (Subtag 15696170601293021202)

 -- | @LV@. Description: Latvia.
pattern LV :: Region
pattern LV = Region (Subtag 15697296501199863826)

 -- | @LY@. Description: Libya.
pattern LY :: Region
pattern LY = Region (Subtag 15700674200920391698)

 -- | @MA@. Description: Morocco.
pattern MA :: Region
pattern MA = Region (Subtag 15817767791232024594)

 -- | @MC@. Description: Monaco.
pattern MC :: Region
pattern MC = Region (Subtag 15820019591045709842)

 -- | @MD@. Description: Moldova.
pattern MD :: Region
pattern MD = Region (Subtag 15821145490952552466)

 -- | @ME@. Description: Montenegro.
pattern ME :: Region
pattern ME = Region (Subtag 15822271390859395090)

 -- | @MF@. Description: Saint Martin (French part).
pattern MF :: Region
pattern MF = Region (Subtag 15823397290766237714)

 -- | @MG@. Description: Madagascar.
pattern MG :: Region
pattern MG = Region (Subtag 15824523190673080338)

 -- | @MH@. Description: Marshall Islands.
pattern MH :: Region
pattern MH = Region (Subtag 15825649090579922962)

 -- | @MK@. Description: North Macedonia.
pattern MK :: Region
pattern MK = Region (Subtag 15829026790300450834)

 -- | @ML@. Description: Mali.
pattern ML :: Region
pattern ML = Region (Subtag 15830152690207293458)

 -- | @MM@. Description: Myanmar.
pattern MM :: Region
pattern MM = Region (Subtag 15831278590114136082)

 -- | @MN@. Description: Mongolia.
pattern MN :: Region
pattern MN = Region (Subtag 15832404490020978706)

 -- | @MO@. Description: Macao.
pattern MO :: Region
pattern MO = Region (Subtag 15833530389927821330)

 -- | @MP@. Description: Northern Mariana Islands.
pattern MP :: Region
pattern MP = Region (Subtag 15834656289834663954)

 -- | @MQ@. Description: Martinique.
pattern MQ :: Region
pattern MQ = Region (Subtag 15835782189741506578)

 -- | @MR@. Description: Mauritania.
pattern MR :: Region
pattern MR = Region (Subtag 15836908089648349202)

 -- | @MS@. Description: Montserrat.
pattern MS :: Region
pattern MS = Region (Subtag 15838033989555191826)

 -- | @MT@. Description: Malta.
pattern MT :: Region
pattern MT = Region (Subtag 15839159889462034450)

 -- | @MU@. Description: Mauritius.
pattern MU :: Region
pattern MU = Region (Subtag 15840285789368877074)

 -- | @MV@. Description: Maldives.
pattern MV :: Region
pattern MV = Region (Subtag 15841411689275719698)

 -- | @MW@. Description: Malawi.
pattern MW :: Region
pattern MW = Region (Subtag 15842537589182562322)

 -- | @MX@. Description: Mexico.
pattern MX :: Region
pattern MX = Region (Subtag 15843663489089404946)

 -- | @MY@. Description: Malaysia.
pattern MY :: Region
pattern MY = Region (Subtag 15844789388996247570)

 -- | @MZ@. Description: Mozambique.
pattern MZ :: Region
pattern MZ = Region (Subtag 15845915288903090194)

 -- | @NA@. Description: Namibia.
pattern NA :: Region
pattern NA = Region (Subtag 15961882979307880466)

 -- | @NC@. Description: New Caledonia.
pattern NC :: Region
pattern NC = Region (Subtag 15964134779121565714)

 -- | @NE@. Description: Niger.
pattern NE :: Region
pattern NE = Region (Subtag 15966386578935250962)

 -- | @NF@. Description: Norfolk Island.
pattern NF :: Region
pattern NF = Region (Subtag 15967512478842093586)

 -- | @NG@. Description: Nigeria.
pattern NG :: Region
pattern NG = Region (Subtag 15968638378748936210)

 -- | @NI@. Description: Nicaragua.
pattern NI :: Region
pattern NI = Region (Subtag 15970890178562621458)

 -- | @NL@. Description: Netherlands.
pattern NL :: Region
pattern NL = Region (Subtag 15974267878283149330)

 -- | @NO@. Description: Norway.
pattern NO :: Region
pattern NO = Region (Subtag 15977645578003677202)

 -- | @NP@. Description: Nepal.
pattern NP :: Region
pattern NP = Region (Subtag 15978771477910519826)

 -- | @NR@. Description: Nauru.
pattern NR :: Region
pattern NR = Region (Subtag 15981023277724205074)

 -- | @NT@. Description: Neutral Zone. Deprecated.
pattern NT :: Region
pattern NT = Region (Subtag 15983275077537890322)

 -- | @NU@. Description: Niue.
pattern NU :: Region
pattern NU = Region (Subtag 15984400977444732946)

 -- | @NZ@. Description: New Zealand.
pattern NZ :: Region
pattern NZ = Region (Subtag 15990030476978946066)

 -- | @OM@. Description: Oman.
pattern OM :: Region
pattern OM = Region (Subtag 16119508966265847826)

 -- | @PA@. Description: Panama.
pattern PA :: Region
pattern PA = Region (Subtag 16250113355459592210)

 -- | @PE@. Description: Peru.
pattern PE :: Region
pattern PE = Region (Subtag 16254616955086962706)

 -- | @PF@. Description: French Polynesia.
pattern PF :: Region
pattern PF = Region (Subtag 16255742854993805330)

 -- | @PG@. Description: Papua New Guinea.
pattern PG :: Region
pattern PG = Region (Subtag 16256868754900647954)

 -- | @PH@. Description: Philippines.
pattern PH :: Region
pattern PH = Region (Subtag 16257994654807490578)

 -- | @PK@. Description: Pakistan.
pattern PK :: Region
pattern PK = Region (Subtag 16261372354528018450)

 -- | @PL@. Description: Poland.
pattern PL :: Region
pattern PL = Region (Subtag 16262498254434861074)

 -- | @PM@. Description: Saint Pierre and Miquelon.
pattern PM :: Region
pattern PM = Region (Subtag 16263624154341703698)

 -- | @PN@. Description: Pitcairn.
pattern PN :: Region
pattern PN = Region (Subtag 16264750054248546322)

 -- | @PR@. Description: Puerto Rico.
pattern PR :: Region
pattern PR = Region (Subtag 16269253653875916818)

 -- | @PS@. Description: State of Palestine.
pattern PS :: Region
pattern PS = Region (Subtag 16270379553782759442)

 -- | @PT@. Description: Portugal.
pattern PT :: Region
pattern PT = Region (Subtag 16271505453689602066)

 -- | @PW@. Description: Palau.
pattern PW :: Region
pattern PW = Region (Subtag 16274883153410129938)

 -- | @PY@. Description: Paraguay.
pattern PY :: Region
pattern PY = Region (Subtag 16277134953223815186)

 -- | @QA@. Description: Qatar.
pattern QA :: Region
pattern QA = Region (Subtag 16394228543535448082)

 -- | @QM@. Description: Private use.
pattern QM :: Region
pattern QM = Region (Subtag 16407739342417559570)

 -- | @QN@. Description: Private use.
pattern QN :: Region
pattern QN = Region (Subtag 16408865242324402194)

 -- | @QO@. Description: Private use.
pattern QO :: Region
pattern QO = Region (Subtag 16409991142231244818)

 -- | @QP@. Description: Private use.
pattern QP :: Region
pattern QP = Region (Subtag 16411117042138087442)

 -- | @QQ@. Description: Private use.
pattern QQ :: Region
pattern QQ = Region (Subtag 16412242942044930066)

 -- | @QR@. Description: Private use.
pattern QR :: Region
pattern QR = Region (Subtag 16413368841951772690)

 -- | @QS@. Description: Private use.
pattern QS :: Region
pattern QS = Region (Subtag 16414494741858615314)

 -- | @QT@. Description: Private use.
pattern QT :: Region
pattern QT = Region (Subtag 16415620641765457938)

 -- | @QU@. Description: Private use.
pattern QU :: Region
pattern QU = Region (Subtag 16416746541672300562)

 -- | @QV@. Description: Private use.
pattern QV :: Region
pattern QV = Region (Subtag 16417872441579143186)

 -- | @QW@. Description: Private use.
pattern QW :: Region
pattern QW = Region (Subtag 16418998341485985810)

 -- | @QX@. Description: Private use.
pattern QX :: Region
pattern QX = Region (Subtag 16420124241392828434)

 -- | @QY@. Description: Private use.
pattern QY :: Region
pattern QY = Region (Subtag 16421250141299671058)

 -- | @QZ@. Description: Private use.
pattern QZ :: Region
pattern QZ = Region (Subtag 16422376041206513682)

 -- | @RE@. Description: Réunion.
pattern RE :: Region
pattern RE = Region (Subtag 16542847331238674450)

 -- | @RO@. Description: Romania.
pattern RO :: Region
pattern RO = Region (Subtag 16554106330307100690)

 -- | @RS@. Description: Serbia.
pattern RS :: Region
pattern RS = Region (Subtag 16558609929934471186)

 -- | @RU@. Description: Russian Federation.
pattern RU :: Region
pattern RU = Region (Subtag 16560861729748156434)

 -- | @RW@. Description: Rwanda.
pattern RW :: Region
pattern RW = Region (Subtag 16563113529561841682)

 -- | @SA@. Description: Saudi Arabia.
pattern SA :: Region
pattern SA = Region (Subtag 16682458919687159826)

 -- | @SB@. Description: Solomon Islands.
pattern SB :: Region
pattern SB = Region (Subtag 16683584819594002450)

 -- | @SC@. Description: Seychelles.
pattern SC :: Region
pattern SC = Region (Subtag 16684710719500845074)

 -- | @SD@. Description: Sudan.
pattern SD :: Region
pattern SD = Region (Subtag 16685836619407687698)

 -- | @SE@. Description: Sweden.
pattern SE :: Region
pattern SE = Region (Subtag 16686962519314530322)

 -- | @SG@. Description: Singapore.
pattern SG :: Region
pattern SG = Region (Subtag 16689214319128215570)

 -- | @SH@. Description: Saint Helena, Ascension and Tristan da Cunha.
pattern SH :: Region
pattern SH = Region (Subtag 16690340219035058194)

 -- | @SI@. Description: Slovenia.
pattern SI :: Region
pattern SI = Region (Subtag 16691466118941900818)

 -- | @SJ@. Description: Svalbard and Jan Mayen.
pattern SJ :: Region
pattern SJ = Region (Subtag 16692592018848743442)

 -- | @SK@. Description: Slovakia.
pattern SK :: Region
pattern SK = Region (Subtag 16693717918755586066)

 -- | @SL@. Description: Sierra Leone.
pattern SL :: Region
pattern SL = Region (Subtag 16694843818662428690)

 -- | @SM@. Description: San Marino.
pattern SM :: Region
pattern SM = Region (Subtag 16695969718569271314)

 -- | @SN@. Description: Senegal.
pattern SN :: Region
pattern SN = Region (Subtag 16697095618476113938)

 -- | @SO@. Description: Somalia.
pattern SO :: Region
pattern SO = Region (Subtag 16698221518382956562)

 -- | @SR@. Description: Suriname.
pattern SR :: Region
pattern SR = Region (Subtag 16701599218103484434)

 -- | @SS@. Description: South Sudan.
pattern SS :: Region
pattern SS = Region (Subtag 16702725118010327058)

 -- | @ST@. Description: Sao Tome and Principe.
pattern ST :: Region
pattern ST = Region (Subtag 16703851017917169682)

 -- | @SU@. Description: Union of Soviet Socialist Republics. Deprecated.
pattern SU :: Region
pattern SU = Region (Subtag 16704976917824012306)

 -- | @SV@. Description: El Salvador.
pattern SV :: Region
pattern SV = Region (Subtag 16706102817730854930)

 -- | @SX@. Description: Sint Maarten (Dutch part).
pattern SX :: Region
pattern SX = Region (Subtag 16708354617544540178)

 -- | @SY@. Description: Syrian Arab Republic.
pattern SY :: Region
pattern SY = Region (Subtag 16709480517451382802)

 -- | @SZ@. Description: Eswatini; eSwatini; Swaziland.
pattern SZ :: Region
pattern SZ = Region (Subtag 16710606417358225426)

 -- | @TA@. Description: Tristan da Cunha.
pattern TA :: Region
pattern TA = Region (Subtag 16826574107763015698)

 -- | @TC@. Description: Turks and Caicos Islands.
pattern TC :: Region
pattern TC = Region (Subtag 16828825907576700946)

 -- | @TD@. Description: Chad.
pattern TD :: Region
pattern TD = Region (Subtag 16829951807483543570)

 -- | @TF@. Description: French Southern Territories.
pattern TF :: Region
pattern TF = Region (Subtag 16832203607297228818)

 -- | @TG@. Description: Togo.
pattern TG :: Region
pattern TG = Region (Subtag 16833329507204071442)

 -- | @TH@. Description: Thailand.
pattern TH :: Region
pattern TH = Region (Subtag 16834455407110914066)

 -- | @TJ@. Description: Tajikistan.
pattern TJ :: Region
pattern TJ = Region (Subtag 16836707206924599314)

 -- | @TK@. Description: Tokelau.
pattern TK :: Region
pattern TK = Region (Subtag 16837833106831441938)

 -- | @TL@. Description: Timor-Leste.
pattern TL :: Region
pattern TL = Region (Subtag 16838959006738284562)

 -- | @TM@. Description: Turkmenistan.
pattern TM :: Region
pattern TM = Region (Subtag 16840084906645127186)

 -- | @TN@. Description: Tunisia.
pattern TN :: Region
pattern TN = Region (Subtag 16841210806551969810)

 -- | @TO@. Description: Tonga.
pattern TO :: Region
pattern TO = Region (Subtag 16842336706458812434)

 -- | @TP@. Description: East Timor. Deprecated. Preferred value: TL.
pattern TP :: Region
pattern TP = Region (Subtag 16843462606365655058)

 -- | @TR@. Description: Turkey.
pattern TR :: Region
pattern TR = Region (Subtag 16845714406179340306)

 -- | @TT@. Description: Trinidad and Tobago.
pattern TT :: Region
pattern TT = Region (Subtag 16847966205993025554)

 -- | @TV@. Description: Tuvalu.
pattern TV :: Region
pattern TV = Region (Subtag 16850218005806710802)

 -- | @TW@. Description: Taiwan, Province of China.
pattern TW :: Region
pattern TW = Region (Subtag 16851343905713553426)

 -- | @TZ@. Description: United Republic of Tanzania.
pattern TZ :: Region
pattern TZ = Region (Subtag 16854721605434081298)

 -- | @UA@. Description: Ukraine.
pattern UA :: Region
pattern UA = Region (Subtag 16970689295838871570)

 -- | @UG@. Description: Uganda.
pattern UG :: Region
pattern UG = Region (Subtag 16977444695279927314)

 -- | @UM@. Description: United States Minor Outlying Islands.
pattern UM :: Region
pattern UM = Region (Subtag 16984200094720983058)

 -- | @UN@. Description: United Nations.
pattern UN :: Region
pattern UN = Region (Subtag 16985325994627825682)

 -- | @US@. Description: United States.
pattern US :: Region
pattern US = Region (Subtag 16990955494162038802)

 -- | @UY@. Description: Uruguay.
pattern UY :: Region
pattern UY = Region (Subtag 16997710893603094546)

 -- | @UZ@. Description: Uzbekistan.
pattern UZ :: Region
pattern UZ = Region (Subtag 16998836793509937170)

 -- | @VA@. Description: Holy See (Vatican City State).
pattern VA :: Region
pattern VA = Region (Subtag 17114804483914727442)

 -- | @VC@. Description: Saint Vincent and the Grenadines.
pattern VC :: Region
pattern VC = Region (Subtag 17117056283728412690)

 -- | @VE@. Description: Venezuela.
pattern VE :: Region
pattern VE = Region (Subtag 17119308083542097938)

 -- | @VG@. Description: British Virgin Islands.
pattern VG :: Region
pattern VG = Region (Subtag 17121559883355783186)

 -- | @VI@. Description: U.S. Virgin Islands.
pattern VI :: Region
pattern VI = Region (Subtag 17123811683169468434)

 -- | @VN@. Description: Viet Nam.
pattern VN :: Region
pattern VN = Region (Subtag 17129441182703681554)

 -- | @VU@. Description: Vanuatu.
pattern VU :: Region
pattern VU = Region (Subtag 17137322482051579922)

 -- | @WF@. Description: Wallis and Futuna.
pattern WF :: Region
pattern WF = Region (Subtag 17264549171524796434)

 -- | @WS@. Description: Samoa.
pattern WS :: Region
pattern WS = Region (Subtag 17279185870313750546)

 -- | @XA@. Description: Private use.
pattern XA :: Region
pattern XA = Region (Subtag 17403034860066439186)

 -- | @XB@. Description: Private use.
pattern XB :: Region
pattern XB = Region (Subtag 17404160759973281810)

 -- | @XC@. Description: Private use.
pattern XC :: Region
pattern XC = Region (Subtag 17405286659880124434)

 -- | @XD@. Description: Private use.
pattern XD :: Region
pattern XD = Region (Subtag 17406412559786967058)

 -- | @XE@. Description: Private use.
pattern XE :: Region
pattern XE = Region (Subtag 17407538459693809682)

 -- | @XF@. Description: Private use.
pattern XF :: Region
pattern XF = Region (Subtag 17408664359600652306)

 -- | @XG@. Description: Private use.
pattern XG :: Region
pattern XG = Region (Subtag 17409790259507494930)

 -- | @XH@. Description: Private use.
pattern XH :: Region
pattern XH = Region (Subtag 17410916159414337554)

 -- | @XI@. Description: Private use.
pattern XI :: Region
pattern XI = Region (Subtag 17412042059321180178)

 -- | @XJ@. Description: Private use.
pattern XJ :: Region
pattern XJ = Region (Subtag 17413167959228022802)

 -- | @XK@. Description: Private use.
pattern XK :: Region
pattern XK = Region (Subtag 17414293859134865426)

 -- | @XL@. Description: Private use.
pattern XL :: Region
pattern XL = Region (Subtag 17415419759041708050)

 -- | @XM@. Description: Private use.
pattern XM :: Region
pattern XM = Region (Subtag 17416545658948550674)

 -- | @XN@. Description: Private use.
pattern XN :: Region
pattern XN = Region (Subtag 17417671558855393298)

 -- | @XO@. Description: Private use.
pattern XO :: Region
pattern XO = Region (Subtag 17418797458762235922)

 -- | @XP@. Description: Private use.
pattern XP :: Region
pattern XP = Region (Subtag 17419923358669078546)

 -- | @XQ@. Description: Private use.
pattern XQ :: Region
pattern XQ = Region (Subtag 17421049258575921170)

 -- | @XR@. Description: Private use.
pattern XR :: Region
pattern XR = Region (Subtag 17422175158482763794)

 -- | @XS@. Description: Private use.
pattern XS :: Region
pattern XS = Region (Subtag 17423301058389606418)

 -- | @XT@. Description: Private use.
pattern XT :: Region
pattern XT = Region (Subtag 17424426958296449042)

 -- | @XU@. Description: Private use.
pattern XU :: Region
pattern XU = Region (Subtag 17425552858203291666)

 -- | @XV@. Description: Private use.
pattern XV :: Region
pattern XV = Region (Subtag 17426678758110134290)

 -- | @XW@. Description: Private use.
pattern XW :: Region
pattern XW = Region (Subtag 17427804658016976914)

 -- | @XX@. Description: Private use.
pattern XX :: Region
pattern XX = Region (Subtag 17428930557923819538)

 -- | @XY@. Description: Private use.
pattern XY :: Region
pattern XY = Region (Subtag 17430056457830662162)

 -- | @XZ@. Description: Private use.
pattern XZ :: Region
pattern XZ = Region (Subtag 17431182357737504786)

 -- | @YD@. Description: Democratic Yemen. Deprecated. Preferred value: YE.
pattern YD :: Region
pattern YD = Region (Subtag 17550527747862822930)

 -- | @YE@. Description: Yemen.
pattern YE :: Region
pattern YE = Region (Subtag 17551653647769665554)

 -- | @YT@. Description: Mayotte.
pattern YT :: Region
pattern YT = Region (Subtag 17568542146372304914)

 -- | @YU@. Description: Yugoslavia. Deprecated.
pattern YU :: Region
pattern YU = Region (Subtag 17569668046279147538)

 -- | @ZA@. Description: South Africa.
pattern ZA :: Region
pattern ZA = Region (Subtag 17691265236218150930)

 -- | @ZM@. Description: Zambia.
pattern ZM :: Region
pattern ZM = Region (Subtag 17704776035100262418)

 -- | @ZR@. Description: Zaire. Deprecated. Preferred value: CD.
pattern ZR :: Region
pattern ZR = Region (Subtag 17710405534634475538)

 -- | @ZW@. Description: Zimbabwe.
pattern ZW :: Region
pattern ZW = Region (Subtag 17716035034168688658)

 -- | @ZZ@. Description: Private use.
pattern ZZ :: Region
pattern ZZ = Region (Subtag 17719412733889216530)
