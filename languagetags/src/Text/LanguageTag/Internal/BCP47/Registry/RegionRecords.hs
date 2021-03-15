-- This is an auto-generated file. Do not edit by hand.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LanguageTag.Internal.BCP47.Registry.RegionRecords
  (lookupRegionDetails, validateRegion, regionToSubtag, lookupRegionRecord) where

import Prelude hiding (LT, GT)
import Text.LanguageTag.Internal.BCP47.Registry.Region
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag(..))

-- | The subtag and record information associated to the 'Region' type.
regionDetails :: Vector (Subtag, RegionRecord)
regionDetails = V.fromList
  [(Subtag 6972003231727616035, RegionRecord ("World" :| []) NotDeprecated)
  ,(Subtag 6972012027820638243, RegionRecord ("Africa" :| []) NotDeprecated)
  ,(Subtag 6972020823913660451, RegionRecord ("North America" :| []) NotDeprecated)
  ,(Subtag 6972038416099704867, RegionRecord ("South America" :| []) NotDeprecated)
  ,(Subtag 6972073600471793699, RegionRecord ("Oceania" :| []) NotDeprecated)
  ,(Subtag 6973129131634458659, RegionRecord ("Western Africa" :| []) NotDeprecated)
  ,(Subtag 6973146723820503075, RegionRecord ("Central America" :| []) NotDeprecated)
  ,(Subtag 6973155519913525283, RegionRecord ("Eastern Africa" :| []) NotDeprecated)
  ,(Subtag 6973164316006547491, RegionRecord ("Northern Africa" :| []) NotDeprecated)
  ,(Subtag 6973181908192591907, RegionRecord ("Middle Africa" :| []) NotDeprecated)
  ,(Subtag 6973190704285614115, RegionRecord ("Southern Africa" :| []) NotDeprecated)
  ,(Subtag 6973199500378636323, RegionRecord ("Americas" :| []) NotDeprecated)
  ,(Subtag 6974255031541301283, RegionRecord ("Northern America" :| []) NotDeprecated)
  ,(Subtag 6974325400285478947, RegionRecord ("Caribbean" :| []) NotDeprecated)
  ,(Subtag 6975372135355121699, RegionRecord ("Eastern Asia" :| []) NotDeprecated)
  ,(Subtag 6975407319727210531, RegionRecord ("Southern Asia" :| []) NotDeprecated)
  ,(Subtag 6975416115820232739, RegionRecord ("South-Eastern Asia" :| []) NotDeprecated)
  ,(Subtag 6975451300192321571, RegionRecord ("Southern Europe" :| []) NotDeprecated)
  ,(Subtag 6977650323447873571, RegionRecord ("Australia and New Zealand" :| []) NotDeprecated)
  ,(Subtag 6977659119540895779, RegionRecord ("Melanesia" :| []) NotDeprecated)
  ,(Subtag 6977685507819962403, RegionRecord ("Micronesia" :| []) NotDeprecated)
  ,(Subtag 6978758631168671779, RegionRecord ("Polynesia" :| []) NotDeprecated)
  ,(Subtag 7120630815523864611, RegionRecord ("Asia" :| []) NotDeprecated)
  ,(Subtag 7120639611616886819, RegionRecord ("Central Asia" :| []) NotDeprecated)
  ,(Subtag 7120657203802931235, RegionRecord ("Western Asia" :| []) NotDeprecated)
  ,(Subtag 7121739123244662819, RegionRecord ("Europe" :| []) NotDeprecated)
  ,(Subtag 7121747919337685027, RegionRecord ("Eastern Europe" :| []) NotDeprecated)
  ,(Subtag 7121774307616751651, RegionRecord ("Northern Europe" :| []) NotDeprecated)
  ,(Subtag 7121783103709773859, RegionRecord ("Western Europe" :| []) NotDeprecated)
  ,(Subtag 7260242403972349987, RegionRecord ("Sub-Saharan Africa" :| []) NotDeprecated)
  ,(Subtag 7549660252682059811, RegionRecord ("Latin America and the Caribbean" :| []) NotDeprecated)
  ,(Subtag 14088385534321754130, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 14090637334135439378, RegionRecord ("Ascension Island" :| []) NotDeprecated)
  ,(Subtag 14091763234042282002, RegionRecord ("Andorra" :| []) NotDeprecated)
  ,(Subtag 14092889133949124626, RegionRecord ("United Arab Emirates" :| []) NotDeprecated)
  ,(Subtag 14094015033855967250, RegionRecord ("Afghanistan" :| []) NotDeprecated)
  ,(Subtag 14095140933762809874, RegionRecord ("Antigua and Barbuda" :| []) NotDeprecated)
  ,(Subtag 14097392733576495122, RegionRecord ("Anguilla" :| []) NotDeprecated)
  ,(Subtag 14100770433297022994, RegionRecord ("Albania" :| []) NotDeprecated)
  ,(Subtag 14101896333203865618, RegionRecord ("Armenia" :| []) NotDeprecated)
  ,(Subtag 14103022233110708242, RegionRecord ("Netherlands Antilles" :| []) DeprecatedSimple)
  ,(Subtag 14104148133017550866, RegionRecord ("Angola" :| []) NotDeprecated)
  ,(Subtag 14106399932831236114, RegionRecord ("Antarctica" :| []) NotDeprecated)
  ,(Subtag 14107525832738078738, RegionRecord ("Argentina" :| []) NotDeprecated)
  ,(Subtag 14108651732644921362, RegionRecord ("American Samoa" :| []) NotDeprecated)
  ,(Subtag 14109777632551763986, RegionRecord ("Austria" :| []) NotDeprecated)
  ,(Subtag 14110903532458606610, RegionRecord ("Australia" :| []) NotDeprecated)
  ,(Subtag 14113155332272291858, RegionRecord ("Aruba" :| []) NotDeprecated)
  ,(Subtag 14114281232179134482, RegionRecord ("\197land Islands" :| []) NotDeprecated)
  ,(Subtag 14116533031992819730, RegionRecord ("Azerbaijan" :| []) NotDeprecated)
  ,(Subtag 14232500722397610002, RegionRecord ("Bosnia and Herzegovina" :| []) NotDeprecated)
  ,(Subtag 14233626622304452626, RegionRecord ("Barbados" :| []) NotDeprecated)
  ,(Subtag 14235878422118137874, RegionRecord ("Bangladesh" :| []) NotDeprecated)
  ,(Subtag 14237004322024980498, RegionRecord ("Belgium" :| []) NotDeprecated)
  ,(Subtag 14238130221931823122, RegionRecord ("Burkina Faso" :| []) NotDeprecated)
  ,(Subtag 14239256121838665746, RegionRecord ("Bulgaria" :| []) NotDeprecated)
  ,(Subtag 14240382021745508370, RegionRecord ("Bahrain" :| []) NotDeprecated)
  ,(Subtag 14241507921652350994, RegionRecord ("Burundi" :| []) NotDeprecated)
  ,(Subtag 14242633821559193618, RegionRecord ("Benin" :| []) NotDeprecated)
  ,(Subtag 14244885621372878866, RegionRecord ("Saint Barth\233lemy" :| []) NotDeprecated)
  ,(Subtag 14246011521279721490, RegionRecord ("Bermuda" :| []) NotDeprecated)
  ,(Subtag 14247137421186564114, RegionRecord ("Brunei Darussalam" :| []) NotDeprecated)
  ,(Subtag 14248263321093406738, RegionRecord ("Bolivia" :| []) NotDeprecated)
  ,(Subtag 14250515120907091986, RegionRecord ("Bonaire, Sint Eustatius and Saba" :| []) NotDeprecated)
  ,(Subtag 14251641020813934610, RegionRecord ("Brazil" :| []) NotDeprecated)
  ,(Subtag 14252766920720777234, RegionRecord ("Bahamas" :| []) NotDeprecated)
  ,(Subtag 14253892820627619858, RegionRecord ("Bhutan" :| []) NotDeprecated)
  ,(Subtag 14255018720534462482, RegionRecord ("Burma" :| []) (DeprecatedPreferred MM))
  ,(Subtag 14256144620441305106, RegionRecord ("Bouvet Island" :| []) NotDeprecated)
  ,(Subtag 14257270520348147730, RegionRecord ("Botswana" :| []) NotDeprecated)
  ,(Subtag 14259522320161832978, RegionRecord ("Belarus" :| []) NotDeprecated)
  ,(Subtag 14260648220068675602, RegionRecord ("Belize" :| []) NotDeprecated)
  ,(Subtag 14376615910473465874, RegionRecord ("Canada" :| []) NotDeprecated)
  ,(Subtag 14378867710287151122, RegionRecord ("Cocos (Keeling) Islands" :| []) NotDeprecated)
  ,(Subtag 14379993610193993746, RegionRecord ("The Democratic Republic of the Congo" :| []) NotDeprecated)
  ,(Subtag 14382245410007678994, RegionRecord ("Central African Republic" :| []) NotDeprecated)
  ,(Subtag 14383371309914521618, RegionRecord ("Congo" :| []) NotDeprecated)
  ,(Subtag 14384497209821364242, RegionRecord ("Switzerland" :| []) NotDeprecated)
  ,(Subtag 14385623109728206866, RegionRecord ("C\244te d'Ivoire" :| []) NotDeprecated)
  ,(Subtag 14387874909541892114, RegionRecord ("Cook Islands" :| []) NotDeprecated)
  ,(Subtag 14389000809448734738, RegionRecord ("Chile" :| []) NotDeprecated)
  ,(Subtag 14390126709355577362, RegionRecord ("Cameroon" :| []) NotDeprecated)
  ,(Subtag 14391252609262419986, RegionRecord ("China" :| []) NotDeprecated)
  ,(Subtag 14392378509169262610, RegionRecord ("Colombia" :| []) NotDeprecated)
  ,(Subtag 14393504409076105234, RegionRecord ("Clipperton Island" :| []) NotDeprecated)
  ,(Subtag 14395756208889790482, RegionRecord ("Costa Rica" :| []) NotDeprecated)
  ,(Subtag 14396882108796633106, RegionRecord ("Serbia and Montenegro" :| []) DeprecatedSimple)
  ,(Subtag 14399133908610318354, RegionRecord ("Cuba" :| []) NotDeprecated)
  ,(Subtag 14400259808517160978, RegionRecord ("Cabo Verde" :| ["Cape Verde"]) NotDeprecated)
  ,(Subtag 14401385708424003602, RegionRecord ("Cura\231ao" :| []) NotDeprecated)
  ,(Subtag 14402511608330846226, RegionRecord ("Christmas Island" :| []) NotDeprecated)
  ,(Subtag 14403637508237688850, RegionRecord ("Cyprus" :| []) NotDeprecated)
  ,(Subtag 14404763408144531474, RegionRecord ("Czechia" :| ["Czech Republic"]) NotDeprecated)
  ,(Subtag 14524108798269849618, RegionRecord ("German Democratic Republic" :| []) (DeprecatedPreferred DE))
  ,(Subtag 14525234698176692242, RegionRecord ("Germany" :| []) NotDeprecated)
  ,(Subtag 14527486497990377490, RegionRecord ("Diego Garcia" :| []) NotDeprecated)
  ,(Subtag 14530864197710905362, RegionRecord ("Djibouti" :| []) NotDeprecated)
  ,(Subtag 14531990097617747986, RegionRecord ("Denmark" :| []) NotDeprecated)
  ,(Subtag 14534241897431433234, RegionRecord ("Dominica" :| []) NotDeprecated)
  ,(Subtag 14536493697245118482, RegionRecord ("Dominican Republic" :| []) NotDeprecated)
  ,(Subtag 14548878596220387346, RegionRecord ("Algeria" :| []) NotDeprecated)
  ,(Subtag 14664846286625177618, RegionRecord ("Ceuta, Melilla" :| []) NotDeprecated)
  ,(Subtag 14667098086438862866, RegionRecord ("Ecuador" :| []) NotDeprecated)
  ,(Subtag 14669349886252548114, RegionRecord ("Estonia" :| []) NotDeprecated)
  ,(Subtag 14671601686066233362, RegionRecord ("Egypt" :| []) NotDeprecated)
  ,(Subtag 14672727585973075986, RegionRecord ("Western Sahara" :| []) NotDeprecated)
  ,(Subtag 14683986585041502226, RegionRecord ("Eritrea" :| []) NotDeprecated)
  ,(Subtag 14685112484948344850, RegionRecord ("Spain" :| []) NotDeprecated)
  ,(Subtag 14686238384855187474, RegionRecord ("Ethiopia" :| []) NotDeprecated)
  ,(Subtag 14687364284762030098, RegionRecord ("European Union" :| []) NotDeprecated)
  ,(Subtag 14692993784296243218, RegionRecord ("Eurozone" :| []) NotDeprecated)
  ,(Subtag 14817968673955774482, RegionRecord ("Finland" :| []) NotDeprecated)
  ,(Subtag 14819094573862617106, RegionRecord ("Fiji" :| []) NotDeprecated)
  ,(Subtag 14820220473769459730, RegionRecord ("Falkland Islands (Malvinas)" :| []) NotDeprecated)
  ,(Subtag 14822472273583144978, RegionRecord ("Federated States of Micronesia" :| []) NotDeprecated)
  ,(Subtag 14824724073396830226, RegionRecord ("Faroe Islands" :| []) NotDeprecated)
  ,(Subtag 14828101773117358098, RegionRecord ("France" :| []) NotDeprecated)
  ,(Subtag 14834857172558413842, RegionRecord ("Metropolitan France" :| []) (DeprecatedPreferred FR))
  ,(Subtag 14953076662776889362, RegionRecord ("Gabon" :| []) NotDeprecated)
  ,(Subtag 14954202562683731986, RegionRecord ("United Kingdom" :| []) NotDeprecated)
  ,(Subtag 14956454362497417234, RegionRecord ("Grenada" :| []) NotDeprecated)
  ,(Subtag 14957580262404259858, RegionRecord ("Georgia" :| []) NotDeprecated)
  ,(Subtag 14958706162311102482, RegionRecord ("French Guiana" :| []) NotDeprecated)
  ,(Subtag 14959832062217945106, RegionRecord ("Guernsey" :| []) NotDeprecated)
  ,(Subtag 14960957962124787730, RegionRecord ("Ghana" :| []) NotDeprecated)
  ,(Subtag 14962083862031630354, RegionRecord ("Gibraltar" :| []) NotDeprecated)
  ,(Subtag 14965461561752158226, RegionRecord ("Greenland" :| []) NotDeprecated)
  ,(Subtag 14966587461659000850, RegionRecord ("Gambia" :| []) NotDeprecated)
  ,(Subtag 14967713361565843474, RegionRecord ("Guinea" :| []) NotDeprecated)
  ,(Subtag 14969965161379528722, RegionRecord ("Guadeloupe" :| []) NotDeprecated)
  ,(Subtag 14971091061286371346, RegionRecord ("Equatorial Guinea" :| []) NotDeprecated)
  ,(Subtag 14972216961193213970, RegionRecord ("Greece" :| []) NotDeprecated)
  ,(Subtag 14973342861100056594, RegionRecord ("South Georgia and the South Sandwich Islands" :| []) NotDeprecated)
  ,(Subtag 14974468761006899218, RegionRecord ("Guatemala" :| []) NotDeprecated)
  ,(Subtag 14975594660913741842, RegionRecord ("Guam" :| []) NotDeprecated)
  ,(Subtag 14977846460727427090, RegionRecord ("Guinea-Bissau" :| []) NotDeprecated)
  ,(Subtag 14980098260541112338, RegionRecord ("Guyana" :| []) NotDeprecated)
  ,(Subtag 15108450849921171474, RegionRecord ("Hong Kong" :| []) NotDeprecated)
  ,(Subtag 15110702649734856722, RegionRecord ("Heard Island and McDonald Islands" :| []) NotDeprecated)
  ,(Subtag 15111828549641699346, RegionRecord ("Honduras" :| []) NotDeprecated)
  ,(Subtag 15116332149269069842, RegionRecord ("Croatia" :| []) NotDeprecated)
  ,(Subtag 15118583949082755090, RegionRecord ("Haiti" :| []) NotDeprecated)
  ,(Subtag 15119709848989597714, RegionRecord ("Hungary" :| []) NotDeprecated)
  ,(Subtag 15243558838742286354, RegionRecord ("Canary Islands" :| []) NotDeprecated)
  ,(Subtag 15244684738649128978, RegionRecord ("Indonesia" :| []) NotDeprecated)
  ,(Subtag 15245810638555971602, RegionRecord ("Ireland" :| []) NotDeprecated)
  ,(Subtag 15253691937903869970, RegionRecord ("Israel" :| []) NotDeprecated)
  ,(Subtag 15254817837810712594, RegionRecord ("Isle of Man" :| []) NotDeprecated)
  ,(Subtag 15255943737717555218, RegionRecord ("India" :| []) NotDeprecated)
  ,(Subtag 15257069637624397842, RegionRecord ("British Indian Ocean Territory" :| []) NotDeprecated)
  ,(Subtag 15259321437438083090, RegionRecord ("Iraq" :| []) NotDeprecated)
  ,(Subtag 15260447337344925714, RegionRecord ("Islamic Republic of Iran" :| []) NotDeprecated)
  ,(Subtag 15261573237251768338, RegionRecord ("Iceland" :| []) NotDeprecated)
  ,(Subtag 15262699137158610962, RegionRecord ("Italy" :| []) NotDeprecated)
  ,(Subtag 15389925826631827474, RegionRecord ("Jersey" :| []) NotDeprecated)
  ,(Subtag 15398933025886568466, RegionRecord ("Jamaica" :| []) NotDeprecated)
  ,(Subtag 15401184825700253714, RegionRecord ("Jordan" :| []) NotDeprecated)
  ,(Subtag 15402310725607096338, RegionRecord ("Japan" :| []) NotDeprecated)
  ,(Subtag 15534041014707683346, RegionRecord ("Kenya" :| []) NotDeprecated)
  ,(Subtag 15536292814521368594, RegionRecord ("Kyrgyzstan" :| []) NotDeprecated)
  ,(Subtag 15537418714428211218, RegionRecord ("Cambodia" :| []) NotDeprecated)
  ,(Subtag 15538544614335053842, RegionRecord ("Kiribati" :| []) NotDeprecated)
  ,(Subtag 15543048213962424338, RegionRecord ("Comoros" :| []) NotDeprecated)
  ,(Subtag 15544174113869266962, RegionRecord ("Saint Kitts and Nevis" :| []) NotDeprecated)
  ,(Subtag 15546425913682952210, RegionRecord ("Democratic People's Republic of Korea" :| []) NotDeprecated)
  ,(Subtag 15548677713496637458, RegionRecord ("Republic of Korea" :| []) NotDeprecated)
  ,(Subtag 15554307213030850578, RegionRecord ("Kuwait" :| []) NotDeprecated)
  ,(Subtag 15556559012844535826, RegionRecord ("Cayman Islands" :| []) NotDeprecated)
  ,(Subtag 15557684912751378450, RegionRecord ("Kazakhstan" :| []) NotDeprecated)
  ,(Subtag 15673652603156168722, RegionRecord ("Lao People's Democratic Republic" :| []) NotDeprecated)
  ,(Subtag 15674778503063011346, RegionRecord ("Lebanon" :| []) NotDeprecated)
  ,(Subtag 15675904402969853970, RegionRecord ("Saint Lucia" :| []) NotDeprecated)
  ,(Subtag 15682659802410909714, RegionRecord ("Liechtenstein" :| []) NotDeprecated)
  ,(Subtag 15684911602224594962, RegionRecord ("Sri Lanka" :| []) NotDeprecated)
  ,(Subtag 15692792901572493330, RegionRecord ("Liberia" :| []) NotDeprecated)
  ,(Subtag 15693918801479335954, RegionRecord ("Lesotho" :| []) NotDeprecated)
  ,(Subtag 15695044701386178578, RegionRecord ("Lithuania" :| []) NotDeprecated)
  ,(Subtag 15696170601293021202, RegionRecord ("Luxembourg" :| []) NotDeprecated)
  ,(Subtag 15697296501199863826, RegionRecord ("Latvia" :| []) NotDeprecated)
  ,(Subtag 15700674200920391698, RegionRecord ("Libya" :| []) NotDeprecated)
  ,(Subtag 15817767791232024594, RegionRecord ("Morocco" :| []) NotDeprecated)
  ,(Subtag 15820019591045709842, RegionRecord ("Monaco" :| []) NotDeprecated)
  ,(Subtag 15821145490952552466, RegionRecord ("Moldova" :| []) NotDeprecated)
  ,(Subtag 15822271390859395090, RegionRecord ("Montenegro" :| []) NotDeprecated)
  ,(Subtag 15823397290766237714, RegionRecord ("Saint Martin (French part)" :| []) NotDeprecated)
  ,(Subtag 15824523190673080338, RegionRecord ("Madagascar" :| []) NotDeprecated)
  ,(Subtag 15825649090579922962, RegionRecord ("Marshall Islands" :| []) NotDeprecated)
  ,(Subtag 15829026790300450834, RegionRecord ("North Macedonia" :| []) NotDeprecated)
  ,(Subtag 15830152690207293458, RegionRecord ("Mali" :| []) NotDeprecated)
  ,(Subtag 15831278590114136082, RegionRecord ("Myanmar" :| []) NotDeprecated)
  ,(Subtag 15832404490020978706, RegionRecord ("Mongolia" :| []) NotDeprecated)
  ,(Subtag 15833530389927821330, RegionRecord ("Macao" :| []) NotDeprecated)
  ,(Subtag 15834656289834663954, RegionRecord ("Northern Mariana Islands" :| []) NotDeprecated)
  ,(Subtag 15835782189741506578, RegionRecord ("Martinique" :| []) NotDeprecated)
  ,(Subtag 15836908089648349202, RegionRecord ("Mauritania" :| []) NotDeprecated)
  ,(Subtag 15838033989555191826, RegionRecord ("Montserrat" :| []) NotDeprecated)
  ,(Subtag 15839159889462034450, RegionRecord ("Malta" :| []) NotDeprecated)
  ,(Subtag 15840285789368877074, RegionRecord ("Mauritius" :| []) NotDeprecated)
  ,(Subtag 15841411689275719698, RegionRecord ("Maldives" :| []) NotDeprecated)
  ,(Subtag 15842537589182562322, RegionRecord ("Malawi" :| []) NotDeprecated)
  ,(Subtag 15843663489089404946, RegionRecord ("Mexico" :| []) NotDeprecated)
  ,(Subtag 15844789388996247570, RegionRecord ("Malaysia" :| []) NotDeprecated)
  ,(Subtag 15845915288903090194, RegionRecord ("Mozambique" :| []) NotDeprecated)
  ,(Subtag 15961882979307880466, RegionRecord ("Namibia" :| []) NotDeprecated)
  ,(Subtag 15964134779121565714, RegionRecord ("New Caledonia" :| []) NotDeprecated)
  ,(Subtag 15966386578935250962, RegionRecord ("Niger" :| []) NotDeprecated)
  ,(Subtag 15967512478842093586, RegionRecord ("Norfolk Island" :| []) NotDeprecated)
  ,(Subtag 15968638378748936210, RegionRecord ("Nigeria" :| []) NotDeprecated)
  ,(Subtag 15970890178562621458, RegionRecord ("Nicaragua" :| []) NotDeprecated)
  ,(Subtag 15974267878283149330, RegionRecord ("Netherlands" :| []) NotDeprecated)
  ,(Subtag 15977645578003677202, RegionRecord ("Norway" :| []) NotDeprecated)
  ,(Subtag 15978771477910519826, RegionRecord ("Nepal" :| []) NotDeprecated)
  ,(Subtag 15981023277724205074, RegionRecord ("Nauru" :| []) NotDeprecated)
  ,(Subtag 15983275077537890322, RegionRecord ("Neutral Zone" :| []) DeprecatedSimple)
  ,(Subtag 15984400977444732946, RegionRecord ("Niue" :| []) NotDeprecated)
  ,(Subtag 15990030476978946066, RegionRecord ("New Zealand" :| []) NotDeprecated)
  ,(Subtag 16119508966265847826, RegionRecord ("Oman" :| []) NotDeprecated)
  ,(Subtag 16250113355459592210, RegionRecord ("Panama" :| []) NotDeprecated)
  ,(Subtag 16254616955086962706, RegionRecord ("Peru" :| []) NotDeprecated)
  ,(Subtag 16255742854993805330, RegionRecord ("French Polynesia" :| []) NotDeprecated)
  ,(Subtag 16256868754900647954, RegionRecord ("Papua New Guinea" :| []) NotDeprecated)
  ,(Subtag 16257994654807490578, RegionRecord ("Philippines" :| []) NotDeprecated)
  ,(Subtag 16261372354528018450, RegionRecord ("Pakistan" :| []) NotDeprecated)
  ,(Subtag 16262498254434861074, RegionRecord ("Poland" :| []) NotDeprecated)
  ,(Subtag 16263624154341703698, RegionRecord ("Saint Pierre and Miquelon" :| []) NotDeprecated)
  ,(Subtag 16264750054248546322, RegionRecord ("Pitcairn" :| []) NotDeprecated)
  ,(Subtag 16269253653875916818, RegionRecord ("Puerto Rico" :| []) NotDeprecated)
  ,(Subtag 16270379553782759442, RegionRecord ("State of Palestine" :| []) NotDeprecated)
  ,(Subtag 16271505453689602066, RegionRecord ("Portugal" :| []) NotDeprecated)
  ,(Subtag 16274883153410129938, RegionRecord ("Palau" :| []) NotDeprecated)
  ,(Subtag 16277134953223815186, RegionRecord ("Paraguay" :| []) NotDeprecated)
  ,(Subtag 16394228543535448082, RegionRecord ("Qatar" :| []) NotDeprecated)
  ,(Subtag 16407739342417559570, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16408865242324402194, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16409991142231244818, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16411117042138087442, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16412242942044930066, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16413368841951772690, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16414494741858615314, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16415620641765457938, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16416746541672300562, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16417872441579143186, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16418998341485985810, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16420124241392828434, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16421250141299671058, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16422376041206513682, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 16542847331238674450, RegionRecord ("R\233union" :| []) NotDeprecated)
  ,(Subtag 16554106330307100690, RegionRecord ("Romania" :| []) NotDeprecated)
  ,(Subtag 16558609929934471186, RegionRecord ("Serbia" :| []) NotDeprecated)
  ,(Subtag 16560861729748156434, RegionRecord ("Russian Federation" :| []) NotDeprecated)
  ,(Subtag 16563113529561841682, RegionRecord ("Rwanda" :| []) NotDeprecated)
  ,(Subtag 16682458919687159826, RegionRecord ("Saudi Arabia" :| []) NotDeprecated)
  ,(Subtag 16683584819594002450, RegionRecord ("Solomon Islands" :| []) NotDeprecated)
  ,(Subtag 16684710719500845074, RegionRecord ("Seychelles" :| []) NotDeprecated)
  ,(Subtag 16685836619407687698, RegionRecord ("Sudan" :| []) NotDeprecated)
  ,(Subtag 16686962519314530322, RegionRecord ("Sweden" :| []) NotDeprecated)
  ,(Subtag 16689214319128215570, RegionRecord ("Singapore" :| []) NotDeprecated)
  ,(Subtag 16690340219035058194, RegionRecord ("Saint Helena, Ascension and Tristan da Cunha" :| []) NotDeprecated)
  ,(Subtag 16691466118941900818, RegionRecord ("Slovenia" :| []) NotDeprecated)
  ,(Subtag 16692592018848743442, RegionRecord ("Svalbard and Jan Mayen" :| []) NotDeprecated)
  ,(Subtag 16693717918755586066, RegionRecord ("Slovakia" :| []) NotDeprecated)
  ,(Subtag 16694843818662428690, RegionRecord ("Sierra Leone" :| []) NotDeprecated)
  ,(Subtag 16695969718569271314, RegionRecord ("San Marino" :| []) NotDeprecated)
  ,(Subtag 16697095618476113938, RegionRecord ("Senegal" :| []) NotDeprecated)
  ,(Subtag 16698221518382956562, RegionRecord ("Somalia" :| []) NotDeprecated)
  ,(Subtag 16701599218103484434, RegionRecord ("Suriname" :| []) NotDeprecated)
  ,(Subtag 16702725118010327058, RegionRecord ("South Sudan" :| []) NotDeprecated)
  ,(Subtag 16703851017917169682, RegionRecord ("Sao Tome and Principe" :| []) NotDeprecated)
  ,(Subtag 16704976917824012306, RegionRecord ("Union of Soviet Socialist Republics" :| []) DeprecatedSimple)
  ,(Subtag 16706102817730854930, RegionRecord ("El Salvador" :| []) NotDeprecated)
  ,(Subtag 16708354617544540178, RegionRecord ("Sint Maarten (Dutch part)" :| []) NotDeprecated)
  ,(Subtag 16709480517451382802, RegionRecord ("Syrian Arab Republic" :| []) NotDeprecated)
  ,(Subtag 16710606417358225426, RegionRecord ("Eswatini" :| ["eSwatini","Swaziland"]) NotDeprecated)
  ,(Subtag 16826574107763015698, RegionRecord ("Tristan da Cunha" :| []) NotDeprecated)
  ,(Subtag 16828825907576700946, RegionRecord ("Turks and Caicos Islands" :| []) NotDeprecated)
  ,(Subtag 16829951807483543570, RegionRecord ("Chad" :| []) NotDeprecated)
  ,(Subtag 16832203607297228818, RegionRecord ("French Southern Territories" :| []) NotDeprecated)
  ,(Subtag 16833329507204071442, RegionRecord ("Togo" :| []) NotDeprecated)
  ,(Subtag 16834455407110914066, RegionRecord ("Thailand" :| []) NotDeprecated)
  ,(Subtag 16836707206924599314, RegionRecord ("Tajikistan" :| []) NotDeprecated)
  ,(Subtag 16837833106831441938, RegionRecord ("Tokelau" :| []) NotDeprecated)
  ,(Subtag 16838959006738284562, RegionRecord ("Timor-Leste" :| []) NotDeprecated)
  ,(Subtag 16840084906645127186, RegionRecord ("Turkmenistan" :| []) NotDeprecated)
  ,(Subtag 16841210806551969810, RegionRecord ("Tunisia" :| []) NotDeprecated)
  ,(Subtag 16842336706458812434, RegionRecord ("Tonga" :| []) NotDeprecated)
  ,(Subtag 16843462606365655058, RegionRecord ("East Timor" :| []) (DeprecatedPreferred TL))
  ,(Subtag 16845714406179340306, RegionRecord ("Turkey" :| []) NotDeprecated)
  ,(Subtag 16847966205993025554, RegionRecord ("Trinidad and Tobago" :| []) NotDeprecated)
  ,(Subtag 16850218005806710802, RegionRecord ("Tuvalu" :| []) NotDeprecated)
  ,(Subtag 16851343905713553426, RegionRecord ("Taiwan, Province of China" :| []) NotDeprecated)
  ,(Subtag 16854721605434081298, RegionRecord ("United Republic of Tanzania" :| []) NotDeprecated)
  ,(Subtag 16970689295838871570, RegionRecord ("Ukraine" :| []) NotDeprecated)
  ,(Subtag 16977444695279927314, RegionRecord ("Uganda" :| []) NotDeprecated)
  ,(Subtag 16984200094720983058, RegionRecord ("United States Minor Outlying Islands" :| []) NotDeprecated)
  ,(Subtag 16985325994627825682, RegionRecord ("United Nations" :| []) NotDeprecated)
  ,(Subtag 16990955494162038802, RegionRecord ("United States" :| []) NotDeprecated)
  ,(Subtag 16997710893603094546, RegionRecord ("Uruguay" :| []) NotDeprecated)
  ,(Subtag 16998836793509937170, RegionRecord ("Uzbekistan" :| []) NotDeprecated)
  ,(Subtag 17114804483914727442, RegionRecord ("Holy See (Vatican City State)" :| []) NotDeprecated)
  ,(Subtag 17117056283728412690, RegionRecord ("Saint Vincent and the Grenadines" :| []) NotDeprecated)
  ,(Subtag 17119308083542097938, RegionRecord ("Venezuela" :| []) NotDeprecated)
  ,(Subtag 17121559883355783186, RegionRecord ("British Virgin Islands" :| []) NotDeprecated)
  ,(Subtag 17123811683169468434, RegionRecord ("U.S. Virgin Islands" :| []) NotDeprecated)
  ,(Subtag 17129441182703681554, RegionRecord ("Viet Nam" :| []) NotDeprecated)
  ,(Subtag 17137322482051579922, RegionRecord ("Vanuatu" :| []) NotDeprecated)
  ,(Subtag 17264549171524796434, RegionRecord ("Wallis and Futuna" :| []) NotDeprecated)
  ,(Subtag 17279185870313750546, RegionRecord ("Samoa" :| []) NotDeprecated)
  ,(Subtag 17403034860066439186, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17404160759973281810, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17405286659880124434, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17406412559786967058, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17407538459693809682, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17408664359600652306, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17409790259507494930, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17410916159414337554, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17412042059321180178, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17413167959228022802, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17414293859134865426, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17415419759041708050, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17416545658948550674, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17417671558855393298, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17418797458762235922, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17419923358669078546, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17421049258575921170, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17422175158482763794, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17423301058389606418, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17424426958296449042, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17425552858203291666, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17426678758110134290, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17427804658016976914, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17428930557923819538, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17430056457830662162, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17431182357737504786, RegionRecord ("Private use" :| []) NotDeprecated)
  ,(Subtag 17550527747862822930, RegionRecord ("Democratic Yemen" :| []) (DeprecatedPreferred YE))
  ,(Subtag 17551653647769665554, RegionRecord ("Yemen" :| []) NotDeprecated)
  ,(Subtag 17568542146372304914, RegionRecord ("Mayotte" :| []) NotDeprecated)
  ,(Subtag 17569668046279147538, RegionRecord ("Yugoslavia" :| []) DeprecatedSimple)
  ,(Subtag 17691265236218150930, RegionRecord ("South Africa" :| []) NotDeprecated)
  ,(Subtag 17704776035100262418, RegionRecord ("Zambia" :| []) NotDeprecated)
  ,(Subtag 17710405534634475538, RegionRecord ("Zaire" :| []) (DeprecatedPreferred CD))
  ,(Subtag 17716035034168688658, RegionRecord ("Zimbabwe" :| []) NotDeprecated)
  ,(Subtag 17719412733889216530, RegionRecord ("Private use" :| []) NotDeprecated)]

-- | Look up the subtag and record details associated to the given 'Region' subtag
lookupRegionDetails :: Region -> (Subtag, RegionRecord)
lookupRegionDetails = V.unsafeIndex regionDetails . fromEnum

-- | Validate the given 'Subtag' against the region records in the registry
validateRegion :: Subtag -> Maybe Region
validateRegion = fmap toEnum . flip (binSearchIndexOn fst) regionDetails

-- | Look up the 'Subtag' associated to the given 'Region'
regionToSubtag :: Region -> Subtag
regionToSubtag = fst . lookupRegionDetails

-- | Look up the 'RegionRecord' associated to the given 'Region'
lookupRegionRecord :: Region -> RegionRecord
lookupRegionRecord = snd . lookupRegionDetails