-- |
-- Description : Grandfathered and redundant tags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The types in this module enumerate the grandfathered and redundant BCP47
-- tags. The grandfathered tags are tags that are explicitly enumerated in the
-- BCP47 standard itself and defined to be well-formed and valid, even though
-- they may not satisfy the conditions required for either of those categories.
-- The redundant tags are BCP47 tags that were added in their entirety to the
-- IANA registry using a registration procedure from an older standard, but
-- which are otherwise well-formed and valid. The tags comprising both
-- categories form a fixed and immutable list; under the BCP47 standard no tags
-- will be added or removed from either category.
--
-- An awareness of both types of tags is required for proper BCP47 language tag
-- processing: grandfathered tags must be detected when checking for tag
-- well-formedness, and, since grandfathered and redundant tags can be
-- deprecated, both types of tags must be detected during tag canonicalization.
module LanguageTag.BCP47.LegacyTag
  ( -- * Grandfathered tags
    Grandfathered (..),
    renderGrandfathered,
    renderGrandfatheredBuilder,
    grandfatheredSyntax,
    Syn.grandfatheredToSubtags,

    -- * Redundant tags
    Redundant (..),

    -- ** Recognition
    recognizeRedundant,

    -- ** Rendering and conversion
    renderRedundant,
    renderRedundantBuilder,
    redundantToSyntaxTag,
    redundantToSyntaxNormal,

    -- ** Redundant tag trie
    redundantTrie,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import LanguageTag.BCP47.Subtag.Trie
  ( Trie,
    branch,
    leaf,
    path,
    stepTrie,
  )
import qualified LanguageTag.BCP47.Subtag.Trie as Trie
import LanguageTag.Internal.BCP47.LegacyTag.Grandfathered
import LanguageTag.Internal.BCP47.LegacyTag.Redundant
import LanguageTag.Internal.BCP47.Subtag (MaybeSubtag (..), Subtag (..), nullSubtag)
import qualified LanguageTag.Internal.BCP47.Syntax as Syn

-- | Convert a 'Grandfathered' tag to a merely well-formed 'Syn.BCP47'
-- tag
grandfatheredSyntax :: Grandfathered -> Syn.BCP47
grandfatheredSyntax = Syn.GrandfatheredTag

-- | Render a 'Grandfathered' tag to a strict text value
renderGrandfathered :: Grandfathered -> Text
renderGrandfathered = Syn.renderBCP47 . grandfatheredSyntax

-- | Render a 'Grandfathered' tag to a lazy text builder
renderGrandfatheredBuilder :: Grandfathered -> TB.Builder
renderGrandfatheredBuilder = Syn.renderBCP47Builder . grandfatheredSyntax

-- | Render a 'Redundant' tag to a strict text value
renderRedundant :: Redundant -> Text
renderRedundant = Syn.renderBCP47 . redundantToSyntaxTag

-- | Render a 'Redundant' tag to a lazy text builder
renderRedundantBuilder :: Redundant -> TB.Builder
renderRedundantBuilder = Syn.renderBCP47Builder . redundantToSyntaxTag

-- | Convert a 'Redundant' tag to a well-formed 'Syn.BCP47' tag
redundantToSyntaxTag :: Redundant -> Syn.BCP47
redundantToSyntaxTag = Syn.NormalTag . redundantToSyntaxNormal

-- | Convert a 'Redundant' tag to a well-formed 'Syn.Normal' tag
redundantToSyntaxNormal :: Redundant -> Syn.Normal
redundantToSyntaxNormal = go
  where
    ns = nullSubtag
    con l el sc reg v =
      Syn.Normal
        (Subtag l)
        (MaybeSubtag $ Subtag el)
        ns
        ns
        (MaybeSubtag $ Subtag sc)
        (MaybeSubtag $ Subtag reg)
        v
        []
        []
    go AzArab = con 14116533031992819714 0 14108385788269953028 0 []
    go AzCyrl = con 14116533031992819714 0 14404647684545708036 0 []
    go AzLatn = con 14116533031992819714 0 15674680509089185796 0 []
    go BeLatn = con 14237004322024980482 0 15674680509089185796 0 []
    go BsCyrl = con 14252766920720777218 0 14404647684545708036 0 []
    go BsLatn = con 14252766920720777218 0 15674680509089185796 0 []
    go De1901 = con 14525234698176692226 0 0 0 [Subtag 7126246090126393348]
    go De1996 = con 14525234698176692226 0 0 0 [Subtag 7126325598560976900]
    go DeAt1901 = con 14525234698176692226 0 0 14109777632551763970 [Subtag 7126246090126393348]
    go DeAt1996 = con 14525234698176692226 0 0 14109777632551763970 [Subtag 7126325598560976900]
    go DeCh1901 = con 14525234698176692226 0 0 14384497209821364226 [Subtag 7126246090126393348]
    go DeCh1996 = con 14525234698176692226 0 0 14384497209821364226 [Subtag 7126325598560976900]
    go DeDe1901 = con 14525234698176692226 0 0 14525234698176692226 [Subtag 7126246090126393348]
    go DeDe1996 = con 14525234698176692226 0 0 14525234698176692226 [Subtag 7126325598560976900]
    go EnBoont = con 14679482985414131714 0 0 0 [Subtag 14249247308838338565]
    go EnScouse = con 14679482985414131714 0 0 0 [Subtag 16685695188168867846]
    go Es419 = con 14685112484948344834 0 0 7549660252682059779 []
    go IuCans = con 15263825037065453570 0 14377591383445733380 0 []
    go IuLatn = con 15263825037065453570 0 15674680509089185796 0 []
    go MnCyrl = con 15832404490020978690 0 14404647684545708036 0 []
    go MnMong = con 15832404490020978690 0 15834505038266368004 0 []
    go SgnBr = con 16690181889360658435 0 0 14251641020813934594 []
    go SgnCo = con 16690181889360658435 0 0 14392378509169262594 []
    go SgnDe = con 16690181889360658435 0 0 14525234698176692226 []
    go SgnDk = con 16690181889360658435 0 0 14531990097617747970 []
    go SgnEs = con 16690181889360658435 0 0 14685112484948344834 []
    go SgnFr = con 16690181889360658435 0 0 14828101773117358082 []
    go SgnGb = con 16690181889360658435 0 0 14954202562683731970 []
    go SgnGr = con 16690181889360658435 0 0 14972216961193213954 []
    go SgnIe = con 16690181889360658435 0 0 15245810638555971586 []
    go SgnIt = con 16690181889360658435 0 0 15262699137158610946 []
    go SgnJp = con 16690181889360658435 0 0 15402310725607096322 []
    go SgnMx = con 16690181889360658435 0 0 15843663489089404930 []
    go SgnNi = con 16690181889360658435 0 0 15970890178562621442 []
    go SgnNl = con 16690181889360658435 0 0 15974267878283149314 []
    go SgnNo = con 16690181889360658435 0 0 15977645578003677186 []
    go SgnPt = con 16690181889360658435 0 0 16271505453689602050 []
    go SgnSe = con 16690181889360658435 0 0 16686962519314530306 []
    go SgnUs = con 16690181889360658435 0 0 16990955494162038786 []
    go SgnZa = con 16690181889360658435 0 0 17691265236218150914 []
    go SlNedis = con 16694843818662428674 0 0 0 [Subtag 15967273465522683909]
    go SlRozaj = con 16694843818662428674 0 0 0 [Subtag 16555186176353370117]
    go SrCyrl = con 16701599218103484418 0 14404647684545708036 0 []
    go SrLatn = con 16701599218103484418 0 15674680509089185796 0 []
    go TgArab = con 16833329507204071426 0 14108385788269953028 0 []
    go TgCyrl = con 16833329507204071426 0 14404647684545708036 0 []
    go UzCyrl = con 16998836793509937154 0 14404647684545708036 0 []
    go UzLatn = con 16998836793509937154 0 15674680509089185796 0 []
    go YiLatn = con 17556157247397036034 0 15674680509089185796 0 []
    go ZhHans = con 17699146535566049282 0 15098167323825012740 0 []
    go ZhHansCn = con 17699146535566049282 0 15098167323825012740 14391252609262419970 []
    go ZhHansHk = con 17699146535566049282 0 15098167323825012740 15108450849921171458 []
    go ZhHansMo = con 17699146535566049282 0 15098167323825012740 15833530389927821314 []
    go ZhHansSg = con 17699146535566049282 0 15098167323825012740 16689214319128215554 []
    go ZhHansTw = con 17699146535566049282 0 15098167323825012740 16851343905713553410 []
    go ZhHant = con 17699146535566049282 0 15098167392544489476 0 []
    go ZhHantCn = con 17699146535566049282 0 15098167392544489476 14391252609262419970 []
    go ZhHantHk = con 17699146535566049282 0 15098167392544489476 15108450849921171458 []
    go ZhHantMo = con 17699146535566049282 0 15098167392544489476 15833530389927821314 []
    go ZhHantSg = con 17699146535566049282 0 15098167392544489476 16689214319128215554 []
    go ZhHantTw = con 17699146535566049282 0 15098167392544489476 16851343905713553410 []
    go ZhCmn = con 17699146535566049282 14391094279588020227 0 0 []
    go ZhCmnHans = con 17699146535566049282 14391094279588020227 15098167323825012740 0 []
    go ZhCmnHant = con 17699146535566049282 14391094279588020227 15098167392544489476 0 []
    go ZhGan = con 17699146535566049282 14954044233009332227 0 0 []
    go ZhWuu = con 17699146535566049282 17282466813011034115 0 0 []
    go ZhYue = con 17699146535566049282 17570556451674390531 0 0 []

-- | A 'Trie' associating the 'Redundant' subtags to their
-- @['Subtag']@ representation

-- This can be implemented here because the list of redundant tags
-- will never be changed
redundantTrie :: Trie Redundant
redundantTrie =
  stepTrie
    Nothing
    [ bRaw
        14116533031992819714
        Nothing
        [ lRaw 14108385788269953028 AzArab,
          lRaw 14404647684545708036 AzCyrl,
          lRaw 15674680509089185796 AzLatn
        ],
      pRaw 14237004322024980482 [15674680509089185796] BeLatn,
      bRaw
        14252766920720777218
        Nothing
        [ lRaw 14404647684545708036 BsCyrl,
          lRaw 15674680509089185796 BsLatn
        ],
      bRaw
        14525234698176692226
        Nothing
        [ lRaw 7126246090126393348 De1901,
          lRaw 7126325598560976900 De1996,
          bRaw
            14109777632551763970
            Nothing
            [ lRaw 7126246090126393348 DeAt1901,
              lRaw 7126325598560976900 DeAt1996
            ],
          bRaw
            14384497209821364226
            Nothing
            [ lRaw 7126246090126393348 DeCh1901,
              lRaw 7126325598560976900 DeCh1996
            ],
          bRaw
            14525234698176692226
            Nothing
            [ lRaw 7126246090126393348 DeDe1901,
              lRaw 7126325598560976900 DeDe1996
            ]
        ],
      bRaw
        14679482985414131714
        Nothing
        [ lRaw 14249247308838338565 EnBoont,
          lRaw 16685695188168867846 EnScouse
        ],
      pRaw 14685112484948344834 [7549660252682059779] Es419,
      bRaw
        15263825037065453570
        Nothing
        [ lRaw 14377591383445733380 IuCans,
          lRaw 15674680509089185796 IuLatn
        ],
      bRaw
        15832404490020978690
        Nothing
        [ lRaw 14404647684545708036 MnCyrl,
          lRaw 15834505038266368004 MnMong
        ],
      bRaw
        16690181889360658435
        Nothing
        [ lRaw 14251641020813934594 SgnBr,
          lRaw 14392378509169262594 SgnCo,
          lRaw 14525234698176692226 SgnDe,
          lRaw 14531990097617747970 SgnDk,
          lRaw 14685112484948344834 SgnEs,
          lRaw 14828101773117358082 SgnFr,
          lRaw 14954202562683731970 SgnGb,
          lRaw 14972216961193213954 SgnGr,
          lRaw 15245810638555971586 SgnIe,
          lRaw 15262699137158610946 SgnIt,
          lRaw 15402310725607096322 SgnJp,
          lRaw 15843663489089404930 SgnMx,
          lRaw 15970890178562621442 SgnNi,
          lRaw 15974267878283149314 SgnNl,
          lRaw 15977645578003677186 SgnNo,
          lRaw 16271505453689602050 SgnPt,
          lRaw 16686962519314530306 SgnSe,
          lRaw 16990955494162038786 SgnUs,
          lRaw 17691265236218150914 SgnZa
        ],
      bRaw
        16694843818662428674
        Nothing
        [ lRaw 15967273465522683909 SlNedis,
          lRaw 16555186176353370117 SlRozaj
        ],
      bRaw
        16701599218103484418
        Nothing
        [ lRaw 14404647684545708036 SrCyrl,
          lRaw 15674680509089185796 SrLatn
        ],
      bRaw
        16833329507204071426
        Nothing
        [ lRaw 14108385788269953028 TgArab,
          lRaw 14404647684545708036 TgCyrl
        ],
      bRaw
        16998836793509937154
        Nothing
        [ lRaw 14404647684545708036 UzCyrl,
          lRaw 15674680509089185796 UzLatn
        ],
      pRaw 17556157247397036034 [15674680509089185796] YiLatn,
      bRaw
        17699146535566049282
        Nothing
        [ bRaw
            15098167323825012740
            (Just ZhHans)
            [ lRaw 14391252609262419970 ZhHansCn,
              lRaw 15108450849921171458 ZhHansHk,
              lRaw 15833530389927821314 ZhHansMo,
              lRaw 16689214319128215554 ZhHansSg,
              lRaw 16851343905713553410 ZhHansTw
            ],
          bRaw
            15098167392544489476
            (Just ZhHant)
            [ lRaw 14391252609262419970 ZhHantCn,
              lRaw 15108450849921171458 ZhHantHk,
              lRaw 15833530389927821314 ZhHantMo,
              lRaw 16689214319128215554 ZhHantSg,
              lRaw 16851343905713553410 ZhHantTw
            ],
          bRaw
            14391094279588020227
            (Just ZhCmn)
            [ lRaw 15098167323825012740 ZhCmnHans,
              lRaw 15098167392544489476 ZhCmnHant
            ],
          lRaw 14954044233009332227 ZhGan,
          lRaw 17282466813011034115 ZhWuu,
          lRaw 17570556451674390531 ZhYue
        ]
    ]
  where
    bRaw = branch . Subtag
    lRaw = leaf . Subtag
    pRaw k = path (Subtag k) . fmap Subtag

-- | Determine whether or not the sequence of subtags represents a 'Redundant' tag
recognizeRedundant :: [Subtag] -> Maybe Redundant
recognizeRedundant = flip Trie.lookup redundantTrie
