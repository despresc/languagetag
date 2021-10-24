-- |
-- Description : Redundant subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module exports the 'Redundant' type, which enumerates all of
-- the redundant tags in the IANA registry, the current version of
-- which (not necessarily the one used in the library) is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module LanguageTag.BCP47.Registry.Redundant
  ( -- * Redundant tags
    Redundant (..),

    -- * Recognition
    recognizeRedundantNormal,
    recognizeRedundantTag,

    -- * Rendering and conversion
    renderRedundant,
    renderRedundantBuilder,
    redundantToSyntaxTag,
    redundantToNormalTag,
    redundantToValidTag,

    -- * Records and lookup
    RangeRecord (..),
    lookupRedundantRecord,

    -- * Redundant tag trie
    redundantTrie,
  )
where

import Data.Maybe (catMaybes)
import qualified Data.Set as S
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
import LanguageTag.Internal.BCP47.Registry.ExtlangRecords (extlangToSubtag)
import LanguageTag.Internal.BCP47.Registry.LanguageRecords (languageToSubtag)
import LanguageTag.Internal.BCP47.Registry.Redundant
import LanguageTag.Internal.BCP47.Registry.RedundantRecords
import qualified LanguageTag.Internal.BCP47.Registry.RedundantRecords as Internal
import LanguageTag.Internal.BCP47.Registry.RegionRecords (regionToSubtag)
import LanguageTag.Internal.BCP47.Registry.ScriptRecords (scriptToSubtag)
import LanguageTag.Internal.BCP47.Registry.Types
import LanguageTag.Internal.BCP47.Registry.VariantRecords (variantToSubtag)
import LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import qualified LanguageTag.Internal.BCP47.Syntax as Syn

-- | Render a 'Redundant' tag to a strict text value
renderRedundant :: Redundant -> Text
renderRedundant = Syn.renderBCP47 . redundantToSyntaxTag

-- | Render a 'Redundant' tag to a lazy text builder
renderRedundantBuilder :: Redundant -> TB.Builder
renderRedundantBuilder = Syn.renderBCP47Builder . redundantToSyntaxTag

-- | Convert a 'Redundant' tag to a merely well-formed 'Syn.BCP47' tag
redundantToSyntaxTag :: Redundant -> Syn.BCP47
redundantToSyntaxTag = Syn.NormalTag . Internal.redundantToSyntaxNormal

-- | Convert a 'Redundant' tag to a valid 'Normal' tag
redundantToNormalTag :: Redundant -> Normal
redundantToNormalTag = Internal.redundantToValidNormal

-- | Convert a 'Redundant' tag to a valid 'BCP47' tag
redundantToValidTag :: Redundant -> BCP47
redundantToValidTag = NormalTag . redundantToNormalTag

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

-- | Determine whether or not a valid 'BCP47' tag represents a
-- 'Redundant' tag
recognizeRedundantNormal :: Normal -> Maybe Redundant
recognizeRedundantNormal n
  | null $ extensions n,
    null $ privateUse n =
    Trie.lookup (l' : (catMaybes [e', s', r'] <> vs')) redundantTrie
  | otherwise = Nothing
  where
    l' = languageToSubtag $ language n
    e' = extlangToSubtag <$> extlang n
    s' = scriptToSubtag <$> script n
    r' = regionToSubtag <$> region n
    -- not the correct presentation in general, but none of the
    -- redundant tags have more than one variant
    vs' = fmap variantToSubtag $ S.toAscList $ variants n

-- | Determine whether or not a valid 'BCP47' tag represents a
-- 'Redundant' tag
recognizeRedundantTag :: BCP47 -> Maybe Redundant
recognizeRedundantTag (NormalTag n) = recognizeRedundantNormal n
recognizeRedundantTag _ = Nothing
