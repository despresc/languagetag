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
module Text.LanguageTag.BCP47.Registry.Redundant
  ( -- * Redundant tags
    Redundant (..),
    recognizeRedundantNormal,
    recognizeRedundantTag,
    redundantToSyntaxTag,
    redundantToNormalTag,
    redundantToValidTag,

    -- * Redundant tag records
    RangeRecord (..),
    lookupRedundantRecord,
    redundantDetails,

    -- * Redundant tag trie
    redundantTrie,
  )
where

import qualified Data.List.NonEmpty as NE
import Text.LanguageTag.BCP47.Subtag.Trie
  ( Trie,
    lookupTrie,
    stepBranch,
    stepLeaf,
    stepPath,
    trieStep,
  )
import Text.LanguageTag.Internal.BCP47.Registry
import Text.LanguageTag.Internal.BCP47.Registry.Redundant
import Text.LanguageTag.Internal.BCP47.Registry.RedundantRecords hiding
  ( redundantToSyntaxTag,
    redundantToValidTag,
  )
import qualified Text.LanguageTag.Internal.BCP47.Registry.RedundantRecords as Internal
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Convert a 'Redundant' tag to a merely well-formed 'Syn.BCP47' tag
redundantToSyntaxTag :: Redundant -> Syn.BCP47
redundantToSyntaxTag = Syn.NormalTag . Internal.redundantToSyntaxTag
{-# INLINE redundantToSyntaxTag #-}

-- | Convert a 'Redundant' tag to a valid 'Normal' tag
redundantToNormalTag :: Redundant -> Normal
redundantToNormalTag = Internal.redundantToValidTag
{-# INLINE redundantToNormalTag #-}

-- | Convert a 'Redundant' tag to a valid 'BCP47' tag
redundantToValidTag :: Redundant -> BCP47
redundantToValidTag = NormalTag . Internal.redundantToValidTag
{-# INLINE redundantToValidTag #-}

-- | A 'Trie' associating the 'Redundant' subtags to their
-- @['Subtag']@ representation

-- This can be implemented here because the list of redundant tags
-- will never be changed
redundantTrie :: Trie Redundant
redundantTrie =
  trieStep
    Nothing
    [ bRaw
        14116533031992819730
        Nothing
        [ lRaw 14108385788269953044 AzArab,
          lRaw 14404647684545708052 AzCyrl,
          lRaw 15674680509089185812 AzLatn
        ],
      pRaw 14237004322024980498 [15674680509089185812] BeLatn,
      bRaw
        14252766920720777234
        Nothing
        [ lRaw 14404647684545708052 BsCyrl,
          lRaw 15674680509089185812 BsLatn
        ],
      bRaw
        14525234698176692242
        Nothing
        [ lRaw 7126246090126393380 De1901,
          lRaw 7126325598560976932 De1996,
          bRaw
            14109777632551763986
            Nothing
            [ lRaw 7126246090126393380 DeAt1901,
              lRaw 7126325598560976932 DeAt1996
            ],
          bRaw
            14384497209821364242
            Nothing
            [ lRaw 7126246090126393380 DeCh1901,
              lRaw 7126325598560976932 DeCh1996
            ],
          bRaw
            14525234698176692242
            Nothing
            [ lRaw 7126246090126393380 DeDe1901,
              lRaw 7126325598560976932 DeDe1996
            ]
        ],
      bRaw
        14679482985414131730
        Nothing
        [ lRaw 14249247308838338581 EnBoont,
          lRaw 16685695188168867862 EnScouse
        ],
      pRaw 14685112484948344850 [7549660252682059811] Es419,
      bRaw
        15263825037065453586
        Nothing
        [ lRaw 14377591383445733396 IuCans,
          lRaw 15674680509089185812 IuLatn
        ],
      bRaw
        15832404490020978706
        Nothing
        [ lRaw 14404647684545708052 MnCyrl,
          lRaw 15834505038266368020 MnMong
        ],
      bRaw
        16690181889360658451
        Nothing
        [ lRaw 14251641020813934610 SgnBr,
          lRaw 14392378509169262610 SgnCo,
          lRaw 14392378509169262610 SgnCo,
          lRaw 14525234698176692242 SgnDe,
          lRaw 14531990097617747986 SgnDk,
          lRaw 14685112484948344850 SgnEs,
          lRaw 14828101773117358098 SgnFr,
          lRaw 14954202562683731986 SgnGb,
          lRaw 14972216961193213970 SgnGr,
          lRaw 15245810638555971602 SgnIe,
          lRaw 15262699137158610962 SgnIt,
          lRaw 15402310725607096338 SgnJp,
          lRaw 15843663489089404946 SgnMx,
          lRaw 15970890178562621458 SgnNi,
          lRaw 15974267878283149330 SgnNl,
          lRaw 15977645578003677202 SgnNo,
          lRaw 16271505453689602066 SgnPt,
          lRaw 16686962519314530322 SgnSe,
          lRaw 16990955494162038802 SgnUs,
          lRaw 17691265236218150930 SgnZa
        ],
      bRaw
        16694843818662428690
        Nothing
        [ lRaw 15967273465522683925 SlNedis,
          lRaw 16555186176353370133 SlRozaj
        ],
      bRaw
        16701599218103484434
        Nothing
        [ lRaw 14404647684545708052 SrCyrl,
          lRaw 15674680509089185812 SrLatn
        ],
      bRaw
        16833329507204071442
        Nothing
        [ lRaw 14108385788269953044 TgArab,
          lRaw 14404647684545708052 TgCyrl
        ],
      bRaw
        16998836793509937170
        Nothing
        [ lRaw 14404647684545708052 UzCyrl,
          lRaw 15674680509089185812 UzLatn
        ],
      pRaw 17556157247397036050 [15674680509089185812] YiLatn,
      bRaw
        17699146535566049298
        Nothing
        [ bRaw
            15098167323825012756
            (Just ZhHans)
            [ lRaw 14391252609262419986 ZhHansCn,
              lRaw 15108450849921171474 ZhHansHk,
              lRaw 15833530389927821330 ZhHansMo,
              lRaw 16689214319128215570 ZhHansSg,
              lRaw 16851343905713553426 ZhHansTw
            ],
          bRaw
            15098167392544489492
            (Just ZhHant)
            [ lRaw 14391252609262419986 ZhHantCn,
              lRaw 15108450849921171474 ZhHantHk,
              lRaw 15833530389927821330 ZhHantMo,
              lRaw 16689214319128215570 ZhHantSg,
              lRaw 16851343905713553426 ZhHantTw
            ],
          bRaw
            14391094279588020243
            (Just ZhCmn)
            [ lRaw 15098167323825012756 ZhCmnHans,
              lRaw 15098167392544489492 ZhCmnHant
            ],
          lRaw 14954044233009332243 ZhGan,
          lRaw 17282466813011034131 ZhWuu,
          lRaw 17570556451674390547 ZhYue
        ]
    ]
  where
    bRaw = stepBranch . Subtag
    lRaw = stepLeaf . Subtag
    pRaw k = stepPath (Subtag k) . fmap Subtag

-- | Determine whether or not a valid 'BCP47' tag represents a
-- 'Redundant' tag
recognizeRedundantNormal :: Normal -> Maybe Redundant
recognizeRedundantNormal = flip (lookupTrie . NE.toList . toSubtags . NormalTag) redundantTrie

-- | Determine whether or not a valid 'BCP47' tag represents a
-- 'Redundant' tag
recognizeRedundantTag :: BCP47 -> Maybe Redundant
recognizeRedundantTag (NormalTag n) = recognizeRedundantNormal n
recognizeRedundantTag _ = Nothing
