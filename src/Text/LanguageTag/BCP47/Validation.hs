-- |
-- Module      : Text.LanguageTag.BCP47.Validation
-- Description : Language tag validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'validateBCP47' function to validate a
-- syntactically well-formed 'LanguageTag', transforming it into a
-- 'BCP47Tag' value, as well as functions to validate the different
-- types of subtags. See
-- 'Text.LanguageTag.BCP47.Registry.bcp47RegistryDate' for the version
-- of the registry that this library uses; the current version of the
-- IANA subtag registry is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Validation
  ( -- * Tag validation
    validateBCP47,
    BCP47Tag (..),
    Normal (..),
    ExtensionSubtag,
    toExtensionSubtag,
    fromExtensionSubtag,

    -- * Subtag validation
    validateLanguage,
    validateExtlang,
    validateScript,
    validateRegion,
    validateVariant,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Registry.LanguageRecords
import Text.LanguageTag.Internal.BCP47.Registry.RegionRecords
import Text.LanguageTag.Internal.BCP47.Registry.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Text.LanguageTag.Internal.BCP47.Registry.VariantRecords
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.Subtag (Subtag (..), Trie (..))
import Text.LanguageTag.Subtag
  ( MaybeSubtag,
    maybeSubtag,
    stepBranch,
    stepLeaf,
    stepPath,
    trieStep,
  )

data ValidateError
  = ErrorLanguage Subtag
  | ErrorExtlang Subtag
  | ErrorExcessExtlang Subtag
  | ErrorScript Subtag
  | ErrorRegion Subtag
  | ErrorVariant Subtag
  | ErrorDuplicateVariant Variant
  | ErrorExtension Subtag
  | ErrorDuplicateExtension Syn.ExtensionChar
  deriving (Show)

type VM a = Either ValidateError a

throw :: ValidateError -> VM a
throw = Left

validate :: (Subtag -> ValidateError) -> (Subtag -> Maybe a) -> Subtag -> VM a
validate ve f s = case f s of
  Nothing -> throw $ ve s
  Just x -> pure x

maybeValidate ::
  (Subtag -> ValidateError) ->
  (Subtag -> Maybe a) ->
  MaybeSubtag ->
  VM (Maybe a)
maybeValidate cmp f = maybeSubtag (pure Nothing) (fmap Just . validate cmp f)

-- | Validate a language tag strictly: for normal tags, this means
-- there must be no duplicate variants, duplicate extension
-- singletons, more than one extended language subtag, or unregistered
-- subtags. Any 'Grandfathered' tag will be valid as well.
validateBCP47 :: Syn.LanguageTag -> Either ValidateError BCP47Tag
validateBCP47 = validateBCP47'

-- TODO: bring back the private use tag in syntax, I think?
-- TODO: should probably write the lax version of this
validateBCP47' :: Syn.LanguageTag -> VM BCP47Tag
validateBCP47' (Syn.NormalTag (Syn.Normal mpl e1 e2 e3 s r vs es ps)) =
  maybeSubtag neps go mpl
  where
    guardNull = maybeSubtag (pure ()) $ throw . ErrorExcessExtlang
    neps = case NE.nonEmpty ps of
      Just l -> pure $ PrivateUse l
      Nothing -> error "internal invariant violated: tag has no primary language and an empty private use section"
    addVariant st v = do
      st' <- st
      v' <- validate ErrorVariant validateVariant v
      S.alterF (insertVariant v') v' st'
    insertVariant v b
      | b = throw $ ErrorDuplicateVariant v
      | otherwise = pure True
    addExtension m e = do
      m' <- m
      let c = Syn.extSingleton e
      M.alterF (insertExtension c $ Syn.extTags e) c m'
    insertExtension _ ts Nothing = pure $ Just $ ExtensionSubtag <$> ts
    insertExtension c _ (Just _) = throw $ ErrorDuplicateExtension c
    go pl = case validateLanguage pl of
      Nothing -> throw $ ErrorLanguage pl
      Just valpl -> do
        vale1 <- maybeValidate ErrorExtlang validateExtlang e1
        guardNull e2
        guardNull e3
        vals <- maybeValidate ErrorScript validateScript s
        valr <- maybeValidate ErrorRegion validateRegion r
        valvs <- List.foldl' addVariant (pure mempty) vs
        vales <- List.foldl' addExtension (pure mempty) es
        pure $
          NormalTag
            Normal
              { language = valpl,
                extlang = vale1,
                script = vals,
                region = valr,
                variants = valvs,
                extensions = vales,
                privateUse = ps
              }
validateBCP47' (Syn.Grandfathered x) = pure $ GrandfatheredTag x

{-

The rules:

- primary subtags - be registered

- extended language subtags - be registered. recommended that the
  related primary language subtag be used instead. must only have one
  extended language subtag (zh-min-nan excluded, I suppose)

- script subtags - should be omitted if it adds no value or if the
  primary language subtag has a relevant suppress-script field.

- variants - must not be repeated. variants sharing a prefix should
  not be used in the same tag (variants that can be used in
  combination should have multiple prefix fields indicating that
  fact).

- extension subtags - a singleton must appear at most once (except in
  private use), must have been registered. should canonicalize by
  ordering the singletions by ascii order.

- grandfathered/redundant - must match exactly, of course.

valid tag:

- well-formed
- either grandfathered or all the primary language, extended language,
  script, region, and variant subtags appear in the IANA subtag
  registry.
- no duplicate variant subtags
- no duplicate singleton (extension) subtags

canonicalizing:

- extension sequences are ordered by singleton subtag (ascii
  case-insensitive)
- redundant or grandfathered tags are replaced by their preferred
  value
- subtags are replaced by their preferred value (notably this means
  that there will be no extlangs in canonical form)

(so maybe we define a separate canonical tag type? and the canonical
form -> extlang form transformation can be defined as a CanonicalTag
-> ValidTag function).

-}

----------------------------------------------------------------
-- Validation
----------------------------------------------------------------

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

----------------------------------------------------------------
-- Tag exports
----------------------------------------------------------------

-- $thetags
--
-- Below we have re-exported the modules containing all of the subtags
-- that are registered with the IANA. Check 'bcp47RegistryDate' for
-- the version of the registry that this library uses.
--
-- The names of the subtag data constructors are roughly the same as
-- the subtags themselves; all of them are camel case except for the
-- 'Region' tags, which are all upper case. Additionally, the
-- 'Extlang' constructors and the constructors corresponding to
-- subtags that start with a digit are prefixed with the first three
-- letters of their types. These modifications were necessary to
-- arrive at constructor names that are valid and free of name
-- collisions. Some examples:
--
-- @
--  -- for the language subtag en
-- 'En' :: 'Language'
-- -- for the extended language subtag sgg
-- 'ExtSgg' :: 'Extlang'
-- -- for the script subtag moon
-- 'Moon' :: 'Script'
-- -- for the region subtag AU
-- 'AU' :: 'Region'
-- -- for the region subtag 419
-- 'Reg419' :: 'Region'
-- -- for the variant subtag gascon
-- 'Gascon' :: 'Variant'
-- -- for the variant subtag 1606nict
-- 'Var1606nict' :: 'Variant'
-- -- for the grandfathered subtag i-enochian
-- 'IEnochian' :: 'Grandfathered'
-- -- for the redundant subtag en-scouse
-- 'EnScouse' :: 'Redundant'
-- @

-- $therecords
--
-- The subtag registry contains records for language, extended
-- language, script, and region subtags, and grandfathered and
-- redundants tags. Some general notes on these records:
--
-- * Each record contains at least one (non-normative) description of
--   what the subtag represents. The registry does not guarantee that
--   this description will be in any particular language, and these
--   descriptions may be added, changed, or removed as the registry
--   updates.
--
-- * Each record may contain a deprecation notice, indicating that the
--   associated tag or subtag should not be used. This value may be
--   added, changed, or removed as the registry updates.
--
-- * If deprecated, a record may contain a preferred value that is
--   recommended for use instead of the deprected tag. Note that for
--   regions, this preferred value may not have exactly the same
--   meaning as the old tag. This value may be added, removed or
--   modified as the registry updates, and a change in this value does
--   not imply that the affected subtag needs to be retagged. Tags
--   that appear as preferred values will never have a preferred value
--   themselves.
--
-- * Extlang and variant records may have a prefix (more than one in
--   the case of variant records) that is recommended as a prefix to
--   the record's subtag. If a record does not have a prefix field,
--   one will not be added as the registry updates, and changes to a
--   prefix field must only widen the range of possible prefixes.
--
-- * Language an extlang records have macrolanguage and scope fields,
--   which are informational, and indicate an encompassing language
--   and a classification of the language, respectively. These may be
--   added, removed, or changed as the registry updates.
