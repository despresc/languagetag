{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Parsing the BCP47 registry
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module parses the raw 'Jar' data of the BCP47 registry into a
-- more structured 'Registry'
module LanguageTag.Gen.BCP47.Parse
  ( -- * The raw registry
    parseRegistry,
    RawRegistry (..),
    TagRecord (..),
    TagType (..),

    -- * The (more) parsed library
    splitRegistry,
    Registry (..),
    LanguageRecord (..),
    ExtlangRecord (..),
    ScriptRecord (..),
    RegionRecord (..),
    VariantRecord (..),
    RangeRecord (..),
    Err (..),
    FieldTagType (..),

    -- * temporary compatibility exports
    parseRegistryNew,
  )
where

import Data.Either (partitionEithers)
import Data.Foldable (toList, traverse_)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day (..))
import qualified LanguageTag.BCP47.Dynamic.RecordJar as Jar
import qualified LanguageTag.BCP47.Dynamic.Registry as Reg
import qualified LanguageTag.BCP47.LegacyTag as Syn
import LanguageTag.BCP47.Registry (Deprecation (..), Scope (..))
import LanguageTag.BCP47.Subtag
  ( IsSubtag (..),
    Subtag,
    renderSubtagLower,
    renderSubtagTitle,
    renderSubtagUpper,
  )
import qualified LanguageTag.BCP47.Syntax as Syn
import qualified LanguageTag.BCP47.Syntax.Language as Syn
import qualified LanguageTag.BCP47.Syntax.Region as Syn
import qualified LanguageTag.BCP47.Syntax.Script as Syn
import qualified LanguageTag.BCP47.Syntax.Variant as Syn
import LanguageTag.Gen.BCP47.Common
import LanguageTag.Gen.Jar
import LanguageTag.Internal.BCP47.Registry.DataConShow

-- | An error that may occur in registry parsing
data Err
  = -- | record had unknown type
    ErrUnknownType LineNum
  | -- | errors during parsing a record
    ErrRecord [(FieldTagType, Text, LineNum, Text)]
  | -- | the registry did not have a proper date record
    ErrBadDate
  | -- | the input was empty
    ErrEmptyInput

-- | The tag types with their specific data
data TagType
  = -- | optional script suppression, optional macrolanguage, optional scope
    Language (Maybe Text) (Maybe Text) (Maybe Scope)
  | -- | optional preferred value without a deprecation notice,
    -- mandatory prefix, optional script suppression, optional
    -- macrolanguage, optional scope
    Extlang (Maybe Text) Text (Maybe Text) (Maybe Text) (Maybe Scope)
  | Script
  | Region
  | -- | optional prefix values
    Variant [Text]
  | Grandfathered
  | Redundant
  deriving (Show)

-- | The type of the record we are parsing, for use in errors
data FieldTagType
  = FieldLanguage
  | FieldExtlang
  | FieldScript
  | FieldRegion
  | FieldVariant
  | FieldGrandfathered
  | FieldRedundant
  | FieldUnknown
  deriving (Show)

-- | A raw record with the fields: tag or subtag, tag type,
-- descriptions, deprecation, preferred value, prefixes, script
-- suppression, macrolanguage, scope
data TagRecord
  = TagRecord
      Text
      TagType
      (NonEmpty Text)
      (Deprecation Text)
  deriving (Show)

-- | The raw registry with the date and the raw tag information.
data RawRegistry = RawRegistry Day [TagRecord]

-- | Parse a mandatory field in a 'Jar' record, returning all of the
-- values of the field
field :: FieldTagType -> Jar -> Text -> Either Err (NonEmpty (LineNum, Text))
field t (ln, j) k = case HM.lookup k j of
  Just v -> pure v
  Nothing -> Left $ ErrRecord [(t, k, ln, "not present")]

-- | Parse an optional field in a 'Jar' record
mfield :: Jar -> Text -> Either Err [(LineNum, Text)]
mfield (_, j) k = pure $ maybe [] toList $ HM.lookup k j

-- | Parse a field in a 'Jar' record that must appear precisely once
fieldOne :: FieldTagType -> Jar -> Text -> Either Err Text
fieldOne t (ln, j) k = case HM.lookup k j of
  Just (v NE.:| vs)
    | null vs -> pure $ snd v
  _ -> Left $ ErrRecord [(t, k, ln, "must occur exactly once")]

-- | Parse a field in a 'Jar' record that must appear at most once
optionalOne :: FieldTagType -> Jar -> Text -> Either Err (Maybe Text)
optionalOne tt (ln, j) k = case HM.lookup k j of
  Nothing -> pure Nothing
  Just (v NE.:| vs)
    | null vs -> pure $ Just $ snd v
  _ -> Left $ ErrRecord [(tt, k, ln, "must occur at most once")]

-- | The fields that we can expect to find in any particular tag or
-- subtag record
expectedFields :: HashSet Text
expectedFields =
  HS.fromList
    [ "Type",
      "Subtag",
      "Tag",
      "Description",
      "Deprecated",
      "Preferred-Value",
      "Macrolanguage",
      "Scope",
      "Comments",
      "Added",
      "Prefix",
      "Suppress-Script"
    ]

-- | Returns any unknown fields in a 'Jar' file
unexpectedFields :: Jar -> [(LineNum, Text)]
unexpectedFields (_, j) = concatMap go $ HM.toList $ HM.filterWithKey notExpected j
  where
    go (k, v) = (\(ln, _) -> (ln, k)) <$> toList v
    notExpected k _ = not $ k `HS.member` expectedFields

-- | Merging the 'ErrRecord' in a list together, throwing away
-- non-'ErrRecord' values if there is at least on 'ErrRecord', and
-- otherwise returning the leftmost error
combining ::
  [Either Err a] ->
  Either Err a
combining l
  | (v : _) <- successes = Right v
  | otherwise = Left $ foldr combineFailures (ErrRecord []) failures
  where
    (failures, successes) = partitionEithers l
    combineFailures (ErrRecord x) (ErrRecord y) = ErrRecord $ x <> y
    combineFailures x@(ErrRecord _) _ = x
    combineFailures _ x@(ErrRecord _) = x
    combineFailures x _ = x

-- | Parse a raw 'Jar' file into a 'RawRegistry', also returning any
-- unexpected fields that were encountered
parseRegistry :: [Jar] -> Either Err ([(LineNum, Text)], RawRegistry)
parseRegistry ((_, datejar) : tagjars) = case HM.toList datejar of
  [("File-Date", (_, dv) NE.:| [])] ->
    case reads (T.unpack dv) of
      [(x, "")] -> parseRegistry' x id id tagjars
      _ -> Left ErrBadDate
  _ -> Left ErrBadDate
  where
    parseRegistry' rdate unexpecteds recs (j : js) = case parseRawRecord j of
      Right r -> parseRegistry' rdate (unexpecteds . (unexpectedFields j ++)) (recs . (r :)) js
      Left e -> Left e
    parseRegistry' rdate unexpecteds recs [] =
      Right (unexpecteds [], RawRegistry rdate $ recs [])
    parseRawRecord j = do
      combining
        [ parseLanguage j,
          parseExtlang j,
          parseScript j,
          parseRegion j,
          parseVariant j,
          parseGrandfathered j,
          parseRedundant j
        ]

    guardTy j k = case lookupJar "Type" j of
      [] -> Left $ ErrRecord [(FieldUnknown, "Type", jarLine j, "is not present")]
      [(_, x)]
        | x == k -> Right ()
        | otherwise -> Left $ ErrUnknownType $ jarLine j
      _ -> Left $ ErrRecord [(FieldUnknown, "Type", jarLine j, "has multiple values")]

    guardAbsent tt j k = case lookupJar k j of
      [] -> Right ()
      _ -> Left $ ErrRecord [(tt, k, jarLine j, "is present")]

    allAbsent tt j = traverse_ (guardAbsent tt j)

    withStandardFields tt ty p j = do
      guardTy j ty
      descriptions <- fmap snd <$> field tt j "Description"
      deprecation <- do
        mdep <- optionalOne tt j "Deprecated"
        case mdep of
          Nothing -> Right NotDeprecated
          Just _ ->
            maybe DeprecatedSimple DeprecatedPreferred
              <$> optionalOne tt j "Preferred-Value"
      p j descriptions deprecation

    parseScope tt j = do
      mscope <- optionalOne tt j "Scope"
      case mscope of
        Nothing -> pure Nothing
        Just sc -> case sc of
          "macrolanguage" -> pure $ Just Macrolanguage
          "collection" -> pure $ Just Collection
          "special" -> pure $ Just Special
          "private-use" -> pure $ Just PrivateUseScope
          _ ->
            Left $
              ErrRecord
                [ ( tt,
                    "Scope",
                    jarLine j,
                    "is: " <> sc
                      <> " and not one of: macrolanguage, collection, special, private-use"
                  )
                ]

    parseLanguage = withStandardFields FieldLanguage "language" $
      \j descr depr -> do
        allAbsent FieldLanguage j ["Tag", "Prefix"]
        subtag <- fieldOne FieldLanguage j "Subtag"
        suppress <- optionalOne FieldLanguage j "Suppress-Script"
        macro <- optionalOne FieldLanguage j "Macrolanguage"
        scope <- parseScope FieldLanguage j
        pure $ TagRecord subtag (Language suppress macro scope) descr depr

    parseExtlang = withStandardFields FieldExtlang "extlang" $
      \j descr depr -> do
        allAbsent FieldExtlang j ["Tag"]
        subtag <- fieldOne FieldExtlang j "Subtag"
        preferred <- case depr of
          NotDeprecated -> optionalOne FieldExtlang j "Preferred-Value"
          _ -> pure Nothing
        prefix <- fieldOne FieldExtlang j "Prefix"
        suppress <- optionalOne FieldExtlang j "Suppress-Script"
        macro <- optionalOne FieldExtlang j "Macrolanguage"
        scope <- parseScope FieldExtlang j
        pure $
          TagRecord
            subtag
            ( Extlang
                preferred
                prefix
                suppress
                macro
                scope
            )
            descr
            depr

    parseScript = withStandardFields FieldScript "script" $
      \j descr depr -> do
        allAbsent FieldScript j ["Tag", "Prefix", "Suppress-Script", "Macrolanguage", "Scope"]
        subtag <- fieldOne FieldScript j "Subtag"
        pure $ TagRecord subtag Script descr depr

    parseRegion = withStandardFields FieldRegion "region" $
      \j descr depr -> do
        allAbsent FieldRegion j ["Tag", "Prefix", "Suppress-Script", "Macrolanguage", "Scope"]
        subtag <- fieldOne FieldRegion j "Subtag"
        pure $ TagRecord subtag Region descr depr

    parseVariant = withStandardFields FieldVariant "variant" $
      \j descr depr -> do
        allAbsent FieldVariant j ["Tag", "Suppress-Script", "Macrolanguage", "Scope"]
        subtag <- fieldOne FieldVariant j "Subtag"
        prefix <- fmap snd <$> mfield j "Prefix"
        pure $ TagRecord subtag (Variant prefix) descr depr

    parseGrandfathered = withStandardFields FieldGrandfathered "grandfathered" $
      \j descr depr -> do
        allAbsent FieldGrandfathered j ["Subtag", "Prefix", "Suppress-Script", "Macrolanguage", "Scope"]
        tag <- fieldOne FieldGrandfathered j "Tag"
        pure $ TagRecord tag Grandfathered descr depr

    parseRedundant = withStandardFields FieldRedundant "redundant" $
      \j descr depr -> do
        allAbsent FieldRedundant j ["Subtag", "Prefix", "Suppress-Script", "Macrolanguage", "Scope"]
        tag <- fieldOne FieldRedundant j "Tag"
        pure $ TagRecord tag Redundant descr depr
parseRegistry [] = Left ErrEmptyInput

-- | Unpack the four known registry ranges. In the unlikely event that
-- more are added, the code generator will probably throw an
-- exception.
unpackRegistryRanges :: [TagRecord] -> [TagRecord]
unpackRegistryRanges = concatMap unpackRecord
  where
    unpackRecord r@(TagRecord t ty desc dep)
      | t == "qaa..qtz" = (\x -> TagRecord x ty desc dep) <$> qaa
      | t == "Qaaa..Qabx" = (\x -> TagRecord x ty desc dep) <$> qaaa
      | t == "QM..QZ" = (\x -> TagRecord x ty desc dep) <$> qm
      | t == "XA..XZ" = (\x -> TagRecord x ty desc dep) <$> xa
      | otherwise = [r]
    qaa = [T.pack ['q', x, y] | x <- ['a' .. 't'], y <- ['a' .. 'z']]
    qaaa =
      [T.pack ['Q', 'a', 'a', x] | x <- ['a' .. 'z']]
        <> [T.pack ['Q', 'a', 'b', x] | x <- ['a' .. 'x']]
    qm = [T.pack ['Q', x] | x <- ['M' .. 'Z']]
    xa = [T.pack ['X', x] | x <- ['A' .. 'Z']]

-- | A raw language subtag record
data LanguageRecord = LanguageRecord
  { langTyCon :: Text,
    langDescription :: NonEmpty Text,
    langDeprecation :: Deprecation Text,
    langScriptSuppression :: Maybe Text,
    langMacrolanguage :: Maybe Text,
    langScope :: Maybe Scope
  }

-- | A raw extended language subtag record
data ExtlangRecord = ExtlangRecord
  { extlangTyCon :: Text,
    extlangDescription :: NonEmpty Text,
    extlangDeprecation :: Bool,
    extlangPreferredValue :: Text,
    extlangPrefix :: Text,
    extlangScriptSuppression :: Maybe Text,
    extlangMacrolanguage :: Maybe Text,
    extlangScope :: Maybe Scope
  }

-- | A raw variant subtag record
data VariantRecord = VariantRecord
  { variantTyCon :: Text,
    variantDescription :: NonEmpty Text,
    variantDeprecation :: Deprecation Text,
    variantPrefixes :: [Text]
  }

-- | A raw script record
data ScriptRecord = ScriptRecord
  { scriptTyCon :: Text,
    scriptDescription :: NonEmpty Text,
    scriptDeprecation :: Deprecation Text
  }

-- | A raw region record
data RegionRecord = RegionRecord
  { regionTyCon :: Text,
    regionDescription :: NonEmpty Text,
    regionDeprecation :: Deprecation Text
  }

-- | A grandfathered or redundant subtag record. These records are
-- distinguished from the others in that they define entire tags, and
-- that the preferred values associated to their deprecation are an
-- "extended language range", which is an entire tag that is strongly
-- recommended as the replacement for the tag.
data RangeRecord = RangeRecord
  { rangeTyCon :: Text,
    rangeDescription :: NonEmpty Text,
    rangeDeprecation :: Deprecation Text
  }

-- | The full BCP47 subtag registry. Note that the registry file may
-- also contain ranges of values, like @a..c@ for @a, b, c@. These are
-- expanded here, so that only individual values remain.
data Registry = Registry
  { date :: Day,
    languageRecords :: Map Text LanguageRecord,
    extlangRecords :: Map Text ExtlangRecord,
    scriptRecords :: Map Text ScriptRecord,
    regionRecords :: Map Text RegionRecord,
    variantRecords :: Map Text VariantRecord,
    grandfatheredRecords :: Map Text RangeRecord,
    redundantRecords :: Map Text RangeRecord
  }

-- | Take the 'RawRegistry' data and split the different record types
-- apart
splitRegistry :: RawRegistry -> Registry
splitRegistry (RawRegistry regdate rs) =
  Registry
    { date = regdate,
      languageRecords = go plang,
      extlangRecords = go pextlang,
      scriptRecords = go pscr,
      regionRecords = go preg,
      variantRecords = go pvar,
      grandfatheredRecords = go pgra,
      redundantRecords = go prdn
    }
  where
    go proj = M.fromList $ mapMaybe proj $ unpackRegistryRanges rs

    plang (TagRecord tg (Language x y z) descrs deprs) =
      Just (tg, LanguageRecord (T.pack $ languageConShow $ parseSubtag' tg) descrs deprs x y z)
    plang _ = Nothing
    pextlang (TagRecord tg (Extlang _ b c d e) descrs deprs) =
      Just (tg, ExtlangRecord (T.pack $ extlangConShow $ parseSubtag' tg) descrs deprs' tg b c d e)
      where
        deprs' = case deprs of
          NotDeprecated -> False
          _ -> True
    pextlang _ = Nothing
    pscr (TagRecord tg Script descrs deprs) =
      Just (tg, ScriptRecord (T.pack $ scriptConShow $ parseSubtag' tg) descrs deprs)
    pscr _ = Nothing
    preg (TagRecord tg Region descrs deprs) =
      Just (tg, RegionRecord (T.pack $ regionConShow $ parseSubtag' tg) descrs deprs)
    preg _ = Nothing
    pvar (TagRecord tg (Variant l) descrs deprs) =
      Just (tg, VariantRecord (T.pack $ variantConShow $ parseSubtag' tg) descrs deprs l)
    pvar _ = Nothing
    pgra (TagRecord tg Grandfathered descrs deprs) =
      Just (tg, RangeRecord (renderRangeCon tg) descrs deprs)
    pgra _ = Nothing
    prdn (TagRecord tg Redundant descrs deprs) =
      Just (tg, RangeRecord (renderRangeCon tg) descrs deprs)
    prdn _ = Nothing

----------------------------------------------------------------
-- Compatibility functions for languagetag-bcp47-dynamic
----------------------------------------------------------------
-- (so we can use that package here without touching the rendering functions)

-- obviously very unprincipled!
parseRegistryNew :: Jar.JarRegistry -> Either Text Registry
parseRegistryNew = either (Left . T.pack . show) (Right . fromNewReg) . Reg.parseRegistry

onM :: Ord k2 => (k1 -> a -> (k2, b)) -> Map k1 a -> Map k2 b
onM f = M.fromList . fmap (uncurry f) . M.toList

conShow :: IsSubtag a => (Subtag -> String) -> a -> Text
conShow f = T.pack . f . toSubtag

showTitle :: IsSubtag a => a -> Text
showTitle = renderSubtagTitle . toSubtag

showUpper :: IsSubtag a => a -> Text
showUpper = renderSubtagUpper . toSubtag

showLower :: IsSubtag a => a -> Text
showLower = renderSubtagLower . toSubtag

renderRangeCon :: Text -> Text
renderRangeCon = mconcat . fmap T.toTitle . T.split (== '-')

fromDep :: (a -> b) -> Reg.Deprecation a -> Deprecation b
fromDep f (Reg.DeprecatedPreferred _ x) = DeprecatedPreferred $ f x
fromDep _ (Reg.DeprecatedSimple _) = DeprecatedSimple
fromDep _ Reg.NotDeprecated = NotDeprecated

fromScope :: Reg.Scope -> Scope
fromScope Reg.Macrolanguage = Macrolanguage
fromScope Reg.Collection = Collection
fromScope Reg.Special = Special
fromScope Reg.PrivateUseScope = PrivateUseScope

fromNewReg :: Reg.Registry -> Registry
fromNewReg newreg =
  Registry
    { date = Reg.registryDate newreg,
      languageRecords = fromLangRecords $ Reg.registryLanguages newreg,
      extlangRecords = fromExtlangRecords $ Reg.registryExtlangs newreg,
      scriptRecords = fromScriptRecords $ Reg.registryScripts newreg,
      regionRecords = fromRegionRecords $ Reg.registryRegions newreg,
      variantRecords = fromVariantRecords $ Reg.registryVariants newreg,
      grandfatheredRecords = fromGrandfatheredRecords $ Reg.registryGrandfatheredTags newreg,
      redundantRecords = fromRedundantRecords $ Reg.registryRedundantTags newreg
    }

fromLangRecords :: Map Syn.Language Reg.LanguageRecord -> Map Text LanguageRecord
fromLangRecords = onM go
  where
    go lang record = (showLower lang, record')
      where
        record' =
          LanguageRecord
            { langTyCon = conShow languageConShow lang,
              langDescription = Reg.languageDescription record,
              langDeprecation =
                fromDep showLower $ Reg.languageDeprecation record,
              langScriptSuppression =
                showTitle <$> Reg.languageScriptSuppression record,
              langMacrolanguage = showLower <$> Reg.languageMacrolanguage record,
              langScope = fromScope <$> Reg.languageScope record
            }

fromExtlangRecords :: Map Syn.Extlang Reg.ExtlangRecord -> Map Text ExtlangRecord
fromExtlangRecords = onM go
  where
    go extlang record = (showLower extlang, record')
      where
        record' =
          ExtlangRecord
            { extlangTyCon = conShow extlangConShow extlang,
              extlangPreferredValue = showLower extlang,
              extlangPrefix = showLower $ Reg.extlangPrefix record,
              extlangDescription = Reg.extlangDescription record,
              extlangDeprecation = maybe False (const True) $ Reg.extlangDeprecation record,
              extlangScriptSuppression =
                showTitle <$> Reg.extlangScriptSuppression record,
              extlangMacrolanguage = showLower <$> Reg.extlangMacrolanguage record,
              extlangScope = fromScope <$> Reg.extlangScope record
            }

fromScriptRecords :: Map Syn.Script Reg.ScriptRecord -> Map Text ScriptRecord
fromScriptRecords = onM go
  where
    go script record = (showTitle script, record')
      where
        record' =
          ScriptRecord
            { scriptTyCon = conShow scriptConShow script,
              scriptDescription = Reg.scriptDescription record,
              scriptDeprecation =
                fromDep showTitle $ Reg.scriptDeprecation record
            }

fromRegionRecords :: Map Syn.Region Reg.RegionRecord -> Map Text RegionRecord
fromRegionRecords = onM go
  where
    go region record = (showUpper region, record')
      where
        record' =
          RegionRecord
            { regionTyCon = conShow regionConShow region,
              regionDescription = Reg.regionDescription record,
              regionDeprecation =
                fromDep showUpper $ Reg.regionDeprecation record
            }

fromVariantRecords :: Map Syn.Variant Reg.VariantRecord -> Map Text VariantRecord
fromVariantRecords = onM go
  where
    go variant record = (showLower variant, record')
      where
        record' =
          VariantRecord
            { variantTyCon = conShow variantConShow variant,
              variantDescription = Reg.variantDescription record,
              variantDeprecation =
                fromDep showLower $ Reg.variantDeprecation record,
              variantPrefixes = Syn.renderBCP47 <$> Reg.variantPrefixes record
            }

fromGrandfatheredRecords :: Map Syn.Grandfathered Reg.TagRecord -> Map Text RangeRecord
fromGrandfatheredRecords = onM go
  where
    go grandfathered record = (Syn.renderGrandfathered grandfathered, record')
      where
        record' =
          RangeRecord
            { rangeTyCon = renderRangeCon $ Syn.renderGrandfathered grandfathered,
              rangeDescription = Reg.tagDescription record,
              rangeDeprecation =
                fromDep Syn.renderBCP47 $ Reg.tagDeprecation record
            }

fromRedundantRecords :: Map Syn.Redundant Reg.TagRecord -> Map Text RangeRecord
fromRedundantRecords = onM go
  where
    go redundant record = (Syn.renderRedundant redundant, record')
      where
        record' =
          RangeRecord
            { rangeTyCon = renderRangeCon $ Syn.renderRedundant redundant,
              rangeDescription = Reg.tagDescription record,
              rangeDeprecation =
                fromDep Syn.renderBCP47 $ Reg.tagDeprecation record
            }
