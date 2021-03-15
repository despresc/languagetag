{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Parse language code registries, generate code
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- A quick executable to parse the content of various registries and
-- render some internal modules. The sources for the registries:
--
-- * The BCP47 data in @registry/bcp47@ can be obtained from
--  <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>,
--  which periodically updates in-place.
module Main where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (toList, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (Day (..))
import Text.LanguageTag.BCP47.Registry (Deprecation (..), Scope (..))
import Text.LanguageTag.BCP47.Subtag
  ( Subtag,
    maybeSubtag,
    nullSubtag,
    parseSubtag,
    renderSubtagLower,
    unwrapSubtag,
  )
import Text.LanguageTag.BCP47.Syntax (parseBCP47)
import Text.LanguageTag.Internal.BCP47.Registry.DataConShow
import Text.LanguageTag.Internal.BCP47.Syntax (BCP47 (NormalTag), Normal (..))

{-
TODO:

Tests of:

- parseRawRecord, at least on select tags of different types

Might also like to diff the parsed registry against the old one, for
linting purposes (e.g. to verify that we haven't registered changes in
(sub)tag information that should never be changed).

Should move this to a separate package (making this a monorepo).

I think that the registry records could be vectors, actually, because
the registered subtag enums and their corresponding subtags have the
same ordering and we already produce a sorted list of records. So we
could implement a binary search and only keep around the one copy of
the registries. Might be more efficient.
-}

main :: IO ()
main = do
  (u, r) <- readLocalRegistry
  unless (null u) $ do
    putStrLn "Unrecognized BCP47 registry tag fields:"
    print u
  putStrLn "writing the internal modules"
  renderSplitRegistry $ splitRegistry r

----------------------------------------------------------------
-- Parsing record jars and fetching the BCP47 registry
----------------------------------------------------------------

type LineNum = Int

type Jar = (LineNum, HashMap Text (NonEmpty (LineNum, Text)))

lookupJar :: Text -> Jar -> [(LineNum, Text)]
lookupJar t (_, j) = maybe [] toList $ HM.lookup t j

jarLine :: Jar -> LineNum
jarLine = fst

-- | Group the fields of a record jar together with any following line
-- folds.
groupFields :: [(LineNum, Text)] -> [(LineNum, Text, [Text])]
groupFields ((ln, t) : ts) =
  let (folds, ts') = grabFolds id ts
   in (ln, t, folds) : groupFields ts'
  where
    grabFolds l ((n, y) : ys)
      | T.take 1 y == " " = grabFolds (l . (y :)) ys
      | otherwise = (l [], (n, y) : ys)
    grabFolds l [] = (l [], [])
groupFields [] = []

parseFields :: [(LineNum, Text, [Text])] -> Either (LineNum, Text) [(LineNum, Text, Text)]
parseFields = traverse go
  where
    go (ln, t, ts)
      | (tag, rest) <- T.span (/= ':') t,
        not (T.null tag),
        not (T.any isSpace tag) =
        let rest' = T.drop 1 rest
            spacenorm = T.intercalate " " . T.split isSpace . T.strip
            body =
              T.intercalate "\n" $ fmap spacenorm $ rest' : ts
         in Right (ln, tag, body)
      | otherwise = Left (ln, t)

parseJar :: (LineNum, [(LineNum, Text)]) -> Either (LineNum, Text) Jar
parseJar (ln, inp) = (,) ln . go <$> parseFields (groupFields inp)
  where
    go = HM.fromList . gather
    getTag (_, x, _) = x
    gather =
      fmap fuse . List.groupBy (\x y -> getTag x == getTag y)
        . List.sortBy (\x y -> getTag x `compare` getTag y)
    fuse ((l, tag, body) : rest) = (tag, (l, body) NE.:| ((\(x, _, z) -> (x, z)) <$> rest))
    fuse _ = error "internal error: parseJar.fuse"

groupJars :: [(LineNum, Text)] -> [(LineNum, [(LineNum, Text)])]
groupJars inp = start : manyJars inp'
  where
    jarSpan n l = let (x, y) = List.span (\(_, t) -> t /= "%%") l in ((n, x), y)
    (start, inp') = jarSpan 0 inp
    manyJars [] = []
    manyJars ((l, _) : ts) =
      let (a, b) = jarSpan (l + 1) ts
       in a : manyJars b

parseJarFile :: Text -> Either (LineNum, Text) [Jar]
parseJarFile = traverse parseJar . groupJars . zip [0 ..] . T.lines

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

-- The registry with the date and the raw tag information.
data RawRegistry = RawRegistry Day [TagRecord]

field :: FieldTagType -> Jar -> Text -> Either Err (NonEmpty (LineNum, Text))
field t (ln, j) k = case HM.lookup k j of
  Just v -> pure v
  Nothing -> Left $ ErrRecord [(t, k, ln, "not present")]

mfield :: Jar -> Text -> Either Err [(LineNum, Text)]
mfield (_, j) k = pure $ maybe [] toList $ HM.lookup k j

fieldOne :: FieldTagType -> Jar -> Text -> Either Err Text
fieldOne t (ln, j) k = case HM.lookup k j of
  Just (v NE.:| vs)
    | null vs -> pure $ snd v
  _ -> Left $ ErrRecord [(t, k, ln, "must occur exactly once")]

optionalOne :: FieldTagType -> Jar -> Text -> Either Err (Maybe Text)
optionalOne tt (ln, j) k = case HM.lookup k j of
  Nothing -> pure Nothing
  Just (v NE.:| vs)
    | null vs -> pure $ Just $ snd v
  _ -> Left $ ErrRecord [(tt, k, ln, "must occur at most once")]

-- expected fields for the subtag records, not the initial file date
-- record.
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

unexpectedFields :: Jar -> [(LineNum, Text)]
unexpectedFields (_, j) = concatMap go $ HM.toList $ HM.filterWithKey notExpected j
  where
    go (k, v) = (\(ln, _) -> (ln, k)) <$> toList v
    notExpected k _ = not $ k `HS.member` expectedFields

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

data Err
  = -- | record had unknown type
    ErrUnknownType LineNum
  | -- | errors during parsing a record
    ErrRecord [(FieldTagType, Text, LineNum, Text)]
  | -- | the registry did not have a proper date record
    ErrBadDate
  | -- | the input was empty
    ErrEmptyInput

-- Also returns any fields we didn't expect. The error list will be
-- empty exactly when the file date is malformed or the input is empty.

--TODO: split off parseRawRecord
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

-- Does /not/ unpack ranges
parseRegistryThrow :: Text -> IO ([(LineNum, Text)], RawRegistry)
parseRegistryThrow inp = do
  let jarErr (lineNum, t) =
        fail $
          "couldn't parse tag "
            <> T.unpack t
            <> " at line "
            <> show lineNum
  let withLine n t = "at line " <> show n <> "\n" <> t
  let tagErr (ErrUnknownType n) = fail $ withLine n "unknown type"
      tagErr (ErrRecord rs) = fail $ List.intercalate "\n" $ fmap allErrs rs
        where
          allErrs (tt, k, l, t) =
            withLine l $
              "record type " <> show tt <> "; field " <> T.unpack k
                <> "\n"
                <> T.unpack t
      tagErr ErrBadDate = fail "bad date record"
      tagErr ErrEmptyInput = fail "empty input"
  jars <- either jarErr pure $ parseJarFile inp
  either tagErr pure $ parseRegistry jars

readLocalRegistry :: IO ([(LineNum, Text)], RawRegistry)
readLocalRegistry = T.readFile "./registry/bcp47" >>= parseRegistryThrow

-- Unpack the four known registry ranges. In the unlikely even that
-- more are added, the code generator will probably fail!
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

----------------------------------------------------------------
-- Parsing the registry and tag records
----------------------------------------------------------------

data LanguageRecord = LanguageRecord
  { langTyCon :: Text,
    langDescription :: NonEmpty Text,
    langDeprecation :: Deprecation Text,
    langScriptSuppression :: Maybe Text,
    langMacrolanguage :: Maybe Text,
    langScope :: Maybe Scope
  }

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

data VariantRecord = VariantRecord
  { variantTyCon :: Text,
    variantDescription :: NonEmpty Text,
    variantDeprecation :: Deprecation Text,
    variantPrefixes :: [Text]
  }

data ScriptRecord = ScriptRecord
  { scriptTyCon :: Text,
    scriptDescription :: NonEmpty Text,
    scriptDeprecation :: Deprecation Text
  }

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
    renderRangeCon = mconcat . fmap T.toTitle . T.split (== '-')

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
-- Rendering code
----------------------------------------------------------------

warning :: Text
warning = "-- This is an auto-generated file. Do not edit by hand."

escapeHaddockChars :: Text -> Text
escapeHaddockChars = T.concatMap go
  where
    go c = case c of
      '\\' -> "\\\\"
      '/' -> "\\/"
      '\'' -> "\\'"
      '`' -> "\\`"
      '"' -> "\\\""
      '@' -> "\\@"
      '<' -> "\\<"
      '$' -> "\\$"
      '#' -> "\\#"
      _ -> T.singleton c

-- | Render an internal subtag module
renderSubtagModuleWith ::
  -- | the desired type name
  Text ->
  -- | a description of the type
  Text ->
  -- | an additional note in the documentation
  Text ->
  -- | Projection returning the appropriate map
  (Registry -> Map Text a) ->
  -- | projection returning the constructor name, description,
  -- deprecation and optional preferred value without deprecation (for
  -- extlang only, essentially)
  (a -> (Text, NonEmpty Text, Deprecation Text, Maybe Text)) ->
  -- | The registry itself
  Registry ->
  Text
renderSubtagModuleWith tyname tydescription docnote proj sel reg =
  T.unlines $
    [ warning,
      "",
      "{-# LANGUAGE NoImplicitPrelude #-}",
      "",
      "module Text.LanguageTag.Internal.BCP47.Registry." <> tyname <> " where",
      "",
      "import Prelude hiding (LT, GT)",
      "import Control.DeepSeq (NFData(..), rwhnf)",
      "import Data.Hashable (Hashable(..), hashUsing)"
    ]
      <> [ "",
           "-- | The BCP47 " <> tydescription <> " tags as of " <> T.pack (show $ date reg) <> "." <> docnote',
           "data " <> tyname
         ]
      <> theConstructors
      <> theInstances
      <> [""]
      <> theNFData
      <> [""]
      <> theHashable
  where
    docnote'
      | T.null docnote = ""
      | otherwise = " " <> docnote
    rs' = M.toAscList $ M.map sel $ proj reg
    renderDescrs descrs =
      "Description: " <> T.intercalate "; " (toList $ renderDescr <$> descrs) <> "."
    renderDescr = T.intercalate " " . T.words
    renderDepr NotDeprecated = ""
    renderDepr DeprecatedSimple = " Deprecated."
    renderDepr (DeprecatedPreferred t) = " Deprecated. Preferred value: " <> t <> "."
    renderPref Nothing = ""
    renderPref (Just x) = " Preferred value: " <> x <> "."
    conBody (x, (a, y, z, mpref)) =
      mconcat
        [ a,
          " -- ^ @",
          escapeHaddockChars x,
          "@. ",
          escapeHaddockChars $ renderDescrs y,
          escapeHaddockChars $ renderDepr z,
          escapeHaddockChars $ renderPref mpref
        ]
    theConstructors = case rs' of
      (x : xs) -> ("  = " <> conBody x) : fmap (\y -> "  | " <> conBody y) xs
      [] -> error "given empty registry!"
    theInstances = ["  deriving (Eq, Ord, Enum, Bounded)"]
    theNFData =
      [ "instance NFData " <> tyname <> " where",
        "  rnf = rwhnf"
      ]
    theHashable =
      [ "instance Hashable " <> tyname <> " where",
        "  hashWithSalt = hashUsing fromEnum"
      ]

renderRecordModuleWith ::
  -- | the type name
  Text ->
  -- | additional imports
  [Text] ->
  -- | project the relevant map from the registry
  (Registry -> Map Text a) ->
  -- | render an entry in the record table
  (Registry -> Text -> a -> Text) ->
  -- | the registry itself
  Registry ->
  Text
renderRecordModuleWith tyname imps proj rend reg =
  T.unlines $
    [ warning,
      "",
      "{-# LANGUAGE NoImplicitPrelude #-}",
      "{-# LANGUAGE OverloadedStrings #-}",
      "",
      "module Text.LanguageTag.Internal.BCP47.Registry." <> tyname <> "Records",
      "  ("
        <> T.intercalate
          ", "
          [ lookupname1,
            lookupname2,
            lookupname3,
            lookupname4
          ]
        <> ") where",
      "",
      "import Prelude hiding (LT, GT)",
      "import Text.LanguageTag.Internal.BCP47.Registry." <> tyname,
      "import Text.LanguageTag.Internal.BCP47.Registry.Types",
      "import Data.List.NonEmpty (NonEmpty(..))",
      "import Data.Vector (Vector)",
      "import qualified Data.Vector as V",
      "import Text.LanguageTag.Internal.BCP47.Subtag (Subtag(..))"
    ]
      <> imps
      <> [""]
      <> theDetailTable
      <> [""]
      <> lookup1
      <> [""]
      <> lookup2
      <> [""]
      <> lookup3
      <> [""]
      <> lookup4
  where
    rend' = uncurry $ rend reg
    details = rend' <$> M.toAscList (proj reg)
    detailTableEntries = ["  [" <> T.intercalate "\n  ," details <> "]"]
    detailTableName = T.toLower tyname <> "Details"
    theDetailTable =
      [ "-- | The subtag and record information associated to the '" <> tyname <> "' type.",
        detailTableName <> " :: Vector (Subtag, " <> tyname <> "Record)",
        detailTableName <> " = V.fromList"
      ]
        <> detailTableEntries

    lookupname1 = "lookup" <> tyname <> "Details"
    lookup1 =
      [ "-- | Look up the subtag and record details associated to the given '" <> tyname <> "' subtag",
        lookupname1 <> " :: " <> tyname <> " -> (Subtag, " <> tyname <> "Record)",
        lookupname1 <> " = V.unsafeIndex " <> detailTableName <> " . fromEnum"
      ]
    lookupname2 = "validate" <> tyname
    lookup2 =
      [ "-- | Validate the given 'Subtag' against the " <> T.toLower tyname <> " records in the registry",
        lookupname2 <> " :: Subtag -> Maybe " <> tyname,
        lookupname2 <> " = fmap toEnum . flip (binSearchIndexOn fst) " <> detailTableName
      ]
    lookupname3 = T.toLower tyname <> "ToSubtag"
    lookup3 =
      [ "-- | Look up the 'Subtag' associated to the given '" <> tyname <> "'",
        lookupname3 <> " :: " <> tyname <> " -> Subtag",
        lookupname3 <> " = fst . " <> lookupname1
      ]
    lookupname4 = "lookup" <> tyname <> "Record"
    lookup4 =
      [ "-- | Look up the '" <> tyname <> "Record" <> "' associated to the given '" <> tyname <> "'",
        lookupname4 <> " :: " <> tyname <> " -> " <> tyname <> "Record",
        lookupname4 <> " = snd . " <> lookupname1
      ]

-- TODO: duplication, obviously, and rename this to renderRedundant...
renderRangeRecordModuleWith ::
  -- | the type name
  Text ->
  -- | additional imports
  [Text] ->
  -- | project the relevant map from the registry
  (Registry -> Map Text a) ->
  -- | render an entry in the record table
  (Registry -> Text -> a -> Text) ->
  -- | the registry itself
  Registry ->
  Text
renderRangeRecordModuleWith tyname imps proj rend reg =
  T.unlines $
    [ warning,
      "",
      "{-# LANGUAGE NoImplicitPrelude #-}",
      "{-# LANGUAGE OverloadedStrings #-}",
      "",
      "module Text.LanguageTag.Internal.BCP47.Registry." <> tyname <> "Records",
      "  (" <> T.intercalate ", " [lookupname1, lookupname2, lookupname3, lookupname4] <> ") where",
      "",
      "import Prelude hiding (LT, GT)",
      "import Text.LanguageTag.Internal.BCP47.Registry." <> tyname,
      "import Text.LanguageTag.Internal.BCP47.Registry.Types",
      "import Data.List.NonEmpty (NonEmpty(..))",
      "import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn",
      "import Data.Vector (Vector)",
      "import qualified Data.Vector as V"
    ]
      <> imps
      <> [""]
      <> detailsTable
      <> [""]
      <> lookup1
      <> [""]
      <> lookup2
      <> [""]
      <> lookup3
      <> [""]
      <> lookup4
  where
    detailTableName = T.toLower tyname <> "Details"
    rend' = uncurry $ rend reg
    tableEntries = case M.toAscList $ proj reg of
      (x : xs) -> "  [" <> rend' x : tableMid xs
      [] -> error $ "renderRecordModuleWith: given an empty registry for" <> T.unpack tyname
    tableMid [x] = ["  ," <> rend' x <> "]"]
    tableMid (x : xs) = "  ," <> rend' x : tableMid xs
    tableMid [] =
      error $ "renderRecordModuleWith: given a registry with one entry for" <> T.unpack tyname
    lookupname1 = "lookup" <> tyname <> "Details"
    lookup1 =
      [ "-- | Look up the tag and record details associated to the given '" <> tyname <> "' tag",
        lookupname1 <> " :: " <> tyname <> " -> (Normal, Syn.Normal, RangeRecord)",
        "lookup" <> tyname <> "Details = V.unsafeIndex " <> detailTableName <> " . fromEnum"
      ]
    lookupname2 = T.toLower tyname <> "ToValidTag"
    lookup2 =
      [ "-- | Convert a '" <> tyname <> "' tag to a 'Normal' validated tag",
        lookupname2 <> " :: " <> tyname <> " -> Normal",
        lookupname2 <> " = (\\(x, _, _) -> x) . " <> lookupname1
      ]
    lookupname3 = T.toLower tyname <> "ToSyntaxTag"
    lookup3 =
      [ "-- | Convert a '" <> tyname <> "' tag to a merely well-formed tag",
        lookupname3 <> " :: " <> tyname <> " -> Syn.Normal",
        lookupname3 <> " = (\\(_, y, _) -> y) . " <> lookupname1
      ]
    lookupname4 = "lookup" <> tyname <> "Record"
    lookup4 =
      [ "-- | Look up the record associated to the given '" <> tyname <> "' tag",
        lookupname4 <> " :: " <> tyname <> " -> RangeRecord",
        lookupname4 <> " = (\\(_, _, z) -> z) . " <> lookupname1
      ]
    detailsTable =
      [ detailTableName <> " :: Vector (Normal, Syn.Normal, RangeRecord)",
        detailTableName <> " = V.fromList"
      ]
        <> tableEntries

-- TODO: duplication, but sadly we need to treat the gradfathered
-- records separately from the redundant records. They might both have
-- to be separate, honestly. Also should look at where this is called
-- to reduce the number of parameters this takes.
renderGrandfatheredRecordModule ::
  -- | the type name
  Text ->
  -- | additional imports
  [Text] ->
  -- | project the relevant map from the registry
  (Registry -> Map Text a) ->
  -- | render an entry in the record table
  (Registry -> Text -> a -> Text) ->
  -- | the registry itself
  Registry ->
  Text
renderGrandfatheredRecordModule tyname imps proj rend reg =
  T.unlines $
    [ warning,
      "",
      "{-# LANGUAGE NoImplicitPrelude #-}",
      "{-# LANGUAGE OverloadedStrings #-}",
      "",
      "module Text.LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords",
      "  (lookupGrandfatheredRecord) where",
      "",
      "import Prelude hiding (LT, GT)",
      "import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered",
      "import Text.LanguageTag.Internal.BCP47.Registry.Types",
      "import Data.List.NonEmpty (NonEmpty(..))",
      "import Data.Vector (Vector)",
      "import qualified Data.Vector as V"
    ]
      <> imps
      <> [""]
      <> detailsTable
      <> [""]
      <> lookup1
  where
    detailTableName = T.toLower tyname <> "Details"
    rend' = uncurry $ rend reg
    tableEntries = case M.toAscList $ proj reg of
      (x : xs) -> "  [" <> rend' x : tableMid xs
      [] -> error $ "renderRecordModuleWith: given an empty registry for" <> T.unpack tyname
    -- FIXME: intercalate
    tableMid [x] = ["  ," <> rend' x <> "]"]
    tableMid (x : xs) = "  ," <> rend' x : tableMid xs
    tableMid [] =
      error $ "renderRecordModuleWith: given a registry with one entry for" <> T.unpack tyname
    lookupname1 = "lookup" <> tyname <> "Record"
    lookup1 =
      [ "-- | Look up the subtag and record details associated to the given 'Grandfathered' tag.",
        lookupname1 <> " :: " <> tyname <> " -> RangeRecord",
        lookupname1 <> " = V.unsafeIndex " <> detailTableName <> " . fromEnum"
      ]
    detailsTable =
      [ detailTableName <> " :: Vector RangeRecord",
        detailTableName <> " = V.fromList"
      ]
        <> tableEntries

-- | Render an internal subtag module.

-- TODO: change this name (only used with redundant/grandfathered),
-- also duplication.
renderModuleWith ::
  -- | the desired type name
  Text ->
  -- | a description of the type
  Text ->
  -- | an additional note in the documentation
  Text ->
  -- | the date of the registry that was used
  Day ->
  -- | projection returning the constructor name, description,
  -- deprecation and optional preferred value without deprecation (for
  -- extlang only, essentially)
  (a -> (Text, NonEmpty Text, Deprecation Text, Maybe Text)) ->
  -- | the actual subtag type registry
  Map Text a ->
  Text
renderModuleWith tyname tydescription docnote d sel rs =
  T.unlines $
    [ warning,
      "",
      "{-# LANGUAGE NoImplicitPrelude #-}",
      "",
      "module " <> modulename <> " where",
      "",
      "import Prelude hiding (LT, GT)",
      "import Control.DeepSeq (NFData(..), rwhnf)",
      "import Data.Hashable (Hashable(..), hashUsing)"
    ]
      <> [ "",
           "-- | The BCP47 " <> tydescription <> " tags as of " <> T.pack (show d) <> "." <> docnote',
           "data " <> tyname
         ]
      <> theConstructors
      <> theInstances
      <> [""]
      <> theNFData
      <> [""]
      <> theHashable
  where
    modulename = "Text.LanguageTag.Internal.BCP47.Registry." <> tyname
    docnote'
      | T.null docnote = ""
      | otherwise = " " <> docnote
    rs' = M.toAscList $ M.map sel rs
    renderDescrs descrs =
      "Description: " <> T.intercalate "; " (toList $ renderDescr <$> descrs) <> "."
    renderDescr = T.intercalate " " . T.words
    renderDepr NotDeprecated = ""
    renderDepr DeprecatedSimple = " Deprecated."
    renderDepr (DeprecatedPreferred t) = " Deprecated. Preferred value: " <> t <> "."
    renderPref Nothing = ""
    renderPref (Just x) = " Preferred value: " <> x <> "."
    conBody (x, (a, y, z, mpref)) =
      mconcat
        [ a,
          " -- ^ @",
          escapeHaddockChars x,
          "@. ",
          escapeHaddockChars $ renderDescrs y,
          escapeHaddockChars $ renderDepr z,
          escapeHaddockChars $ renderPref mpref
        ]
    theConstructors = case rs' of
      (x : xs) -> ("  = " <> conBody x) : fmap (\y -> "  | " <> conBody y) xs
      [] -> error "given empty registry!"
    theInstances = ["  deriving (Eq, Ord, Show, Enum, Bounded)"]
    theNFData =
      [ "instance NFData " <> tyname <> " where",
        "  rnf = rwhnf"
      ]
    theHashable =
      [ "instance Hashable " <> tyname <> " where",
        "  hashWithSalt = hashUsing fromEnum"
      ]

parseSubtag' :: Text -> Subtag
parseSubtag' t = case parseSubtag t of
  Nothing -> error $ T.unpack $ "couldn't parse subtag: " <> t
  Just a -> a

-- Render a subtag, assuming that it is well-formed
rendSubtag :: Text -> Text
rendSubtag x = T.pack $ "Subtag " <> show (unwrapSubtag $ parseSubtag' x)

-- | Write the various internal subtag modules.

-- N.B. if four-letter primary language subtags are standardized (and
-- we support the future standard using the same BCP47 modules and
-- types) then there could be a name collision between the Language
-- and Script modules. If that happens there will need to be special
-- casing to deal with it.

-- TODO: write resolveRef functions for each of the registry components

renderSplitRegistry :: Registry -> IO ()
renderSplitRegistry sr = do
  traverse_
    (rendwrite intprefix)
    [ ("Language.hs", rendlang),
      ("Extlang.hs", rendextlang),
      ("Script.hs", rendscript),
      ("Region.hs", rendregion),
      ("Variant.hs", rendvariant),
      ("Redundant.hs", rendredundant . redundantRecords),
      ("Date.hs", const regdatemodule),
      ("Grandfathered.hs", rendgrandfathered . grandfatheredRecords),
      ("LanguageRecords.hs", rendreclang),
      ("ExtlangRecords.hs", rendrecextlang),
      ("ScriptRecords.hs", rendrecscript),
      ("RegionRecords.hs", rendrecregion),
      ("VariantRecords.hs", rendrecvariant),
      ("GrandfatheredRecords.hs", rendrecgrandfathered),
      ("RedundantRecords.hs", rendrecredundant)
    ]
  where
    rendwrite p (x, y) = T.writeFile (p <> x) $ y sr
    intprefix = "../languagetags/src/Text/LanguageTag/Internal/BCP47/Registry/"
    regdatemodule =
      T.unlines
        [ warning,
          "",
          "module Text.LanguageTag.Internal.BCP47.Registry.Date where",
          "",
          "import Data.Time.Calendar (Day(..))",
          "",
          "-- | The date of the BCP47 subtag registry that this library uses. The current value is: "
            <> T.pack (show (date sr))
            <> ".",
          "bcp47RegistryDate :: Day",
          "bcp47RegistryDate = ModifiedJulianDay "
            <> T.pack (show $ toModifiedJulianDay $ date sr)
        ]

    rendlang = renderSubtagModuleWith
      "Language"
      "primary language"
      ""
      languageRecords
      $ \(LanguageRecord a x y _ _ _) -> (a, x, y, Nothing)
    rendextlang = renderSubtagModuleWith
      "Extlang"
      "extended language"
      "These are prefixed with \"Ext\" because they may overlap with primary language subtags. Note that if extended language subtags have a preferred value, then it refers to a primary subtag."
      extlangRecords
      $ \(ExtlangRecord a x y z _ _ _ _) ->
        ( a,
          x,
          if y then DeprecatedPreferred z else NotDeprecated,
          if y then Nothing else Just z
        )
    rendscript =
      renderSubtagModuleWith
        "Script"
        "script"
        ""
        scriptRecords
        $ \(ScriptRecord a x y) -> (a, x, y, Nothing)
    rendregion =
      renderSubtagModuleWith
        "Region"
        "region"
        "The names of region constructors come from the corresponding subtag, except that region subtags beginning with a number are prefixed with @Reg@."
        regionRecords
        $ \(RegionRecord a x y) -> (a, x, y, Nothing)
    rendvariant = renderSubtagModuleWith
      "Variant"
      "variant"
      "The names of region constructors come from the corresponding subtag, except that they are in title case and variant subtags beginning with a number are prefixed with @Var@."
      variantRecords
      $ \(VariantRecord a x y _) -> (a, x, y, Nothing)
    rendgrandfathered =
      renderModuleWith
        "Grandfathered"
        "grandfathered"
        ""
        (date sr)
        $ \(RangeRecord a x y) -> (a, x, y, Nothing)
    rendredundant =
      renderModuleWith
        "Redundant"
        "redundant"
        ""
        (date sr)
        $ \(RangeRecord a x y) -> (a, x, y, Nothing)

    parens x = "(" <> x <> ")"

    resolveRef m proj x = case M.lookup x m of
      Nothing -> error $ T.unpack $ "reference to subtag " <> x <> ", which doesn't exist"
      Just r -> proj r

    resolvePl reg = resolveRef (languageRecords reg) langTyCon
    resolveExt reg = resolveRef (extlangRecords reg) extlangTyCon
    resolveScr reg = resolveRef (scriptRecords reg) scriptTyCon . T.toTitle
    resolveReg reg = resolveRef (regionRecords reg) regionTyCon . T.toUpper
    resolveVar reg = resolveRef (variantRecords reg) variantTyCon

    resolveDepr _ _ NotDeprecated = "NotDeprecated"
    resolveDepr _ _ DeprecatedSimple = "DeprecatedSimple"
    resolveDepr m proj (DeprecatedPreferred y) =
      parens $
        "DeprecatedPreferred " <> resolveRef m proj y

    -- TODO: showPrec?
    mrender Nothing _ = "Nothing"
    mrender (Just x) f = parens $ "Just " <> f x

    mrender' x f = maybeSubtag "Nothing" (\s -> parens $ "Just " <> f s) x

    rendreclang = renderRecordModuleWith
      "Language"
      [ "import Text.LanguageTag.Internal.BCP47.Registry.Script"
      ]
      languageRecords
      $ \reg tg (LanguageRecord _ desc depr ssup ml sc) ->
        let rendRec =
              T.intercalate
                " "
                [ "LanguageRecord",
                  parens $ T.pack $ show desc,
                  resolveDepr (languageRecords reg) langTyCon depr,
                  mrender ssup $ resolveScr reg,
                  mrender ml $ resolvePl reg,
                  mrender sc $ T.pack . show
                ]
         in parens $ rendSubtag tg <> ", " <> rendRec
    rendrecextlang = renderRecordModuleWith
      "Extlang"
      [ "import Text.LanguageTag.Internal.BCP47.Registry.Language"
      ]
      extlangRecords
      $ \reg tg (ExtlangRecord _ desc depr prefer prefix ssup ml sc) ->
        let rendRec =
              T.intercalate
                " "
                [ "ExtlangRecord",
                  parens $ T.pack $ show desc,
                  T.pack $ show depr,
                  resolvePl reg prefer,
                  resolvePl reg prefix,
                  mrender ssup $ resolveScr reg,
                  mrender ml $ resolvePl reg,
                  mrender sc $ T.pack . show
                ]
         in parens $ rendSubtag tg <> ", " <> rendRec
    rendrecscript = renderRecordModuleWith
      "Script"
      []
      scriptRecords
      $ \reg tg (ScriptRecord _ desc depr) ->
        let rendRec =
              T.intercalate
                " "
                [ "ScriptRecord",
                  parens $ T.pack $ show desc,
                  resolveDepr (scriptRecords reg) scriptTyCon depr
                ]
         in parens $ rendSubtag tg <> ", " <> rendRec
    rendrecregion = renderRecordModuleWith
      "Region"
      []
      regionRecords
      $ \reg tg (RegionRecord _ desc depr) ->
        let rendRec =
              T.intercalate
                " "
                [ "RegionRecord",
                  parens $ T.pack $ show desc,
                  resolveDepr (regionRecords reg) regionTyCon depr
                ]
         in parens $ rendSubtag tg <> ", " <> rendRec

    showPrefs reg l = "[" <> T.intercalate "," (showTag reg <$> l) <> "]"
    showTag reg tag = case parseBCP47 tag of
      Right (NormalTag n) -> printNormalTag reg tag n
      _ -> error $ T.unpack $ "can't parse tag value " <> tag
    printNormalTag reg tag (Normal pl e1 e2 e3 sc regn vars exts pus)
      | not $ null exts && null pus && nullSubtag == e2 && nullSubtag == e3 =
        error $ T.unpack $ "registry tag " <> tag <> " somehow has extensions or private use fields or more than one extended language"
      | otherwise =
        T.intercalate
          " "
          [ "Normal",
            resolvePl reg $ renderSubtagLower pl,
            mrender' e1 (resolveExt reg . renderSubtagLower),
            mrender' sc (resolveScr reg . renderSubtagLower),
            mrender' regn (resolveReg reg . renderSubtagLower),
            "(S.fromList [" <> T.intercalate ", " (resolveVar reg . renderSubtagLower <$> vars) <> "])",
            "M.empty",
            "[]"
          ]

    variantImports =
      tagImports
        <> [ "import Text.LanguageTag.Internal.BCP47.Registry.Language",
             "import Text.LanguageTag.Internal.BCP47.Registry.Script",
             "import Text.LanguageTag.Internal.BCP47.Registry.Region"
           ]

    rendrecvariant = renderRecordModuleWith
      "Variant"
      variantImports
      variantRecords
      $ \reg tg (VariantRecord _ desc depr prefs) ->
        let rendRec =
              T.intercalate
                " "
                [ "VariantRecord",
                  parens $ T.pack $ show desc,
                  resolveDepr (variantRecords reg) variantTyCon depr,
                  showPrefs reg prefs
                ]
         in parens $ rendSubtag tg <> ", " <> rendRec

    resolveDeprGrand _ NotDeprecated = "NotDeprecated"
    resolveDeprGrand _ DeprecatedSimple = "DeprecatedSimple"
    resolveDeprGrand reg (DeprecatedPreferred x) = case x of
      "en-GB-oxendict" -> parens "DeprecatedPreferred $ Normal En Nothing Nothing (Just GB) (S.singleton Oxendict) M.empty []"
      _ ->
        parens $
          "DeprecatedPreferred $ Normal "
            <> resolvePl reg x
            <> " Nothing Nothing Nothing S.empty M.empty []"

    resolveDeprRedundant _ NotDeprecated = "NotDeprecated"
    resolveDeprRedundant _ DeprecatedSimple = "DeprecatedSimple"
    resolveDeprRedundant reg (DeprecatedPreferred x) = case x of
      "cmn-Hans" -> parens "DeprecatedPreferred $ Normal Cmn Nothing (Just Hans) Nothing S.empty M.empty []"
      "cmn-Hant" -> parens "DeprecatedPreferred $ Normal Cmn Nothing (Just Hant) Nothing S.empty M.empty []"
      _ ->
        parens $
          "DeprecatedPreferred $ Normal "
            <> resolvePl reg x
            <> " Nothing Nothing Nothing S.empty M.empty []"

    tagImports =
      [ "import qualified Data.Map.Strict as M",
        "import qualified Data.Set as S"
      ]

    prefixedImports =
      tagImports
        <> [ "import Text.LanguageTag.Internal.BCP47.Registry.Language",
             "import Text.LanguageTag.Internal.BCP47.Registry.Region",
             "import Text.LanguageTag.Internal.BCP47.Registry.Variant"
           ]

    rendrecgrandfathered = renderGrandfatheredRecordModule
      "Grandfathered"
      prefixedImports
      grandfatheredRecords
      $ \reg _ (RangeRecord _ desc depr) ->
        let rendRec =
              T.intercalate
                " "
                [ "RangeRecord",
                  parens $ T.pack $ show desc,
                  resolveDeprGrand reg depr
                ]
         in rendRec

    showSynTag reg tag = case parseBCP47 tag of
      Right (NormalTag n) -> printSyn reg tag n
      _ -> error $ T.unpack $ "can't parse tag value " <> tag
    printSyn reg tag (Normal pl e1 e2 e3 sc regn vars exts pus)
      | not $ null exts && null pus && nullSubtag == e2 && nullSubtag == e3 =
        error $ T.unpack $ "registry tag " <> tag <> " somehow has extensions or private use fields or more than one extended language"
      | otherwise =
        T.intercalate
          " "
          [ "Syn.Normal",
            resolvePl' pl,
            msrender e1 resolveExt',
            "nullSubtag",
            "nullSubtag",
            msrender sc resolveScr',
            msrender regn resolveReg',
            "[" <> T.intercalate ", " (resolveVar' <$> vars) <> "]",
            "[]",
            "[]"
          ]
      where
        showSubtag x = "Subtag " <> T.pack (show $ unwrapSubtag x)
        msrender x f = maybeSubtag "nullSubtag" (\s -> parens $ "justSubtag " <> f s) x
        -- FIXME: horrifying
        resolve' f x = f reg (renderSubtagLower x) `seq` parens (showSubtag x)
        resolvePl' = resolve' resolvePl
        resolveExt' = resolve' resolveExt
        resolveScr' = resolve' resolveScr
        resolveReg' = resolve' resolveReg
        resolveVar' x = resolveVar reg (renderSubtagLower x) `seq` showSubtag x

    redundantImports =
      tagImports
        <> [ "import Text.LanguageTag.BCP47.Subtag (nullSubtag, justSubtag)",
             "import Text.LanguageTag.Internal.BCP47.Registry.Script",
             "import Text.LanguageTag.Internal.BCP47.Registry.Language",
             "import Text.LanguageTag.Internal.BCP47.Registry.Region",
             "import Text.LanguageTag.Internal.BCP47.Registry.Extlang",
             "import Text.LanguageTag.Internal.BCP47.Registry.Variant",
             "import Text.LanguageTag.Internal.BCP47.Subtag (Subtag(..))"
           ]
    rendrecredundant = renderRangeRecordModuleWith "Redundant" redundantImports redundantRecords $
      \reg tag (RangeRecord _ desc depr) ->
        let rendRec =
              T.intercalate
                " "
                [ "RangeRecord",
                  parens $ T.pack $ show desc,
                  resolveDeprRedundant reg depr
                ]
         in parens $ showTag reg tag <> ", " <> showSynTag reg tag <> ", " <> rendRec

----------------------------------------------------------------
-- Testing functions
----------------------------------------------------------------

renderRecord :: TagRecord -> [Text]
renderRecord (TagRecord t tt descs dep) = case tt of
  Language mscr mmacro mscope ->
    ["Type: language", "Subtag: " <> t] <> renderDescs <> renderDep
      <> renderSuppress mscr
      <> renderMacro mmacro
      <> renderScope mscope
  Extlang mpref pfx mscr mmacro mscope ->
    ["Type: extlang", "Subtag: " <> t] <> renderDescs <> renderDep <> renderPreferred mpref
      <> ["Prefix: " <> pfx]
      <> renderSuppress mscr
      <> renderMacro mmacro
      <> renderScope mscope
  Script -> ["Type: script", "Subtag: " <> t] <> renderDescs <> renderDep
  Region -> ["Type: region", "Subtag: " <> t] <> renderDescs <> renderDep
  Variant vs ->
    ["Type: variant", "Subtag: " <> t] <> renderDescs <> renderDep
      <> fmap ("Prefix: " <>) vs
  Grandfathered -> ["Type: grandfathered", "Tag: " <> t] <> renderDescs <> renderDep
  Redundant -> ["Type: redundant", "Tag: " <> t] <> renderDescs <> renderDep
  where
    renderPreferred Nothing = []
    renderPreferred (Just x) = ["Preferred-Value: " <> x]
    takeFirstLine = fst . T.span (/= '\n')
    renderSuppress Nothing = []
    renderSuppress (Just x) = ["Suppress-Script: " <> x]
    renderMacro Nothing = []
    renderMacro (Just x) = ["Macrolanguage: " <> x]
    renderDep = case dep of
      NotDeprecated -> []
      DeprecatedSimple -> ["Deprecated"]
      DeprecatedPreferred v -> ["Deprecated", "Preferred-Value: " <> v]
    renderDescs = toList $ fmap (\d -> takeFirstLine $ "Description: " <> d) descs
    renderScope (Just Macrolanguage) = ["Scope: macrolanguage"]
    renderScope (Just Collection) = ["Scope: collection"]
    renderScope (Just Special) = ["Scope: special"]
    renderScope (Just PrivateUseScope) = ["Scope: private-use"]
    renderScope Nothing = []

mParsed :: Text -> Maybe Text
mParsed t
  | Just _ <- T.stripPrefix "Comments" t =
    Nothing
  | Just _ <- T.stripPrefix "Added" t =
    Nothing
  | Just (c, _) <- T.uncons t,
    isSpace c =
    Nothing
  | t == "%%" =
    Nothing
  | Just _ <- T.stripPrefix "Deprecated" t =
    Just "Deprecated"
  | otherwise = Just t

rerenderRegistryFile :: Text -> [Text]
rerenderRegistryFile = mapMaybe mParsed . T.lines

renderRegistry :: RawRegistry -> [Text]
renderRegistry (RawRegistry rdate rs) =
  ("File-Date: " <> T.pack (show rdate)) :
  concatMap renderRecord rs

-- The idea is that the rendered parsed registry should be equal to
-- the original file (up to some necessary stripping). Here, that
-- means that firstDiff applied to those two things should be Right [].
firstDiff :: [Text] -> [Text] -> Either (Either [Text] [Text]) [(Text, Text)]
firstDiff (x : xs) (y : ys)
  | x == y = firstDiff xs ys
  | otherwise = Right $ (x, y) : take 10 (zip xs ys)
firstDiff (x : xs) [] = Left $ Left $ take 10 (x : xs)
firstDiff [] (x : xs) = Left $ Right $ take 10 (x : xs)
firstDiff [] [] = Right []