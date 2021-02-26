{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Fetch code registries, generate code
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
import Data.Char (isDigit, isSpace)
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

{-
TODO:

Tests of:

- parseRawRecord, at least on select tags of different types

- parseRegistry - easy test is that there must be as many registry tag
  records as there are '%%' lines. another: strip out all the lines
  that start with %% or whitespace, and strip the date, and strip any
  fields we don't parse; the number of resulting lines must be equal
  to a certain (sum . fmap weight) over the record list.

Might also like to diff the parsed registry against the old one, for
linting purposes (e.g. to verify that we haven't registered changes in
(sub)tag information that should never be changed).

Should move this to a separate package (making this a monorepo).

-}

main :: IO ()
main = do
  (u, r) <- readLocalRegistry
  unless (null u) $ do
    putStrLn "Unrecognized BCP47 registry tag fields:"
    print u
  putStrLn "writing the internal modules"
  renderSplitRegistry $ splitRegistry r

--  writeLanguageModule r

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

data Scope
  = Macrolanguage
  | Collection
  | Special
  | PrivateUse
  deriving (Show)

data Deprecation
  = NotDeprecated
  | DeprecatedSimple
  | DeprecatedPreferred Text
  deriving (Show)

-- | A raw record with the fields: tag or subtag, tag type,
-- descriptions, deprecation, preferred value, prefixes, script
-- suppression, macrolanguage, scope
data TagRecord
  = TagRecord
      Text
      TagType
      [Text]
      Deprecation
  deriving (Show)

-- The registry with the date and the raw tag information.
data Registry = Registry Day [TagRecord]

lookupInRegistry :: Text -> Registry -> Maybe TagRecord
lookupInRegistry t (Registry _ rs) = List.find (\(TagRecord x _ _ _) -> x == t) rs

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
parseRegistry :: [Jar] -> Either Err ([(LineNum, Text)], Registry)
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
      Right (unexpecteds [], Registry rdate $ recs [])
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
      descriptions <- fmap snd <$> mfield j "Description"
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
          "private-use" -> pure $ Just PrivateUse
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
parseRegistryThrow :: Text -> IO ([(LineNum, Text)], Registry)
parseRegistryThrow inp = do
  let jarErr (lineNum, t) =
        fail $
          "couldn't parse tag "
            <> T.unpack t
            <> " at line "
            <> show lineNum
  let withLine n t = "at line " <> show n <> "\n" <> t
  let tagErr (ErrUnknownType n) = fail $ withLine n $ "unknown type"
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

readLocalRegistry :: IO ([(LineNum, Text)], Registry)
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

data SplitRegistry = SplitRegistry
  { date :: Day,
    -- | optional script suppression, macrolanguage, scope
    languages ::
      Map Text ([Text], Deprecation, Maybe Text, Maybe Text, Maybe Scope),
    -- | optional preferred value without a deprecation notice,
    -- mandatory prefix, optional script suppression, optional
    -- macrolanguage, optional scope
    extlangs ::
      Map Text ([Text], Deprecation, Maybe Text, Text, Maybe Text, Maybe Text, Maybe Scope),
    scripts :: Map Text ([Text], Deprecation),
    regions :: Map Text ([Text], Deprecation),
    -- | Optional prefix values
    variants :: Map Text ([Text], Deprecation, [Text]),
    grandfathered :: Map Text ([Text], Deprecation),
    redundant :: Map Text ([Text], Deprecation)
  }

lookupSplit :: (SplitRegistry -> Map Text a) -> SplitRegistry -> Text -> Maybe a
lookupSplit proj r t = M.lookup t $ proj r

splitRegistry :: Registry -> SplitRegistry
splitRegistry (Registry regdate rs) =
  SplitRegistry
    { date = regdate,
      languages = go plang,
      extlangs = go pextlang,
      scripts = go pscr,
      regions = go preg,
      variants = go pvar,
      grandfathered = go pgra,
      redundant = go prdn
    }
  where
    go proj = M.fromList $ mapMaybe proj $ unpackRegistryRanges rs
    plang (TagRecord tg (Language x y z) descrs deprs) =
      Just (tg, (descrs, deprs, x, y, z))
    plang _ = Nothing
    pextlang (TagRecord tg (Extlang a b c d e) descrs deprs) =
      Just (tg, (descrs, deprs, a, b, c, d, e))
    pextlang _ = Nothing
    pscr (TagRecord tg Script descrs deprs) =
      Just (tg, (descrs, deprs))
    pscr _ = Nothing
    preg (TagRecord tg Region descrs deprs) =
      Just (tg, (descrs, deprs))
    preg _ = Nothing
    pvar (TagRecord tg (Variant l) descrs deprs) =
      Just (tg, (descrs, deprs, l))
    pvar _ = Nothing
    pgra (TagRecord tg Grandfathered descrs deprs) =
      Just (tg, (descrs, deprs))
    pgra _ = Nothing
    prdn (TagRecord tg Redundant descrs deprs) =
      Just (tg, (descrs, deprs))
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

-- | Render an internal subtag module.
renderModuleWith ::
  -- | the desired type name
  Text ->
  -- | a prefix to add to the data constructors
  Text ->
  -- | a description of the type
  Text ->
  -- | an additional note in the documentation
  Text ->
  -- | a transformation to apply to the data constructors at the end
  (Text -> Text) ->
  -- | the date of the registry that was used
  Day ->
  -- | projection returning the description, deprecation and optional
  -- preferred value without deprecation (for extlang only,
  -- essentially)
  (a -> ([Text], Deprecation, Maybe Text)) ->
  -- | the actual subtag type registry
  Map Text a ->
  Text
renderModuleWith tyname typref tydescription docnote contrans d sel rs =
  T.unlines $
    [ warning,
      "",
      "module Text.LanguageTag.Internal.BCP47." <> tyname <> " where",
      "",
      "import Control.DeepSeq (NFData(..))",
      "import Data.Hashable (Hashable(..), hashUsing)",
      "",
      "-- | The BCP47 " <> tydescription <> " tags as of " <> T.pack (show d) <> "." <> docnote',
      "data " <> tyname
    ]
      <> theConstructors
      <> [""]
      <> theInstances
      <> [""]
      <> theNFData
      <> [""]
      <> theHashable
  where
    docnote'
      | T.null docnote = ""
      | otherwise = " " <> docnote
    rs' = M.toAscList $ M.map sel rs
    renderDescrs descrs =
      "Description: " <> T.intercalate "; " (renderDescr <$> descrs) <> "."
    renderDescr = T.intercalate " " . T.words
    renderDepr NotDeprecated = ""
    renderDepr DeprecatedSimple = " Deprecated."
    renderDepr (DeprecatedPreferred t) = " Deprecated. Preferred value: " <> t <> "."
    renderPref Nothing = ""
    renderPref (Just x) = " Preferred value: " <> x <> "."
    renderTyCon = (typref <>) . mconcat . renderTyPieces . T.split (== '-')
    renderTyPieces (x : xs)
      | isDigit (T.head x) = [T.singleton (T.head tyname), T.toLower x] <> renderTyTail xs
      | otherwise = [T.toTitle x] <> renderTyTail xs
    renderTyPieces [] = error "Gen.renderTyPieces: empty tag encountered"
    renderTyTail = fmap T.toTitle
    conBody (x, (y, z, mpref)) =
      mconcat
        [ contrans $ renderTyCon x,
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
        "  rnf a = seq a ()"
      ]
    theHashable =
      [ "instance Hashable " <> tyname <> " where",
        "  hashWithSalt = hashUsing fromEnum"
      ]

-- | Write the various internal subtag modules.

-- N.B. if four-letter primary language subtags are standardized (and
-- we support the future standard using the same BCP47 modules and
-- types) then there could be a name collision between the Language
-- and Script modules. If that happens there will need to be special
-- casing to deal with it.
renderSplitRegistry :: SplitRegistry -> IO ()
renderSplitRegistry sr = do
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Language.hs" $
    rendlang $ languages sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Extlang.hs" $
    rendextlang $ extlangs sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Script.hs" $
    rendscript $ scripts sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Region.hs" $
    rendregion $ regions sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Variant.hs" $
    rendvariant $ variants sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Grandfathered.hs" $
    rendgrandfathered $ grandfathered sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/Redundant.hs" $
    rendredundant $ redundant sr
  T.writeFile "./src/Text/LanguageTag/Internal/BCP47/RegistryDate.hs" $
    T.unlines
      [ warning,
        "",
        "module Text.LanguageTag.Internal.BCP47.RegistryDate where",
        "",
        "import Data.Time.Calendar (Day(..))",
        "",
        "-- | The date of the BCP47 subtag registry that this library uses. The current value is: " <> T.pack (show (date sr)) <> ".",
        "bcp47RegistryDate :: Day",
        "bcp47RegistryDate = ModifiedJulianDay "
          <> T.pack (show $ toModifiedJulianDay $ date sr)
      ]
  where
    addNothing (x, y) = (x, y, Nothing)
    rendlang = renderModuleWith "Language" "" "primary language" "" id (date sr) $
      \(x, y, _, _, _) -> (x, y, Nothing)
    rendextlang = renderModuleWith
      "Extlang"
      "Ext"
      "extended language"
      "These are prefixed with \"Ext\" because they may overlap with primary language subtags. Note that if extended language subtags have a preferred value, then it refers to a primary subtag."
      id
      (date sr)
      $ \(x, y, z, _, _, _, _) -> (x, y, z)
    rendscript = renderModuleWith "Script" "" "script" "" id (date sr) addNothing
    rendregion = renderModuleWith "Region" "" "region" "" T.toUpper (date sr) addNothing
    rendvariant = renderModuleWith "Variant" "" "variant" "" id (date sr) $
      \(x, y, _) -> (x, y, Nothing)
    rendgrandfathered =
      renderModuleWith
        "Grandfathered"
        ""
        "grandfathered"
        ""
        id
        (date sr)
        addNothing
    rendredundant =
      renderModuleWith
        "Redundant"
        ""
        "redundant"
        ""
        id
        (date sr)
        addNothing

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
      <> fmap (\x -> "Prefix: " <> x) vs
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
    renderDescs = fmap (\d -> takeFirstLine $ "Description: " <> d) descs
    renderScope (Just Macrolanguage) = ["Scope: macrolanguage"]
    renderScope (Just Collection) = ["Scope: collection"]
    renderScope (Just Special) = ["Scope: special"]
    renderScope (Just PrivateUse) = ["Scope: private-use"]
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

renderRegistry :: Registry -> [Text]
renderRegistry (Registry rdate rs) =
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
