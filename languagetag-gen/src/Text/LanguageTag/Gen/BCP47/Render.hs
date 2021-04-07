{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Rendering internal BCP47 modules
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- The functions in this module generate the text of the internal
-- BCP47 modules that are generated from the registry data parsed by
-- "Text.LanguageTag.Gen.BCP47.Parse"
module Text.LanguageTag.Gen.BCP47.Render (renderSplitRegistry) where

import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (Day (..))
import Text.LanguageTag.BCP47.Registry (Deprecation (..))
import Text.LanguageTag.BCP47.Subtag
  ( maybeSubtag,
    nullSubtag,
    renderSubtagLower,
    unwrapSubtag,
  )
import Text.LanguageTag.BCP47.Syntax (parseBCP47)
import Text.LanguageTag.Gen.BCP47.Common
import Text.LanguageTag.Gen.BCP47.Parse
import Text.LanguageTag.Internal.BCP47.Syntax (BCP47 (NormalTag), Normal (..))

{- TODO:

fix the horrifying duplication in the module rendering functions

-}

-- | A comment warning people not to edit the file
warning :: Text
warning = "-- This is an auto-generated file. Do not edit by hand."

-- | Generate the documentation of a module given a short module
-- description and the lines of a longer module description
moduleDoc :: Text -> [Text] -> [Text]
moduleDoc shortdesc desc =
  [ "-- |",
    "-- Description : " <> shortdesc,
    "-- Copyright   : 2021 Christian Despres",
    "-- License     : BSD-2-Clause",
    "-- Maintainer  : Christian Despres",
    "--"
  ]
    <> (go <$> desc)
  where
    go t
      | T.null t = "--"
      | otherwise = "-- " <> t

-- | Escape all of the special haddock characters in the given string
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
      "{-# OPTIONS_HADDOCK not-home #-}",
      "",
      "module Text.LanguageTag.Internal.BCP47.Registry." <> tyname <> " where",
      "",
      "import Prelude hiding (LT, GT)",
      "import Control.DeepSeq (NFData(..), rwhnf)",
      "import Data.Hashable (Hashable(..), hashUsing)"
    ]
      <> [ "",
           "-- | The BCP47 " <> tydescription <> " subtags as of " <> T.pack (show $ date reg) <> "." <> docnote',
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

-- | Render a subtag record module
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
    moduleDoc
      (tyname <> " record definitions")
      [ "Warning\\: this is an internal module and may change or disappear",
        "without regard to the PVP.",
        "",
        "Internal definitions for the records in the registry for '" <> tyname <> "' subtags."
      ]
      <> [ "",
           warning,
           "",
           "{-# LANGUAGE NoImplicitPrelude #-}",
           "{-# LANGUAGE OverloadedStrings #-}",
           "{-# OPTIONS_HADDOCK not-home #-}",
           "",
           "module Text.LanguageTag.Internal.BCP47.Registry." <> tyname <> "Records",
           "  ("
             <> T.intercalate
               ", "
               [ lookupname1,
                 lookupname2,
                 lookupname3,
                 lookupname4,
                 detailTableName
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
      [ "-- | All of the record information associated to '" <> tyname <> "' subtags, together with their corresponding 'Subtag's, occurring in the same order as that type's constructors",
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
        lookupname2 <> " = fmap toEnum . flip (unsafeBinSearchIndexOn fst) " <> detailTableName
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

-- | Render a redundant tag record module
renderRedundantRecordModule ::
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
renderRedundantRecordModule tyname imps proj rend reg =
  T.unlines $
    moduleDoc
      (tyname <> " record definitions")
      [ "Warning\\: this is an internal module and may change or disappear",
        "without regard to the PVP.",
        "",
        "Internal definitions for the records in the registry for '" <> tyname <> "' tags"
      ]
      <> [ "",
           warning,
           "",
           "{-# LANGUAGE NoImplicitPrelude #-}",
           "{-# LANGUAGE OverloadedStrings #-}",
           "{-# OPTIONS_HADDOCK not-home #-}",
           "",
           "module Text.LanguageTag.Internal.BCP47.Registry." <> tyname <> "Records",
           "  ("
             <> T.intercalate
               ", "
               [ lookupname1,
                 lookupname2,
                 lookupname3,
                 lookupname4,
                 detailTableName
               ]
             <> ") where",
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
        lookupname1 <> " = V.unsafeIndex " <> detailTableName <> " . fromEnum"
      ]
    lookupname2 = T.toLower tyname <> "ToValidNormal"
    lookup2 =
      [ "-- | Convert a '" <> tyname <> "' tag to a 'Normal' validated tag",
        lookupname2 <> " :: " <> tyname <> " -> Normal",
        lookupname2 <> " = (\\(x, _, _) -> x) . " <> lookupname1
      ]
    lookupname3 = T.toLower tyname <> "ToSyntaxNormal"
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
      [ "-- | All of the records for '" <> tyname <> "' tags, together with their corresponding valid and well-formed forms, occurring in the same order as that type's constructors",
        detailTableName <> " :: Vector (Normal, Syn.Normal, RangeRecord)",
        detailTableName <> " = V.fromList"
      ]
        <> tableEntries

-- | Render the grandfathered tag record module
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
    moduleDoc
      ("Internal " <> tyname <> " records")
      [ "Warning\\: this is an internal module and may change or disappear",
        "without regard to the PVP.",
        "",
        "Internal definitions for the records in the registry for '" <> tyname <> "' tags"
      ]
      <> [ "",
           warning,
           "",
           "{-# LANGUAGE NoImplicitPrelude #-}",
           "{-# LANGUAGE OverloadedStrings #-}",
           "{-# OPTIONS_HADDOCK not-home #-}",
           "",
           "module Text.LanguageTag.Internal.BCP47.Registry.GrandfatheredRecords",
           "  (lookupGrandfatheredRecord, grandfatheredDetails) where",
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
      [ "-- | Look up the record details associated to the given 'Grandfathered' tag.",
        lookupname1 <> " :: " <> tyname <> " -> RangeRecord",
        lookupname1 <> " = V.unsafeIndex " <> detailTableName <> " . fromEnum"
      ]
    detailsTable =
      [ "-- | All of the records for '" <> tyname <> "' tags, occurring in the same order as that type's constructors",
        detailTableName <> " :: Vector RangeRecord",
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
      "{-# OPTIONS_HADDOCK not-home #-}",
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

-- | Write the various internal subtag modules.

-- N.B. if four-letter primary language subtags are standardized (and
-- we support the future standard using the same BCP47 modules and
-- types) then there could be a name collision between the Language
-- and Script modules. If that happens there will need to be special
-- casing to deal with it.

renderSplitRegistry :: FilePath -> Registry -> IO ()
renderSplitRegistry pref sr = do
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
    intprefix = pref <> "/src/Text/LanguageTag/Internal/BCP47/Registry/"
    regdatemodule =
      T.unlines
        [ warning,
          "",
          "module Text.LanguageTag.Internal.BCP47.Registry.Date where",
          "",
          "import Data.Time.Calendar (Day(..))",
          "",
          "-- | The date of the BCP47 registry that this library uses. The current value is: "
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
      "These are prefixed with \\\"Ext\\\" because they would otherwise overlap with their corresponding primary language subtags. Note that the preferred values of these subtags refer to primary language subtags."
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
        "The names of region constructors come from the corresponding subtag, except that subtags beginning with a number are prefixed with @Reg@."
        regionRecords
        $ \(RegionRecord a x y) -> (a, x, y, Nothing)
    rendvariant = renderSubtagModuleWith
      "Variant"
      "variant"
      "The names of variant constructors come from the corresponding subtag, except that they are in title case and subtags beginning with a number are prefixed with @Var@."
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
        "The names of redundant constructors are derived from the corresponding tag by converting the subtags to title case and then removing the intermediate dashes."
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
        -- FIXME: should not be necessary - the type system will
        -- complain about unregistered tags on its own
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
    rendrecredundant = renderRedundantRecordModule "Redundant" redundantImports redundantRecords $
      \reg tag (RangeRecord _ desc depr) ->
        let rendRec =
              T.intercalate
                " "
                [ "RangeRecord",
                  parens $ T.pack $ show desc,
                  resolveDeprRedundant reg depr
                ]
         in parens $ showTag reg tag <> ", " <> showSynTag reg tag <> ", " <> rendRec
