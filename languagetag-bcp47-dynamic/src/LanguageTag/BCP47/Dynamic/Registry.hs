{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Dynamic registry handling
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module LanguageTag.BCP47.Dynamic.Registry where

import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Word (Word8)
import qualified LanguageTag.BCP47.Dynamic.RecordJar as RJ
import LanguageTag.BCP47.LegacyTag (Grandfathered (..), Redundant (..), recognizeRedundant)
import LanguageTag.BCP47.Subtag (IsSubtag (..), Subtag, ToSubtags (..), parseSubtagText)
import qualified LanguageTag.BCP47.Subtag as Sub
import LanguageTag.BCP47.Syntax (parseBCP47)
import LanguageTag.BCP47.Syntax.Language
import LanguageTag.BCP47.Syntax.Region
import LanguageTag.BCP47.Syntax.Script
import LanguageTag.BCP47.Syntax.Variant
import LanguageTag.Internal.BCP47.Subtag (SubtagChar (..), unsafeSubtagPackLen)
import LanguageTag.Internal.BCP47.Syntax (BCP47 (..))

{- TODO

- note that some of this needs to be available in the static package (scope,
  deprecation)

- maybe put a block somewhere talking about the descriptions and comments,
  factoring that out of the type descriptions themselves (i.e. may note that the
  registry does not guarantee the descriptions will be in any particular
  language or script, but maybe note that in practice they tend to be in a Latin
  script, and the first one tends to be an English name for them), maybe also
  noting that the text values adhere to the usual newline-only convention of
  Text unlike what we expect of the registry file

- pull out most of the where clause stuff in the registry parser into top-level,
  documented functions

- qualify exports

- make sure we have tests that the current registry has no long language
  subtags, and tests that all of the tags in the registry contain no extension
  or private use sections (or see if the standard forbids that sort of thing,
  which it should, I think)

- maybe make the registry type opaque (in an internal module) and just provide
  lookup functions? since we will be checking that grandfathered and redundant
  tags appear exactly once in the registry we can have those lookup functions be
  total. could switch to vectors for the registry at the same time.

-}

-- | The scope of a language or extended language. If this is not
-- present in a record then the language is an "individual language",
-- i.e., what one would normally consider to be a language.
data Scope
  = -- | a cluster of closely related languages that are sometimes
    -- considered to be a single language
    Macrolanguage
  | -- | a collection of languages usually related by history or
    -- geography; a looser relationship than a 'Macrolanguage'
    Collection
  | -- | a subtag identifying something not particularly associated
    -- with a concrete language
    Special
  | -- | a subtag reserved for private use
    PrivateUseScope
  deriving (Eq, Ord, Show)

-- | The deprecation status of a subtag, including the date the record was
-- deprecated. Note that this date may in some cases be earlier than the date on
-- which the record was added to the registry.
data Deprecation a
  = -- | not deprecated
    NotDeprecated
  | -- | deprecated but without a preferred value
    DeprecatedSimple Day
  | -- | deprecated with a preferred value
    DeprecatedPreferred Day a
  deriving (Eq, Ord, Show, Functor)

-- | A primary language subtag record. Note that the descriptions are
-- not guaranteed by the registry to be in any particular language or
-- script.
data LanguageRecord = LanguageRecord
  { languageDateAdded :: Day,
    languageDescription :: NonEmpty Text,
    languageDeprecation :: Deprecation Language,
    languageScriptSuppression :: Maybe Script,
    languageMacrolanguage :: Maybe Language,
    languageScope :: Maybe Scope,
    languageComments :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | An extended language subtag record. In these records, a preferred value
-- always appears and is always equal to the subtag, so the 'extlangDeprecation'
-- is a simple 'Bool' ('True' meaning "is deprecated"). Note also that these
-- records are the only exception to the general rule that a preferred value
-- (whether tag or subtag) will never be deprecated.
--
-- The descriptions are not guaranteed by the registry to be in any
-- particular language or script.
data ExtlangRecord = ExtlangRecord
  { extlangDateAdded :: Day,
    extlangDescription :: NonEmpty Text,
    extlangDeprecation :: Maybe Day,
    extlangPrefix :: Language,
    extlangScriptSuppression :: Maybe Script,
    extlangMacrolanguage :: Maybe Language,
    extlangScope :: Maybe Scope,
    extlangComments :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | A variant subtag record. Note that the descriptions are not
-- guaranteed by the registry to be in any particular language or
-- script.
data VariantRecord = VariantRecord
  { variantDateAdded :: Day,
    variantDescription :: NonEmpty Text,
    variantDeprecation :: Deprecation Variant,
    variantPrefixes :: [BCP47],
    variantComments :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | A script subtag record. Note that the descriptions are not
-- guaranteed by the registry to be in any particular language or
-- script.
data ScriptRecord = ScriptRecord
  { scriptDateAdded :: Day,
    scriptDescription :: NonEmpty Text,
    scriptDeprecation :: Deprecation Script,
    scriptComments :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | A region subtag record. Note that for deprecated region records,
--  the associated preferred value may not have exactly the same
--  meaning as the deprecated subtag. Note also that the descriptions
--  are not guaranteed by the registry to be in any particular
--  language or script.
data RegionRecord = RegionRecord
  { regionDateAdded :: Day,
    regionDescription :: NonEmpty Text,
    regionDeprecation :: Deprecation Region,
    regionComments :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | A grandfathered or redundant subtag record. These records are
-- distinguished from the others in that they define entire tags, and
-- that the preferred values associated to their deprecation are an
-- "extended language range", which is an entire tag that is strongly
-- recommended as the replacement for the tag.
--
-- The descriptions are not guaranteed by the registry to be in any
-- particular language or script.
data TagRecord = TagRecord
  { tagDateAdded :: Day,
    tagDescription :: NonEmpty Text,
    tagDeprecation :: Deprecation BCP47,
    tagComments :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | An error that may occur when parsing a 'Registry' from an unstructured
-- 'RJ.JarRegistry'. If the 'RJ.JarRegistry' being parsed was originally read
-- from a file from the official registry, it is unlikely that any of these will
-- occur in practice.
data ParseError
  = -- | the field should appear at least once and didn't
    FieldNotPresent RJ.LineNum Text
  | -- | the field should not appear more than once and did
    FieldPresentMultipleTimes RJ.LineNum Text
  | -- | a field name was present that was unrecognized or invalid for the
    -- record's type (records record line number and field line number)
    FieldUnrecognized RJ.LineNum RJ.LineNum Text
  | -- | the value of the Type field was not recognized
    InvalidType RJ.LineNum Text
  | -- | the date at the top of the file was ill-formed
    BadDate RJ.LineNum Text
  | -- | a subtag in the file was not well-formed
    UnparsableSubtag RJ.LineNum (Sub.ParseError Char)
  | -- | a subtag in the file was not valid for the record type or field
    BadSubtag RJ.LineNum Subtag
  | -- | a record had a Preferred-Value without a corresponding Deprecated and
    -- wasn't an extlang record
    PreferredWithoutDeprecation RJ.LineNum
  | -- | the scope of a language record was invalid
    BadScope RJ.LineNum Text
  | -- | a record contained an ill-formed @Subtag@ field value
    BadSubtagField RJ.LineNum Text
  | -- | two records for the same tag or subtag were encountered
    DuplicateRecord DuplicateRecordKey
  | -- | an extlang did not have a preferred value field equal to itself
    ExtlangLangMismatch RJ.LineNum
  | -- | a tag could not be parsed
    BadTag RJ.LineNum Text
  | -- | the tag in a grandfathered record was not recognized
    TagNotGrandfathered RJ.LineNum BCP47
  | -- | the tag in a redundant record was not recognized
    TagNotRedundant RJ.LineNum BCP47
  deriving (Eq, Ord, Show)

data DuplicateRecordKey
  = DuplicateSubtag Subtag
  | DuplicateRedundant Redundant
  | DuplicateGrandfathered Grandfathered
  deriving (Eq, Ord, Show)

-- | A fully-parsed and structured subtag registry
data Registry = Registry
  { registryDate :: !Day,
    registryLanguages :: !(Map Language LanguageRecord),
    registryExtlangs :: !(Map Extlang ExtlangRecord),
    registryScripts :: !(Map Script ScriptRecord),
    registryRegions :: !(Map Region RegionRecord),
    registryVariants :: !(Map Variant VariantRecord),
    registryGrandfatheredTags :: !(Map Grandfathered TagRecord),
    registryRedundantTags :: !(Map Redundant TagRecord)
  }
  deriving (Eq, Ord, Show)

----------------------------------------------------------------
-- Jar parser combinators
----------------------------------------------------------------

-- | A slightly laxer version of 'RJ.JarRecord' that may be empty. Useful for
-- the incremental parsing that's done in this module.
data JarRecord = JarRecord
  { recordLineNum :: RJ.LineNum,
    recordBody :: [RJ.JarField]
  }
  deriving (Eq, Ord, Show)

toJarRecord :: RJ.JarRecord -> JarRecord
toJarRecord
  RJ.JarRecord
    { RJ.recordLineNum = n,
      RJ.recordBody = b
    } =
    JarRecord
      { recordLineNum = n,
        recordBody = NE.toList b
      }

newtype RecordM a = RecordM
  {unRecordM :: JarRecord -> Either ParseError (JarRecord, a)}

instance Functor RecordM where
  fmap f (RecordM act) = RecordM $ \jr -> case act jr of
    Left e -> Left e
    Right (jr', a) -> Right (jr', f a)

instance Applicative RecordM where
  pure x = RecordM $ \jr -> Right (jr, x)
  RecordM mf <*> RecordM mx = RecordM $ \jr -> case mf jr of
    Left e -> Left e
    Right (jr', f) -> case mx jr' of
      Left e -> Left e
      Right (jr'', x) -> Right (jr'', f x)

instance Monad RecordM where
  RecordM mx >>= mf = RecordM $ \jr -> case mx jr of
    Left e -> Left e
    Right (jr', x) -> unRecordM (mf x) jr'

getRec :: RecordM JarRecord
getRec = RecordM $ \jr -> Right (jr, jr)

putRec :: JarRecord -> RecordM ()
putRec jr = RecordM $ \_ -> Right (jr, ())

throwErr :: ParseError -> RecordM a
throwErr e = RecordM $ \_ -> Left e

throwErrAt :: (RJ.LineNum -> ParseError) -> RecordM a
throwErrAt f = do
  n <- getLineNum
  throwErr $ f n

getLineNum :: RecordM RJ.LineNum
getLineNum = recordLineNum <$> getRec

-- | Get the line number and body from a 'RJ.JarField'
fieldContent :: RJ.JarField -> (RJ.LineNum, RJ.FieldBody)
fieldContent jf = (RJ.fieldLineNum jf, RJ.fieldBodyLines jf)

-- | Remove all fields from the record with the given field name, returning the
-- contents of the removed fields
partitionKey :: Text -> RecordM [(RJ.LineNum, RJ.FieldBody)]
partitionKey k = do
  jarrec <- getRec
  let (entries, others) = flip List.partition (recordBody jarrec) $ \f -> RJ.fieldName f == k
      jarrec' = jarrec {recordBody = others}
  putRec jarrec'
  pure $ fieldContent <$> entries

convertBody :: Functor f => f (RJ.LineNum, RJ.FieldBody) -> f (RJ.LineNum, Text)
convertBody = fmap $ \(x, y) -> (x, RJ.fieldBodyLF y)

-- | Parse a field in a 'Jar' record that must occur at least once
oneMany :: Text -> RecordM (NonEmpty (RJ.LineNum, Text))
oneMany k = do
  entries <- partitionKey k
  case entries of
    x : xs -> pure $ convertBody $ x :| xs
    [] -> throwErrAt $ \n -> FieldNotPresent n k

-- | Parse a field in a 'Jar' record that may appear any number of times
zeroMany :: Text -> RecordM [(RJ.LineNum, Text)]
zeroMany = fmap convertBody . partitionKey

-- | Parse a field in a 'Jar' record that must appear precisely once
oneExactly :: Text -> RecordM (RJ.LineNum, Text)
oneExactly k = do
  entries <- partitionKey k
  case entries of
    [(x, y)] -> pure (x, RJ.fieldBodyLF y)
    [] -> throwErrAt $ \n -> FieldNotPresent n k
    (_ : _ : _) -> throwErrAt $ \n -> FieldPresentMultipleTimes n k

-- | Parse a field in a 'Jar' record that must appear at most once
zeroOne :: Text -> RecordM (Maybe (RJ.LineNum, Text))
zeroOne k = do
  entries <- partitionKey k
  case entries of
    [(x, y)] -> pure $ Just (x, RJ.fieldBodyLF y)
    [] -> pure Nothing
    (_ : _ : _) -> throwErrAt $ \n -> FieldPresentMultipleTimes n k

-- | Return the result of the 'RecordM' action, throwing a 'FieldUnrecognized'
-- error if the record in the state has any fields left
guardUnexpectedFields :: JarRecord -> RecordM a -> Either ParseError a
guardUnexpectedFields jarrec act = do
  (jarrec', a) <- unRecordM act jarrec
  case recordBody jarrec' of
    [] -> Right a
    (x : _) -> Left $ FieldUnrecognized (recordLineNum jarrec) (RJ.fieldLineNum x) (RJ.fieldName x)

-- | A single subtag @subtag@ or a range @subtag1..subtag2@
data ValOrRange
  = Val Subtag
  | Range Subtag Subtag
  deriving (Eq, Ord, Show)

-- TODO: may want to put this/these in an internal module and test it

-- | Given a 'Word8' upper and lower bound on the range of possible characters,
-- generate all of the subtags of the given length that lie between the two
-- zipped subtags, lexicographically. This function does not check that the
-- lower bound is lower than the upper bound, that the range of characters
-- between the lower and upper bound are all subtag characters, or that the
-- length matches the length of the passed subtags.
genSubtagsBetween ::
  -- | lower bound
  Word8 ->
  -- | upper bound
  Word8 ->
  -- | subtag length
  Word8 ->
  -- | two zipped subtags
  [(SubtagChar, SubtagChar)] ->
  [Subtag]
genSubtagsBetween rangeLow rangeHigh len =
  fmap (unsafeSubtagPackLen len) . genLetterSubtagsBetween'
  where
    genLetterSubtagsBetween' ((SubtagChar low, SubtagChar high) : rest) =
      case compare low high of
        LT -> ((SubtagChar low :) <$> lowTail) <> mid <> ((SubtagChar high :) <$> highTail)
          where
            lowTail = genAllAbove $ fst <$> rest
            mid = do
              x <- [low + 1 .. high - 1]
              midTail <- genAll rest
              pure $ SubtagChar x : midTail
            highTail = genAllBelow $ snd <$> rest
        EQ -> (SubtagChar low :) <$> genLetterSubtagsBetween' rest
        GT -> []
    genLetterSubtagsBetween' [] = [[]]
    genAllAbove (SubtagChar low : rest) = ((SubtagChar low :) <$> genAllAbove rest) <> high
      where
        high = do
          x <- [low + 1 .. rangeHigh]
          highTail <- genAll rest
          pure $ SubtagChar x : highTail
    genAllAbove [] = [[]]
    genAllBelow (SubtagChar high : rest) = low <> ((SubtagChar high :) <$> genAllBelow rest)
      where
        low = do
          x <- [rangeLow .. high - 1]
          lowTail <- genAll rest
          pure $ SubtagChar x : lowTail
    genAllBelow [] = [[]]
    genAll (_ : rest) = do
      x <- [rangeLow .. rangeHigh]
      rest' <- genAll rest
      pure $ SubtagChar x : rest'
    genAll [] = [[]]

-- | Generate the list of all subtags between two subtags of the same length and
-- consisting entirely of letters. That condition and the condition that the
-- 'Word8' is equal to the length of the passed list are not checked.
genLetterSubtagsBetween :: Word8 -> [(SubtagChar, SubtagChar)] -> [Subtag]
genLetterSubtagsBetween = genSubtagsBetween 97 122

-- | Generate all of the subtags consisting only of digits between the two given
-- subtags and with the same length as those subtags. The condition that the two
-- subtags have the same length is not checked, nor is the condition that both
-- contain only digits.
genDigitSubtagsBetween :: Word8 -> [(SubtagChar, SubtagChar)] -> [Subtag]
genDigitSubtagsBetween = genSubtagsBetween 48 57

{- TODO: move this to testing - want to test that the naive implementation is --
equal to the above implementation

subtagsBetweenNaive :: Word8 -> [(SubtagChar, SubtagChar)] -> [Subtag]
subtagsBetweenNaive len stuff = filter p $ unsafeSubtagPackLen len <$> chars len
  where
    (fir, sen) = unzip stuff
    st1 = unsafeSubtagPackLen len fir
    st2 = unsafeSubtagPackLen len sen
    p x  = x <= st2 && x >= st1
    chars n
      | n == 0 = [[]]
      | otherwise = do
          x <- [97 .. 122]
          xs <- chars (n - 1)
          pure $ SubtagChar x : xs
-}

-- | Various obligations that are incurred by the registry that need to be
-- checked at the end. All of the subtag obligations but the language obligation
-- require that the subtag in question exists and is not deprecated with a
-- preferred value. The 'TagExists' value records the expectation that all of
-- its subtags exist in the registry; the 'Bool' in that constructor and the
-- 'LanguageExists' constructor records whether or not we also expect the tag to
-- be normalized.

-- TODO: I think the 'TagExists' might want to have a syntactic BCP47 component
-- and not a Normal component?
data Obligation
  = LanguageExists Bool RJ.LineNum Language
  | ExtlangNormalized RJ.LineNum Extlang
  | ScriptNormalized RJ.LineNum Script
  | RegionNormalized RJ.LineNum Region
  | VariantNormalized RJ.LineNum Variant
  | TagExists Bool RJ.LineNum BCP47
  deriving (Eq, Ord, Show)

locatedObligation ::
  Maybe (RJ.LineNum, a) ->
  (RJ.LineNum -> a -> Obligation) ->
  (Maybe a, Maybe Obligation)
locatedObligation Nothing _ = (Nothing, Nothing)
locatedObligation (Just (ln, a)) con = (Just a, Just $ con ln a)

----------------------------------------------------------------
-- Registry parsers
----------------------------------------------------------------

parseSubtag :: (RJ.LineNum, Text) -> RecordM Subtag
parseSubtag t = case parseSubtagText $ snd t of
  Left e -> throwErr $ UnparsableSubtag (fst t) e
  Right a -> pure a

-- | Parse a field body that is either a single subtag or a subtag range,
-- expanding the range as necessary
parseSubtagOrRange :: IsSubtag a => (RJ.LineNum, Text) -> RecordM (NonEmpty a)
-- TODO: not sure about the mapMaybe. maybe generating an invalid subtag for
-- the type should be an error? requiring a traversal, then.
--
-- TODO: I think genLetterSubtags/genDigitSubtags calls also need to be behind a
-- test that the first tag is <= the second.
parseSubtagOrRange t
  | T.null rest = do
    x <- parseSubtag t
    case fromSubtag x of
      Nothing -> throwErr $ BadSubtag (fst t) x
      Just a -> pure $ a :| []
  | Just st2 <- T.stripPrefix ".." rest = do
    st1' <- parseSubtag (fst t, st1)
    st2' <- parseSubtag (fst t, st2)
    resolveRange st1' st2'
  | otherwise = throwErr $ BadSubtagField (fst t) (snd t)
  where
    (st1, rest) = T.span (/= '.') $ snd t
    resolveRange x y
      | Sub.containsOnlyDigits x,
        Sub.containsOnlyDigits y,
        Sub.subtagLength x == Sub.subtagLength y =
        case mapMaybe fromSubtag $
          genDigitSubtagsBetween (Sub.subtagLength x) $
            zip (Sub.unpackSubtag x) (Sub.unpackSubtag y) of
          (st : sts) -> pure $ st :| sts
          [] -> throwErr $ BadSubtagField (fst t) (snd t)
      | Sub.containsOnlyLetters x,
        Sub.containsOnlyLetters y,
        Sub.subtagLength x == Sub.subtagLength y =
        case mapMaybe fromSubtag $
          genLetterSubtagsBetween (Sub.subtagLength x) $
            zip (Sub.unpackSubtag x) (Sub.unpackSubtag y) of
          (st : sts) -> pure $ st :| sts
          [] -> throwErr $ BadSubtagField (fst t) (snd t)
      | otherwise = throwErr $ BadSubtagField (fst t) (snd t)

-- TODO: remember to lint the registry at the end! i.e. make sure that all the
-- referenced subtags are in the registry, all the tags refer to things in the
-- registry, etc. maybe provide a findLint :: Registry -> Maybe Lint or
-- registryLint :: Registry :: -> [Lint], and then modify the parseRegistry
-- function below (and perhaps also the ParseError) so that the lint is fatal.
-- also want to check that every grandfathered and redundant tag has exactly one
-- record (and maybe that every tag/subtag has at most one record)
--
-- could also modify the parseRegistry' and parseEntry functions so that they
-- accumulate a list of obligations that the final registry must satisfy
-- (particular subtags being present, mostly), then check them all at the end?
-- seems nice.

-- | Parse a 'Registry' from a raw 'RJ.JarRegistry'
parseRegistry :: RJ.JarRegistry -> Either ParseError Registry
parseRegistry rawregistry = do
  let (daterec :| regentries) = toJarRecord <$> rawregistry
  date <- guardUnexpectedFields daterec parseDateRec
  let initreg =
        Registry
          { registryDate = date,
            registryLanguages = mempty,
            registryExtlangs = mempty,
            registryScripts = mempty,
            registryRegions = mempty,
            registryVariants = mempty,
            registryGrandfatheredTags = mempty,
            registryRedundantTags = mempty
          }
  (registry, obligations) <- parseRegistry' initreg [] regentries
  checkObligations registry obligations
  pure registry
  where
    parseDateRec = do
      date <- oneExactly "File-Date"
      parseDate date

    parseRegistry' !regacc !obacc (x : xs) = case guardUnexpectedFields x $ parseEntry regacc of
      Left e -> Left e
      Right (regacc', xobligation) -> parseRegistry' regacc' (xobligation <> obacc) xs
    parseRegistry' !regacc !obacc [] = Right (regacc, obacc)

    parseEntry registry = do
      (tyln, tyt) <- oneExactly "Type"
      case tyt of
        "language" -> parseLangRec registry
        "extlang" -> parseExtlRec registry
        "script" -> parseScrRec registry
        "region" -> parseRegRec registry
        "variant" -> parseVarRec registry
        "grandfathered" -> parseGrandRec registry
        "redundant" -> parseRedRec registry
        _ -> throwErr $ InvalidType tyln tyt

    -- TODO: make this do something
    checkObligations _registry _obligations = pure ()

type LocatedText = (RJ.LineNum, Text)

parseDate :: LocatedText -> RecordM Day
parseDate (ln, datet) = do
  case reads $ T.unpack datet of
    [(a, "")] -> pure (a :: Day)
    _ -> throwErr $ BadDate ln datet

parseFromSubtag :: IsSubtag a => LocatedText -> RecordM a
parseFromSubtag t = do
  st <- parseSubtag t
  case fromSubtag st of
    Just a -> pure a
    Nothing -> throwErr $ BadSubtag (fst t) st

-- | Parse a subtag and retain its line number
parseFromSubtagLoc :: IsSubtag a => LocatedText -> RecordM (RJ.LineNum, a)
parseFromSubtagLoc t = do
  st <- parseFromSubtag t
  pure (fst t, st)

parseScope :: LocatedText -> RecordM Scope
parseScope t = case snd t of
  "macrolanguage" -> pure Macrolanguage
  "collection" -> pure Collection
  "special" -> pure Special
  "private-use" -> pure PrivateUseScope
  _ -> throwErr $ BadScope (fst t) (snd t)

getDeprStatus :: Maybe Day -> Maybe a -> RecordM (Deprecation a)
getDeprStatus Nothing Nothing = pure NotDeprecated
getDeprStatus (Just d) Nothing = pure $ DeprecatedSimple d
getDeprStatus (Just d) (Just a) = pure $ DeprecatedPreferred d a
getDeprStatus Nothing (Just _) = do
  ln <- getLineNum
  throwErr $ PreferredWithoutDeprecation ln

addRec :: Ord k => (k -> DuplicateRecordKey) -> k -> v -> Map k v -> RecordM (Map k v)
addRec err k v m = case Map.lookup k m of
  Nothing -> pure $ Map.insert k v m
  Just _ -> throwErr $ DuplicateRecord $ err k

addAll ::
  Ord k =>
  (Registry -> Map k v) ->
  (Map k v -> Registry -> Registry) ->
  (k -> DuplicateRecordKey) ->
  NonEmpty k ->
  v ->
  Registry ->
  RecordM Registry
addAll get set err (k :| ks) v r = add k initm >>= go ks
  where
    add x = addRec err x v
    initm = get r
    go (x : xs) !m = add x m >>= go xs
    go [] !m = pure $ set m r

parseLangRec :: Registry -> RecordM (Registry, [Obligation])
parseLangRec registry = do
  subtags <- oneExactly "Subtag" >>= parseSubtagOrRange
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- zeroOne "Preferred-Value" >>= traverse parseFromSubtagLoc
  let (preferredValue', pvObl) = locatedObligation preferredValue (LanguageExists True)
  scriptSup <- zeroOne "Suppress-Script" >>= traverse parseFromSubtagLoc
  let (scriptSup', ssObl) = locatedObligation scriptSup ScriptNormalized
  macro <- zeroOne "Macrolanguage" >>= traverse parseFromSubtagLoc
  -- TODO: not sure if the macrolanguage must not be deprecated
  let (macro', macroObl) = locatedObligation macro (LanguageExists False)
  scope <- zeroOne "Scope" >>= traverse parseScope
  comments <- zeroMany "Comments"
  depr <- getDeprStatus deprDate preferredValue'
  let langrec =
        LanguageRecord
          { languageDateAdded = addDate,
            languageDescription = snd <$> desc,
            languageDeprecation = depr,
            languageScriptSuppression = scriptSup',
            languageMacrolanguage = macro',
            languageScope = scope,
            languageComments = snd <$> comments
          }
  registry' <-
    addAll
      registryLanguages
      (\m r -> r {registryLanguages = m})
      (DuplicateSubtag . toSubtag)
      subtags
      langrec
      registry
  let obligations = catMaybes [pvObl, ssObl, macroObl]
  pure (registry', obligations)

-- | Parse an extlang record. Note that we do not allow ranges to appear in the
-- Subtag field, as these would conflict with the constraint that the preferred
-- value must be equal to the subtag itself. Technically speaking a range
-- @xyz..xyz@ would also satisfy that constraint, but I sincerely doubt they
-- will do that.
parseExtlRec :: Registry -> RecordM (Registry, [Obligation])
parseExtlRec registry = do
  subtag <- oneExactly "Subtag" >>= parseFromSubtag
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- oneExactly "Preferred-Value" >>= parseFromSubtagLoc
  if toSubtag (snd preferredValue) /= toSubtag subtag
    then throwErrAt ExtlangLangMismatch
    else pure ()
  let pvObl = LanguageExists False (fst preferredValue) (snd preferredValue)
  prefix <- oneExactly "Prefix" >>= parseFromSubtag
  scriptSup <- zeroOne "Suppress-Script" >>= traverse parseFromSubtagLoc
  let (scriptSup', ssObl) = locatedObligation scriptSup ScriptNormalized
  macro <- zeroOne "Macrolanguage" >>= traverse parseFromSubtagLoc
  let (macro', macroObl) = locatedObligation macro $ LanguageExists True
  scope <- zeroOne "Scope" >>= traverse parseScope
  comments <- zeroMany "Comments"
  let extlrec =
        ExtlangRecord
          { extlangDateAdded = addDate,
            extlangDescription = snd <$> desc,
            extlangDeprecation = deprDate,
            extlangPrefix = prefix,
            extlangScriptSuppression = scriptSup',
            extlangMacrolanguage = macro',
            extlangScope = scope,
            extlangComments = snd <$> comments
          }
  registry' <-
    addAll
      registryExtlangs
      (\m r -> r {registryExtlangs = m})
      (DuplicateSubtag . toSubtag)
      (subtag :| [])
      extlrec
      registry
  let obligations = pvObl : catMaybes [ssObl, macroObl]
  pure (registry', obligations)

-- | Parse a script record
parseScrRec :: Registry -> RecordM (Registry, [Obligation])
parseScrRec registry = do
  subtags <- oneExactly "Subtag" >>= parseSubtagOrRange
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- zeroOne "Preferred-Value" >>= traverse parseFromSubtagLoc
  let (preferredValue', pvObl) = locatedObligation preferredValue ScriptNormalized
  comments <- zeroMany "Comments"
  depr <- getDeprStatus deprDate preferredValue'
  let scrrec =
        ScriptRecord
          { scriptDateAdded = addDate,
            scriptDescription = snd <$> desc,
            scriptDeprecation = depr,
            scriptComments = snd <$> comments
          }
  registry' <-
    addAll
      registryScripts
      (\m r -> r {registryScripts = m})
      (DuplicateSubtag . toSubtag)
      subtags
      scrrec
      registry
  let obligations = toList pvObl
  pure (registry', obligations)

-- | Parse a region record
parseRegRec :: Registry -> RecordM (Registry, [Obligation])
parseRegRec registry = do
  subtags <- oneExactly "Subtag" >>= parseSubtagOrRange
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- zeroOne "Preferred-Value" >>= traverse parseFromSubtagLoc
  let (preferredValue', pvObl) = locatedObligation preferredValue RegionNormalized
  comments <- zeroMany "Comments"
  depr <- getDeprStatus deprDate preferredValue'
  let regrec =
        RegionRecord
          { regionDateAdded = addDate,
            regionDescription = snd <$> desc,
            regionDeprecation = depr,
            regionComments = snd <$> comments
          }
  registry' <-
    addAll
      registryRegions
      (\m r -> r {registryRegions = m})
      (DuplicateSubtag . toSubtag)
      subtags
      regrec
      registry
  let obligations = toList pvObl
  pure (registry', obligations)

-- | Parse a variant record
parseVarRec :: Registry -> RecordM (Registry, [Obligation])
parseVarRec registry = do
  subtags <- oneExactly "Subtag" >>= parseSubtagOrRange
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- zeroOne "Preferred-Value" >>= traverse parseFromSubtagLoc
  let (preferredValue', pvObl) = locatedObligation preferredValue VariantNormalized
  comments <- zeroMany "Comments"
  depr <- getDeprStatus deprDate preferredValue'
  prefixes <- zeroMany "Prefix" >>= traverse parseTag
  let varrec =
        VariantRecord
          { variantDateAdded = addDate,
            variantDescription = snd <$> desc,
            variantDeprecation = depr,
            variantPrefixes = prefixes,
            variantComments = snd <$> comments
          }
  registry' <-
    addAll
      registryVariants
      (\m r -> r {registryVariants = m})
      (DuplicateSubtag . toSubtag)
      subtags
      varrec
      registry
  let obligations = toList pvObl
  pure (registry', obligations)

parseTag :: LocatedText -> RecordM BCP47
parseTag t = case parseBCP47 $ snd t of
  Right tag -> pure tag
  Left _ -> throwErr $ BadTag (fst t) (snd t)

parseTagLoc :: LocatedText -> RecordM (RJ.LineNum, BCP47)
parseTagLoc t = do
  tag <- parseTag t
  pure (fst t, tag)

-- | Parse a grandfathered record
parseGrandRec :: Registry -> RecordM (Registry, [Obligation])
parseGrandRec registry = do
  (tagloc, tag) <- oneExactly "Tag" >>= parseTagLoc
  tag' <- case tag of
    GrandfatheredTag g -> pure g
    _ -> throwErr $ TagNotGrandfathered tagloc tag
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- zeroOne "Preferred-Value" >>= traverse parseTagLoc
  let (preferredValue', pvObl) = locatedObligation preferredValue (TagExists True)
  comments <- zeroMany "Comments"
  depr <- getDeprStatus deprDate preferredValue'
  let regrec =
        TagRecord
          { tagDateAdded = addDate,
            tagDescription = snd <$> desc,
            tagDeprecation = depr,
            tagComments = snd <$> comments
          }
  registry' <-
    addAll
      registryGrandfatheredTags
      (\m r -> r {registryGrandfatheredTags = m})
      DuplicateGrandfathered
      (tag' :| [])
      regrec
      registry
  let obligations = toList pvObl
  pure (registry', obligations)

-- | Parse a grandfathered record
parseRedRec :: Registry -> RecordM (Registry, [Obligation])
parseRedRec registry = do
  (tagloc, tag) <- oneExactly "Tag" >>= parseTagLoc
  tag' <- case tag of
    NormalTag n
      | Just r <- recognizeRedundant (toSubtags n) ->
        pure r
    _ -> throwErr $ TagNotRedundant tagloc tag
  desc <- oneMany "Description"
  addDate <- oneExactly "Added" >>= parseDate
  deprDate <- zeroOne "Deprecated" >>= traverse parseDate
  preferredValue <- zeroOne "Preferred-Value" >>= traverse parseTagLoc
  let (preferredValue', pvObl) = locatedObligation preferredValue (TagExists True)
  comments <- zeroMany "Comments"
  depr <- getDeprStatus deprDate preferredValue'
  let regrec =
        TagRecord
          { tagDateAdded = addDate,
            tagDescription = snd <$> desc,
            tagDeprecation = depr,
            tagComments = snd <$> comments
          }
  registry' <-
    addAll
      registryRedundantTags
      (\m r -> r {registryRedundantTags = m})
      DuplicateRedundant
      (tag' :| [])
      regrec
      registry
  let obligations = toList pvObl
  pure (registry', obligations)
