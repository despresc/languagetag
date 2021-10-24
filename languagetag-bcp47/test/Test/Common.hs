{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Common testing values
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module Test.Common where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
  ( Expectation,
    HasCallStack,
    shouldBe,
  )
import Test.QuickCheck
  ( Gen,
    arbitrary,
    arbitraryASCIIChar,
    arbitraryBoundedEnum,
    chooseEnum,
    chooseInt,
    elements,
    liftArbitrary,
    liftShrink,
    oneof,
    shrink,
    suchThat,
    suchThatMap,
    vectorOf,
  )
import LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Deprecation (..),
    Extlang,
    Grandfathered,
    Language,
    Normal (..),
    Region,
    Script,
    Variant,
    toExtensionSubtag,
  )
import LanguageTag.BCP47.Subtag
  ( MaybeSubtag,
    Subtag,
    SubtagChar,
    isSubtagChar,
    justSubtag,
    maybeSubtag,
    nullSubtag,
    packChar,
    popSubtagText,
    renderSubtagLower,
    unpackCharLower,
  )
import LanguageTag.BCP47.Subtag.Trie (Trie)
import qualified LanguageTag.BCP47.Subtag.Trie as Trie
import qualified LanguageTag.BCP47.Syntax as Syn
import LanguageTag.Internal.BCP47.Registry.Types
  ( ExtensionSubtag (..),
  )
import qualified LanguageTag.Internal.BCP47.Subtag.NonEmptyTrie as NET
import qualified LanguageTag.Internal.BCP47.Syntax as Syn

-- TODO: add a qualified export list to this module

-- | Parse a full 'Subtag' from the given input
justFullSubtag :: Text -> Maybe Subtag
justFullSubtag t = case popSubtagText t of
  Left _ -> Nothing
  Right (st, t')
    | T.null t' -> Just st
    | otherwise -> Nothing

----------------------------------------------------------------
-- Generating
----------------------------------------------------------------

-- | Generate a text string that is a candidate text string from the first
-- generator, then an optional string consisting of a single character
-- failing the predicate and a text string from the second generator
genPoppableText ::
  (Char -> Bool) ->
  Gen Text ->
  Gen Text ->
  Gen Text
genPoppableText p candidate garbage = do
  t <- candidate
  c <- arbitrary `suchThat` (not . p)
  t' <- garbage
  b <- arbitrary
  pure $ if b then t else t <> T.singleton c <> t'

-- | A random ASCII digit
genDigit :: Gen Char
genDigit = chooseEnum ('0', '9')

-- | A random ASCII lower case letter
genLowLetter :: Gen Char
genLowLetter = chooseEnum ('a', 'z')

-- | A random ASCII upper case letter
genHighLetter :: Gen Char
genHighLetter = chooseEnum ('A', 'Z')

-- | A random ASCII letter
genLetter :: Gen Char
genLetter = oneof [genLowLetter, genHighLetter]

-- | A random digit or letter
genSubtagChar :: Gen Char
genSubtagChar = oneof [genDigit, genLetter]

-- | A random valid subtag consisting entirely of letters
genSubtagLetterText :: Gen Text
genSubtagLetterText = do
  len <- chooseInt (1, 8)
  T.pack <$> vectorOf len genLetter

-- | A random valid subtag consisting entirely of digits
genSubtagDigitText :: Gen Text
genSubtagDigitText = do
  len <- chooseInt (1, 8)
  T.pack <$> vectorOf len genDigit

-- | A random string of letters and digits of length between one and
-- eight
genSubtagText :: Gen Text
genSubtagText = do
  len <- chooseInt (1, 8)
  T.pack <$> vectorOf len genSubtagChar

-- | An initial subtag followed by either nothing or an invalid subtag character
-- and subsequent text
genPopSubtagText :: Gen Text
genPopSubtagText = genPoppableText isSubtagChar genSubtagText genSubtagishText

-- | A valid subtag character or some invalid character, biased
-- towards ASCII characters, and valid subtag characters in particular
genSubtagishChar :: Gen Char
genSubtagishChar = oneof [genSubtagChar, arbitraryASCIIChar, arbitrary]

-- | A string of characters of length between zero and ten
genSubtagishText :: Gen Text
genSubtagishText = do
  len <- chooseInt (0, 10)
  T.pack <$> vectorOf len genSubtagishChar

-- | A valid 'Subtag'
genSubtag :: Gen Subtag
genSubtag = genSubtagText `suchThatMap` justFullSubtag

genMaybeSubtagWith :: Gen Subtag -> Gen MaybeSubtag
genMaybeSubtagWith g = oneof [justSubtag <$> g, pure nullSubtag]

-- | A valid 'MaybeSubtag'
genMaybeSubtag :: Gen MaybeSubtag
genMaybeSubtag = genMaybeSubtagWith genSubtag

-- | A valid 'SubtagChar'
genSubtagSubtagChar :: Gen SubtagChar
genSubtagSubtagChar = suchThatMap genLowLetter packChar

-- | A string of letters of the given length
genLanguageText :: Int -> Gen Text
genLanguageText n = T.pack <$> vectorOf n genLetter

-- | A language subtag of the given length
genLanguageSubtag :: Int -> Gen Subtag
genLanguageSubtag n = genLanguageText n `suchThatMap` justFullSubtag

-- | An extlang subtag 'Text' value
genExtlangText :: Gen Text
genExtlangText = T.pack <$> vectorOf 3 genLetter

-- | An extlang 'Subtag'
genExtlangSubtag :: Gen Subtag
genExtlangSubtag = genExtlangText `suchThatMap` justFullSubtag

-- | An script subtag 'Text' value
genScriptText :: Gen Text
genScriptText = T.pack <$> vectorOf 4 genLetter

-- | An script 'Subtag'
genScriptSubtag :: Gen Subtag
genScriptSubtag = genScriptText `suchThatMap` justFullSubtag

-- | An region subtag 'Text' value
genRegionText :: Gen Text
genRegionText = oneof [digReg, letReg]
  where
    digReg = T.pack <$> vectorOf 3 genDigit
    letReg = T.pack <$> vectorOf 2 genLetter

-- | An region 'Subtag'
genRegionSubtag :: Gen Subtag
genRegionSubtag = genRegionText `suchThatMap` justFullSubtag

-- | An variant subtag 'Text' value
genVariantText :: Gen Text
genVariantText = chooseInt (4, 8) >>= go
  where
    go n
      | n == 4 = T.cons <$> genDigit <*> (T.pack <$> vectorOf 3 genLetter)
      | otherwise = T.pack <$> vectorOf n genSubtagChar

-- | An variant 'Subtag'
genVariantSubtag :: Gen Subtag
genVariantSubtag = genVariantText `suchThatMap` justFullSubtag

-- | An extension character
genExtensionChar :: Gen Char
genExtensionChar = genSubtagChar `suchThat` (/= 'x')

-- | An 'Syn.ExtensionChar'
genExtensionChar' :: Gen Syn.ExtensionChar
genExtensionChar' = arbitraryBoundedEnum

-- | Text for an extension subtag (other than the initial singleton)
genExtensionSubtagText :: Gen Text
genExtensionSubtagText = do
  n <- chooseInt (2, 8)
  T.pack <$> vectorOf n genSubtagChar

-- | An extension subtag (other than the initial singleton)
genExtensionSubtag :: Gen Subtag
genExtensionSubtag = genExtensionSubtagText `suchThatMap` justFullSubtag

genExtensionSubtag' :: Gen ExtensionSubtag
genExtensionSubtag' = ExtensionSubtag <$> genExtensionSubtag

-- | Generate an entire extension section, with initial singleton
genExtensionSectionTexts :: Gen [Text]
genExtensionSectionTexts = do
  c <- genExtensionChar
  len <- chooseInt (1, 10)
  exts <- vectorOf len genExtensionSubtagText
  pure $ T.singleton c : exts

-- | Generate a syntactic extension section
genSynExtensionSection :: Gen Syn.Extension
genSynExtensionSection = do
  c <- genExtensionChar'
  len <- chooseInt (1, 10)
  exts <- vectorOf len genExtensionSubtag
  pure $ Syn.Extension c (NE.fromList exts)

-- | Text for a private use subtag (other than the initial singleton)
genPrivateUseSubtagText :: Gen Text
genPrivateUseSubtagText = do
  n <- chooseInt (1, 8)
  T.pack <$> vectorOf n genSubtagChar

-- | A private use subtag (other than the initial singleton)
genPrivateUseSubtag :: Gen Subtag
genPrivateUseSubtag = genPrivateUseSubtagText `suchThatMap` justFullSubtag

-- | Generate an entire privateuse section, with initial singleton
genPrivateUseSectionTexts :: Gen [Text]
genPrivateUseSectionTexts = do
  len <- chooseInt (1, 10)
  pus <- vectorOf len genPrivateUseSubtagText
  pure $ "x" : pus

genPrivateUseSection :: Gen [Subtag]
genPrivateUseSection = do
  len <- chooseInt (0, 10)
  vectorOf len genPrivateUseSubtag

-- | Generate well-formed BCP47 normal tag text
genNormalTagText :: Gen Text
genNormalTagText = chooseInt (2, 8) >>= go
  where
    go n
      | n <= 3 = do
        langTag <- genLanguageText n
        extlen <- chooseInt (0, 3)
        exts <- vectorOf extlen genExtlangText
        genTail $ langTag : exts
      | otherwise = genLanguageText n >>= (genTail . (: []))
    genTail x = do
      scr <- genScriptText
      reg <- genRegionText
      varlen <- chooseInt (0, 7)
      vars <- vectorOf varlen genVariantText
      extslen <- chooseInt (0, 7)
      exts <- concat <$> vectorOf extslen genExtensionSectionTexts
      pus <- oneof [pure [], genPrivateUseSectionTexts]
      pure $ T.intercalate "-" $ x <> [scr, reg] <> vars <> exts <> pus

genPrivateUseTagText :: Gen Text
genPrivateUseTagText = T.intercalate "-" <$> genPrivateUseSectionTexts

-- | Randomly mix up the case of a text value
caseScramble :: Gen Text -> Gen Text
caseScramble gt = do
  t <- gt
  cm <- vectorOf (T.length t) arbitrary
  pure $ T.pack $ zipWith go (T.unpack t) cm
  where
    go c b
      | b = Char.toLower c
      | otherwise = Char.toUpper c

genGrandfatheredTagText :: Gen Text
genGrandfatheredTagText =
  caseScramble $
    elements
      [ "art-lojban",
        "cel-gaulish",
        "en-GB-oed",
        "i-ami",
        "i-bnn",
        "i-default",
        "i-enochian",
        "i-hak",
        "i-klingon",
        "i-lux",
        "i-mingo",
        "i-navajo",
        "i-pwn",
        "i-tao",
        "i-tay",
        "i-tsu",
        "no-bok",
        "no-nyn",
        "sgn-BE-FR",
        "sgn-BE-NL",
        "sgn-CH-DE",
        "zh-guoyu",
        "zh-hakka",
        "zh-min",
        "zh-min-nan",
        "zh-xiang"
      ]

-- | Generate well-formed tag text
genTagText :: Gen Text
genTagText = oneof [genNormalTagText, genPrivateUseTagText, genGrandfatheredTagText]

genPopTagText :: Gen Text
genPopTagText = genPoppableText Syn.isTagChar genTagText genTagishText

-- | Generate near-well-formed (and possibly empty) tag text
genTagishText :: Gen Text
genTagishText = oneof [genTagText, g]
  where
    g = do
      n <- chooseInt (0, 10)
      T.intercalate "-" <$> vectorOf n genSubtagishText

-- | Generate a normal well-formed tag
genSynNormal :: Gen Syn.Normal
genSynNormal = do
  n <- chooseInt (2, 8)
  langTag <- genLanguageSubtag n
  let con =
        Syn.Normal
          langTag
          nullSubtag
          nullSubtag
          nullSubtag
          nullSubtag
          nullSubtag
          []
          []
          []
  go n con
  where
    go n con
      | n <= 3 = do
        extlen <- chooseInt (0, 3)
        con' <- setExtlangs extlen con
        genTail con'
      | otherwise = genTail con
    genTail x = do
      scr <- genMaybeSubtagWith genScriptSubtag
      reg <- genMaybeSubtagWith genRegionSubtag
      varlen <- chooseInt (0, 7)
      vars <- vectorOf varlen genVariantSubtag
      extslen <- chooseInt (0, 7)
      exts <- vectorOf extslen genSynExtensionSection
      pus <- oneof [pure [], genPrivateUseSection]
      pure
        x
          { Syn.script = scr,
            Syn.region = reg,
            Syn.variants = vars,
            Syn.extensions = exts,
            Syn.privateUse = pus
          }
    setExtlangs n con
      | n == 3 = setExtlangs3 con
      | n == 2 = setExtlangs2 con
      | n == 1 = setExtlangs1 con
      | otherwise = pure con
    setExtlangs3 con = do
      e <- justSubtag <$> genExtlangSubtag
      setExtlangs2 con {Syn.extlang3 = e}
    setExtlangs2 con = do
      e <- justSubtag <$> genExtlangSubtag
      setExtlangs1 con {Syn.extlang2 = e}
    setExtlangs1 con = do
      e <- justSubtag <$> genExtlangSubtag
      pure con {Syn.extlang1 = e}

genGrandfathered :: Gen Grandfathered
genGrandfathered = arbitraryBoundedEnum

genPrivateUseTag :: Gen (NonEmpty Subtag)
genPrivateUseTag = do
  len <- chooseInt (1, 10)
  NE.fromList <$> vectorOf len genPrivateUseSubtag

genSynTag :: Gen Syn.BCP47
genSynTag =
  oneof
    [ Syn.NormalTag <$> genSynNormal,
      Syn.GrandfatheredTag <$> genGrandfathered,
      Syn.PrivateUse <$> genPrivateUseTag
    ]

genValidLanguage :: Gen Language
genValidLanguage = arbitraryBoundedEnum

genValidExtlang :: Gen Extlang
genValidExtlang = arbitraryBoundedEnum

genValidScript :: Gen Script
genValidScript = arbitraryBoundedEnum

genValidRegion :: Gen Region
genValidRegion = arbitraryBoundedEnum

genValidVariant :: Gen Variant
genValidVariant = arbitraryBoundedEnum

genValidNormal :: Gen Normal
genValidNormal = do
  l <- genValidLanguage
  e <- liftArbitrary genValidExtlang
  s <- liftArbitrary genValidScript
  r <- liftArbitrary genValidRegion
  vs <- S.fromList <$> liftArbitrary genValidVariant
  es <-
    M.fromList
      <$> liftArbitrary
        ( (,) <$> genExtensionChar'
            <*> (NE.fromList <$> liftArbitrary genExtensionSubtag' `suchThat` (not . null))
        )
  ps <- genPrivateUseSection
  pure
    Normal
      { language = l,
        extlang = e,
        script = s,
        region = r,
        variants = vs,
        extensions = es,
        privateUse = ps
      }

genValidTag :: Gen BCP47
genValidTag =
  oneof
    [ NormalTag <$> genValidNormal,
      PrivateUseTag <$> genPrivateUseTag,
      GrandfatheredTag <$> genGrandfathered
    ]

liftGenTrie :: Gen a -> Gen (Trie a)
liftGenTrie p = oneof [Trie.Trie <$> liftGenNETrie p, pure Trie.empty]

liftGenNETrie :: Gen a -> Gen (NET.Trie a)
liftGenNETrie p = go 7
  where
    go depth = oneof [deeper depth, NET.Leaf <$> p]
    deeper depth
      | depth == 0 = NET.Leaf <$> p
      | otherwise = do
        a <- oneof [NET.Node <$> p, pure NET.Nil]
        let dgen = chooseInt (0, depth - 1)
        width <- chooseInt (1, 6)
        l <- vectorOf width $ do
          s <- genSubtag
          t <- dgen >>= deeper
          pure (s, t)
        let m = M.fromList l
        let (s, t, m') = case M.toList m of
              ((x, y) : _) -> (x, y, M.delete x m)
              [] -> error "vectorOf gave us an empty list"
        pure $ NET.Branch a (NET.Step s t) m'

----------------------------------------------------------------
-- Shrinking
----------------------------------------------------------------

-- | Shrink a 'Text' value of the form @\"candidate\" <> \"garbage\"@, where the
-- @candidate@ portion of the text is the initial span of text for which the
-- predicate is true. This function either applies the first shrinking function
-- to @candidate@ or applies the second shrinking function to @garbage@
shrinkPoppableWith ::
  (Char -> Bool) ->
  (Text -> [Text]) ->
  (Text -> [Text]) ->
  Text ->
  [Text]
shrinkPoppableWith p shrinkCandidate shrinkGarbage t =
  [start <> garbage | start <- candidates]
    <> [candidate <> rest | rest <- garbagePieces]
  where
    (candidate, garbage) = T.span p t
    candidates = shrinkCandidate candidate
    garbagePieces = shrinkGarbage garbage

-- | Shrink a 'Text' value
shrinkText :: Text -> [Text]
shrinkText = fmap T.pack . shrink . T.unpack

-- | Shrink a 'Text' value generated by 'genPopTagText'
shrinkPopTagText :: Text -> [Text]
shrinkPopTagText = shrinkPoppableWith Syn.isTagChar shrinkTagText shrinkText

-- | Shrink a 'Text' value generated by 'genPopSubtagText'
shrinkPopSubtagText :: Text -> [Text]
shrinkPopSubtagText = shrinkPoppableWith isSubtagChar shrinkSubtagText shrinkText

-- | Shrink a subtag 'Text' value
shrinkSubtagText :: Text -> [Text]
shrinkSubtagText t =
  filter (not . T.null) $ fmap T.pack $ init $ List.subsequences $ T.unpack t

-- | Shrink a 'Subtag' value
shrinkSubtag :: Subtag -> [Subtag]
shrinkSubtag = mapMaybe justFullSubtag . shrinkText . renderSubtagLower

-- | Shrink a 'SubtagChar' value
shrinkSubtagChar :: SubtagChar -> [SubtagChar]
shrinkSubtagChar = mapMaybe packChar . shrink . unpackCharLower

-- | Shrink a 'MaybeSubtag' value
shrinkMaybeSubtag :: MaybeSubtag -> [MaybeSubtag]
shrinkMaybeSubtag = maybeSubtag [] go
  where
    go st = (justSubtag <$> shrinkSubtag st) <> [nullSubtag]

-- | Shrink an entire candidate text tag

-- A mix of more or less lawful shrinking
shrinkTagishText :: Text -> [Text]
shrinkTagishText t =
  filter (not . T.null) $
    fmap (T.intercalate "-") (shrinkListWith shrinkText (T.split (== '-') t))
      <> shrinkText t

-- | Shrink a text tag

-- Very conservative until I feel like writing it properly
shrinkTagText :: Text -> [Text]
shrinkTagText = fmap (T.intercalate "-") . go . T.split (== '-')
  where
    go (x : xs)
      | x == "x" =
        ("x" :) <$> filter (not . null) (shrinkListWith shrinkSubtagText xs)
    go _ = []

-- | Shrink a syntactic extension
shrinkSynExtension :: Syn.Extension -> [Syn.Extension]
shrinkSynExtension (Syn.Extension c es) = Syn.Extension c <$> mapMaybe NE.nonEmpty shrinks
  where
    es' = NE.toList es
    shrinks = shrinkListWith shrinkSubtag es'

-- | Shrink a syntactic normal tag

-- Very conservative until I feel like writing it properly
shrinkSynNormal :: Syn.Normal -> [Syn.Normal]
shrinkSynNormal = const []

liftShrinkNE :: (a -> [a]) -> NonEmpty a -> [NonEmpty a]
liftShrinkNE f = mapMaybe NE.nonEmpty . shrinkListWith f . NE.toList

shrinkPrivateUseTag :: NonEmpty Subtag -> [NonEmpty Subtag]
shrinkPrivateUseTag = liftShrinkNE shrinkSubtag

shrinkSynTag :: Syn.BCP47 -> [Syn.BCP47]
shrinkSynTag (Syn.NormalTag n) = Syn.NormalTag <$> shrinkSynNormal n
shrinkSynTag (Syn.PrivateUse t) = Syn.PrivateUse <$> shrinkPrivateUseTag t
shrinkSynTag (Syn.GrandfatheredTag _) = []

shrinkValidNormal :: Normal -> [Normal]
shrinkValidNormal (Normal l e s r v es ps) =
  [Normal l e' s r v es ps | e' <- shrinkMaybe e]
    <> [Normal l e s' r v es ps | s' <- shrinkMaybe s]
    <> [Normal l e s r' v es ps | r' <- shrinkMaybe r]
    <> [Normal l e s r v' es ps | v' <- S.fromList <$> liftShrink (const []) (S.toList v)]
    <> [Normal l e s r v es' ps | es' <- shrinkValidExtensions es]
    <> [Normal l e s r v es ps' | ps' <- liftShrink shrinkSubtag ps]
  where
    shrinkMaybe = liftShrink (const [])
    shrinkE (c, xs) = (,) c <$> liftShrinkNE shrinkExtensionSubtag xs
    shrinkValidExtensions = fmap M.fromList . shrinkListWith shrinkE . M.toList
    shrinkExtensionSubtag (ExtensionSubtag t) = mapMaybe toExtensionSubtag $ shrinkSubtag t

shrinkValidTag :: BCP47 -> [BCP47]
shrinkValidTag (NormalTag n) = fmap NormalTag $ shrinkValidNormal n
shrinkValidTag (PrivateUseTag t) = PrivateUseTag <$> shrinkPrivateUseTag t
shrinkValidTag (GrandfatheredTag _) = []

----------------------------------------------------------------
-- Utility
----------------------------------------------------------------

shrinkListWith :: (a -> [a]) -> [a] -> [[a]]
shrinkListWith _ [] = []
shrinkListWith f l = init (List.subsequences l) <> liftShrink f l

liftShrinkNode :: (a -> [a]) -> NET.Node a -> [NET.Node a]
liftShrinkNode f (NET.Node a) = NET.Nil : (NET.Node <$> f a)
liftShrinkNode _ NET.Nil = []

liftShrinkTrie :: (a -> [a]) -> Trie a -> [Trie a]
liftShrinkTrie f (Trie.Trie x) = Trie.TrieNil : (Trie.Trie <$> liftShrinkNETrie f x)
liftShrinkTrie _ Trie.TrieNil = []

liftShrinkNETrie :: (a -> [a]) -> NET.Trie a -> [NET.Trie a]
liftShrinkNETrie f (NET.Branch n (NET.Step x y) m) =
  fromN n
    <> [y]
    <> (snd <$> M.toList m)
    <> amalgam
    <> [NET.Branch n' (NET.Step x y) m | n' <- liftShrinkNode f n]
  where
    fromN (NET.Node a) = [NET.Leaf a]
    fromN NET.Nil = []
    go (s, t) =
      [(s, t') | t' <- liftShrinkNETrie f t]
        <> [(s', t) | s' <- shrinkSubtag s]
    amalgam = do
      l <- shrinkListWith go $ M.toAscList $ M.insert x y m
      let m' = M.fromList l
      case M.toAscList m' of
        ((a, b) : _) -> pure $ NET.Branch n (NET.Step a b) (M.delete a m')
        [] -> []
liftShrinkNETrie f (NET.Leaf a) = NET.Leaf <$> f a

data DifferentElem a
  = DifferentLeft a
  | DifferentRight a
  | DifferentBoth a a
  deriving (Eq, Ord, Show)

findFirstDiff :: Eq a => [a] -> [a] -> Maybe (DifferentElem a, [a], [a])
findFirstDiff (x : xs) (y : ys)
  | x == y = findFirstDiff xs ys
  | otherwise = Just (DifferentBoth x y, xs, ys)
findFirstDiff (x : xs) [] = Just (DifferentLeft x, xs, [])
findFirstDiff [] (y : ys) = Just (DifferentRight y, [], ys)
findFirstDiff [] [] = Nothing

-- | @'badRound' f x@ returns 'Nothing' exactly when the given
-- function returns 'Just' something equal to @x@, otherwise returning
-- @'Just' x@
badRound :: Eq a => (a -> Maybe a) -> a -> Maybe a
badRound = badRoundWith (==)

-- | Like badRound, but with a user-supplied predicate that is given
-- the new and old values
badRoundWith :: (a -> a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
badRoundWith p f x = case f x of
  Just x'
    | p x' x -> Nothing
  _ -> Just x

-- | Like 'badRoundWith', but with a transformation that never fails
badRoundWith' :: (a -> a -> Bool) -> (a -> a) -> a -> Maybe a
badRoundWith' p f = badRoundWith p (Just . f)

-- | Like 'badRound', but with a transformation that never fails
badRound' :: Eq a => (a -> a) -> a -> Maybe a
badRound' = badRoundWith' (==)

findMaybe :: (a -> Maybe b) -> [a] -> Maybe (a, b)
findMaybe f (x : xs) = case f x of
  Just y -> Just (x, y)
  Nothing -> findMaybe f xs
findMaybe _ [] = Nothing

shouldNotMatch ::
  ( HasCallStack,
    Show a,
    Show b,
    Eq a,
    Eq b
  ) =>
  (b -> Maybe a) ->
  [b] ->
  Expectation
shouldNotMatch f l = findMaybe f l `shouldBe` Nothing

infix 1 `shouldNotMatch`

shouldNotFind :: (HasCallStack, Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
shouldNotFind f l = List.find f l `shouldBe` Nothing

infix 1 `shouldNotFind`

maybePreferred :: Deprecation a -> Maybe a
maybePreferred (DeprecatedPreferred a) = Just a
maybePreferred _ = Nothing

badSortOnPair :: Ord b => (a -> b) -> Vector a -> Maybe (a, a)
badSortOnPair proj = go . V.toList
  where
    go (x : y : ys)
      | proj x <= proj y = go (y : ys)
      | otherwise = Just (x, y)
    go _ = Nothing

collapseLeft :: Either b a -> Maybe a
collapseLeft (Right a) = Just a
collapseLeft (Left _) = Nothing

mapEither :: (a -> Either b c) -> [a] -> [c]
mapEither f = mapMaybe (collapseLeft . f)

suchThatRight :: Gen a -> (a -> Either b c) -> Gen c
suchThatRight x y = x `suchThatMap` (collapseLeft . y)
