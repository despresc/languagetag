{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.BCP47.Syntax
-- Description : Language tag parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'parseBCP47' function to parse well-formed
-- (but not necessarily valid) BCP47 language tags. Note that in the
-- documentation below, "letter" means "ASCII alphabetic 'Char'" and
-- "digit" means "ASCII digit 'Char'". So a phrase like "four letters
-- long" means "a string of 'Text' of length exactly 4, consisting
-- entirely of ASCII alphabetic characters".
module Text.BCP47.Syntax
  ( -- * Parsing and rendering
    parseBCP47,
    LanguageTag,
    renderLanguageTag,
    renderLanguageTagPretty,
    toComponents,
    toComponentsSep,

    -- * Constructing values directly
    -- $valueconstruction

    -- ** Typical tags
    unsafeNormalTag,
    unsafePrivateTag,

    -- *** Irregular grandfathered tags
    enGbOed,
    iAmi,
    iBnn,
    iDefault,
    iEnochian,
    iHak,
    iKlingon,
    iLux,
    iMingo,
    iNavajo,
    iPwn,
    iTao,
    iTay,
    iTsu,
    sgnBeFr,
    sgnBeNl,
    sgnChDe,

    -- ** Regular grandfathered tags
    artLojban,
    celGaulish,
    noBok,
    noNyn,
    zhGuoyu,
    zhHakka,
    zhMin,
    zhMinNan,
    zhXiang,

    -- * Working with tags
    compareTag,
    equalTag,

    -- * Error types
    Err (..),
    ErrMessage (..),
    Component (..),

    -- * Utilities
    isBCP47Char,
    propercase

  )
where

import Data.Char
  ( isAsciiLower,
    isAsciiUpper,
    isDigit,
  )
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.BCP47.Internal.Syntax

{-
TODO:

- spin out the try* functions into their own things
- check that the actual syntax documentation is correct and
  implemented correctly (and maybe change the letter/digit
  terminology?)
- write renderer and check round-tripping
- write pretty renderer and check correctness in the proper direction
- qualify exports (don't export M or the M parsers, certainly)
- better errors - might like to say something like: "subtag whatever
  is not one of <stuff>, at <location>", but in a data type of course
- consider not caring about exact re-rendering (who would care, after all?)
- put the regular/irregular symbols in their own internal module so
  that their documentation can be generated from the IANA.
- if we move to pretty-parsing (i.e. parse-and-case at the same time),
  might be good to have a "lax" module that simply verifies that the
  tag is valid, perhaps outputting the rough components (something
  like what one of the toComponents* produces).

benchmarks, including:

- faster lower, upper, title (hence the different names)
- whether the current parseIrregularI is worth it
- faster character recognition in isAsciiAlphaNum and related
- different branching strategies at the beginning (analyzing the first
  letter differently, parsing the grandfathered tags faster based on
  single-letter comparisons...)
- simpler parseNormalish and others (e.g. not treating
  RegularGrandfathered separately and instead just doing an ==
  or subset comparison at the very end of parsing).
- more exotic internal representation (shortbytestring, raw word8, whatever). consider

  data Smallish = Smallish Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8

  (i.e. storing the length then the content. oh, but that's not
   word-aligned?  could test it, anyway). We could also use "" instead
   of Nothing for various strings, which would help.

  consider unfoldrN from bytestring!

tests:
- check roundtripping of renderer
- check pretty renderer is correct in the right direction
- check case insensitivity of parsing
- check parsing of all grandfathered tags explicitly (case insensitively!)
- check that regular grandfathered tags forming an initial segment of
  a tag get parsed properly
- check that irregular grandfathered tags with any components after
  them are never parsed
-}

{-

Make LangugeTag opaque? Could be freer to change the internal
representations of things if we do. Unsure. Would want various unsafe*
functions to convert directly to a tag. Seems okay?

Could change the parser so that we pass around an (a -> ... ->
LanguageTag) or something.

Should probably remove the Tag: Description: stuff from the
IrregularGrandfathered and leave only the tag itself (and link to the
spec and whatever auto-generated function produces the preferred
values of the regular grandfathered tags). Otherwise the documentation
might get out of sync with the actual registry. Also check that I
don't do anything like that in the rest of the documentation while I'm
giving the documentation a once over.

For the irregulars, could instead wait until we get an error at the
end, then attempt to parse them as irregular. Probably nicer!

-}

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | An error that may occur during parsing
data Err = Err
  { -- | the start of the tag where the error occurred
    errPos :: Int,
    -- | the error itself
    errMessage :: ErrMessage
  }
  deriving (Eq, Ord, Show, Read)

-- | The content of an error message
data ErrMessage
  = -- | input was empty
    ErrEmpty
  | -- | invalid tag and the last thing that was parsed correctly
    ErrBadTag !Text !Component
  | -- | empty private use section
    ErrPrivateEmpty
  | -- | empty extension section
    ErrExtensionEmpty
  | -- | did not have exactly one tag after an @i-@
    ErrIrregINum
  deriving (Eq, Ord, Show, Read)

type InputStream = [(Text, Int)]

guardTag :: Component -> (Text -> Bool) -> Int -> Text -> Either Err ()
guardTag e p pos t
  | p t = Right ()
  | otherwise = Left $ Err pos $ ErrBadTag t e

-- Split the input, also keeping track of the start positions of the
-- tags.
inputSplit :: Text -> InputStream
inputSplit = List.unfoldr go . (,) 0 . T.split (== '-')
  where
    go (!pos, tag : tags) = Just ((tag, pos), (pos + T.length tag + 1, tags))
    go (_, []) = Nothing

-- | Parse a syntactically well-formed BCP47 language tag.
parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 = go . inputSplit
  where
    go ts = catchIrregulars ts $ parseBCP47' ts

    -- FIXME: Good enought for now
    catchIrregulars _ (Right !a) = Right a
    catchIrregulars ts e@(Left (Err _ (ErrBadTag _ cmp))) =
      case cmp of
        Cbeginning
          | ((x, _) : xs) <- ts,
            ciEq' x "i" ->
            parseIrregI x xs
        Cregion
          | [(x, _), (y, _), (z, _)] <- ts,
            ciEq' x "en" && ciEq' y "gb" && ciEq' z "oed" ->
            pure $ IrregularGrandfathered $ EnGBoed x y z
          | [(x, _), (y, _), (z, _)] <- ts,
            ciEq' x "sgn" && ciEq' y "be" && ciEq' z "fr" ->
            pure $ IrregularGrandfathered $ SgnBEFR x y z
          | [(x, _), (y, _), (z, _)] <- ts,
            ciEq' x "sgn" && ciEq' y "be" && ciEq' z "nl" ->
            pure $ IrregularGrandfathered $ SgnBENL x y z
          | [(x, _), (y, _), (z, _)] <- ts,
            ciEq' x "sgn" && ciEq' y "ch" && ciEq' z "de" ->
            pure $ IrregularGrandfathered $ SgnCHDE x y z
        _ -> e
    catchIrregulars _ e = e

-- TODO: experimental
propercase :: LanguageTag -> LanguageTag
{-
propercase (NormalTag (Normal l s r v e p))
  = NormalTag $ Normal (fromLang l) (fmap title s) (fmap upper r) (lower <$> v) (lower <$> e) (fmap lower <$> p)
  where
    fromLang (Language l
-}
propercase (PrivateTag (PrivateUse _ (st NE.:| sts))) = PrivateTag $ PrivateUse False new
  where
    !st' = lower st
    con !b = st' NE.:| b
    new = go con sts
    strictColon !x !xs = x : xs
    go !l (t:ts) = let t' = lower t in t' `seq` go (l . (strictColon t')) ts
    go l [] = l []
propercase x = x

parseBCP47' :: InputStream -> Either Err LanguageTag
parseBCP47' ((t, _) : ts)
  | len == 0 = Left $ Err 0 ErrEmpty
  | len == 1 = case T.head t of
    'x' -> PrivateTag . PrivateUse False <$> parsePrivate 0 ts
    'X' -> PrivateTag . PrivateUse True <$> parsePrivate 0 ts
    _ -> Left $ Err 0 $ ErrBadTag t Cbeginning
  | len >= 2 && len <= 8 = parseNormalish t ts
  | otherwise = Left $ Err 0 $ ErrBadTag t Cbeginning
  where
    len = T.length t
parseBCP47' [] = Left $ Err 0 ErrEmpty

-- Takes the start of the private section for errors
parsePrivate :: Int -> InputStream -> Either Err (NonEmpty Text)
parsePrivate !_ ((x, xpos) : xs) = do
  guardPrivate xpos x
  let con !b = x NE.:| b
  go con xs
  where
    guardPrivate = guardTag Cprivateuse $ \t ->
      T.length t >= 1
        && T.length t <= 8
        && T.all isAsciiAlphaNum t
    go !l ((t, pos) : ts) = do
      guardPrivate pos t
      go (l . (lower t :)) ts
    go !l ![]
      = let l' = l []
        in forcing l' `seq` (pure l')
    forcing (x NE.:| xs) = x `seq` forcing' xs
    forcing' (x : xs) = x `seq` forcing' xs
    forcing' [] = ()
parsePrivate privatestart [] = Left $ Err privatestart ErrPrivateEmpty
{-# INLINE parsePrivate #-}

-- TODO: the tao/etc stuff may not be more efficient, because of O(n)
-- text indexing.
parseIrregI :: Text -> InputStream -> Either Err LanguageTag
parseIrregI inittag [(t, pos)]
  | T.null t = bad
  | otherwise = case T.head t of
    'a' | T.drop 1 t == "mi" -> good Iami
    'b' | T.drop 1 t == "nn" -> good Ibnn
    'd' | T.drop 1 t == "efault" -> good Idefault
    'e' | T.drop 1 t == "nochian" -> good Ienochian
    'h' | T.drop 1 t == "ak" -> good Ihak
    'k' | T.drop 1 t == "lingon" -> good Iklingon
    'l' | T.drop 1 t == "ux" -> good Ilux
    'm' | T.drop 1 t == "ingo" -> good Imingo
    'n' | T.drop 1 t == "avajo" -> good Inavajo
    'p' | T.drop 1 t == "wn" -> good Ipwn
    't'
      | T.length t == 3 -> case T.index t 2 of
        'o' | T.index t 1 == 'a' -> good Itao
        'y' | T.index t 1 == 'a' -> good Itay
        'u' | T.index t 1 == 's' -> good Itsu
        _ -> bad
    _ -> bad
  where
    bad = Left $ Err pos $ ErrBadTag t CirregI
    good f = pure $ IrregularGrandfathered $ f inittag t
parseIrregI _ _ = Left $ Err 0 ErrIrregINum
{-# INLINE parseIrregI #-}

class Finishing a where
  finish :: a -> LanguageTag

instance
  Finishing
    ( Maybe ExtendedLanguage ->
      Maybe Script ->
      Maybe Region ->
      [Variant] ->
      [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f Nothing Nothing Nothing [] [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( Maybe Text ->
      Maybe Script ->
      Maybe Region ->
      [Variant] ->
      [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f Nothing Nothing Nothing [] [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( Maybe FurtherExtension ->
      Maybe Script ->
      Maybe Region ->
      [Variant] ->
      [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f Nothing Nothing Nothing [] [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( Maybe Script ->
      Maybe Region ->
      [Variant] ->
      [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f Nothing Nothing [] [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( Maybe Region ->
      [Variant] ->
      [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f Nothing [] [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( [Variant] ->
      [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f [] [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( [Extension] ->
      Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f [] Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( Maybe PrivateUse ->
      LanguageTag
    )
  where
  finish f = f Nothing
  {-# INLINE finish #-}

instance
  Finishing
    ( [Text] ->
      LanguageTag
    )
  where
  finish f = f []
  {-# INLINE finish #-}

instance Finishing LanguageTag where
  finish = id
  {-# INLINE finish #-}

-- TODO: better docs

-- | The component just before what we're trying to parse
data Component
  = -- | just started
    Cbeginning
  | -- | primary language tag
    Cprimary
  | -- | first language extension
    Clext1
  | -- | second language extension
    Clext2
  | -- | the entire language tag
    Clanguage
  | -- | script tag
    Cscript
  | -- | region tag
    Cregion
  | -- | variant tag
    Cvariant
  | -- | extension tag
    Cextension
  | -- | private use tag
    Cprivateuse
  | -- | tag right after an initial @i-@
    CirregI
  deriving (Eq, Ord, Show, Read)

-- case-insensitive match
-- TODO: optimize for ascii
ciEq :: Text -> Text -> Bool
ciEq x y = T.toLower x == T.toLower y

-- case-insensitive match when the second part is known to be ascii
-- lower case
ciEq' :: Text -> Text -> Bool
ciEq' x y = T.toLower x == T.toLower y

lower :: Text -> Text
lower !t = T.toLower t

upper :: Text -> Text
upper = T.toUpper

title :: Text -> Text
title = T.toTitle

parseNormalish :: Text -> InputStream -> Either Err LanguageTag
parseNormalish langtag othersubs
  | T.length langtag <= 3 = do
    guardTag Cbeginning (T.all isAsciiAlpha) 0 langtag
    tryGrandPrimary langtag othersubs
  | otherwise = do
    guardTag Cbeginning (T.all isAsciiAlphaNum) 0 langtag
    case othersubs of
      ((sometag, somepos) : othersubs') ->
        tryScript
          Cprimary
          (initcon $ Language langtag Nothing)
          sometag
          somepos
          othersubs'
      [] ->
        pure $
          initcon
            (Language langtag Nothing)
            Nothing
            Nothing
            []
            []
            Nothing
  where
    initcon l s r v e p = NormalTag $ Normal l s r v e p

    tryGrandPrimary t ts = case T.head t of
      'z' | ciEq' (T.drop 1 t) "h" -> tryZh (ts :: InputStream)
      'a' | ciEq' (T.drop 1 t) "rt" -> tryArt ts
      'c' | ciEq' (T.drop 1 t) "el" -> tryCel ts
      'n' | ciEq' (T.drop 1 t) "o" -> tryNo ts
      'Z' | ciEq' (T.drop 1 t) "h" -> tryZh ts
      'A' | ciEq' (T.drop 1 t) "rt" -> tryArt ts
      'C' | ciEq' (T.drop 1 t) "el" -> tryCel ts
      'N' | ciEq' (T.drop 1 t) "o" -> tryNo ts
      _ -> mfinish (initcon . Language t) tryLext1 ts
      where
        tryZh ((x, xpos) : xs) = case T.head x of
          'g'
            | null xs,
              ciEq' (T.drop 1 x) "uoyu" ->
              pure $ RegularGrandfathered $ Zhguoyu t x
          'h'
            | null xs,
              ciEq' (T.drop 1 x) "akka" ->
              pure $ RegularGrandfathered $ Zhhakka t x
          'm'
            | ciEq' (T.drop 1 x) "in" -> case xs of
              [] -> pure $ RegularGrandfathered $ Zhmin t x
              [(x', _)]
                | ciEq' x' "nan" ->
                  pure $
                    RegularGrandfathered $ Zhminnan t x x'
              _ -> tryLext1 (initcon . Language t) x xpos xs
          'x'
            | null xs,
              ciEq' (T.drop 1 x) "iang" ->
              pure $ RegularGrandfathered $ Zhxiang t x
          'G'
            | null xs,
              ciEq' (T.drop 1 x) "uoyu" ->
              pure $ RegularGrandfathered $ Zhguoyu t x
          'H'
            | null xs,
              ciEq' (T.drop 1 x) "akka" ->
              pure $ RegularGrandfathered $ Zhhakka t x
          'M'
            | ciEq' (T.drop 1 x) "in" -> case xs of
              [] -> pure $ RegularGrandfathered $ Zhmin t x
              [(x', _)]
                | ciEq' x' "nan" ->
                  pure $
                    RegularGrandfathered $ Zhminnan t x x'
              _ -> tryLext1 (initcon . Language t) x xpos xs
          'X'
            | null xs,
              ciEq' (T.drop 1 x) "iang" ->
              pure $ RegularGrandfathered $ Zhxiang t x
          _ -> tryLext1 (initcon . Language t) x xpos xs
        tryZh [] = pure $ finish $ initcon . Language t

        tryArt [(x, _)]
          | ciEq' x "lojban" = pure $ RegularGrandfathered $ Artlojban t x
        tryArt _ = mfinish (initcon . Language t) tryLext1 ts

        tryCel [(x, _)]
          | ciEq' x "gaulish" = pure $ RegularGrandfathered $ Celgaulish t x
        tryCel _ = mfinish (initcon . Language t) tryLext1 ts

        tryNo [(x, _)] = case T.head x of
          'b' | ciEq' (T.drop 1 x) "ok" -> pure $ RegularGrandfathered $ Nobok t x
          'n' | ciEq' (T.drop 1 x) "yn" -> pure $ RegularGrandfathered $ Nonyn t x
          _ -> mfinish (initcon . Language t) tryLext1 ts
        tryNo _ = mfinish (initcon . Language t) tryLext1 ts

    tryLext1 con t pos ts
      | T.length t == 3,
        T.all isAsciiAlpha t =
        mfinish (con . Just . ExtendedLanguage t) tryLext2 ts
      | otherwise =
        tryScript Cprimary (con Nothing) t pos ts

    tryLext2 con t pos ts
      | T.length t == 3,
        T.all isAsciiAlpha t =
        mfinish (con . Just . (,) t) tryLext3 ts
      | otherwise =
        tryScript Clext1 (con Nothing) t pos ts

    tryLext3 con t pos ts
      | T.length t == 3,
        T.all isAsciiAlpha t =
        mfinish (con $ Just t) (tryScript Clanguage) ts
      | otherwise =
        tryScript Clext2 (con Nothing) t pos ts

    tryScript clast con t pos ts
      | T.length t == 4,
        T.all isAsciiAlpha t =
        mfinish (con $ Just t) (tryRegion Cscript) ts
      | otherwise = tryRegion clast (con Nothing) t pos ts

    tryRegion clast con t pos ts = case mreg of
      Just !reg -> mfinish (con $ Just reg) (tryVariant Cregion) ts
      Nothing -> tryVariant clast (con Nothing) t pos ts
      where
        mreg
          | T.length t == 2,
            T.all isAsciiAlpha t =
            Just t
          | T.length t == 3,
            T.all isDigit t =
            Just t
          | otherwise = Nothing

    tryVariant clast con t pos ts
      | T.length t == 4,
        isDigit (T.head t),
        T.all isAsciiAlphaNum (T.drop 1 t) =
        mfinish (con . (t :)) (tryVariant Cvariant) ts
      | T.length t >= 5,
        T.length t <= 8,
        T.all isAsciiAlphaNum (T.drop 1 t) =
        mfinish (con . (t :)) (tryVariant Cvariant) ts
      | otherwise = trySingleton clast (con []) t pos ts

    trySingleton clast con t pos ts
      | T.length t == 1 = case T.head t of
        'x' -> tryPrivateUse (con [] . Just . PrivateUse False) pos ts
        'X' -> tryPrivateUse (con [] . Just . PrivateUse True) pos ts
        c | isAsciiAlphaNum c -> tryExtension (\ne -> con . (Extension c ne :)) pos ts
        _ -> Left $ Err pos $ ErrBadTag t clast
      | otherwise = Left $ Err pos $ ErrBadTag t clast

    isExtensionText t =
      T.length t >= 2
        && T.length t <= 8
        && T.all isAsciiAlphaNum t

    tryExtension con _ ((t, pos) : ts)
      | isExtensionText t = mfinish (con . (t NE.:|)) tryExtensionPart ts
      | otherwise = Left $ Err pos $ ErrBadTag t Cextension
    tryExtension _ !oldpos [] = Left $ Err oldpos ErrExtensionEmpty

    tryExtensionPart con t pos ts
      | isExtensionText t = mfinish (con . (t :)) tryExtensionPart ts
      | otherwise = Left $ Err pos $ ErrBadTag t Cextension

    tryPrivateUse con _ ((t, pos) : ts)
      | isExtensionText t = mfinish (con . (t NE.:|)) tryPrivateUsePart ts
      | otherwise = Left $ Err pos $ ErrBadTag t Cprivateuse
    tryPrivateUse _ !oldpos [] = Left $ Err oldpos ErrPrivateEmpty

    tryPrivateUsePart con t pos ts
      | isExtensionText t = mfinish (con . (t :)) tryPrivateUsePart ts
      | otherwise = Left $ Err pos $ ErrBadTag t Cprivateuse
{-# INLINE parseNormalish #-}

mfinish ::
  Finishing a =>
  a ->
  (a -> Text -> Int -> InputStream -> Either Err LanguageTag) ->
  InputStream ->
  Either Err LanguageTag
mfinish x _ [] = pure $ finish x
mfinish x f ((t, pos) : ts) = f x t pos ts
{-# INLINE mfinish #-}

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

-- Keep extensions and private use separate
toComponents' ::
  LanguageTag ->
  Either (NonEmpty Text) (NonEmpty Text, [(Char, NonEmpty Text)], [Text])
toComponents' (NormalTag (Normal l s r v e p)) =
  Right
    ( fromLang l $ toList s <> toList r <> v,
      fromExtension <$> e,
      maybe [] fromPrivate p
    )
  where
    fromLang (Language t me) x = t NE.:| maybe x (fromExtended x) me
    fromLang (LanguageOther t) x = t NE.:| x
    fromExtended x (ExtendedLanguage t mf) = t : maybe x (fromFurther x) mf
    fromFurther x (t, mt) = t : (toList mt <> x)
    fromExtension (Extension c ts) = (c, ts)
    fromPrivate (PrivateUse b ts)
      | b = "X" : toList ts
      | otherwise = "x" : toList ts
toComponents' (PrivateTag (PrivateUse b ps))
  | b = Left $ NE.cons "X" ps
  | otherwise = Left $ NE.cons "x" ps
toComponents' (RegularGrandfathered t) = case t of
  _ -> undefined
toComponents' (IrregularGrandfathered t) = case t of
  _ -> undefined

-- | Break a language tag down into its individual subtags.
toComponents :: LanguageTag -> NonEmpty Text
toComponents lt = case toComponents' lt of
  Left x -> x
  Right (l NE.:| xs, es, ps) -> l NE.:| (xs <> concatMap fromExtension es <> ps)
  where
    fromExtension (c, t) = T.singleton c : toList t

-- | Break a language tag down into its individual subtags, keeping
-- the extension and private use sections separate.
toComponentsSep :: LanguageTag -> ([Text], [(Char, NonEmpty Text)], [Text])
toComponentsSep lt = case toComponents' lt of
  Left x -> ([], [], toList x)
  Right (x, y, z) -> (toList x, y, z)

-- | Render a 'LanguageTag' as-is. See also 'renderLanguageTagPretty'.
renderLanguageTag :: LanguageTag -> Text
renderLanguageTag = T.intercalate "-" . toList . toComponents

-- | Render a 'LanguageTag' according to the BCP47 tag formatting
-- guidelines.
renderLanguageTagPretty :: LanguageTag -> Text
renderLanguageTagPretty = T.intercalate "-" . go . toComponents'
  where
    go (Left x) = toList $ lower <$> x
    go (Right (l NE.:| xs, es, ps)) =
      lower l :
      ( (treatLater <$> xs)
          <> (concatMap fromExtension es)
          <> (lower <$> ps)
      )
    treatLater l
      | T.length l == 2 = upper l
      | T.length l == 4 = title l
      | otherwise = lower l
    fromExtension (c, xs) = fmap lower $ T.singleton c : toList xs

----------------------------------------------------------------
-- Value construction
----------------------------------------------------------------

-- $valueconstruction

-- | Construct a normal tag from its components. The well-formedness
-- of the input is /not/ checked; if an ill-formed tag component is
-- given then certain functions in this library may behave
-- unpredictably when given the resulting tag. See
-- <https://tools.ietf.org/html/bcp47#section-2.1> for the exact
-- grammar. A summary of the rules to follow to ensure that the input
-- is well-formed:
--
-- * Primary language: between two and eight letters.
--
-- * Extended language: exactly three letters. If the primary language
--   is four letters or longer then the extended language must be
--   @Nothing@.
--
-- * Script: exactly four letters.
--
-- * Region: either exactly two letters or exactly three digits.
--
-- * Variant: between four and eight letters or digits. If the variant
--   has length four then it must begin with a digit.
--
-- * Extension sections: the character must be a digit or a letter
--   other than @x@ or @X@ and the 'Text' values must be between two
--   and eight digits or letters long.
--
-- * Private use subtags: between one and eight digits or letters.
unsafeNormalTag ::
  -- | primary language
  Text ->
  -- | extended language
  Maybe Text ->
  -- | script
  Maybe Text ->
  -- | region
  Maybe Text ->
  -- | variant subtags
  [Text] ->
  -- | extension sections
  [(Char, NonEmpty Text)] ->
  -- | private use subtags
  [Text] ->
  LanguageTag
unsafeNormalTag l me ms mr mv es pus =
  NormalTag $
    Normal
      { language = Language l ((`ExtendedLanguage` Nothing) <$> me),
        script = ms,
        region = mr,
        variants = mv,
        extensions = uncurry Extension <$> es,
        privateUse = PrivateUse False <$> NE.nonEmpty pus
      }

-- | A private use tag starts with @x-@, which is followed by one or
-- more private use subtags, each of which is between one and eight
-- digits or letters long. This function constructs such a tag given
-- those private use subtags. The condition on the 'Text' values in
-- the input list is not checked, and if it does not hold then certain
-- functions in this library may behave unpredictably when given the
-- resulting tag.
unsafePrivateTag :: NonEmpty Text -> LanguageTag
unsafePrivateTag = PrivateTag . PrivateUse False

-- | Tag @en-GB-oed@. English, Oxford English Dictionary
-- spelling. Deprecated. Preferred value: @en-GB-oxendict@.
enGbOed :: LanguageTag
enGbOed = IrregularGrandfathered $ EnGBoed "en" "GB" "oed"

-- | Tag @i-ami@. Amis. Deprecated. Preferred value: @ami@.
iAmi :: LanguageTag
iAmi = IrregularGrandfathered $ Iami "i" "ami"

-- | Tag @-ibnn@. Bunun. Deprecated. Preferred value: @bnn@.
iBnn :: LanguageTag
iBnn = IrregularGrandfathered $ Ibnn "i" "bnn"

-- | Tag @i-default@. Default Language.
iDefault :: LanguageTag
iDefault = IrregularGrandfathered $ Idefault "i" "default"

-- | Tag @i-enochian@. Enochian. Deprecated.
iEnochian :: LanguageTag
iEnochian = IrregularGrandfathered $ Ienochian "i" "enochian"

-- | Tag @i-hak@. Hakka. Deprecated. Preferred value: @hak@.
iHak :: LanguageTag
iHak = IrregularGrandfathered $ Ihak "i" "hak"

-- | Tag @i-klingon@. Klingon. Deprecated. Preferred value: @tlh@.
iKlingon :: LanguageTag
iKlingon = IrregularGrandfathered $ Iklingon "i" "klingon"

-- | Tag @i-lux@. Luxembourgish. Deprecated. Preferred value: @lb@.
iLux :: LanguageTag
iLux = IrregularGrandfathered $ Ilux "i" "lux"

-- | Tag @i-mingo@. Mingo.
iMingo :: LanguageTag
iMingo = IrregularGrandfathered $ Imingo "i" "mingo"

-- | Tag @i-navajo@. Navajo. Deprecated. Preferred value: @nv@.
iNavajo :: LanguageTag
iNavajo = IrregularGrandfathered $ Inavajo "i" "navajo"

-- | Tag @i-pwn@. Paiwan. Deprecated. Preferred value: @pwn@.
iPwn :: LanguageTag
iPwn = IrregularGrandfathered $ Ipwn "i" "pwn"

-- | Tag @i-tao@. Tao. Deprecated. Preferred value: @i-tao@.
iTao :: LanguageTag
iTao = IrregularGrandfathered $ Itao "i" "tao"

-- | Tag @i-tay@. Tayal. Deprecated. Preferred value: @i-tay@.
iTay :: LanguageTag
iTay = IrregularGrandfathered $ Itay "i" "tay"

-- | Tag @i-tsu@. Tsou. Deprecated. Preferred value: @i-tsu@.
iTsu :: LanguageTag
iTsu = IrregularGrandfathered $ Itsu "i" "tsu"

-- | Tag @sgn-BE-FR@. Belgian-French Sign
-- Language. Deprecated. Preferred value: @sfb@.
sgnBeFr :: LanguageTag
sgnBeFr = IrregularGrandfathered $ SgnBEFR "sgn" "BE" "FR"

-- | Tag @sgn-BE-NL@. Belgian-Flemish Sign
-- Language. Deprecated. Preferred value: @vgt@.
sgnBeNl :: LanguageTag
sgnBeNl = IrregularGrandfathered $ SgnBENL "sgn" "BE" "NL"

-- | Tag @sgn-CH-DE@. Swiss-German Sign
-- Language. Deprecated. Preferred value: @sgg@.
sgnChDe :: LanguageTag
sgnChDe = IrregularGrandfathered $ SgnCHDE "sgn" "CH" "DE"

-- | Tag @art-lojban@. Lojban. Deprecated. Preferred value: @jbo@.
artLojban :: LanguageTag
artLojban = RegularGrandfathered $ Artlojban "art" "lojban"

-- | Tag @cel-gaulish@. Gaulish. Deprecated. See @xcg@ (Cisalpine
-- Gaulish), @xga@ (Galatian), @xtg@ (Transalpine Gaulish).
celGaulish :: LanguageTag
celGaulish = RegularGrandfathered $ Celgaulish "cel" "gaulish"

-- | Tag @no-bok@. Norwegian Bokmal. Deprecated. Preferred value:
-- @nb@.
noBok :: LanguageTag
noBok = RegularGrandfathered $ Nobok "no" "bok"

-- | Tag @no-nyn@. Norwegian Nynorsk. Deprecated. Preferred value:
-- @nn@.
noNyn :: LanguageTag
noNyn = RegularGrandfathered $ Nonyn "no" "nyn"

-- | Tag @zh-guoyu@. Mandarin or Standard
-- Chinese. Deprecated. Preferred value: @cmn@.
zhGuoyu :: LanguageTag
zhGuoyu = RegularGrandfathered $ Zhguoyu "zh" "guoyu"

-- | Tag @zh-hakka@. Hakka. Deprecated. Preferred value: @hak@.
zhHakka :: LanguageTag
zhHakka = RegularGrandfathered $ Zhhakka "zh" "hakka"

-- | Tag @zh-min@. Min, Fuzhou, Hokkien, Amoy, or
-- Taiwanese. Deprecated. See @cdo@ (Min Dong Chinese), @cpx@ (Pu-Xian
-- Chinese), @czo@ (Min Zhong Chinese), @mnp@ (Min Bei Chinese), @nan@
-- (Min Nan Chinese).
zhMin :: LanguageTag
zhMin = RegularGrandfathered $ Zhmin "zh" "min"

-- | Tag @zh-min-nan@. Minnan, Hokkien, Amoy, Taiwanese, Southern Min,
-- Southern Fujian, Hoklo, Southern Fukien,
-- Ho-lo. Deprecated. Preferred value: @nan@.
zhMinNan :: LanguageTag
zhMinNan = RegularGrandfathered $ Zhminnan "zh" "min" "nan"

-- | Tag @zh-xiang@. Xiang or Hunanese. Deprecated. Preferred value:
-- @hsn@.
zhXiang :: LanguageTag
zhXiang = RegularGrandfathered $ Zhxiang "zh" "xiang"

----------------------------------------------------------------
-- Functions on tags
----------------------------------------------------------------

-- | @'compareTag' x y@ is @Just LT@ when @x@ is a strict prefix of
-- @y@, is @Just GT@ when @y@ is a strict prefix of @x@, and @Just EQ@
-- when they are equal. All comparisons are performed at the subtag
-- level (so @en@ is a prefix of @en-GB@ but not of @ena@) and
-- case-insensitively (so @en@ is equal to @EN@).
compareTag :: LanguageTag -> LanguageTag -> Maybe Ordering
compareTag (NormalTag x) (NormalTag y) = undefined
compareTag (PrivateTag x) (PrivateTag y) = undefined
compareTag (RegularGrandfathered x) (NormalTag y) = undefined
compareTag (NormalTag x) (RegularGrandfathered y) = undefined
compareTag (RegularGrandfathered x) (RegularGrandfathered y) = undefined
compareTag (IrregularGrandfathered x) (IrregularGrandfathered y) = undefined
compareTag _ _ = Nothing

-- | Test if two tags are equal. The comparison is performed
-- case-insensitively.
equalTag :: LanguageTag -> LanguageTag -> Bool
equalTag x y = case compareTag x y of
  Just EQ -> True
  _ -> False

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

-- | @isBCP47Char c@ is @True@ if @c@ is an ASCII alphanumeric
-- character or a dash @\'-\'@ character.
isBCP47Char :: Char -> Bool
isBCP47Char c = isAsciiAlphaNum c || c == '-'

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isDigit c || isAsciiAlpha c

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

{- TODO: may not need
-- | Convert a 'RegularGrandfathered' tag into a 'Normal' tag. The
-- correctness of the tag (e.g. that in @'Artlojban' x y@ the @x@ is
-- @art@ up to a change of case) is not checked.
regularToNormal :: RegularGrandfathered -> Normal
regularToNormal = \case
  Artlojban x y -> nvar x y
  Celgaulish x y -> nvar x y
  Nobok x y -> nextl x y
  Nonyn x y -> nextl x y
  Zhguoyu x y -> nvar x y
  Zhhakka x y -> nvar x y
  Zhmin x y -> nextl x y
  Zhminnan x y z ->
    Normal
      { language =
          Language x $
            Just
              ExtendedLanguage
                { extlang = y,
                  extlangRemainder = Just (z, Nothing)
                },
        script = Nothing,
        region = Nothing,
        variants = [],
        extensions = [],
        privateUse = Nothing
      }
  Zhxiang x y -> nvar x y
  where
    nvar x y =
      Normal
        { language = Language x Nothing,
          script = Nothing,
          region = Nothing,
          variants = [y],
          extensions = [],
          privateUse = Nothing
        }
    nextl x y =
      Normal
        { language =
            Language x $
              Just
                ExtendedLanguage
                  { extlang = y,
                    extlangRemainder = Nothing
                  },
          script = Nothing,
          region = Nothing,
          variants = [],
          extensions = [],
          privateUse = Nothing
        }
-}
