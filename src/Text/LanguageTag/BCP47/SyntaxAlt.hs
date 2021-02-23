{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: remove me or integrate me with Syntax

module Text.LanguageTag.BCP47.SyntaxAlt where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.LanguageTag.Internal.BCP47.SyntaxAlt

-- | The component just before what we're trying to parse.
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

-- | An error that may occur during parsing
data Err = Err
  { -- | the start of the tag where the error occurred
    errPos :: !Int,
    -- | the section just before the erroneous tag
    errComponent :: !Component,
    -- | the error itself
    errType :: !ErrType
  }
  deriving (Eq, Ord, Show, Read)

-- TODO: could distinguish between "extremely bad" (not even a
-- alphanum/dash) and merely inappropriate
-- TODO: remember that Err 0 Cprimary ErrNeededTag can only occur
-- when the start is "x" or "i".
data ErrType
  = -- | empty input
    ErrEmpty
  | -- | character was encountered that was incorrect for the section
    ErrBadChar
  | -- | another tag was expected
    ErrNeededTag
  | -- | expecting a tag separator or the end of input
    ErrTagEnd
  | -- | invalid tag
    ErrBadTag
  | -- | an @i-@ tag should be followed by exactly one
    -- tag
    ErrIrregINum
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Pop a tag from the input stream
tagPopDetail ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, SeenChar, Text)
tagPopDetail initchar inp clast pos = case popSubtagDetail initchar inp of
  Just (x, y, z) -> Right (x, y, z)
  Nothing -> Left $ Err pos clast ErrBadChar
{-# INLINE tagPopDetail #-}

-- | Pop a tag from the input stream when we don't care about the
-- 'SeenChar'.
tagPop ::
  Char ->
  Text ->
  Component ->
  Int ->
  Either Err (Subtag, Text)
tagPop initchar inp clast pos = case popSubtag initchar inp of
  Just (s, t) -> Right (s, t)
  Nothing -> Left $ Err pos clast ErrBadChar
{-# INLINE tagPop #-}

-- returns the character immediately after the separator if there was
-- one, and Nothing on end of input.
tagSep :: Component -> Int -> Text -> Either Err (Maybe (Char, Text))
tagSep !clast !pos !inp = case T.uncons inp of
  Just (c, t)
    | c == '-' -> case T.uncons t of
      Just (c', t') -> Right $ Just (c', t')
      Nothing -> Left $ Err pos clast ErrTagEnd
    | otherwise -> Left $ Err pos clast ErrTagEnd
  Nothing -> Right Nothing
{-# INLINE tagSep #-}

mfinish ::
  Finishing a =>
  Word8 ->
  Component ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> SeenChar -> Component -> Int -> Text -> Either Err LanguageTag) ->
  Either Err LanguageTag
mfinish !len !clast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  mc <- tagSep clast pos' inp
  case mc of
    Just (c, t) ->
      tagPopDetail c t clast pos' >>= \(sbs, sc, t') ->
        pr con sbs sc clast pos' t'
    Nothing -> pure $ finish con
{-# INLINE mfinish #-}

-- TODO: better name for this and the *Detail functions?
mfinishSimple ::
  Finishing a =>
  Word8 ->
  Component ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> Component -> Int -> Text -> Either Err LanguageTag) ->
  Either Err LanguageTag
mfinishSimple !len !clast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  mc <- tagSep clast pos' inp
  case mc of
    Just (c, t) ->
      tagPop c t clast pos' >>= \(sbs, t') ->
        pr con sbs clast pos' t'
    Nothing -> pure $ finish con
{-# INLINE mfinishSimple #-}

isDigit :: SubtagChar -> Bool
isDigit (SubtagChar w) = w < 10
{-# INLINE isDigit #-}

parseBCP47 :: Text -> Either Err LanguageTag
parseBCP47 inp = case T.uncons inp of
  Just (c, t) -> catchIrregulars $ parseBCP47' c t
  Nothing -> Left $ Err 0 Cbeginning ErrEmpty
  where
    -- FIXME: sufficient for the moment
    catchIrregulars (Right a) = Right a
    catchIrregulars (Left e)
      | T.length inp == 9,
        T.toLower inp == T.pack "en-gb-oed" =
        Right $ IrregularGrandfathered EnGBoed
      | T.length inp == 10,
        T.toLower inp == T.pack "sgn-be-fr" =
        Right $ IrregularGrandfathered SgnBEFR
      | T.length inp == 10,
        T.toLower inp == T.pack "sgn-be-nl" =
        Right $ IrregularGrandfathered SgnBENL
      | T.length inp == 10,
        T.toLower inp == T.pack "sgn-ch-de" =
        Right $ IrregularGrandfathered SgnCHDE
      | otherwise = Left e

-- TODO: also test out the normal approach of 'split'ting the input beforehand
parseBCP47' :: Char -> Text -> Either Err LanguageTag
parseBCP47' !initchar !inp = tagPopDetail initchar inp Cbeginning 0 >>= parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- TODO: could be optimized a bit
    parsePrimary (st, OnlyLetter, t)
      | tagLength st == 1 =
        if subtagHead st == subtagCharx
          then PrivateTag <$> parsePrivate 0 t
          else do
            msep <- tagSep Cprimary 0 t
            case msep of
              Just (c, t') -> parseIrregularI st c t'
              Nothing -> Left $ Err 0 Cprimary ErrNeededTag
      | tagLength st >= 4 =
        mfinish
          (tagLength st)
          Cprimary
          0
          t
          (initcon st nullSubtag nullSubtag nullSubtag)
          tryScript
      | otherwise = mfinish (tagLength st) Cprimary 0 t (initcon st) (tryGrandPrimary st)
    parsePrimary _ = Left $ Err 0 Cbeginning ErrBadChar

    tryGrandPrimary st0 con st1 sc clast pos t =
      case (unSubtag st0, unSubtag st1) of
        (10618854602642030595, 13775215567545827334)
          | T.null t -> pure $ RegularGrandfathered Artlojban
        (11136205609836216323, 12271798482267799559)
          | T.null t -> pure $ RegularGrandfathered Celgaulish
        (14348468412802400258, 10892940861214031875)
          | T.null t -> pure $ RegularGrandfathered Nobok
        (14348468412802400258, 14396952477540810755)
          | T.null t -> pure $ RegularGrandfathered Nonyn
        (17775707729231347714, 12361462747483865093)
          | T.null t -> pure $ RegularGrandfathered Zhguoyu
        (17775707729231347714, 12559323919351283717)
          | T.null t -> pure $ RegularGrandfathered Zhhakka
        (17775707729231347714, 14036664507351171075) -> do
          let pos' = pos + fromIntegral (tagLength st1) + 1
          msep <- tagSep clast pos' t
          case msep of
            Nothing -> pure $ RegularGrandfathered Zhmin
            Just (c, t') -> do
              (st2, sc', t'') <- tagPopDetail c t' Clext1 pos'
              case unSubtag st2 of
                14288866086483918851
                  | T.null t'' -> pure $ RegularGrandfathered Zhminnan
                _ -> tryLext2 (con $ justSubtag st1) st2 sc' Clext1 pos' t''
        (17775707729231347714, 17206338448969957381)
          | T.null t -> pure $ RegularGrandfathered Zhxiang
        _ -> tryLext1 con st1 sc clast pos t

    tryLext1 !con st sc clast pos t
      | OnlyLetter <- sc,
        tagLength st == 3 =
        mfinish (tagLength st) Clext1 pos t (con $ justSubtag st) tryLext2
      | otherwise = tryScript (con nullSubtag nullSubtag nullSubtag) st sc clast pos t

    tryLext2 !con st sc clast pos t
      | OnlyLetter <- sc,
        tagLength st == 3 =
        mfinish (tagLength st) Clext2 pos t (con $ justSubtag st) tryLext3
      | otherwise = tryScript (con nullSubtag nullSubtag) st sc clast pos t

    tryLext3 !con st sc clast pos t
      | OnlyLetter <- sc,
        tagLength st == 3 =
        mfinish (tagLength st) Clanguage pos t (con $ justSubtag st) tryScript
      | otherwise = tryScript (con nullSubtag) st sc clast pos t

    tryScript !con st sc clast pos t
      | tagLength st == 4,
        OnlyLetter <- sc =
        mfinish (tagLength st) Cscript pos t (con $ justSubtag st) tryRegion
      | otherwise = tryRegion (con nullSubtag) st sc clast pos t

    tryRegion !con st sc clast pos t
      | tagLength st == 2 = case sc of
        OnlyLetter -> mfinishSimple (tagLength st) Cregion pos t (con $ justSubtag st) tryVariant
        _ -> Left $ Err pos clast ErrBadTag
      | tagLength st == 3 = case sc of
        OnlyDigit -> mfinishSimple (tagLength st) Cregion pos t (con $ justSubtag st) tryVariant
        _ -> Left $ Err pos clast ErrBadTag
      | otherwise = tryVariant (con nullSubtag) st clast pos t

    tryVariant !con st clast pos t
      | tagLength st == 4 =
        if isDigit $ subtagHead st
          then mfinishSimple (tagLength st) Cvariant pos t (con . strictCons st) tryVariant
          else Left $ Err pos clast ErrBadTag
      | tagLength st >= 5 =
        mfinishSimple (tagLength st) Cvariant pos t (con . strictCons st) tryVariant
      | otherwise = trySingleton (con []) st clast pos t

    trySingleton con st clast pos t
      | tagLength st /= 1 = Left $ Err pos clast ErrBadTag
      | subtagHead st == subtagCharx =
        parsePrivateUse (con []) pos t
      | otherwise = parseExtension (\ne -> con . strictCons (Extension (subtagHead st) ne)) pos t

    parsePrivateUse con pos t = do
      let pos' = pos + 2
      ms <- tagSep Cprivateuse pos' t
      case ms of
        Just (c, t') -> do
          (st, sc, t'') <- tagPopDetail c t' Cprivateuse pos'
          parsePrivateUseTag con st sc Cprivateuse pos' t''
        Nothing -> Left $ Err pos Cprivateuse ErrNeededTag

    parsePrivateUseTag con st _ _ pos t =
      mfinish (tagLength st) Cprivateuse pos t (con . strictCons st) parsePrivateUseTag

    parseExtension con pos t = do
      let pos' = pos + 2
      ms <- tagSep Cextension pos' t
      case ms of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cextension pos'
          if tagLength st >= 2
            then
              mfinishSimple
                (tagLength st)
                Cextension
                pos'
                t''
                (con . strictNE st)
                parseExtensionTag
            else Left $ Err pos Cextension ErrNeededTag
        Nothing -> Left $ Err pos Cextension ErrNeededTag

    parseExtensionTag con st _ pos t
      | tagLength st == 1 = trySingleton (con []) st Cextension pos t
      | otherwise = mfinishSimple (tagLength st) Cextension pos t (con . strictCons st) parseExtensionTag

    parseIrregularI st c t
      | st /= subtagI = Left $ Err 0 Cbeginning ErrBadChar
      | otherwise = case tagPop c t Cbeginning 0 of
        Right (st', t') -> case T.uncons t' of
          Just _ -> Left $ Err 0 Cprimary ErrIrregINum
          Nothing -> recognizeIrregI st'
        Left e -> Left e

    -- TODO: might want to test to make sure these constants remain
    -- accurate
    recognizeIrregI (Subtag n) = case n of
      10595562548319223811 -> Right $ IrregularGrandfathered Iami
      10888648367819194371 -> Right $ IrregularGrandfathered Ibnn
      11424054330861289479 -> Right $ IrregularGrandfathered Idefault
      11753452397160103944 -> Right $ IrregularGrandfathered Ienochian
      12559272723341115395 -> Right $ IrregularGrandfathered Ihak
      13473417321460531207 -> Right $ IrregularGrandfathered Iklingon
      14036711545832996869 -> Right $ IrregularGrandfathered Imingo
      14289469405371826182 -> Right $ IrregularGrandfathered Inavajo
      14964406030589493251 -> Right $ IrregularGrandfathered Ipwn
      16018318712138366979 -> Right $ IrregularGrandfathered Itao
      16019022399580143619 -> Right $ IrregularGrandfathered Itay
      16099805717896101891 -> Right $ IrregularGrandfathered Itsu
      _ -> Left $ Err 2 CirregI ErrBadTag
{-# INLINE parseBCP47' #-}

parsePrivate :: Int -> Text -> Either Err (NE.NonEmpty Subtag)
parsePrivate initpos inp = do
  ms <- tagSep Cprivateuse initpos inp
  case ms of
    Just (c, t) -> do
      (st, t') <- tagPop c t Cprivateuse initpos
      parsePrivateUseTag (strictNE st) (initpos + fromIntegral (tagLength st) + 1) t'
    Nothing -> Left $ Err initpos Cprivateuse ErrNeededTag
  where
    parsePrivateUseTag con pos t = do
      mc <- tagSep Cprivateuse pos t
      case mc of
        Just (c, t') -> do
          (st, t'') <- tagPop c t' Cprivateuse pos
          parsePrivateUseTag (con . strictCons st) (pos + fromIntegral (tagLength st) + 1) t''
        Nothing -> pure $ con []
{-# INLINE parsePrivate #-}

----------------------------------------------------------------
-- Grandfathered (sub)tag constants
----------------------------------------------------------------

subtagI :: Subtag
subtagI = Subtag 12682136550675316737
