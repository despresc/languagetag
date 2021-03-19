{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Language tag types and parser
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'parseBCP47' function to parse well-formed
-- (but not necessarily valid) BCP47 language tags as of the current
-- 2009 version. A copy of this standard is available at
-- <https://tools.ietf.org/html/bcp47>.
module Text.LanguageTag.BCP47.Syntax
  ( -- * Parsing and rendering tags
    BCP47,
    parseBCP47,
    renderBCP47,
    renderBCP47Builder,
    toSubtags,
    parseBCP47FromSubtags,

    -- * Constructing tags directly
    -- $valueconstruction

    -- ** Normal and private use tags
    unsafeNormalTag,
    unsafeFullNormalTag,
    unsafePrivateTag,

    -- ** Grandfathered tags
    -- $grandfathered
    grandfatheredSyntax,
    module Text.LanguageTag.Internal.BCP47.Registry.Grandfathered,

    -- * Errors
    SyntaxError (..),
    Pos,
    AtComponent (..),
  )
where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.LanguageTag.BCP47.Subtag hiding (SyntaxError (..))
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import Text.LanguageTag.Internal.BCP47.Registry.Grandfathered
import Text.LanguageTag.Internal.BCP47.Subtag (Subtag (..), SubtagChar (..))
import Text.LanguageTag.Internal.BCP47.Syntax

{- TODO:
- spin out the try* functions into their own functions?

- enhancement: actually attempt to parse and add the bad
  TrailingTerminator subtag to the returned con (would probably
  require some parser restructuring)

-}

-- | The parser's current location in the tag, named after the most
-- recent component that was successfully parsed, more or less. What
-- it really indicates is what subtags the parser expects to occur
-- next: 'AtPrimary' means that it expects anything from an extended
-- language subtag to the start of the private use section, while
-- 'AtPrivateUse' indicates that it expects only private use subtags.
data AtComponent
  = -- | just started
    AtBeginning
  | -- | primary language subtag
    AtPrimary
  | -- | first extended language subtag
    AtExtl1
  | -- | second extended language subtag
    AtExtl2
  | -- | the entire language subtag section
    AtLanguage
  | -- | script subtag
    AtScript
  | -- | region subtag
    AtRegion
  | -- | variant subtag
    AtVariant
  | -- | extension subtag
    AtExtension
  | -- | private use subtag
    AtPrivateUse
  | -- | subtag right after an initial @i-@
    AtIrregI
  deriving (Eq, Ord, Show)

instance NFData AtComponent where
  rnf = rwhnf

-- | A possible syntax error that may occur. The 'Pos' in the errors
-- indicates where the error occurred, which is:
--
-- * for 'UnparsableSubtag', the start of the ill-formed 'Subtag',
--   and, if applicable, the position of the invalid character inside
--   that subtag
--
-- * for 'BadSubtag', the start of the inappropriate 'Subtag'
--
-- * for 'EmptySingleton', the start of the empty extension or private
--   use section that was encountered
data SyntaxError
  = -- | encountered text that could not be parsed as a 'Subtag' (too
    -- long, or encountered a bad 'Char')
    UnparsableSubtag Pos AtComponent (Maybe (Pos, Char)) (Maybe BCP47)
  | -- | encountered a 'Subtag' that was ill-formed for its location
    BadSubtag Pos AtComponent Subtag (Maybe BCP47)
  | -- | input was empty
    EmptyInput
  | -- | a trailing @-@ terminator was found after the last subtag
    TrailingTerminator Pos AtComponent Subtag (Maybe BCP47)
  | -- | an empty extension (@Just extensionchar@) or private use
    -- section (@Nothing@) was encountered
    EmptySingleton Pos (Maybe ExtensionChar) (Maybe BCP47)
  | -- | an irregular grandfathered tag was encountered that had more
    -- input after it
    IrregNum Grandfathered
  | -- | No tag was found after an initial @i-@
    EmptyIrregI
  deriving (Eq, Ord, Show)

instance NFData SyntaxError where
  rnf (UnparsableSubtag x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w
  rnf (BadSubtag x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w
  rnf (TrailingTerminator x y z w) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w
  rnf EmptyInput = ()
  rnf (EmptySingleton x y z) = rnf x `seq` rnf y `seq` rnf z
  rnf EmptyIrregI = ()
  rnf (IrregNum x) = rnf x

-- | A position in the input 'Text' stream
type Pos = Int

----------------------------------------------------------------
-- Rendering to subtags
----------------------------------------------------------------

-- | Convert a 'BCP47' tag into its component subtags
toSubtags :: BCP47 -> NonEmpty Subtag
toSubtags (NormalTag (Normal p e1 e2 e3 s r vs es ps)) =
  p NE.:| (mapMaybe go [e1, e2, e3, s, r] <> vs <> es' <> ps')
  where
    go = maybeSubtag Nothing Just
    es' = flip concatMap es $ \(Extension c ts) ->
      extensionCharToSubtag c : NE.toList ts
    ps'
      | null ps = []
      | otherwise = subtagX : ps
toSubtags (PrivateUse x) = subtagX NE.:| NE.toList x
toSubtags (Grandfathered g) = case g of
  ArtLojban -> Subtag 14108546179528654867 NE.:| [Subtag 15690354374758891542]
  CelGaulish -> Subtag 14382069488147234835 NE.:| [Subtag 14954113284221173783]
  EnGbOed -> Subtag 14679482985414131730 NE.:| [Subtag 14954202562683731986, Subtag 16111381376313327635]
  IAmi -> Subtag 15132094747964866577 NE.:| [Subtag 14102819922971197459]
  IBnn -> Subtag 15132094747964866577 NE.:| [Subtag 14248104991419006995]
  IDefault -> Subtag 15132094747964866577 NE.:| [Subtag 14526138628724883479]
  IEnochian -> Subtag 15132094747964866577 NE.:| [Subtag 14680466211245977112]
  IHak -> Subtag 15132094747964866577 NE.:| [Subtag 15098133032806121491]
  IKlingon -> Subtag 15132094747964866577 NE.:| [Subtag 15542853518732230679]
  ILux -> Subtag 15132094747964866577 NE.:| [Subtag 15697226132455686163]
  IMingo -> Subtag 15132094747964866577 NE.:| [Subtag 15827749698417983509]
  INavajo -> Subtag 15132094747964866577 NE.:| [Subtag 15962927641447628822]
  IPwn -> Subtag 15132094747964866577 NE.:| [Subtag 16275850723642572819]
  ITao -> Subtag 15132094747964866577 NE.:| [Subtag 16827550474088480787]
  ITay -> Subtag 15132094747964866577 NE.:| [Subtag 16827638435018702867]
  ITsu -> Subtag 15132094747964866577 NE.:| [Subtag 16847869448969781267]
  NoBok -> Subtag 15977645578003677202 NE.:| [Subtag 14249204503046782995]
  NoNyn -> Subtag 15977645578003677202 NE.:| [Subtag 15989872147304546323]
  SgnBeFr -> Subtag 16690181889360658451 NE.:| [Subtag 14237004322024980498, Subtag 14828101773117358098]
  SgnBeNl -> Subtag 16690181889360658451 NE.:| [Subtag 14237004322024980498, Subtag 15974267878283149330]
  SgnChDe -> Subtag 16690181889360658451 NE.:| [Subtag 14384497209821364242, Subtag 14525234698176692242]
  ZhGuoyu -> Subtag 17699146535566049298 NE.:| [Subtag 14976579405109788693]
  ZhHakka -> Subtag 17699146535566049298 NE.:| [Subtag 15098140437866610709]
  ZhMin -> Subtag 17699146535566049298 NE.:| [Subtag 15827742560719208467]
  ZhMinNan -> Subtag 17699146535566049298 NE.:| [Subtag 15827742560719208467, Subtag 15962850549540323347]
  ZhXiang -> Subtag 17699146535566049298 NE.:| [Subtag 17412902894784479253]

-- | Parse a 'BCP47' tag from a list of subtags by first rendering the
-- list to a 'Text' tag then parsing that tag
parseBCP47FromSubtags :: NonEmpty Subtag -> Either SyntaxError BCP47
parseBCP47FromSubtags = parseBCP47 . T.intercalate "-" . NE.toList . fmap renderSubtagLower

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- | Pop a tag from the input stream, returning @Nothing@ if the
-- stream was empty
tagPop ::
  Maybe BCP47 ->
  Text ->
  AtComponent ->
  Int ->
  Either SyntaxError (Maybe (Subtag, Text))
tagPop mcon inp atlast pos = case popSubtag inp of
  Right (st, t) -> Right $ Just (st, t)
  Left Sub.EmptyInput -> Right Nothing
  Left Sub.TagTooLong -> Left $ UnparsableSubtag pos atlast Nothing (finish <$> mcon)
  Left (Sub.TrailingTerminator st) -> Left $ TrailingTerminator pos atlast st mcon
  Left (Sub.InvalidChar n c) -> Left $ UnparsableSubtag pos atlast (Just (pos + n, c)) mcon

mfinish ::
  Finishing a =>
  Word8 ->
  AtComponent ->
  Int ->
  Text ->
  a ->
  (a -> Subtag -> AtComponent -> Int -> Text -> Either SyntaxError BCP47) ->
  Either SyntaxError BCP47
mfinish !len !atlast !pos !inp !con !pr = do
  let pos' = fromIntegral len + pos + 1
  let con' = finish con
  mst <- tagPop (Just con') inp atlast pos'
  case mst of
    Just (st, t) -> pr con st atlast pos' t
    Nothing -> pure con'

isDigit :: SubtagChar -> Bool
isDigit c = w >= 48 && w <= 57
  where
    w = unwrapChar c

-- | Parse a BCP47 language tag
parseBCP47 :: Text -> Either SyntaxError BCP47
parseBCP47 inp = do
  mst <- tagPop Nothing inp AtBeginning 0
  case mst of
    Just (stg, t) -> catchIrregulars $ parseBCP47' stg t
    Nothing -> Left EmptyInput
  where
    -- FIXME: sufficient for the moment, but might want to: 1. be more
    -- efficient; 2. throw a special error when these tags occur as a
    -- strict prefix of the input, as with the @i-@-type tags
    catchIrregulars (Right a) = Right a
    catchIrregulars (Left e)
      | T.length inp == 9 = testIrregs
      | otherwise = Left e
      where
        testIrregs
          | T.toLower inp == "en-gb-oed" =
            Right $ Grandfathered EnGbOed
          | T.toLower inp == "sgn-be-fr" =
            Right $ Grandfathered SgnBeFr
          | T.toLower inp == "sgn-be-nl" =
            Right $ Grandfathered SgnBeNl
          | T.toLower inp == "sgn-ch-de" =
            Right $ Grandfathered SgnChDe
          | otherwise = Left e

parseBCP47' :: Subtag -> Text -> Either SyntaxError BCP47
parseBCP47' = parsePrimary
  where
    initcon l e1 e2 e3 s r v e p = NormalTag $ Normal l e1 e2 e3 s r v e p

    -- TODO: could be optimized a bit
    parsePrimary !st !t
      | containsDigit st = Left $ BadSubtag 0 AtBeginning st Nothing
      | subtagLength st == 1 =
        if subtagHead st == subtagCharx
          then parsePrivateTag t
          else parseIrregularI st t
      | subtagLength st >= 4 =
        mfinish
          (subtagLength st)
          AtPrimary
          0
          t
          (initcon st nullSubtag nullSubtag nullSubtag)
          tryScript
      | otherwise = mfinish (subtagLength st) AtPrimary 0 t (initcon st) (tryGrandPrimary st)

    tryGrandPrimary st0 con st1 atlast pos t =
      case (unwrapSubtag st0, unwrapSubtag st1) of
        (14108546179528654867, 15690354374758891542)
          | T.null t -> pure $ Grandfathered ArtLojban
        (14382069488147234835, 14954113284221173783)
          | T.null t -> pure $ Grandfathered CelGaulish
        (15977645578003677202, 14249204503046782995)
          | T.null t -> pure $ Grandfathered NoBok
        (15977645578003677202, 15989872147304546323)
          | T.null t -> pure $ Grandfathered NoNyn
        (17699146535566049298, 14976579405109788693)
          | T.null t -> pure $ Grandfathered ZhGuoyu
        (17699146535566049298, 15098140437866610709)
          | T.null t -> pure $ Grandfathered ZhHakka
        (17699146535566049298, 15827742560719208467) -> do
          let pos' = pos + fromIntegral (subtagLength st1) + 1
          mst <- tagPop (Just $ Grandfathered ZhMin) t AtExtl1 pos'
          case mst of
            Nothing -> pure $ Grandfathered ZhMin
            Just (st2, t') -> case unwrapSubtag st2 of
              15962850549540323347
                | T.null t' -> pure $ Grandfathered ZhMinNan
              _ -> tryLext2 (con $ justSubtag st1) st2 AtExtl1 pos' t'
        (17699146535566049298, 17412902894784479253)
          | T.null t -> pure $ Grandfathered ZhXiang
        _ -> tryLext1 con st1 atlast pos t

    tryLext1 !con st atlast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) AtExtl1 pos t (con $ justSubtag st) tryLext2
      | otherwise = tryScript (con nullSubtag nullSubtag nullSubtag) st atlast pos t

    tryLext2 !con st atlast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) AtExtl2 pos t (con $ justSubtag st) tryLext3
      | otherwise = tryScript (con nullSubtag nullSubtag) st atlast pos t

    tryLext3 !con st atlast pos t
      | containsOnlyLetters st && subtagLength st == 3 =
        mfinish (subtagLength st) AtLanguage pos t (con $ justSubtag st) tryScript
      | otherwise = tryScript (con nullSubtag) st atlast pos t

    tryScript :: (MaybeSubtag -> MaybeSubtag -> [Subtag] -> [Extension] -> [Subtag] -> BCP47) -> Subtag -> AtComponent -> Int -> Text -> Either SyntaxError BCP47
    tryScript !con st atlast pos t
      | subtagLength st == 4 && containsOnlyLetters st =
        mfinish (subtagLength st) AtScript pos t (con $ justSubtag st) tryRegion
      | otherwise = tryRegion (con nullSubtag) st atlast pos t

    tryRegion :: (MaybeSubtag -> [Subtag] -> [Extension] -> [Subtag] -> BCP47) -> Subtag -> AtComponent -> Int -> Text -> Either SyntaxError BCP47
    tryRegion !con st atlast pos t
      | subtagLength st == 2 =
        if containsDigit st
          then Left $ BadSubtag pos atlast st (Just $ finish con)
          else mfinish (subtagLength st) AtRegion pos t (con $ justSubtag st) tryVariant
      | subtagLength st == 3 =
        if containsLetter st
          then Left $ BadSubtag pos atlast st (Just $ finish con)
          else mfinish (subtagLength st) AtRegion pos t (con $ justSubtag st) tryVariant
      | otherwise = tryVariant (con nullSubtag) st atlast pos t

    tryVariant :: ([Subtag] -> [Extension] -> [Subtag] -> BCP47) -> Subtag -> AtComponent -> Int -> Text -> Either SyntaxError BCP47
    tryVariant !con st atlast pos t
      | subtagLength st == 4 =
        if isDigit $ subtagHead st
          then mfinish (subtagLength st) AtVariant pos t (con . (st :)) tryVariant
          else Left $ BadSubtag pos atlast st (Just $ finish con)
      | subtagLength st >= 5 =
        mfinish (subtagLength st) AtVariant pos t (con . (st :)) tryVariant
      | otherwise = trySingleton (con []) st atlast pos t

    trySingleton :: ([Extension] -> [Subtag] -> BCP47) -> Subtag -> AtComponent -> Int -> Text -> Either SyntaxError BCP47
    trySingleton con st atlast pos t
      | subtagLength st /= 1 = Left $ BadSubtag pos atlast st (Just $ finish con)
      | subtagHead st == subtagCharx =
        parsePrivateUse (con []) pos t
      | otherwise =
        let c = unsafeSubtagCharToExtension $ subtagHead st
         in parseExtension c con pos t

    parsePrivateUse con pos t = do
      let pos' = pos + 2
      mst <- tagPop (Just $ finish con) t AtPrivateUse pos'
      case mst of
        Just (st, t') -> parsePrivateUseTag con st AtPrivateUse pos' t'
        Nothing -> Left $ EmptySingleton pos Nothing (Just $ finish con)

    parsePrivateUseTag con st _ pos t =
      mfinish (subtagLength st) AtPrivateUse pos t (con . (st :)) parsePrivateUseTag

    parseExtension c con pos t = do
      let pos' = pos + 2
      let con' ne = con . (Extension c ne :)
      mst <- tagPop (Just $ finish con) t AtExtension pos'
      case mst of
        Just (st, t')
          | subtagLength st >= 2 ->
            mfinish (subtagLength st) AtExtension pos' t' (con' . (st NE.:|)) parseExtensionTag
          | otherwise -> Left $ EmptySingleton pos (Just c) (Just $ finish con)
        Nothing -> Left $ EmptySingleton pos (Just c) (Just $ finish con)

    parseExtensionTag con st _ pos t
      | subtagLength st == 1 = trySingleton (con []) st AtExtension pos t
      | otherwise = mfinish (subtagLength st) AtExtension pos t (con . (st :)) parseExtensionTag

    parseIrregularI st t
      | st /= subtagI = Left $ BadSubtag 0 AtBeginning st Nothing
      | otherwise = do
        mst <- tagPop Nothing t AtIrregI 2
        case mst of
          Just (st', t') -> case recognizeIrregI st' of
            Just g
              | T.null t' -> Right $ Grandfathered g
              | otherwise -> Left $ IrregNum g
            Nothing -> Left $ BadSubtag 2 AtIrregI st' Nothing
          Nothing -> Left EmptyIrregI

    recognizeIrregI n = case unwrapSubtag n of
      14102819922971197459 -> Just IAmi
      14248104991419006995 -> Just IBnn
      14526138628724883479 -> Just IDefault
      14680466211245977112 -> Just IEnochian
      15098133032806121491 -> Just IHak
      15542853518732230679 -> Just IKlingon
      15697226132455686163 -> Just ILux
      15827749698417983509 -> Just IMingo
      15962927641447628822 -> Just INavajo
      16275850723642572819 -> Just IPwn
      16827550474088480787 -> Just ITao
      16827638435018702867 -> Just ITay
      16847869448969781267 -> Just ITsu
      _ -> Nothing

-- | Parse an entire private use tag
parsePrivateTag :: Text -> Either SyntaxError BCP47
parsePrivateTag inp = do
  mst <- tagPop Nothing inp AtPrivateUse 2
  case mst of
    Just (st, t) ->
      parsePrivateUseTag (PrivateUse . (st NE.:|)) (2 + fromIntegral (subtagLength st) + 1) t
    Nothing -> Left $ EmptySingleton 0 Nothing Nothing
  where
    parsePrivateUseTag con pos t = do
      let con' = con []
      mst <- tagPop (Just con') t AtPrivateUse pos
      case mst of
        Just (st, t') ->
          parsePrivateUseTag (con . (st :)) (pos + fromIntegral (subtagLength st) + 1) t'
        Nothing -> pure con'

-- $valueconstruction
--
-- Other than the 'parseBCP47' function, tags can also be constructed
-- using 'unsafeNormalTag' and 'unsafePrivateTag' if they are known to
-- be well-formed, as well as 'grandfatheredSyntax'.

-- $grandfathered
--
-- In a prior standard it was possible to register entire tags, not
-- simply subtags. Of those tags, the ones that could not be
-- represented via registered subtags were explicitly grandfathered
-- into the current standard via the grammar of the tags itself. All
-- of them are valid, but most are deprecated; see the documentation
-- for
-- 'Text.LanguageTag.Internal.BCP47.Registry.Grandfathered.Grandfathered'
-- for up-to-date details.

-- | Embed a 'Text.LanguageTag.BCP47.Registry.Grandfathered' language
-- tag in the 'BCP47' type
grandfatheredSyntax :: Grandfathered -> BCP47
grandfatheredSyntax = Grandfathered
{-# INLINE grandfatheredSyntax #-}

-- $regular
--
-- Grandfathered tags that conform to the normal language tag grammar,
-- but have one or more subtags that do not appear in the registry, or
-- appear with different semantics.

-- $irregular
--
-- Grandfathered tags that do not conform to the normal language tag
-- grammar.

----------------------------------------------------------------
-- Tag constants
----------------------------------------------------------------

subtagCharx :: SubtagChar
subtagCharx = SubtagChar 120

subtagI :: Subtag
subtagI = Subtag 15132094747964866577

subtagX :: Subtag
subtagX = Subtag 17293822569102704657

----------------------------------------------------------------
-- Internal convenience class
----------------------------------------------------------------

class Finishing a where
  finish :: a -> BCP47

instance Finishing a => Finishing (MaybeSubtag -> a) where
  finish con = finish $ con nullSubtag

instance Finishing a => Finishing ([b] -> a) where
  finish con = finish $ con []

instance Finishing BCP47 where
  finish = id
