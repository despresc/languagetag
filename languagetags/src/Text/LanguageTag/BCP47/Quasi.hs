{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Description : Splices for static subtags and tags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module defines splices that construct compile-time-checked
-- values of the various BCP47 language tag types.
module Text.LanguageTag.BCP47.Quasi
  ( subtag,
    syntag,
    validtag,
    canontag,
  )
where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Language.Haskell.TH (Exp (..), Lit (..), Pat (..))
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
  ( Name (..),
    OccName (..),
  )
import Text.LanguageTag.BCP47.Canonicalization (canonicalizeBCP47)
import Text.LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Extlang (..),
    Language (..),
    Normal (..),
    Region (..),
    Script (..),
    Variant (..),
    renderVariant,
  )
import Text.LanguageTag.BCP47.Subtag (parseSubtag)
import qualified Text.LanguageTag.BCP47.Subtag as Sub
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.BCP47.Validation (ValidationError (..), validateBCP47)
import Text.LanguageTag.Internal.BCP47.Registry.Types (ExtensionSubtag (..))
import Text.LanguageTag.Internal.BCP47.Subtag (MaybeSubtag (..), Subtag (..))
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- Hack to lift enum constructors
mkEx :: Name -> String -> Name
mkEx (Name _ f) s = Name (OccName s) f

mkExS :: Show a => Name -> a -> Name
mkExS n = mkEx n . show

neE :: (a -> Exp) -> NonEmpty a -> Exp
neE f (x NE.:| xs) =
  appl
    (ConE '(NE.:|))
    [f x, ListE $ f <$> xs]

neP :: (a -> Pat) -> NonEmpty a -> Pat
neP f (x NE.:| xs) =
  ConP
    '(NE.:|)
    [f x, ListP $ f <$> xs]

tupE :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
tupE = TupE . fmap Just
#else
tupE = TupE
#endif
{-# INLINE tupE #-}

appl :: Exp -> [Exp] -> Exp
appl = foldl' AppE

wordL :: Word64 -> Lit
wordL = IntegerL . fromIntegral

subtagE :: Subtag -> Exp
subtagE (Subtag a) = AppE (ConE 'Subtag) (LitE $ wordL a)

subtagP :: Subtag -> Pat
subtagP (Subtag a) = ConP 'Subtag [LitP $ wordL a]

msubtagE :: MaybeSubtag -> Exp
msubtagE (MaybeSubtag (Subtag a)) =
  AppE (ConE 'MaybeSubtag) $
    AppE (ConE 'Subtag) (LitE $ wordL a)

grandE :: Syn.Grandfathered -> Exp
grandE = ConE . mkExS 'Syn.ArtLojban

grandP :: Syn.Grandfathered -> Pat
grandP g = ConP (mkExS 'Syn.ArtLojban g) []

msubtagP :: MaybeSubtag -> Pat
msubtagP (MaybeSubtag (Subtag a)) =
  ConP
    'MaybeSubtag
    [ConP 'Subtag [LitP $ wordL a]]

extCharE :: Syn.ExtensionChar -> Exp
extCharE = ConE . mkExS 'Syn.ExtA

extCharP :: Syn.ExtensionChar -> Pat
extCharP x = ConP (mkExS 'Syn.ExtA x) []

extE :: Syn.Extension -> Exp
extE (Syn.Extension x y) =
  appl
    (ConE 'Syn.Extension)
    [ extCharE x,
      neE subtagE y
    ]

syntagE :: Syn.BCP47 -> Exp
syntagE (Syn.NormalTag (Syn.Normal p e1 e2 e3 s r v es ps)) =
  AppE (ConE 'Syn.NormalTag) $
    appl
      (ConE 'Syn.Normal)
      [ subtagE p,
        msubtagE e1,
        msubtagE e2,
        msubtagE e3,
        msubtagE s,
        msubtagE r,
        ListE $ subtagE <$> v,
        ListE $ extE <$> es,
        ListE $ subtagE <$> ps
      ]
syntagE (Syn.PrivateUse x) =
  AppE (ConE 'Syn.PrivateUse) $ neE subtagE x
syntagE (Syn.GrandfatheredTag g) =
  AppE (ConE 'Syn.GrandfatheredTag) $ grandE g

syntagP :: Syn.BCP47 -> Pat
syntagP (Syn.NormalTag (Syn.Normal p e1 e2 e3 s r v es ps)) =
  ConP
    'Syn.NormalTag
    [ ConP
        'Syn.Normal
        [ subtagP p,
          msubtagP e1,
          msubtagP e2,
          msubtagP e3,
          msubtagP s,
          msubtagP r,
          ListP $ subtagP <$> v,
          ListP $ extP <$> es,
          ListP $ subtagP <$> ps
        ]
    ]
  where
    extP (Syn.Extension x y) =
      ConP
        'Syn.Extension
        [extCharP x, neP subtagP y]
syntagP (Syn.PrivateUse x) = ConP 'Syn.PrivateUse [neP subtagP x]
syntagP (Syn.GrandfatheredTag g) = ConP 'Syn.GrandfatheredTag [grandP g]

validtagE :: BCP47 -> Exp
validtagE (NormalTag (Normal l e s r v es ps)) =
  AppE (ConE 'NormalTag) $
    appl
      (ConE 'Normal)
      [ ConE $ mkExS 'Aa l,
        mE 'ExtAao e,
        mE 'Adlm s,
        mE 'Reg001 r,
        AppE
          (VarE 'S.fromList)
          (ListE $ varE <$> S.toList v),
        AppE
          (VarE 'M.fromList)
          (ListE $ extE' <$> M.toList es),
        ListE $ subtagE <$> ps
      ]
  where
    mE _ Nothing = ConE 'Nothing
    mE x (Just y) =
      AppE
        (ConE 'Just)
        (ConE $ mkExS x y)
    varE = ConE . mkExS 'Var1606nict
    extSubtagE (ExtensionSubtag x) =
      AppE
        (ConE 'ExtensionSubtag)
        (subtagE x)
    extE' (x, y) = tupE [extCharE x, neE extSubtagE y]
validtagE (PrivateUseTag x) = AppE (ConE 'PrivateUseTag) (neE subtagE x)
validtagE (GrandfatheredTag g) = AppE (ConE 'GrandfatheredTag) (grandE g)

-- | Create a 'Subtag' value from a raw subtag string. The text in the
-- quasi-quote should be between one and eight ASCII alphanumeric
-- characters. Examples:
--
-- >>> :set -XQuasiQuotes
-- >>> [subtag|sOm3sUb|]
-- "som3sub"
-- >>> [subtag|sub!|]
-- <interactive>:3:9: error:
--     • Invalid subtag: contains character '!' that is not an ASCII letter or number
--     • In the quasi-quotation: [subtag|sub!|]
--
-- The 'subtag' quasi-quoter can be used in expressions and patterns.
subtag :: QuasiQuoter
subtag =
  QuasiQuoter
    { quoteExp = qe,
      quotePat = qp,
      quoteType = const $ fail "Cannot be used in a type context",
      quoteDec = const $ fail "Cannot be used in a declaration context"
    }
  where
    prettyErrorSuggestion Sub.EmptyInput =
      "subtags must contain at least one character"
    prettyErrorSuggestion Sub.EmptySubtag =
      "subtags have at least one character before ending"
    prettyErrorSuggestion Sub.TagTooLong =
      "subtags must be at most eight characters long"
    prettyErrorSuggestion (Sub.TrailingTerminator _) =
      "lone subtags cannot end in a dash character"
    prettyErrorSuggestion (Sub.InvalidChar _ c) =
      "contains character '" <> T.singleton c
        <> "' that is not an ASCII letter or number"
    qe s = case parseSubtag (T.pack s) of
      Left e -> fail $ "Invalid subtag: " <> T.unpack (prettyErrorSuggestion e)
      Right x -> pure $ subtagE x
    qp s = case parseSubtag (T.pack s) of
      Left e -> fail $ "Invalid subtag: " <> T.unpack (prettyErrorSuggestion e)
      Right x -> pure $ subtagP x

-- | Create a merely well-formed 'Syn.BCP47' value from a raw tag
-- string. Examples:
--
-- >>> :set -XQuasiQuotes
-- >>> [syntag|eN-Gb-oxEndict|]
-- "en-GB-oxendict"
-- >>> [syntag|eN-us-hanS|]
-- <interactive>:3:11: error:
--     • Ill-formed tag: after region subtag, expected one of: variant subtag, start of extension or private use section; got "hans".
--     • In the quasi-quotation: [canontag|eN-us-hanS|]
--
-- The 'syntag' quasi-quoter can be used in expressions and patterns.
syntag :: QuasiQuoter
syntag =
  QuasiQuoter
    { quoteExp = qe,
      quotePat = qp,
      quoteType = const $ fail "Cannot be used in a type context",
      quoteDec = const $ fail "Cannot be used in a declaration context"
    }
  where
    qe s = case Syn.parseBCP47 (T.pack s) of
      Left e -> fail $ syntaxErr' s e
      Right x -> pure $ syntagE x
    qp s = case Syn.parseBCP47 (T.pack s) of
      Left e -> fail $ syntaxErr' s e
      Right x -> pure $ syntagP x

-- | Create a valid (but not canonicalized) 'BCP47' value from a raw
-- subtag string. Examples:
--
-- >>> :set -XQuasiQuotes
-- >>> renderBCP47 [validtag|yue-hant|]
-- yue-Hant
-- >>> renderBCP47 [validtag|eN-Gb-OED|]
-- en-GB-oed
-- >>> renderBCP47 [validtag|sgn-br|]
-- "sgn-BR"
-- >>> renderBCP47 [validtag|unknown-US|]
-- <interactive>:5:11: error:
--     • Invalid tag: encountered unregistered primary language subtag "unknown"
--     • In the quasi-quotation: [validtag|unknown-US|]
--
-- The 'validtag' quasi-quoter can only be used as an expression.
validtag :: QuasiQuoter
validtag =
  QuasiQuoter
    { quoteExp = qe,
      quotePat = const $ fail "Cannot be used in a pattern context",
      quoteType = const $ fail "Cannot be used in a type context",
      quoteDec = const $ fail "Cannot be used in a declaration context"
    }
  where
    qe s = case Syn.parseBCP47 (T.pack s) of
      Left e -> fail $ syntaxErr' s e
      Right x -> case validateBCP47 x of
        Left e -> fail $ validErr' e
        Right x' -> pure $ validtagE x'

-- | Create a valid and canonicalized 'BCP47' value from a raw tag
-- string. Examples:
--
-- >>> :set -XQuasiQuotes
-- >>> :set -XQuasiQuotes
-- >>> renderBCP47 [canontag|yue-hant|]
-- yue-Hant
-- >>> renderBCP47 [canontag|eN-Gb-OED|]
-- en-GB-oxendict
-- >>> renderBCP47 [canontag|sgn-br|]
-- "bzs"
-- >> renderBCP47 [canontag|en-gb-oed-x-more|]
-- <interactive>:9:23: error:
--     • Ill-formed tag: the irregular grandfathered tag en-GB-oed cannot be followed by further text
--     • In the quasi-quotation: [canontag|en-gb-oed-x-more|]
--
-- The 'canontag' quasi-quoter can only be used as an expression.
canontag :: QuasiQuoter
canontag =
  QuasiQuoter
    { quoteExp = qe,
      quotePat = const $ fail "Cannot be used in a pattern context",
      quoteType = const $ fail "Cannot be used in a type context",
      quoteDec = const $ fail "Cannot be used in a declaration context"
    }
  where
    qe s = case Syn.parseBCP47 (T.pack s) of
      Left e -> fail $ syntaxErr' s e
      Right x -> case validateBCP47 x of
        Left e -> fail $ validErr' e
        Right x' -> pure $ validtagE $ canonicalizeBCP47 x'

----------------------------------------------------------------
-- Nicer errors
----------------------------------------------------------------

syntaxErr :: Text -> Syn.SyntaxError -> Text
syntaxErr inp (Syn.UnparsableSubtag pos _ mpc _) =
  case mpc of
    Nothing -> "subtag starting with \"" <> badExample <> "\" was too long"
    Just (_, c) ->
      T.concat
        [ "subtag \"",
          badExample,
          "\" contains character '",
          T.singleton c,
          "' that is not an ASCII letter or number"
        ]
  where
    badExample = T.take 8 $ T.takeWhile (/= '-') $ T.drop pos inp
syntaxErr _ (Syn.BadSubtag _ atlast st _) =
  "after " <> Syn.atComponentDescription atlast
    <> ", expected one of: "
    <> T.intercalate
      ", "
      (NE.toList $ Syn.subtagCategoryName <$> Syn.expectedCategories atlast)
    <> "; got \""
    <> Sub.renderSubtagLower st
    <> "\"."
syntaxErr _ Syn.EmptyInput = "input must be non-empty"
syntaxErr _ Syn.EmptySubtag {} = "all subtags must be non-empty"
syntaxErr _ Syn.TrailingTerminator {} = "a tag cannot end in a dash character"
syntaxErr _ (Syn.EmptySingleton _ mc _) = case mc of
  Nothing -> "the private use section after the subtag \"x\" must be non-empty"
  (Just c) ->
    "the extension section after the subtag \"" <> T.singleton (Syn.extensionCharToChar c)
      <> "\" must be non-empty"
syntaxErr _ (Syn.IrregNum g) =
  "the irregular grandfathered tag " <> Syn.renderBCP47 (Syn.GrandfatheredTag g)
    <> " cannot be followed by further text"
syntaxErr _ Syn.EmptyIrregI =
  "the irregular grandfathered subtag \"i\" must be followed by one of the subtags: "
    <> Syn.subtagCategorySyntax Syn.GrandfatheredIFollower

syntaxErr' :: String -> Syn.SyntaxError -> String
syntaxErr' s e = T.unpack $ "Ill-formed tag: " <> syntaxErr (T.pack s) e

validErr :: ValidationError -> Text
validErr e = case e of
  InvalidLanguage st -> stm "primary language" st
  InvalidExtlang st -> stm "extended language" st
  ExcessExtlang st ->
    "a second extended language subtag \"" <> Sub.renderSubtagLower st
      <> "\" - there can be only one extended language subtag"
  InvalidScript st -> stm "script" st
  InvalidRegion st -> stm "region" st
  InvalidVariant st -> stm "variant" st
  DuplicateVariant v ->
    "duplicate variant subtags \"" <> renderVariant v <> "\""
  DuplicateExtensionSection sc ->
    "duplicate extension sections starting with the singleton \""
      <> T.singleton (Syn.extensionCharToChar sc)
      <> "\""
  where
    stm x y = "unregistered " <> x <> " subtag \"" <> Sub.renderSubtagLower y <> "\""

validErr' :: ValidationError -> String
validErr' e = T.unpack $ "Invalid tag: encountered " <> validErr e
