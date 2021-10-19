{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
    tag,
    syntag,
    validtag,
    canontag,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
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
    Q,
  )
import Text.LanguageTag.BCP47.Canonicalization
  ( CanonicalWarnings (..),
    DeprecatedComponent (..),
    ExtlangWarning (..),
    LintWarnings (..),
    PrefixCollision (..),
    SuperfluousScriptWarning (..),
    VariantWarnings (..),
    canonicalizeBCP47,
    lintBCP47,
  )
import Text.LanguageTag.BCP47.Registry
  ( BCP47 (..),
    Extlang (..),
    Language (..),
    Normal (..),
    Region (..),
    Script (..),
    Variant (..),
    renderExtlang,
    renderGrandfathered,
    renderLanguage,
    renderRedundant,
    renderRegion,
    renderScript,
    renderVariant,
  )
import Text.LanguageTag.BCP47.Subtag (popSubtagText)
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
    withPrettyError con s = case popSubtagText s' of
      Left Sub.PopEmptySubtag -> case T.uncons s' of
        Just (c, _) ->
          prefFail $
            "contains character '" <> [c]
              <> "that is not an ascii letter or number"
        Nothing -> prefFail "subtags must contain at least one character"
      Left Sub.PopSubtagTooLong {} ->
        prefFail "subtags must be at most eight characters long"
      Right (st, s'') -> case T.uncons s'' of
        Just (c, _) ->
          prefFail $
            "contains character '" <> [c]
              <> "' that is not an ascii letter or number"
        Nothing -> pure $ con st
      where
        prefFail e = fail $ "Invalid subtag: " <> e
        s' = T.pack s
    qe = withPrettyError subtagE
    qp = withPrettyError subtagP

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
        Right x' -> pure $ validtagE $ snd $ canonicalizeBCP47 x'

-- | Create a 'BCP47' value from a raw tag string that would pass
-- through 'lintBCP47' without warnings; the result will, among other
-- things, be valid and canonicalized. Other quasi-quoters are
-- available in this module that are less strict than this, should you
-- want to create tags that are non-canonical or fail to follow
-- certain recommendations that the standard makes.
--
-- >>> :set -XQuasiQuotes
-- >>> renderBCP47 [tag|yue-hant|]
-- "yue-Hant"
-- >>> renderBCP47 [tag|eN-Gb-oxenDicT|]
-- "en-GB-oxendict"
-- >>> renderBCP47 [tag|sgn-br|]
-- <interactive>:4:18: error:
--    • Linting returned warning: used deprecated redundant tag "sgn-BR"
--    • In the quasi-quotation: [tag|sgn-br|]
-- >>> renderBCP47 [tag|de-1996-1901|]
-- <interactive>:5:18: error:
--    • Linting returned warning: the variant "1901" and the variant "1996" have overlapping
-- prefixes and should not be used together
--    • In the quasi-quotation: [tag|de-1996-1901|]
tag :: QuasiQuoter
tag =
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
        Right x' -> case lintBCP47 x' of
          (w, x'') -> guardNoLintWarnings w $> validtagE x''

----------------------------------------------------------------
-- Nicer errors
----------------------------------------------------------------

syntaxErr :: Text -> Syn.ParseError Char -> Text
syntaxErr inp (Syn.ParseErrorPop (Syn.PopErrorSubtag off _ _ err)) =
  case err of
    Sub.PopEmptySubtag -> "all subtags must be non-empty"
    Sub.PopSubtagTooLong {} ->
      "subtag starting with \"" <> badExample <> "\" was too long"
      where
        badExample = T.take 8 $ T.drop off inp
syntaxErr _ (Syn.ParseErrorPop (Syn.PopErrorStep _ _ loc err)) =
  case err of
    Syn.ErrEmptyExtensionSection ec _ ->
      "the extension section after the subtag \""
        <> T.singleton (Syn.extensionCharToChar ec)
        <> "\" must be non-empty"
    Syn.ErrEmptyPrivateUse ->
      "the private use section after the subtag \"x\" must be non-empty"
    Syn.ErrImproperSubtag st ->
      "after " <> Syn.atComponentDescription' loc
        <> ", expected one of: "
        <> T.intercalate
          ", "
          (NE.toList $ Syn.subtagCategoryName <$> Syn.expectedCategories' loc)
        <> "; got \""
        <> Sub.renderSubtagLower st
        <> "\"."
    Syn.ErrEmptyStartI ->
      "the irregular initial subtag \"i\" must be followed by one of: "
        <> Syn.subtagCategorySyntax Syn.GrandfatheredIFollower
    Syn.ErrSubtagAfterIrreg _ g ->
      "the irregular grandfathered tag " <> Syn.renderBCP47 (Syn.GrandfatheredTag g)
        <> " cannot be followed by further subtags"
syntaxErr inp (Syn.ParseErrorInvalidChar off _ c) =
  T.concat
    [ "subtag \"",
      badExample,
      "\" contains character '",
      T.singleton c,
      "' that is not an ASCII letter or number"
    ]
  where
    badExample = T.take 8 $ T.takeWhile (/= '-') $ T.drop off inp

syntaxErr' :: String -> Syn.ParseError Char -> String
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

-- | Choose one of the warnings and render it. Should only be called
-- on a 'LintWarnings' that is known to have at least one warning in
-- it.
lintWarningsErr :: LintWarnings -> Maybe Text
lintWarningsErr w =
  canonErr (canonicalWarnings w)
    <|> scriptErr (scriptWarning w)
    <|> variantErr (variantWarnings w)
  where
    canonErr (CanonicalWarnings dep ew) = case S.toAscList dep of
      (x : _) ->
        Just $
          "used deprecated " <> case x of
            DeprecatedLanguage l -> "language subtag " <> quoteText (renderLanguage l)
            DeprecatedExtlang e -> "extended language subtag " <> quoteText (renderExtlang e)
            DeprecatedScript s -> "script subtag " <> quoteText (renderScript s)
            DeprecatedRegion r -> "region subtag " <> quoteText (renderRegion r)
            DeprecatedVariant v -> "variant subtag " <> quoteText (renderVariant v)
            DeprecatedRedundant r -> "redundant tag " <> quoteText (renderRedundant r)
            DeprecatedGrandfathered g -> "grandfathered tag " <> quoteText (renderGrandfathered g)
      [] -> case ew of
        ExtlangPrefixMismatch e ->
          Just $
            "used extended language subtag " <> quoteText (renderExtlang e)
              <> " without its registered prefix"
        UsedExtlang e ->
          Just $
            "used the extended language subtag " <> quoteText (renderExtlang e)
              <> " instead of its identical replacement primary language subtag "
              <> quoteText (renderExtlang e)
        NoExtlangWarning -> Nothing
    scriptErr (SuperfluousLanguageScript s l) =
      Just $
        "script subtag "
          <> quoteText (renderScript s)
          <> " is implied by the language subtag "
          <> quoteText (renderLanguage l)
    scriptErr (SuperfluousExtlangScript s e) =
      Just $
        "script subtag "
          <> quoteText (renderScript s)
          <> " is implied by the extended language subtag "
          <> quoteText (renderExtlang e)
    scriptErr NoSuperfluousScript = Nothing
    rendChain (x :| xs)
      | null xs =
        "the variant "
          <> quoteText (renderVariant x)
      | otherwise =
        "the variant chain "
          <> quoteText (T.intercalate "-" $ renderVariant <$> x : xs)

    variantErr (VariantWarnings pc s) = case pc of
      PrefixCollision xs (ys :| _) ->
        Just $
          rendChain xs
            <> " and "
            <> rendChain ys
            <> " have overlapping prefixes and should not be used together"
      NoPrefixCollision -> case S.toAscList s of
        (x : _) ->
          Just $
            "used variant subtag "
              <> quoteText (renderVariant x)
              <> " without one of its registered prefixes"
        [] -> Nothing

guardNoLintWarnings :: LintWarnings -> Q ()
guardNoLintWarnings w = case lintWarningsErr w of
  Nothing -> pure ()
  Just t -> fail $ T.unpack $ "Linting returned warning: " <> t

quoteText :: Text -> Text
quoteText t = "\"" <> t <> "\""
