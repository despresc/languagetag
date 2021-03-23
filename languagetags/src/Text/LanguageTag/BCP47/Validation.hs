{-# LANGUAGE CPP #-}

-- |
-- Description : Language tag validation
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- This module provides the 'validateBCP47' function to validate a
-- syntactically well-formed 'Syn.BCP47' tag, transforming it into a
-- validated 'BCP47' tag, as well as functions to validate the
-- different types of subtags. See
-- 'Text.LanguageTag.BCP47.Registry.bcp47RegistryDate' for the version
-- of the registry that this library uses; the current version of the
-- IANA subtag registry is available at
-- <https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.
module Text.LanguageTag.BCP47.Validation
  ( -- * Tag validation
    validateBCP47,
    ValidationError (..),

    -- * Subtag validation
    validateLanguage,
    validateExtlang,
    validateScript,
    validateRegion,
    validateVariant,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry as Reg
import Text.LanguageTag.BCP47.Subtag
  ( MaybeSubtag,
    Subtag,
    maybeSubtag,
  )
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Registry.ExtlangRecords
import Text.LanguageTag.Internal.BCP47.Registry.LanguageRecords
import Text.LanguageTag.Internal.BCP47.Registry.RegionRecords
import Text.LanguageTag.Internal.BCP47.Registry.ScriptRecords
import Text.LanguageTag.Internal.BCP47.Registry.Types
import Text.LanguageTag.Internal.BCP47.Registry.VariantRecords
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | A possible error during validation
data ValidationError
  = -- | the language was unregistered
    InvalidLanguage Subtag
  | -- | the extended language was unregistered
    InvalidExtlang Subtag
  | -- | more than one extended language was present
    ExcessExtlang Subtag
  | -- | the script was unregistered
    InvalidScript Subtag
  | -- | the region was unregistered
    InvalidRegion Subtag
  | -- | one of the variants was unregistered
    InvalidVariant Subtag
  | -- | one of the variants was repeated
    DuplicateVariant Variant
  | -- | one of the extension sections was repeated
    DuplicateExtensionSection Syn.ExtensionChar
  deriving (Eq, Ord, Show)

type VM a = Either ValidationError a

throw :: ValidationError -> VM a
throw = Left

setAlter :: Functor f => (Bool -> f Bool) -> Variant -> Set Variant -> f (Set Variant)
#if MIN_VERSION_containers(0,6,3)
setAlter = S.alterF
#else
setAlter f k s = go <$> f x
  where
    x = S.member k s
    go b
      | b = if x then s else S.insert k s
      | otherwise = if x then S.delete k s else s
#endif
{-# INLINE setAlter #-}

validate :: (Subtag -> ValidationError) -> (Subtag -> Maybe a) -> Subtag -> VM a
validate ve f s = case f s of
  Nothing -> throw $ ve s
  Just x -> pure x

maybeValidate ::
  (Subtag -> ValidationError) ->
  (Subtag -> Maybe a) ->
  MaybeSubtag ->
  VM (Maybe a)
maybeValidate cmp f = maybeSubtag (pure Nothing) (fmap Just . validate cmp f)

-- | Validate a language tag. For normal tags, this means there must
-- be no duplicate variants, no duplicate extension singletons, no
-- more than one extended language subtag, and no unregistered
-- subtags. Grandfathered and private use tags are automatically valid.
validateBCP47 :: Syn.BCP47 -> Either ValidationError Reg.BCP47
validateBCP47 (Syn.NormalTag (Syn.Normal pl e1 e2 e3 s r vs es ps)) =
  case validateLanguage pl of
    Nothing -> throw $ InvalidLanguage pl
    Just valpl -> do
      vale1 <- maybeValidate InvalidExtlang validateExtlang e1
      guardNull e2
      guardNull e3
      vals <- maybeValidate InvalidScript validateScript s
      valr <- maybeValidate InvalidRegion validateRegion r
      valvs <- List.foldl' addVariant (pure mempty) vs
      vales <- List.foldl' addExtension (pure mempty) es
      pure $
        NormalTag
          Normal
            { language = valpl,
              extlang = vale1,
              script = vals,
              region = valr,
              variants = valvs,
              extensions = vales,
              privateUse = ps
            }
  where
    guardNull = maybeSubtag (pure ()) $ throw . ExcessExtlang
    addVariant st v = do
      st' <- st
      v' <- validate InvalidVariant validateVariant v
      setAlter (insertVariant v') v' st'
    insertVariant v b
      | b = throw $ DuplicateVariant v
      | otherwise = pure True
    addExtension m e = do
      m' <- m
      let c = Syn.extSingleton e
      M.alterF (insertExtension c $ Syn.extTags e) c m'
    insertExtension _ ts Nothing = pure $ Just $ ExtensionSubtag <$> ts
    insertExtension c _ (Just _) = throw $ DuplicateExtensionSection c
validateBCP47 (Syn.PrivateUse x) = pure $ PrivateUseTag x
validateBCP47 (Syn.GrandfatheredTag x) = pure $ GrandfatheredTag x
