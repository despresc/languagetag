-- |
-- Description : Conversions of valid tags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
--
-- Warning: this is an internal module and may change or disappear
-- without regard to the PVP.
module Text.LanguageTag.Internal.BCP47.Registry
  ( toSubtags,
    toSyntaxTag,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.LanguageTag.BCP47.Registry.Extlang
import Text.LanguageTag.BCP47.Registry.Language
import Text.LanguageTag.BCP47.Registry.Region
import Text.LanguageTag.BCP47.Registry.Script
import Text.LanguageTag.BCP47.Registry.Variant
import Text.LanguageTag.BCP47.Subtag (Subtag, justSubtag, nullSubtag)
import qualified Text.LanguageTag.BCP47.Syntax as Syn
import Text.LanguageTag.Internal.BCP47.Registry.Types
import qualified Text.LanguageTag.Internal.BCP47.Syntax as Syn

-- | Convert a 'BCP47' tag to its component subtags
toSubtags :: BCP47 -> NonEmpty Subtag
toSubtags = Syn.toSubtags . toSyntaxTag

-- | Convert a 'BCP47' tag back to a merely well-formed 'Syn.BCP47'
-- tag
toSyntaxTag :: BCP47 -> Syn.BCP47
toSyntaxTag (NormalTag n) =
  Syn.NormalTag $
    Syn.Normal
      { Syn.primlang = languageToSubtag $ language n,
        Syn.extlang1 = mto extlangToSubtag extlang n,
        Syn.extlang2 = nullSubtag,
        Syn.extlang3 = nullSubtag,
        Syn.script = mto scriptToSubtag script n,
        Syn.region = mto regionToSubtag region n,
        Syn.variants = S.toList $ S.map variantToSubtag $ variants n,
        Syn.extensions = fmap toExt $ M.toList $ extensions n,
        Syn.privateUse = privateUse n
      }
  where
    mto f p x = maybe nullSubtag (justSubtag . f) $ p x
    toExt (c, x) = Syn.Extension c $ fromExtensionSubtag <$> x
toSyntaxTag (PrivateUseTag x) = Syn.PrivateUse x
toSyntaxTag (GrandfatheredTag x) = Syn.GrandfatheredTag x
