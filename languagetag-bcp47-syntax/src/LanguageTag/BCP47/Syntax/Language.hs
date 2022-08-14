-- |
-- Description : Well-formed language and extended language subtags
-- Copyright   : 2021 Christian Despres
-- License     : BSD-2-Clause
-- Maintainer  : Christian Despres
module LanguageTag.BCP47.Syntax.Language
  ( ShortLang,
    LongLang,
    Language (..),
    classifyLanguage,
    shortLangToLang,
    longLangToLang,
    Extlang,
    extlangToShortLang,
    LangSec (..),
    Extlangs (..),
  )
where

import LanguageTag.Internal.BCP47.Syntax.Language
