# Languagetag: Language tag analysis

This project aims to support language tag parsing and analysis in
Haskell, *language tag* meaning some code for identifying human
languages. Currently it only includes the `languagetag-bcp47` package,
which supports [BCP47](https://tools.ietf.org/html/bcp47)-compliant
language tag parsing and processing tools. The project may be updated
in future to add support for other language tag resources, like ISO
639 or Glottolog.

Also included in the project is the `languagetag-gen` package, which
generates code in `languagetag-bcp47` related to validation,
canonicalization, and various other registry queries and
processing. This is an internal package and you shouldn't need to use
it unless you are developing `languagetag-bcp47`.
