# Languagetag: Language tag analysis

This project aims to support language tag parsing and analysis in
Haskell, *language tag* meaning some code for identifying human
languages. Currently it only includes the `languagetag-bcp47` package,
which supports [BCP47](https://tools.ietf.org/html/bcp47)-compliant
language tag parsing and processing. The project may be updated in
future to add support for other language tag resources, like ISO 639
or Glottolog.

Also included in the project is the `languagetag-gen` package, which
generates certain types and functions in `languagetag-bcp47`
automatically from a local copy of the [IANA subtag
registry](https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry). It
is an internal package, and you shouldn't need to use it unless you
are developing `languagetag-bcp47`.

## Current state of the packages

The `languagetag-bcp47` package is currently mid-refactor, though it and the
packages that are replacing it are reasonably usable as they are.

- The `languagetag-bcp47-subtag` package defines BCP47 subtags and provides
  parsers and `Trie` structures for them.
- The `languagetag-bcp47-syntax` package defines BCP47 language tags purely
  syntactically and provides parsers for them. These tags meet the "well-formed"
  level of standard compliance defined in BCP47, but may contain tags that don't
  have any registry-defined meaning.
- The `languagetag-bcp47` package itself contains a complete copy of the current
  BCP47 registry rendered to individual Haskell records. It also provides types,
  parsers, and normalization and linting functions for these tags based on that
  copy of the registry. This should be renamed `languagetag-bcp47-static`,
  probably.
- The `languagetag-bcp47-dynamic` package defines tools for parsing the language
  tag registry. Hopefully in future (if I have time) it will contain tools for
  checking that tags meet the "vaild" level of standard compliance in BCP47
  based on a dynamically-loaded copy of the registry, and also normalize and
  lint these tags based on such a registry. This package exists for two reasons:
  one, it will be used in a future rewrite of `languagetag-bcp47-gen` to clean
  that code up; and two, it will be a lighter alternative to
  `languagetag-bcp47`, since that package, and especially the internal
  `Registry.Language` module, is very large.

The refactor is still not quite done - notably, `languagetag-bcp47-dynamic`
needs to be finished and `languagetag-bcp47` needs to be rearranged a bit.
Certain design decisions (regarding particular types, classes, even the package
structure and organization) still need to be made as well, and I haven't had
much free time recently to do any of this.
