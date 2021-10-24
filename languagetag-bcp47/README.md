# Languagetag BCP47

This package provides parsers and tools for analyzing
[BCP47](https://tools.ietf.org/html/bcp47) language tags, as of its 2009
version. The code related to the [IANA subtag
registry](https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry)
is generated automatically from a local copy in `data/registry`. The date of the
repository that is used can be found at the top of that file, or in the
documentation for `LanguageTag.BCP47.Registry.bcp47RegistryDate`.

## The structure of the package

The modules are organized under the `LanguageTag.BCP47` namespace. The main
ones are:

- `Subtag`: defines a compact representation of a BCP47 subtag. In a full tag,
  these can represent languages (`en`, `fr`, `cmn`, `ja`), scripts (`Latn`,
  `Hant`, `Cyrl`), and regions (`US`, `419`), among other things.

- `Syntax`: defines an opaque `BCP47` type that represents well-formed
  (syntactically correct) tags according to the standard, and a `Text` parser
  for that type.

- `Registry`: defines a `BCP47` type that represents valid tags according to the
  standard. It also exports enumerations for all of the different registered
  tags and subtags in the IANA registry, as well as their records and record
  lookup functions.

- `Validation`: defines functions that validate raw `Subtag` values and
  `Syntax.BCP47` tags, returning subtags and `BCP47` tags from `Registry`.

- `Canonicalization`: defines functions that canonicalize valid BCP47 tags
  (replacing deprecated tags and extended language tags) according to the
  standard. It also defines more general tag linting functions that detect and
  sometimes fix aspects of tags that are not recommended by the standard, such
  as the presence of redundant script subtags or the presence of variant subtags
  with unsatisfied prefixes.

- `Quasi`: defines quasi-quoters to construct statically-checked subtags and
  tags. The main `tag` quasi-quoter ensures that its input passes linting
  without any warnings, but there are also other, less strict quasi-quoters in
  the module.

Note that matching based on language ranges (basic or extended) has not yet been
added to the package. The remainder of the standard, however, is fully
supported.

## Package update policy

Since this library will need to update from time to time as the subtag registry
itself updates, the following version-bumping scheme will apply to breaking
changes: if the only breaking change to the package is to update the
automatically-generated functions and types in response to a registry update,
the second major version number will be increased, and for all other breaking
changes the first major version number will be increased.

Certain stability guarantees also happen to apply to the library and its
behaviour, assuming we or the registry haven't made a mistake (as has happened
in the past, though we'll try to catch registry mistakes before updating the
library). These are derived from the underlying BCP47 standard:

- the enumerations of registered subtags will never have a constructor removed
  from them; the set of registered subtags of each type will only ever expand

- the grandfathered and redundant subtag enumerations will never have a
  constructor added or removed from them

- prefixes may be added to records that already have them, or modified in such
  records, as long as such modifications only expand the set of possible
  prefixes; if a record does not have any prefixes then a prefix will never be
  added to the record

Otherwise the relevant types and functions will change as the registry updates.
See the [registry maintenance
section](https://tools.ietf.org/html/bcp47#section-3.3) of the standard for the
full details.
