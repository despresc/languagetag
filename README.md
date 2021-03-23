# Languagetags: Language tag analysis

The `languagetags` library provides
[BCP47](https://tools.ietf.org/html/bcp47)-compliant language tag
parsing and processing tools. The types and functions related to
subtag validation, canonicalization, and other registry-related
processing are generated automatically using the `languagetags-gen`
package in this repository, which reads from a local copy of the [IANA
subtag
registry](https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry)
in `languagetags-gen/registry/bcp47`. The date of the repository that
is used can be found at the top of that file, or in the documentation
for `Text.LanguageTag.BCP47.Registry.bcp47RegistryDate` in the
`languagetags` package here.

Since this library will need to update from time to time as the subtag
registry itself updates, the following version-bumping scheme will
apply to breaking changes: if the only breaking change to the package
is to update the automatically-generated functions and types in
response to a registry update, the second major version number will be
increased, and for all other breaking changes the first major version
number will be increased.

Certain stability guarantees also happen to apply to the library and
its behaviour, assuming we or the registry haven't made a mistake (as
has happened in the past, though we'll try to catch registry mistakes
before updating the library). These are derived from the underlying
BCP47 standard:

- the enumerations of registered subtags will never have a constructor
  removed from them; the set of registered subtags of each type will
  only ever expand

- the grandfathered and redundant subtag enumerations will never have
  a constructor added or removed from them

- prefixes may be added to records that already have them, or modified
  in such records, as long as such modifications only expand the set
  of possible prefixes; if a record does not have any prefixes then a
  prefix will never be added to the record

Otherwise the relevant types and functions will change as the registry
updates. See the [registry maintenance
section](https://tools.ietf.org/html/bcp47#section-3.3) of the
standard for the full details.

## Future

The library may be updated in future to add support for other
language-tag-related standards, like ISO 639.
