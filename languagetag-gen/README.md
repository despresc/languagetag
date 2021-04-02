# Languagetag-gen: Internal code generation executable

The `languagetag-gen` executable provided by this package is
responsible for generating the internal registry-related modules in
the `languagetag-bcp47` package from the data in the `registry/`
directory. The code here is of course extremely specialized, and you
shouldn't need to use this package unless you happen to be developing
`languagetag`. The executable `languagetag-gen` should be run from
either this directory or the project directory.
