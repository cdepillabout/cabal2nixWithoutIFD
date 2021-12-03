{ name = "cabal-parser"
, dependencies = [ "arrays", "control", "either", "parser-combinator", "prelude", "tuples", "unsafe-coerce" ]
, packages = ../packages.dhall
, backend = "purenix"
, sources = [ "src/**/*.purs" ]
}
