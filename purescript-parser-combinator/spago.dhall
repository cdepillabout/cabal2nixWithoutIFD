{ name = "parser-combinator"
, dependencies = [ "control", "either", "prelude", "tuples" ]
, backend = "purenix"
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}
