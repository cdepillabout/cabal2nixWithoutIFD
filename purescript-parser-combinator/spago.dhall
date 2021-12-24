{ name = "parser-combinator"
, dependencies = [ "control", "either", "maybe", "prelude", "tuples" ]
, backend = "purenix"
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}
