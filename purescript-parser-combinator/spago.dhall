{ name = "parser-combinator"

, dependencies =
    [ "prelude"
	, "tuples"
	]

, backend = "purenix"

, packages = ../packages.dhall

, sources =
    [ "src/**/*.purs"
    -- , "test/**/*.purs"
    ]
}
