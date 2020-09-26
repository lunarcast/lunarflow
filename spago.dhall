{ name = "lunarflow"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "free"
  , "generics-rep"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
