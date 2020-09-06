{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "free"
  , "generics-rep"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
