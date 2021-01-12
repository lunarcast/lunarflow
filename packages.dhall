let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201223/packages.dhall sha256:a1a8b096175f841c4fef64c9b605fb0d691229241fd2233f6cf46e213de8a185

let overrides =
      { run =
        { dependencies =
          [ "aff"
          , "effect"
          , "either"
          , "free"
          , "maybe"
          , "newtype"
          , "prelude"
          , "profunctor"
          , "tailrec"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/Mateiadrielrafael/purescript-run.git"
        , version = "master"
        }
      }

let additions =
      { debugged =
        { dependencies =
          [ "prelude"
          , "console"
          , "ordered-collections"
          , "either"
          , "tuples"
          , "lists"
          , "strings"
          , "arrays"
          , "bifunctors"
          , "record"
          , "effect"
          , "generics-rep"
          , "datetime"
          , "enums"
          ]
        , repo = "https://github.com/hdgarrood/purescript-debugged"
        , version = "744498226da3c9b5b37c69771cc0378a65cc8189"
        }
      , undefined-is-not-a-problem =
        { dependencies =
          [ "assert"
          , "console"
          , "effect"
          , "foreign"
          , "prelude"
          , "psci-support"
          , "random"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/paluh/purescript-undefined-is-not-a-problem"
        , version = "2f8b4e91a814cf558bdc15dc96667ac806c4730f"
        }
      }

let packages =
      upstream
      with lunarflow-utils = ./packages/utils/spago.dhall as Location
      with lunarflow-core = ./packages/core/spago.dhall as Location
      with lunarflow-geometry = ./packages/geometry/spago.dhall as Location

in  packages // overrides // additions
