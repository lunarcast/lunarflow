let client = ./packages/client/spago.dhall

let overrides =
      { sources =
        [ "packages/client/src/**/*.purs", "packages/client/test/**/*.purs" ]
      }

in  client ⫽ overrides
