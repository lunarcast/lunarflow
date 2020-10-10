let client = ./packages/core/spago.dhall

let overrides = { sources = [ "packages/client/src/**/*.purs" ] }

in  client ⫽ overrides
