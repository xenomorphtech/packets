{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "arrays"
  , "console"
  , "control"
  , "coroutines"
  , "css"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "freeap"
  , "functions"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "integers"
  , "maybe"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
