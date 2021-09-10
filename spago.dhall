{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "console"
  , "control"
  , "effect"
  , "freeap"
  , "halogen"
  , "halogen-css"
  , "parallel"
  , "psci-support"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
