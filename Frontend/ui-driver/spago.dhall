{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "--"
, dependencies =
  [ "backtrack"
  , "console"
  , "debug"
  , "effect"
  , "formatters"
  , "free"
  , "integers"
  , "js-timers"
  , "numbers"
  , "prelude"
  , "presto"
  , "presto-dom"
  , "profunctor-lenses"
  , "beckn-common"
  , "tailrec"
  , "transformers"
  , "tracker"
  , "otp-reader"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
