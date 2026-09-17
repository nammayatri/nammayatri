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
{ name = "beckn-common"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "backtrack"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "free"
  , "functions"
  , "halogen-vdom"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "presto"
  , "presto-dom"
  , "refs"
  , "strings"
  , "tailrec"
  , "tracker"
  , "transformers"
  , "tuples"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}
