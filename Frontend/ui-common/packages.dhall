{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210226/packages.dhall
        sha256:7e973070e323137f27e12af93bc2c2f600d53ce4ae73bb51f34eb7d7ce0a43ea

in  upstream
  with presto = 
    { dependencies =
        [ "aff"
        , "avar"
        , "console"
        , "control"
        , "datetime"
        , "debug"
        , "exceptions"
        , "exists"
        , "foreign-generic"
        , "foreign-object"
        , "free"
        , "generics-rep"
        , "prelude"
        , "record"
        , "transformers"
        , "tuples"
        , "typelevel-prelude"
        ]
    , repo = "ssh://git@bitbucket.org/juspay/purescript-presto.git"
    , version = "master"
    }
  with backtrack = 
    { dependencies =
        [ "prelude"
        , "console"
        , "transformers"
        , "tailrec"
        ]
    , repo = "https://github.com/juspay/purescript-backtrack.git"
    , version = "master"
    }
  with presto-dom =
    { dependencies = 
        [ "behaviors"
        , "console"
        , "effect"
        , "event"
        , "prelude"
        , "psci-support"
        , "halogen-vdom"
        , "tracker"
        ]
    , repo = "ssh://git@bitbucket.org/juspay/purescript-presto-dom.git"
    , version = "BKN-1862/BKN-1711-shadow-subscreen"
    }
  with halogen-vdom =
    { dependencies = 
        [ "behaviors"
        , "bifunctors"
        , "effect"
        , "foreign"
        , "foreign-object"
        , "maybe"
        , "prelude"
        , "refs"
        , "tuples"
        , "unsafe-coerce"
        , "web-html"
        ]
    , repo = "ssh://git@bitbucket.org/juspay/purescript-halogen-vdom.git"
    , version = "BKN-1862-subscreen"
    }
  with tracker = 
    { dependencies = 
        [ "prelude"
        , "effect"
        , "foreign-generic"
        ]
    , repo = "ssh://git@bitbucket.org/juspay/purescript-tracker.git"
    , version = "master"
    }
  with otp-reader =
    { dependencies = 
        [ "numbers"
        , "tracker"
        , "presto"
        ]
    , repo = "ssh://git@bitbucket.org/juspay/purescript-otp-reader.git"
    , version = "master"
    }
  with beckn-common = ./Common/spago.dhall as Location