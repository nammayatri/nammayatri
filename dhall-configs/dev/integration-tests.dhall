let common = ./common.dhall

let sec = ./secrets/integration-tests.dhall

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

in  { googleCfg = common.mockGoogleCfg
    , encTools
    , snapToRoadSnippetThreshold = common.snapToRoadSnippetThreshold
    }
