let common = ./common.dhall

let sec = ./secrets/integration-tests.dhall

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

in  { googleCfg = common.mockGoogleCfg
    , encTools
    , snapToRoadSnippetThreshold = +300
    , appPrefix = "integration-tests"
    , kafkaProducerCfg
    }
