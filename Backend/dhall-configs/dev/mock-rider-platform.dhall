let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

let noSignatureSubscribers =
      [ "pre-prod-ondc-ticketing-api-delhi.transportstack.in" ]

in  { port = +8027
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-rider-platform.log" }
    , registryUrl = common.registryUrl
    , authEntity =
      { signingKey = sec.signingKey
      , uniqueKeyId = "mock-bap-key"
      , signatureExpiry = common.signatureExpiry
      }
    , selfId = "MOCK.BAP.UAT.1"
    , graceTerminationPeriod = +90
    , hostName = "localhost"
    , disableSignatureAuth = False
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , noSignatureSubscribers
    }
