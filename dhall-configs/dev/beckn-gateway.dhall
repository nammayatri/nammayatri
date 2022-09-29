let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let rcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

in

{ redisCfg = rcfg
, port = +8015
, metricsPort = +9998
, selfId = "JUSPAY.BG.1"
, hostName = "localhost"
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "juspay-bg-1-key"
  , signatureExpiry = common.signatureExpiry
  }
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, graceTerminationPeriod = +90
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, disableSignatureAuth = False
}
