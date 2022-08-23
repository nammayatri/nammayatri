let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let rcfg =
  { connectHost = "beckn-redis-001-001.zkt6uh.0001.aps1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

in

{ redisCfg = rcfg
, port = +8015
, metricsPort = +9999
, selfId = "api.sandbox.beckn.juspay.in/dev/gateway/v1"
, hostName = "juspay.in"
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "22"
  , signatureExpiry = common.signatureExpiry
  }
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, graceTerminationPeriod = +90
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, disableSignatureAuth = False
}
