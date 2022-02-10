let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let rcfg =
  { connectHost = "cache.primary.beckn.juspay.net"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let coreVersions =
  { mobility = "0.8.2"
  , finalMileDelivery = "0.9.1"
  , localRetail = "0.9.1"
  , foodAndBeverage = "0.9.1"
  }

in

{ redisCfg = rcfg
, port = +8015
, metricsPort = +9999
, selfId = "api.beckn.juspay.in/gateway/v1"
, hostName = "juspay.in"
, nwAddress = "https://api.beckn.juspay.in/gateway/v1/"
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "7"
  , signatureExpiry = common.signatureExpiry
  }
, searchTimeout = None Integer
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, coreVersions = coreVersions
, mobilityDomainVersion = "0.9.3"
, graceTerminationPeriod = +90
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
}
