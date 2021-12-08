let common = ./common.dhall
let sec = ./secrets/parking-bap.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5437
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_parking"
  , connectSchemaName = "atlas_parking"
  }

let rcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let nsdlGatewayUrl = "https://gateway-1.beckn.nsdl.co.in"
let juspayGatewayUrl = "http://localhost:8015/v1"

in
{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8022
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/parking-bap.log"}
, graceTerminationPeriod = +90
, selfId = "JUSPAY.MOBILITY.APP.UAT.3"
, selfURI = "http://localhost:8022/parking/v1"
, httpClientOptions = common.httpClientOptions
, authEntity =
    { credRegistry = common.credRegistry
    , signingKeys = common.signingKeys
    , signatureExpiry = common.signatureExpiry
    }
, authServiceUrl = common.authServiceUrl
, gatewayUrl = juspayGatewayUrl
, hostName = "localhost"
, metricsSearchDurationTimeout = +45
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, registryUrl = common.registryUrl
}
