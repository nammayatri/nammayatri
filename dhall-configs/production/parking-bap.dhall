let common = ./common.dhall
let sec = ./secrets/parking-bap.dhall

let esqDBCfg =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5437
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_parking"
  , connectSchemaName = "atlas_parking"
  }

let rcfg =
  { connectHost = "cache.primary.beckn.juspay.net"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let nsdlGatewayUrl = "https://gateway-1.beckn.nsdl.co.in"
let juspayGatewayUrl = "https://api.beckn.juspay.in/gateway/v1"

in
{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8022
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/parking-bap.log"}
, graceTerminationPeriod = +90
, selfId = "api.beckn.juspay.in/bap/parking"
, selfURI = "https://api.beckn.juspay.in/bap/parking/v1"
, httpClientOptions = common.httpClientOptions
, authEntity =
    { credRegistry = common.credRegistry
    , signingKeys = common.signingKeys
    , signatureExpiry = common.signatureExpiry
    }
, authServiceUrl = common.authServiceUrl
, gatewayUrl = juspayGatewayUrl
, hostName = "juspay.in"
, metricsSearchDurationTimeout = +45
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, registryUrl = common.registryUrl
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
