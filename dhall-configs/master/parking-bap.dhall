let common = ./common.dhall
let sec = ./secrets/parking-bap.dhall

let esqDBCfg =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_parking_v2"
  , connectSchemaName = "atlas_parking"
  }

let rcfg =
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let nsdlGatewayUrl = "https://gateway-1.beckn.nsdl.co.in"
let juspayGatewayUrl = "https://api.sandbox.beckn.juspay.in/latest/gateway/v1"

in
{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8022
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/parking-bap.log"}
, graceTerminationPeriod = +90
, selfId = "api.sandbox.beckn.juspay.in/dev/bap/parking/v1"
, selfURI = "https://api.sandbox.beckn.juspay.in/dev/bap/parking/v1"
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
, registryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, disableSignatureAuth = False
}
