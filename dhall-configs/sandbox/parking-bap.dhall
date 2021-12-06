let common = ./common.dhall
let sec = ./secrets/parking-bap.dhall

let esqDBCfg =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5437
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_parking"
  , connectSchemaName = "atlas_parking"
  }

in
{ port = +8022
, esqDBCfg = esqDBCfg
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/parking-bap.log"}
, graceTerminationPeriod = +90
, selfId = "api.sandbox.beckn.juspay.in/bap/parking"
, httpClientOptions = common.httpClientOptions
, authEntity =
    { credRegistry = common.credRegistry
    , signingKeys = common.signingKeys
    , signatureExpiry = common.signatureExpiry
    }
, authServiceUrl = common.authServiceUrl
}
