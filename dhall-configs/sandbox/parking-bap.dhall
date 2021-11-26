let common = ./common.dhall

let esqDBCfg =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5437
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_parking"
  , connectSchemaName = "atlas_parking"
  }

in
{ esqDBCfg = esqDBCfg
, port = +8022
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/parking-bap.log"}
, graceTerminationPeriod = +90
, selfId = "api.sandbox.beckn.juspay.in/bap/parking/v1"
, httpClientOptions = common.httpClientOptions
, authEntity =
    { credRegistry = common.credRegistry
    , signingKeys = common.signingKeys
    , signatureExpiry = common.signatureExpiry
    }
}
