let common = ./common.dhall

let esqDBCfg =
  { connectHost = "localhost"
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
, selfId = "JUSPAY.MOBILITY.APP.UAT.3"
, httpClientOptions = common.httpClientOptions
, authEntity =
    { credRegistry = common.credRegistry
    , signingKeys = common.signingKeys
    , signatureExpiry = common.signatureExpiry
    }
}
