let common = ./common.dhall
let sec = ./secrets/public-transport-bap.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5438
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_public_transport"
  , connectSchemaName = "atlas_public_transport"
  }

in
{ esqDBCfg = esqDBCfg
, port = +8023
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/public-transport-bap.log"}
, graceTerminationPeriod = +90
}
