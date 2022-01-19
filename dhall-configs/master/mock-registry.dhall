let common = ./common.dhall
let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5439
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_registry_v2"
  , connectSchemaName = "atlas_registry"
  }

in

{ port = +8020
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-registry.log"}
, esqDBCfg = esqDBCfg
, autoMigrate = common.autoMigrate
, migrationPath = None Text
}
