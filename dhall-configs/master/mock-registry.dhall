let common = ./common.dhall
let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
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
