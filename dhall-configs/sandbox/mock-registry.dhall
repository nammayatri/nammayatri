let common = ./common.dhall
let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_mock_registry"
  , connectSchemaName = "atlas_mock_registry"
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
