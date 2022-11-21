let common = ./common.dhall
let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_mock_registry_v2"
  , connectSchemaName = "atlas_mock_registry"
  }

let esqDBReplicaCfg =
  { connectHost = esqDBCfg.connectHost
  , connectPort = 5435
  , connectUser = esqDBCfg.connectUser
  , connectPassword = esqDBCfg.connectPassword
  , connectDatabase = esqDBCfg.connectDatabase
  , connectSchemaName = esqDBCfg.connectSchemaName
  }

in

{ port = +8020
, graceTerminationPeriod = +90
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-registry.log"}
, esqDBCfg = esqDBCfg
, esqDBReplicaCfg = esqDBReplicaCfg
, autoMigrate = common.autoMigrate
, migrationPath = None Text
}
