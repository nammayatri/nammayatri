let common = ./common.dhall
let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_registry"
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
, autoMigrate = True
, migrationPath = Some (env:MOCK_REGISTRY_MIGRATION_PATH as Text ? "dev/migrations/mock-registry")
}
