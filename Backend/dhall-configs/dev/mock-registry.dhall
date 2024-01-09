let common = ./common.dhall

let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_registry"
      , connectionPoolCount = +25
      }

in  { port = +8020
    , graceTerminationPeriod = +90
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-registry.log" }
    , esqDBCfg
    , autoMigrate = True
    , migrationPath =
      [   env:MOCK_REGISTRY_MIGRATION_PATH as Text
        ? "dev/migrations/mock-registry"
      ]
    }
