let common = ./common.dhall

let sec = ./secrets/mock-registry.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_registry"
      , connectionPoolCount = +25
      }

in  { port = Natural/toInteger (env:SERVICE_PORT ? 8020)
    , graceTerminationPeriod = +90
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-registry.log" }
    , esqDBCfg
    , autoMigrate = True
    , migrationPath = Some
        (   env:MOCK_REGISTRY_MIGRATION_PATH as Text
          ? "dev/ddl-migrations/mock-registry"
        )
    , internalAuthApiKey = sec.internalAuthApiKey
    }
