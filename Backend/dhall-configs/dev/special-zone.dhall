let sec = ./secrets/provider-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5436
      , connectUser = "atlas_special_zone_user"
      , connectPassword = "atlas"
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_special_zone"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5437
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let loggerConfig =
      { level = LogLevel.DEBUG
      , logToFile = True
      , logFilePath = "/tmp/special-location.log"
      , logToConsole = True
      , logRawSql = True
      , prettyPrinting = True
      }

in  { port = +8032
    , migrationPath = [ "dev/migrations/special-zone" ]
    , autoMigrate = True
    , esqDBCfg
    , esqDBReplicaCfg
    , dashboardToken = sec.specialZoneToken
    , loggerConfig
    , graceTerminationPeriod = +90
    , apiKey = "170f2a2a-b014-4a86-b4c0-e453e8v0b660"
    }
