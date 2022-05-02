module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBConfig (..))
import Beckn.Storage.Esqueleto.Logger
import Beckn.Storage.Esqueleto.Migration
import Beckn.Types.Logging
import Beckn.Utils.App
import Beckn.Utils.Servant.Server (runServer)
import Environment
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  let config = configModifier defaultConfig
  appEnv <- buildAppEnv config
  let runMigrations :: LoggerIO ()
      runMigrations = do
        eithRes <- migrateIfNeeded config.migrationPath config.autoMigrate config.esqDBCfg
        handleLeft exitDBMigrationFailure "Couldn't migrate database: " eithRes
  runLoggerIO appEnv.loggerEnv runMigrations
  runServer appEnv (Proxy @API) handler identity identity EmptyContext (const identity) releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { migrationPath = Just "dev/migrations/scheduler-example",
      autoMigrate = True,
      esqDBCfg =
        EsqDBConfig
          { connectHost = "localhost",
            connectPort = 5434,
            connectUser = "atlas",
            connectPassword = "atlas",
            connectDatabase = "atlas_dev",
            connectSchemaName = "atlas_scheduler_example"
          },
      port = 8050,
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/scheduler-example-app.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 10
    }
