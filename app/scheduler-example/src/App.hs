module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Environment
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBConfig (..))
import Kernel.Storage.Esqueleto.Logger
import Kernel.Storage.Esqueleto.Migration
import Kernel.Types.Logging
import Kernel.Utils.App
import Kernel.Utils.Servant.Server (runServer)
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
            connectUser = "atlas_scheduler_example_user",
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
