module App
  ( runService,
  )
where

import App.Routes (serverAPI, serverHandler)
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBConfig (..))
import Beckn.Types.Logging
import Beckn.Utils.Servant.Server (runServerService)
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerService appEnv serverAPI serverHandler identity identity EmptyContext releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { esqDBCfg =
        EsqDBConfig
          { connectHost = "localhost",
            connectPort = 1234,
            connectUser = "User",
            connectPassword = "Pass",
            connectDatabase = "DB",
            connectSchemaName = "Schema"
          },
      port = 1111,
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/example-service.log",
            logToConsole = True,
            logRawSql = True
          },
      graceTerminationPeriod = 90
    }
