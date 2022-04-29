module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBConfig (..))
import Beckn.Types.Logging
import Beckn.Utils.Servant.Server (runServer)
import Environment
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServer appEnv (Proxy @API) handler identity identity EmptyContext (const identity) releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { esqDBCfg =
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
