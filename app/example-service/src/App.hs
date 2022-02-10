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
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 90
    }
