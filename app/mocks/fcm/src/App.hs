module App
  ( runMockFcm,
  )
where

import App.Routes (mockFcmAPI, mockFcmServer)
import App.Types
import Kernel.Prelude
import Kernel.Types.Logging
import Kernel.Utils.Servant.Server
import Servant (Context (..))

runMockFcm :: (AppCfg -> AppCfg) -> IO ()
runMockFcm configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerWithHealthCheck appEnv mockFcmAPI mockFcmServer identity identity EmptyContext releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { port = 4545,
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/mock-fcm.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 90
    }
