module App
  ( runMockFcm,
  )
where

import App.Types
import Beckn.Types.Logging
import App.Routes (mockFcmAPI, mockFcmServer)
import Beckn.Prelude
import Beckn.Utils.Servant.Server
import Servant (Context (..))

runMockFcm :: (AppCfg -> AppCfg) -> IO ()
runMockFcm configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerService appEnv mockFcmAPI mockFcmServer identity identity EmptyContext releaseAppEnv pure

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
            logRawSql = True
          },
      graceTerminationPeriod = 90
    }
