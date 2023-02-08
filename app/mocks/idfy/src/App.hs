module App where

import App.Routes (mockIdfyAPI, mockIdfyServer)
import App.Types
import qualified EulerHS.Runtime as R
import Kernel.Prelude
import Kernel.Types.Logging
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.Server
import Servant
import Servant.Client.Core

runMockIdfy :: (AppCfg -> AppCfg) -> IO ()
runMockIdfy configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerWithHealthCheck appEnv mockIdfyAPI mockIdfyServer identity identity EmptyContext releaseAppEnv \flowRt -> do
    managers <- createManagers mempty -- default manager is created
    pure $ flowRt {R._httpClientManagers = managers}

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { port = 6235,
      callbackWaitTimeMilliSec = 500,
      webhookUrl = BaseUrl Http "localhost" 8016 "",
      graceTerminationPeriod = 90,
      accountId = "xxxxxxx",
      apiKey = "xxxxxxx",
      secret = "xxxxxxx",
      httpClientOptions =
        HttpClientOptions
          { timeoutMs = 2000
          },
      shortDurationRetryCfg =
        RetryCfg
          { maxRetries = 3,
            baseCoefficient = 1
          },
      longDurationRetryCfg =
        RetryCfg
          { maxRetries = 3,
            baseCoefficient = 2
          },
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/mock-idfy.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          }
    }
