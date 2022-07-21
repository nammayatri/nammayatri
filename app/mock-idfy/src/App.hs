module App where

import App.Routes (mockIdfyAPI, mockIdfyServer)
import App.Types
import Beckn.Prelude
import Beckn.Types.Logging
import Beckn.Utils.Servant.Server
import Servant
import Servant.Client.Core
import qualified EulerHS.Runtime as R
import Beckn.Utils.Servant.Client

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
          { timeoutMs = 2000,
            maxRetries = 3
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
