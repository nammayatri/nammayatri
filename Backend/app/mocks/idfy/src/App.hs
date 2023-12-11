{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import App.Routes (mockIdfyAPI, mockIdfyServer)
import App.Types
import qualified EulerHS.Runtime as R
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (ApiPriorityList (ApiPriorityList))
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
      criticalAPIs = ApiPriorityList [],
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
