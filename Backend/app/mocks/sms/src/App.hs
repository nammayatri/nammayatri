{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE QuantifiedConstraints #-}

module App
  ( runMockSms,
  )
where

import App.Routes (mockSmsAPI, mockSmsServer)
import App.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (ApiPriorityList (ApiPriorityList))
import Kernel.Types.Logging
import Kernel.Utils.Servant.Server
import Servant

runMockSms :: (AppCfg -> AppCfg) -> IO ()
runMockSms configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerWithHealthCheck appEnv mockSmsAPI mockSmsServer identity identity EmptyContext releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { port = 4343,
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/mock-sms.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 90,
      criticalAPIs = ApiPriorityList []
    }
