{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBConfig (..))
import Kernel.Types.Logging
import Kernel.Utils.Servant.Server (runServer)
import Servant (Context (..))
import Tools.Metrics (ApiPriorityList (ApiPriorityList))

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
            connectSchemaName = "Schema",
            connectionPoolCount = 25
          },
      port = 1111,
      criticalAPIs = ApiPriorityList [],
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
