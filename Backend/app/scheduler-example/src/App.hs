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
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBConfig (..))
import Kernel.Storage.Esqueleto.Logger
import Kernel.Storage.Esqueleto.Migration
import Kernel.Storage.Hedis (HedisCfg (..))
import Kernel.Tools.Metrics.CoreMetrics (ApiPriorityList (ApiPriorityList))
import Kernel.Types.Logging
import Kernel.Utils.App
import Kernel.Utils.Servant.Server (runServer)
import Lib.Scheduler (SchedulerType (DbBased))
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  let config = configModifier defaultConfig
  appEnv <- buildAppEnv config
  let runMigrations :: LoggerIO ()
      runMigrations = do
        eithRes <- migrateIfNeeded config.migrationPath config.autoMigrate config.esqDBCfg
        handleLeft exitDBMigrationFailure "Couldn't migrate database: " eithRes
  runLoggerIO appEnv.loggerEnv runMigrations
  runServer appEnv (Proxy @API) handler identity identity EmptyContext (const identity) releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { migrationPath = Just "dev/migrations/scheduler-example",
      autoMigrate = True,
      esqDBCfg =
        EsqDBConfig
          { connectHost = "localhost",
            connectPort = 5434,
            connectUser = "atlas_scheduler_example_user",
            connectPassword = "atlas",
            connectDatabase = "atlas_dev",
            connectSchemaName = "atlas_scheduler_example",
            connectionPoolCount = 25
          },
      port = 8050,
      hedisCfg =
        HedisCfg
          { connectHost = "localhost",
            connectPort = 30001,
            connectAuth = Nothing,
            connectDatabase = 0,
            connectMaxConnections = 50,
            connectMaxIdleTime = 30,
            connectTimeout = Nothing
          },
      hedisClusterCfg =
        HedisCfg
          { connectHost = "localhost",
            connectPort = 30001,
            connectAuth = Nothing,
            connectDatabase = 0,
            connectMaxConnections = 50,
            connectMaxIdleTime = 30,
            connectTimeout = Nothing
          },
      hedisNonCriticalCfg =
        HedisCfg
          { connectHost = "localhost",
            connectPort = 30001,
            connectAuth = Nothing,
            connectDatabase = 0,
            connectMaxConnections = 50,
            connectMaxIdleTime = 30,
            connectTimeout = Nothing
          },
      hedisNonCriticalClusterCfg =
        HedisCfg
          { connectHost = "localhost",
            connectPort = 30001,
            connectAuth = Nothing,
            connectDatabase = 0,
            connectMaxConnections = 50,
            connectMaxIdleTime = 30,
            connectTimeout = Nothing
          },
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/scheduler-example-app.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 10,
      cutOffHedisCluster = False,
      hedisMigrationStage = False,
      enablePrometheusMetricLogging = False,
      jobInfoMapx = mempty,
      schedulerSetName = "",
      streamName = "",
      schedulerType = DbBased,
      enableRedisLatencyLogging = False,
      groupName = "",
      criticalAPIs = ApiPriorityList []
    }
