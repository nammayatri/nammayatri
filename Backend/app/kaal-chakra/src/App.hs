{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import Environment (HandlerCfg, HandlerEnv, buildHandlerEnv)
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App (getPodName, handleLeft)
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import Lib.Yudhishthira.Types
import Storage.Beam.SystemConfigs ()

allocatorHandle :: R.FlowRuntime -> HandlerEnv -> SchedulerHandle Chakra
allocatorHandle _flowRt env =
  SchedulerHandle
    { getTasksById = QAllJ.getTasksById,
      getReadyTasks = QAllJ.getReadyTasks $ Just env.maxShards,
      getReadyTask = QAllJ.getReadyTask,
      markAsComplete = QAllJ.markAsComplete,
      markAsFailed = QAllJ.markAsFailed,
      updateErrorCountAndFail = QAllJ.updateErrorCountAndFail,
      reSchedule = QAllJ.reSchedule,
      updateFailureCount = QAllJ.updateFailureCount,
      reScheduleOnError = QAllJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
        -- & putJobHandlerInList (liftIO . runFlowR flowRt env . sendSearchRequestToDrivers)
    }

runKaalChakra ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runKaalChakra configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "kaal-chakra"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.schedulerConfig.loggerConfig
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow
      flowRt
      ( ( prepareConnectionDriver
            ConnectionConfigDriver
              { esqDBCfg = handlerCfg.schedulerConfig.esqDBCfg,
                esqDBReplicaCfg = handlerCfg.esqDBReplicaCfg,
                hedisClusterCfg = handlerCfg.schedulerConfig.hedisClusterCfg
              }
            handlerCfg.kvConfigUpdateFrequency
        )
      )
    -- R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt handlerEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded handlerCfg.migrationPath handlerCfg.autoMigrate handlerCfg.schedulerConfig.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo "Setting up for signature auth..."
        let kvConfigs =
              Tables
                { enableKVForWriteAlso = [],
                  enableKVForRead = [],
                  useCAC = [],
                  useCACForFrontend = False,
                  readFromMasterDb = []
                }
        L.setOption KBT.Tables kvConfigs
        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency handlerEnv.maxShards $ allocatorHandle flowRt' handlerEnv
