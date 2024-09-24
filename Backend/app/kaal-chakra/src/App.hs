{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import Data.Singletons
import Environment (HandlerCfg, HandlerEnv, buildHandlerEnv)
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Jobs.Daily
import Jobs.Monthly
import Jobs.Quarterly
import Jobs.Weekly
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Storage.Queries.SystemConfigs as QSystemConfigs
import Kernel.Types.Error
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App (getPodName, handleLeft)
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import Lib.Yudhishthira.Types
import Storage.Beam.SchedulerJob ()
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Yudhishthira ()
import qualified Utils.Time as Time

allocatorHandle :: R.FlowRuntime -> HandlerEnv -> SchedulerHandle Chakra
allocatorHandle flowRt env =
  SchedulerHandle
    { getTasksById = QAllJ.getTasksById,
      getReadyTasks = setTablesOption >> QAllJ.getReadyTasks (Just env.maxShards),
      getReadyTask = QAllJ.getReadyTask,
      markAsComplete = QAllJ.markAsComplete,
      markAsFailed = QAllJ.markAsFailed,
      updateErrorCountAndFail = QAllJ.updateErrorCountAndFail,
      reSchedule = QAllJ.reSchedule,
      updateFailureCount = QAllJ.updateFailureCount,
      reScheduleOnError = QAllJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runDailyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runWeeklyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runMonthlyJob)
          & putJobHandlerInList (liftIO . runFlowR flowRt env . runQuarterlyJob)
    }
  where
    setTablesOption = do
      opt <- L.getOption KBT.Tables
      when (isNothing opt) $ do
        logError "KV Tables option did not found, setting up value from db."
        mbKvConfigs <- findById "kv_configs" >>= pure . decodeFromText' @Tables
        when (isNothing mbKvConfigs) $ do
          logError "KV Tables option did not found in db, setting up empty value."
        L.setOption KBT.Tables $ fromMaybe emptyKVConfigs mbKvConfigs

emptyKVConfigs :: Tables
emptyKVConfigs =
  Tables
    { enableKVForWriteAlso = [],
      enableKVForRead = [],
      useCAC = [],
      useCACForFrontend = False,
      readFromMasterDb = []
    }

runKaalChakra ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runKaalChakra configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "kaal-chakra"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.loggerConfigApp
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
        mbKvConfigs <- findById "kv_configs" >>= pure . decodeFromText' @Tables
        when (isNothing mbKvConfigs) $ do
          logError "KV Tables option did not found in db, setting up empty value."
        L.setOption KBT.Tables $ fromMaybe emptyKVConfigs mbKvConfigs
        kafkaProducerTools <- asks (.kafkaProducerTools)
        L.setOption KBT.KafkaConn kafkaProducerTools

        shouldCompleteOldJobs <- asks (.shouldCompleteOldJobs)
        when shouldCompleteOldJobs $ do
          allJobs :: [AnyJob Chakra] <- QAllJ.findAll
          forM_ allJobs $ \(AnyJob job) -> do
            let jobType = fromSing $ job.jobInfo.jobType
            when (jobType `elem` [Daily, Weekly, Monthly, Quarterly]) $ do
              QAllJ.markAsComplete (show jobType) job.id
              logInfo $ "Completed old " <> show jobType <> " job: " <> show job.id

        shouldCreateJobs <- asks (.shouldCreateJobs)
        when shouldCreateJobs $ do
          maxShards <- asks (.maxShards)
          shouldCreateDailyJob <- asks (.shouldCreateDailyJob)
          shouldCreateWeeklyJob <- asks (.shouldCreateWeeklyJob)
          shouldCreateMonthlyJob <- asks (.shouldCreateMonthlyJob)
          shouldCreateQuarterlyJob <- asks (.shouldCreateQuarterlyJob)
          unless (or [shouldCreateDailyJob, shouldCreateWeeklyJob, shouldCreateMonthlyJob, shouldCreateQuarterlyJob]) $ do
            throwError $ InternalError "No jobs enabled"

          dailyJobTime <- asks (.dailySchedulerTime) >>= Time.getCurrentDailyJobTime
          weeklyJobTime <- asks (.weeklySchedulerTime) >>= Time.getCurrentWeeklyJobTime
          monthlyJobTime <- asks (.monthlySchedulerTime) >>= Time.getCurrentMonthlyJobTime
          quarterlyJobTime <- asks (.quarterlySchedulerTime) >>= Time.getCurrentQuarterlyJobTime

          when shouldCreateDailyJob $ do
            QAllJ.createJobByTime @_ @'Daily dailyJobTime maxShards EmptyData
            logInfo "Scheduled new Daily job"
          when shouldCreateWeeklyJob $ do
            QAllJ.createJobByTime @_ @'Weekly weeklyJobTime maxShards EmptyData
            logInfo "Scheduled new Weekly job"
          when shouldCreateMonthlyJob $ do
            QAllJ.createJobByTime @_ @'Monthly monthlyJobTime maxShards EmptyData
            logInfo "Scheduled new Monthly job"
          when shouldCreateQuarterlyJob $ do
            QAllJ.createJobByTime @_ @'Quarterly quarterlyJobTime maxShards EmptyData
            logInfo "Scheduled new Quarterly job"

        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency handlerEnv.maxShards $ allocatorHandle flowRt' handlerEnv
