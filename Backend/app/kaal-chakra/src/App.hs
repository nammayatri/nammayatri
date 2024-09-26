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
import Environment (Flow, HandlerCfg, HandlerEnv, buildHandlerEnv)
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
import Kernel.Types.Flow (runFlowR)
import Kernel.Types.Id
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
        let allChakras = [Daily, Weekly, Monthly, Quarterly]
        shouldCreateAnyJob <- or <$> forM allChakras shouldCreateJob
        when shouldCreateAnyJob $ do
          allJobs :: [AnyJob Chakra] <- QAllJ.findAll
          forM_ allChakras $ updateJobsByChakra allJobs
        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt
    runSchedulerService handlerCfg.schedulerConfig handlerEnv.jobInfoMap handlerEnv.kvConfigUpdateFrequency handlerEnv.maxShards $ allocatorHandle flowRt' handlerEnv

-- TODO This code can be removed now, because we use bpp schema and can create dashboard api instead
updateJobsByChakra :: [AnyJob Chakra] -> Chakra -> Flow ()
updateJobsByChakra allJobs chakra = do
  shouldCreateJob' <- shouldCreateJob chakra
  when shouldCreateJob' $ do
    configuredTime <- getConfiguredTime chakra
    let pendingJobs = filter (isPendingJob chakra) allJobs
    let mbScheduledJob = find (jobAlreadyScheduled configuredTime) pendingJobs
    case mbScheduledJob of
      Just scheduledJob -> do
        -- complete all jobs except this one
        forM_ pendingJobs $ \job -> do
          when (getJobId job /= getJobId scheduledJob) $ do
            completeJob job
      Nothing -> do
        -- complete all jobs and create new one
        forM_ pendingJobs completeJob
        createJob chakra configuredTime

shouldCreateJob :: Chakra -> Flow Bool
shouldCreateJob Daily = asks (.shouldCreateDailyJob)
shouldCreateJob Weekly = asks (.shouldCreateWeeklyJob)
shouldCreateJob Monthly = asks (.shouldCreateMonthlyJob)
shouldCreateJob Quarterly = asks (.shouldCreateQuarterlyJob)

getConfiguredTime :: Chakra -> Flow UTCTime
getConfiguredTime Daily = asks (.dailySchedulerTime) >>= Time.getCurrentDailyJobTime
getConfiguredTime Weekly = asks (.weeklySchedulerTime) >>= Time.getCurrentWeeklyJobTime
getConfiguredTime Monthly = asks (.monthlySchedulerTime) >>= Time.getCurrentMonthlyJobTime
getConfiguredTime Quarterly = asks (.quarterlySchedulerTime) >>= Time.getCurrentQuarterlyJobTime

isPendingJob :: Chakra -> AnyJob Chakra -> Bool
isPendingJob chakra (AnyJob job) = do
  let jobType = fromSing $ job.jobInfo.jobType
  jobType == chakra && job.status == Pending

jobAlreadyScheduled :: UTCTime -> AnyJob Chakra -> Bool
jobAlreadyScheduled configuredTime (AnyJob job) =
  abs (diffUTCTime configuredTime job.scheduledAt) < 30 -- 30 secs in case of any rounding issues

getJobId :: AnyJob Chakra -> Id AnyJob
getJobId (AnyJob job) = job.id

completeJob :: AnyJob Chakra -> Flow ()
completeJob (AnyJob job) = do
  let jobType = fromSing $ job.jobInfo.jobType
  QAllJ.markAsComplete (show jobType) job.id
  logInfo $ "Completed old " <> show jobType <> " job: " <> show job.id

createJob :: Chakra -> UTCTime -> Flow ()
createJob chakra configuredTime = do
  maxShards <- asks (.maxShards)
  case chakra of
    Daily -> QAllJ.createJobByTime @_ @'Daily configuredTime maxShards EmptyData
    Weekly -> QAllJ.createJobByTime @_ @'Weekly configuredTime maxShards EmptyData
    Monthly -> QAllJ.createJobByTime @_ @'Monthly configuredTime maxShards EmptyData
    Quarterly -> QAllJ.createJobByTime @_ @'Quarterly configuredTime maxShards EmptyData
  logInfo $ "Scheduled new " <> show chakra <> " job"
