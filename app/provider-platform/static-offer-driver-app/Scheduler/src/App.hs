module App where

import qualified Control.Monad.Catch as C
import qualified Domain.Types.RideRequest as RideRequest
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging (LoggerEnv)
import Lib.Scheduler
import SharedLogic.Scheduler
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SchedulerJob as QSJ

schedulerHandle :: SchedulerHandle SchedulerJobType
schedulerHandle =
  SchedulerHandle
    { getTasksById = QSJ.getTasksById,
      getReadyTasks = QSJ.getReadyTasks,
      markAsComplete = QSJ.markAsComplete,
      markAsFailed = QSJ.markAsFailed,
      updateErrorCountAndFail = QSJ.updateErrorCountAndFail,
      reSchedule = QSJ.reSchedule,
      updateFailureCount = QSJ.updateFailureCount,
      reScheduleOnError = QSJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
          & putJobHandlerInList allocateRentalRide
    }

runTransporterScheduler ::
  (SchedulerConfig -> SchedulerConfig) ->
  IO ()
runTransporterScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "transporter-scheduler"
  runSchedulerService appCfg schedulerHandle

data HandlerEnv = HandlerEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }

--------------------------------------

allocateRentalRide ::
  Job 'AllocateRental ->
  SchedulerM ExecutionResult
allocateRentalRide job = C.handleAll (const $ pure Retry) $ do
  guid <- Id <$> generateGUIDText
  now <- getCurrentTime
  let rideReq =
        RideRequest.RideRequest
          { id = guid,
            createdAt = now,
            bookingId = job.jobData.bookingId,
            subscriberId = job.jobData.shortOrgId,
            _type = RideRequest.ALLOCATION,
            info = Nothing
          }
  logInfo $ "allocating rental ride for rideReqestId=" <> job.jobData.bookingId.getId
  logPretty DEBUG "ride request" rideReq
  Esq.runTransaction $ RideRequest.create rideReq
  pure Complete
