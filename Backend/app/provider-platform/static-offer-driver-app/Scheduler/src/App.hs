{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import qualified Lib.Scheduler.JobStorageType.DB.Queries as QSJ
import SharedLogic.Scheduler
import qualified Storage.Queries.RideRequest as RideRequest

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
            bookingId = job.jobInfo.jobData.bookingId,
            subscriberId = job.jobInfo.jobData.shortOrgId,
            _type = RideRequest.ALLOCATION,
            info = Nothing
          }
  logInfo $ "allocating rental ride for rideReqestId=" <> job.jobInfo.jobData.bookingId.getId
  logPretty DEBUG "ride request" rideReq
  Esq.runTransaction $ RideRequest.create rideReq
  pure Complete
