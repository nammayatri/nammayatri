{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ProcessCancellationPenaltyStatus
  ( processCancellationPenaltyStatus,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.DriverFee as DF
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.DriverFee as QDF

processCancellationPenaltyStatus ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Job 'ProcessCancellationPenaltyStatus ->
  m ExecutionResult
processCancellationPenaltyStatus Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      driverFeeId = Id jobData.driverFeeId
      targetStatusText = jobData.targetStatus
  logInfo $ "ProcessCancellationPenaltyStatus: Processing cancellation penalty status change for driverFeeId: " <> jobData.driverFeeId <> " to status: " <> targetStatusText
  mbDriverFee <- QDF.findById driverFeeId
  case mbDriverFee of
    Nothing -> do
      logError $ "ProcessCancellationPenaltyStatus: DriverFee not found: " <> jobData.driverFeeId
      return Complete
    Just driverFee -> do
      now <- getCurrentTime
      case targetStatusText of
        "IN_DISPUTE_WINDOW" -> do
          when (driverFee.status == DF.ONGOING) $ do
            logInfo $ "ProcessCancellationPenaltyStatus: Transitioning to IN_DISPUTE_WINDOW for driverFeeId: " <> jobData.driverFeeId
            QDF.updateStatus DF.IN_DISPUTE_WINDOW driverFeeId now
        "PAYMENT_PENDING" -> do
          when (driverFee.status == DF.IN_DISPUTE_WINDOW) $ do
            logInfo $ "ProcessCancellationPenaltyStatus: Transitioning to PAYMENT_PENDING for driverFeeId: " <> jobData.driverFeeId
            QDF.updateStatus DF.PAYMENT_PENDING driverFeeId now
        _ -> do
          logWarning $ "ProcessCancellationPenaltyStatus: Unknown target status: " <> targetStatusText
      return Complete
