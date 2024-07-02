{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckExotelDoFallback where

import Domain.Types.CallStatus as DCallStatus
import qualified Kernel.External.Call.Interface.Types as CallTypes
import qualified Kernel.External.Call.Types as CallTypes
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Tools.Notifications as FCM

checkExotelDoFallback ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    SchedulerFlow r
  ) =>
  Job 'CheckExotelStatusDoFallback ->
  m ExecutionResult
checkExotelDoFallback Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let rideId = jobData.rideId
  let endTime = jobData.endTime
  let personId = jobData.personId
  now <- getCurrentTime
  if now >= endTime
    then do
      callStatus <- QCallStatus.findByRideId (Just rideId) >>= fromMaybeM CallStatusDoesNotExist
      if callStatus.status `elem` failedCallStatuses && callStatus.callService == Just CallTypes.Exotel
        then do
          QCallStatus.updateCallAttempt (Just DCallStatus.Failed) (Just rideId)
          return Complete
        else
          if callStatus.status `elem` failedCallStatuses && callStatus.callService == Just CallTypes.Knowlarity
            then do
              QCallStatus.updateCallAttempt (Just DCallStatus.Failed) (Just rideId)
              FCM.notifyDirectCallOnCallServiceDown personId
              return Complete
            else do
              return Complete
    else throwError (InternalError "currentTime is less than endTime")
  where
    failedCallStatuses = [CallTypes.INVALID_STATUS, CallTypes.NOT_CONNECTED, CallTypes.FAILED]
