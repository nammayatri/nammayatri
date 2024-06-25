{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.SendSafetyIVR where

import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Tools.Call as Call

sendSafetyIVR ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    SchedulerFlow r
  ) =>
  Job 'SendSafetyIVR ->
  m ()
sendSafetyIVR Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
  ride <- QR.findById jobData.rideId >>= fromMaybeM (RideDoesNotExist jobData.rideId.getId)
  if ride.safetyCheckStatus == Just True
    then do
      logTagInfo ("RideId: " <> ride.id.getId) "Safety notification already sent."
      pure ()
    else do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      maxShards <- asks (.maxShards)
      triggerIVR person ride
      let policeJobData = CallPoliceApiJobData {rideId = ride.id, personId = personId}
      createJobIn @_ @'CallPoliceApi (5 * 60) maxShards (policeJobData :: CallPoliceApiJobData)
  where
    triggerIVR ::
      ( EncFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBFlow m r,
        HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
        SchedulerFlow r
      ) =>
      DP.Person ->
      DRide.Ride ->
      m ()
    triggerIVR person ride = do
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      callStatusId <- generateGUID
      let callReq =
            Call.InitiateCallReq
              { fromPhoneNum = phoneNumber,
                toPhoneNum = Nothing,
                attachments = Call.Attachments $ DCall.CallAttachments {callStatusId = callStatusId, rideId = ride.id}
              }
      exotelResponse <- Call.initiateCall person.merchantId person.merchantOperatingCityId callReq
      callStatus <- buildCallStatus callStatusId exotelResponse ride
      QCallStatus.create callStatus
    buildCallStatus callStatusId exotelResponse ride = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = Just ride.id,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = getId <$> ride.merchantId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now
          }
