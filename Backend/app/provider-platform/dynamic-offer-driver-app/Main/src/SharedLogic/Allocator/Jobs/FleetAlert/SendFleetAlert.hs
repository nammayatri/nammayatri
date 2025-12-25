{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.FleetAlert.SendFleetAlert
  ( sendFleetAlert,
  )
where

import Domain.Action.UI.Call
import qualified Domain.Types.Alert as DA
import qualified Domain.Types.CallStatus as SCS
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified Storage.Queries.AlertRequest as QAR
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import Tools.Call as TCall
import Tools.Error

sendFleetAlert ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    MonadFlow m,
    SchedulerFlow r,
    EsqDBFlow m r,
    HasKafkaProducer r
  ) =>
  Job 'FleetAlert ->
  m ExecutionResult
sendFleetAlert Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let fleetOwnerId = jobData.fleetOwnerId
      entityId = jobData.entityId
      appletId = jobData.appletId
  driverRequest <- B.runInReplica $ QAR.findByPrimaryKey entityId >>= fromMaybeM (InvalidRequest "DriverRequest not found")
  unless (driverRequest.status == DA.AWAITING_APPROVAL) $ do
    callStatusId <- generateGUID
    fleetOwner <- B.runInReplica $ QPerson.findById fleetOwnerId >>= fromMaybeM (PersonDoesNotExist fleetOwnerId.getId)
    mobileNumber <- mapM decrypt fleetOwner.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
    let countryCode = fromMaybe "+91" fleetOwner.mobileCountryCode
        callReq =
          InitiateCallReq
            { fromPhoneNum = countryCode <> mobileNumber,
              toPhoneNum = Nothing,
              attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = entityId.getId},
              appletId = appletId
            }
    let merchantId = fleetOwner.merchantId
        merchantOperatingCityId = fleetOwner.merchantOperatingCityId
    exotelResponse <- TCall.initiateCall merchantId merchantOperatingCityId callReq
    logInfo $ "IVR Call initiated to driver for DriverRequestId: " <> entityId.getId
    callStatus <- buildCallStatus callStatusId entityId fleetOwner exotelResponse
    void $ QCallStatus.create callStatus
  return Complete
  where
    buildCallStatus callStatusId entityId fleetOwner exotelResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            entityId = Just entityId.getId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just fleetOwner.merchantId.getId,
            merchantOperatingCityId = Just fleetOwner.merchantOperatingCityId,
            callService = Just Exotel,
            callAttempt = Just SCS.Resolved,
            callError = Nothing,
            createdAt = now,
            aiCallAnalyzed = Nothing
          }
