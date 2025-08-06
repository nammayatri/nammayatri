{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.SafetyIVR where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import Domain.Types.RiderConfig
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Tools.Call as Call
import Tools.Error

sendSafetyIVR ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasKafkaProducer r
  ) =>
  Job 'SafetyIVR ->
  m ExecutionResult
sendSafetyIVR Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
      rideId = jobData.rideId
  ride <- B.runInReplica $ QR.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  if shouldTriggerPostRideCheckCall ride
    then triggerIVRAndCreateCsAlertJob personId ride
    else handleRideJourneyStatus personId ride
  return Complete
  where
    shouldTriggerPostRideCheckCall ride = ride.status == DRide.COMPLETED && isNothing ride.wasRideSafe

    handleRideJourneyStatus ::
      ( EncFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBFlow m r,
        SchedulerFlow r,
        HasKafkaProducer r
      ) =>
      Id DP.Person ->
      DRide.Ride ->
      m ()
    handleRideJourneyStatus personId ride =
      case ride.safetyJourneyStatus of
        Just (DRide.UnexpectedCondition _) -> triggerIVRAndCreateCsAlertJob personId ride
        Nothing -> logError $ "No safety journey status found during IVR calling stage for ride : " <> show ride.id
        _ -> logDebug $ "Ride status is : " <> show ride.safetyJourneyStatus <> ". Skipping IVR call : " <> show ride.id

    triggerIVRAndCreateCsAlertJob ::
      ( EncFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBFlow m r,
        SchedulerFlow r,
        HasKafkaProducer r
      ) =>
      Id DP.Person ->
      DRide.Ride ->
      m ()
    triggerIVRAndCreateCsAlertJob personId ride = do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      merchantOperatingCityId <- maybe (QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId) >>= pure . (.merchantOperatingCityId)) pure ride.merchantOperatingCityId
      riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
      logDebug $ "Triggering IVR for ride with unexpected condition: " <> show ride.id
      triggerIVR person ride riderConfig
    -- createSafetyCSAlertJob person ride riderConfig

    triggerIVR ::
      ( EncFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBFlow m r,
        SchedulerFlow r,
        HasKafkaProducer r
      ) =>
      DP.Person ->
      DRide.Ride ->
      RiderConfig ->
      m ()
    triggerIVR person ride riderConfig = do
      let appletType = if ride.status == DRide.COMPLETED then PostRideSafetyCheckAppletID else SosAppletID
      let maybeAppId = (HM.lookup appletType . exotelMap) =<< riderConfig.exotelAppIdMapping
      logDebug $ "Applet ID for SOS call : " <> show maybeAppId
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      callStatusId <- generateGUID
      let callReq =
            Call.InitiateCallReq
              { fromPhoneNum = phoneNumber,
                toPhoneNum = Nothing,
                attachments = Call.Attachments $ DCall.CallAttachments {callStatusId = callStatusId, rideId = ride.id},
                appletId = maybeAppId
              }
      exotelResponse <- Call.initiateCall person.merchantId person.merchantOperatingCityId callReq
      callStatus <- buildCallStatus callStatusId exotelResponse ride
      QCallStatus.create callStatus
      void $ QR.updateSafetyJourneyStatus ride.id DRide.IVRCallInitiated

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
            merchantOperatingCityId = ride.merchantOperatingCityId,
            callService = Just Call.Exotel,
            callAttempt = Nothing,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }
