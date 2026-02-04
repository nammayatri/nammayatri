{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CallEvent
  ( CallEventReq (..),
    logCallEvent,
    sendCallDataToKafka,
  )
where

import Data.Aeson
import qualified Data.Map as M
import Domain.Types.CallStatus
import qualified Domain.Types.Ride as Ride
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Id as ID
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event

data CallEventReq = CallEventReq
  { rideId :: Id Ride.Ride,
    callType :: Text,
    exophoneNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

logCallEvent :: (EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r, CacheFlow m r, HasField "maxShards" r Int, HasField "schedulerSetName" r Text, HasField "schedulerType" r SchedulerType, HasField "jobInfoMap" r (M.Map Text Bool), HasField "blackListedJobs" r [Text]) => CallEventReq -> m APISuccess.APISuccess
logCallEvent CallEventReq {..} = do
  when (callType == "ANONYMOUS_CALLER") $ callOnClickTracker rideId
  sendCallDataToKafka Nothing (Just rideId) (Just callType) Nothing Nothing User (Just exophoneNumber)
  pure APISuccess.Success

sendCallDataToKafka :: (EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r, CacheFlow m r) => Maybe Text -> Maybe (Id Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> EventTriggeredBy -> Maybe Text -> m ()
sendCallDataToKafka vendor mRideId callType callSid callStatus triggeredBy exophoneNumber = do
  case mRideId of
    Just rideId -> do
      ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ getId ride.bookingId)
      triggerExophoneEvent $ ExophoneEventData vendor callType (Just rideId) callSid callStatus ride.merchantId triggeredBy (Just booking.riderId) exophoneNumber
    Nothing -> triggerExophoneEvent $ ExophoneEventData vendor callType Nothing callSid callStatus Nothing triggeredBy Nothing exophoneNumber

callOnClickTracker :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, EventStreamFlow m r, HasField "maxShards" r Int, HasField "schedulerSetName" r Text, HasField "schedulerType" r SchedulerType, HasField "jobInfoMap" r (M.Map Text Bool), HasField "blackListedJobs" r [Text]) => Id Ride.Ride -> m ()
callOnClickTracker rideId = do
  ride <- runInReplica $ QRide.findById (ID.Id rideId.getId) >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  buildCallStatus <- callStatusObj booking.merchantOperatingCityId booking.merchantId
  QCallStatus.create buildCallStatus
  scheduleJobs ride booking.merchantId booking.merchantOperatingCityId (riderConfig.exotelStatusCheckSchedulerDelay)
  return ()
  where
    callStatusObj merchantOperatingCityId merchantId = do
      id <- generateGUID
      callId <- generateGUID -- added random placeholder for backward compatibility
      now <- getCurrentTime
      return $
        CallStatus
          { id = id,
            callId = callId,
            rideId = Just rideId,
            dtmfNumberUsed = Nothing,
            status = CallTypes.ATTEMPTED,
            callAttempt = Just Attempted,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just merchantId.getId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            callService = Nothing,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }

    scheduleJobs ride merchantId merchantOperatingCityId exotelStatusCheckSchedulerDelay = do
      createJobIn @_ @'CheckExotelCallStatusAndNotifyBPP (Just merchantId) (Just merchantOperatingCityId) (fromIntegral exotelStatusCheckSchedulerDelay) $
        CheckExotelCallStatusAndNotifyBPPJobData
          { rideId = ride.id,
            bppRideId = ride.bppRideId,
            merchantId = merchantId,
            merchantOpCityId = merchantOperatingCityId
          }
