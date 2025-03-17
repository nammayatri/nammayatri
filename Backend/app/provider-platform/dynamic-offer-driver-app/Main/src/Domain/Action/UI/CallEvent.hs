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
import Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Ride as Ride
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Ride as QRide
import Tools.Event

data CallEventReq = CallEventReq
  { rideId :: Id Ride.Ride,
    exophoneNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

logCallEvent :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType, HasField "maxShards" r Int) => CallEventReq -> m APISuccess.APISuccess
logCallEvent CallEventReq {..} = do
  callOnClickTracker rideId
  sendCallDataToKafka Nothing (Just rideId) (Just "ANONYMOUS_CALLER") Nothing Nothing User (Just exophoneNumber)
  pure APISuccess.Success

sendCallDataToKafka :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r) => Maybe Text -> Maybe (Id Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> EventTriggeredBy -> Maybe Text -> m ()
sendCallDataToKafka vendor mRideId callType callSid callStatus triggeredBy exophoneNumber = do
  case mRideId of
    Just rideId -> do
      mRide <- runInReplica $ QRide.findById rideId
      case mRide of
        Just ride -> triggerExophoneEvent $ ExophoneEventData vendor callType (Just rideId) callSid callStatus ride.merchantId triggeredBy (Just ride.driverId) exophoneNumber
        Nothing -> triggerExophoneEvent $ ExophoneEventData vendor callType (Just rideId) callSid callStatus Nothing triggeredBy Nothing exophoneNumber
    Nothing -> triggerExophoneEvent $ ExophoneEventData vendor callType Nothing callSid callStatus Nothing triggeredBy Nothing exophoneNumber

callOnClickTracker :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType, HasField "maxShards" r Int) => Id Ride.Ride -> m ()
callOnClickTracker rideId = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  transporterConfig <-
    CCT.findByMerchantOpCityId booking.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  callStatusObj <- buildCallStatus booking.merchantOperatingCityId booking.providerId
  QCallStatus.create callStatusObj
  createJobIn @_ @'CheckExotelCallStatusAndNotifyBAP (Just booking.providerId) (Just booking.merchantOperatingCityId) (fromIntegral transporterConfig.exotelStatusCheckSchedulerDelay) $
    CheckExotelCallStatusAndNotifyBAPJobData
      { rideId = ride.id,
        merchantOperatingCityId = Just (booking.merchantOperatingCityId)
      }
  return ()
  where
    buildCallStatus merchantOpCityId merchantId = do
      callStatusId <- generateGUID
      callId <- generateGUID -- added random placeholder for backward compatibility
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = callId,
            entityId = Just $ getId rideId,
            dtmfNumberUsed = Nothing,
            status = CallTypes.ATTEMPTED,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just merchantId.getId,
            merchantOperatingCityId = Just merchantOpCityId,
            callService = Nothing,
            callError = Nothing,
            callAttempt = Just DCallStatus.Attempted,
            createdAt = now
          }
