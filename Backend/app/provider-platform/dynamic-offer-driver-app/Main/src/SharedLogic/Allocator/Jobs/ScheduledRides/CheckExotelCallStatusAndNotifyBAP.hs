{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ScheduledRides.CheckExotelCallStatusAndNotifyBAP where

import qualified Data.HashMap.Strict as HMS
import Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import SharedLogic.Allocator
import qualified SharedLogic.CallBAP as CallBAP
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Ride as QRide
import TransactionLogs.Types

checkExotelCallStatusAndNotifyBAP ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv,
    EventStreamFlow m r
  ) =>
  Job 'CheckExotelCallStatusAndNotifyBAP ->
  m ExecutionResult
checkExotelCallStatusAndNotifyBAP Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let rideId = jobData.rideId
  let mOpCityId = jobData.merchantOperatingCityId
  callStatus <- QCallStatus.findOneByEntityId (Just $ getId rideId) >>= fromMaybeM CallStatusDoesNotExist
  handleCallStatus callStatus rideId mOpCityId

handleCallStatus ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "esqDBReplicaEnv" r EsqDBEnv,
    EventStreamFlow m r
  ) =>
  DCallStatus.CallStatus ->
  Id DRide.Ride ->
  Maybe (Id MOC.MerchantOperatingCity) ->
  m ExecutionResult
handleCallStatus callStatus rideId mOpCityId = do
  let mOpCityId' = maybe "UNKNOWN_MERCHANT" getId mOpCityId
  deploymentVersion <- asks (.version)
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound $ getId rideId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound $ getId ride.bookingId)
  case callStatus.callAttempt of
    Just DCallStatus.Resolved ->
      return Complete
    Just DCallStatus.Failed -> do
      return Complete
    Just DCallStatus.Attempted -> do
      fork "updating in prometheus" $ incrementCounter mOpCityId' "call_attempt" deploymentVersion.getDeploymentVersion
      -- attempted to call or unregistered Call Stuck
      CallBAP.sendPhoneCallRequestUpdateToBAP booking ride
      return Complete
    Just DCallStatus.Pending -> do
      -- Exotel Call StatusCallBack Didnt Happen
      CallBAP.sendPhoneCallRequestUpdateToBAP booking ride
      return Complete
    Nothing -> throwError (CallStatusFieldNotPresent "callAttempt")
