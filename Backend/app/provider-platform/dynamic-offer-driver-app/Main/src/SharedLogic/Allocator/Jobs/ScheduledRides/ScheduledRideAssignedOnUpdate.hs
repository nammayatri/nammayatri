{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate where

import qualified AWS.S3 as S3
import qualified Data.HashMap.Strict as HMS
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.CallBAP
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDE
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import TransactionLogs.Types

sendScheduledRideAssignedOnUpdate ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Job 'ScheduledRideAssignedOnUpdate ->
  m ExecutionResult
sendScheduledRideAssignedOnUpdate Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  --to prevent queries we can send whole booking ,driver ,... in job data
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
      driverId = jobData.driverId
      vehicle = jobData.vehicle
      rideId = jobData.rideId
  mbDriverInfo <- QDE.findById driverId
  let driverInfo = fromMaybe (error "DriverInfo not found") mbDriverInfo
  case driverInfo.onRide of
    False -> do
      mbDriver <- QP.findById driverId
      mbRide <- QRide.findById rideId
      mbBooking <- QB.findById bookingId
      void $ QDI.updateOnRide True driverId
      void $ QRide.updateStatus rideId DRide.NEW
      let driver = fromMaybe (error "Driver not found") mbDriver
          ride = fromMaybe (error "Ride not found") mbRide
          booking = fromMaybe (error "Booking not found") mbBooking
      void $ sendRideAssignedUpdateToBAP booking ride driver vehicle
      return Complete
    True -> do
      now <- getCurrentTime
      let rescheduleTime = addUTCTime (5 * 60) now
      return $ ReSchedule rescheduleTime

-- sendScheduledRideAssignedOnUpdate Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
--   let jobData = jobInfo.jobData
--       bookingId = jobData.booking
--       driverId  = jobData.driverId
--       vehicle = jobData.vehicle
--       ride    = jobData.ride
--   mbDriverInfo  <- QDE.findById driver.id
--   let driverInfo = fromJust mbDriverInfo
--   case driverInfo.onRide of
--     False -> do
--       mbdriver <- QP.findById driverId
--       void $ QDI.updateOnRide True driver.id
--       void $ QRide.updateStatus ride.id DRide.NEW
--       let driver = fromJust mbdriver
--       void $ sendRideAssignedUpdateToBAP booking ride driver vehicle
--       return Complete
--     True -> do
--       now <- getCurrentTime
--       ReSchedule <$> addUTCTime now  (5 * 60)
