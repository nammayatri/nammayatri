{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.FleetEngine.Retry
  ( fleetEngineRetryHandler,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.FleetEngine.Types as FETypes
import Kernel.Prelude
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), FleetEngineRetryOperation (..))
import qualified SharedLogic.FleetEngine as FleetEngine
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide

-- | Runs one FE retry attempt. Pattern B: catches its own errors, self-enqueues the
-- next attempt via 'enqueueFleetEngineRetry' until 'feRetryMaxAttempts' is reached,
-- then logs and terminates. Always returns Complete so the scheduler never sees a
-- throw (which would mark the current row Failed and stop the sequence).
fleetEngineRetryHandler ::
  FleetEngine.FleetEngineRetryFlow m r =>
  Job 'FleetEngineRetry ->
  m ExecutionResult
fleetEngineRetryHandler Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let d = jobInfo.jobData
      rideId = d.rideId
      mocId = d.merchantOperatingCityId
      attempt = fromMaybe 1 d.attemptCount
      missingPayload field =
        throwError (InternalError $ "FleetEngineRetry: missing " <> field <> " payload for ride " <> rideId.getId)
      loadBookingRide = do
        ride <- QRide.findById rideId >>= fromMaybeM (InternalError $ "FleetEngineRetry: ride " <> rideId.getId <> " not found")
        booking <- QRB.findById ride.bookingId >>= fromMaybeM (InternalError $ "FleetEngineRetry: booking " <> ride.bookingId.getId <> " not found")
        pure (booking, ride)
      runOp = case d.operation of
        FEOpCompleteTrip -> FleetEngine.notifyTripStatus mocId rideId FETypes.COMPLETE
        FEOpCancelTrip -> FleetEngine.notifyTripStatus mocId rideId FETypes.CANCELED
        FEOpCreateTrip -> do
          (booking, ride) <- loadBookingRide
          FleetEngine.notifyTripCreatedInternal booking ride
        FEOpDriverArrived -> do
          (booking, ride) <- loadBookingRide
          FleetEngine.notifyDriverArrivedInternal booking ride
        FEOpRideStarted -> do
          (booking, ride) <- loadBookingRide
          FleetEngine.notifyRideStartedInternal booking ride
        FEOpDropoffChanged -> case d.mbLatLong of
          Just ll -> FleetEngine.notifyDropoffChangedInternal mocId rideId ll
          Nothing -> missingPayload "latLong"
        FEOpPickupChanged -> case d.mbLatLong of
          Just ll -> FleetEngine.notifyPickupChangedInternal mocId rideId ll
          Nothing -> missingPayload "latLong"
        FEOpStopsChanged -> case d.mbStops of
          Just stops -> FleetEngine.notifyStopsChangedInternal mocId rideId stops
          Nothing -> missingPayload "stops"
        FEOpStopArrived -> case d.mbStopIndex of
          Just idx -> FleetEngine.notifyStopArrivedInternal mocId rideId idx
          Nothing -> missingPayload "stopIndex"
        -- mbStopIndex holds the Maybe Int semantic directly: Nothing = to dropoff, Just idx = to next intermediate.
        FEOpStopDeparted -> FleetEngine.notifyStopDepartedInternal mocId rideId d.mbStopIndex
  logInfo $ "FleetEngineRetry: rideId=" <> rideId.getId <> " op=" <> show d.operation <> " attempt=" <> show attempt
  result <- try runOp
  case result of
    Right () -> do
      logInfo $ "FleetEngineRetry: rideId=" <> rideId.getId <> " op=" <> show d.operation <> " succeeded on attempt " <> show attempt
      pure Complete
    Left (e :: SomeException)
      | attempt >= FleetEngine.feRetryMaxAttempts -> do
        logError $ "FleetEngineRetry: exhausted after " <> show attempt <> " attempts for ride " <> rideId.getId <> " op=" <> show d.operation <> ": " <> T.pack (show e)
        pure Complete
      | otherwise -> do
        let nextAttempt = attempt + 1
        logError $ "FleetEngineRetry: attempt " <> show attempt <> " failed for ride " <> rideId.getId <> " op=" <> show d.operation <> ": " <> T.pack (show e) <> ". Enqueueing attempt " <> show nextAttempt <> "."
        FleetEngine.enqueueFleetEngineRetry Nothing mocId d.operation rideId d.mbLatLong d.mbStops d.mbStopIndex nextAttempt
        pure Complete
