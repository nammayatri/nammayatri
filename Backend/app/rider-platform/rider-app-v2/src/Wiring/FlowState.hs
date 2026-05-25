{-
  Flow state resolver: determines the current RideFlowState for a rider.

  Looks up the rider's most recent booking/ride from DB and maps
  BookingStatus + RideStatus to the unified RideFlowState.

  Used by the wiring layer to validate state transitions before calling handlers.
-}
module Wiring.FlowState
  ( resolveRideFlowState,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Extra.Booking as DBooking
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as RideStatus
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import MobilityFlow.Flows.RideHailing (RideFlowState (..))
import qualified Storage.Queries.BookingExtra as QBooking
import qualified Storage.Queries.RideExtra as QRide

-- | Resolve the current RideFlowState for a rider.
--
-- Maps existing BookingStatus + RideStatus to unified RideFlowState:
--   No active booking            → Idle
--   BookingStatus NEW            → Searching / Quoted (depending on estimates)
--   BookingStatus CONFIRMED      → Booked
--   BookingStatus TRIP_ASSIGNED  → Assigned or InProgress (depending on ride status)
--   BookingStatus COMPLETED      → Completed
--   BookingStatus CANCELLED      → Cancelled
resolveRideFlowState :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> m RideFlowState
resolveRideFlowState personId = do
  -- Find the most recent active booking for this rider
  activeBookings <- QBooking.findByRiderIdAndStatus personId DBooking.activeBookingStatus
  case listToMaybe activeBookings of
    Nothing -> pure Idle
    Just booking -> mapBookingToFlowState booking

-- | Map a Booking (+ its Ride if any) to RideFlowState
mapBookingToFlowState :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DBooking.Booking -> m RideFlowState
mapBookingToFlowState booking = do
  case booking.status of
    DBooking.NEW -> pure Searching
    DBooking.CONFIRMED -> pure Booked
    DBooking.AWAITING_REASSIGNMENT -> pure Booked
    DBooking.REALLOCATED -> pure Booked
    DBooking.TRIP_ASSIGNED -> do
      -- Need to check ride status to distinguish Assigned vs InProgress
      mRide <- QRide.findActiveByRBId booking.id
      case mRide of
        Nothing -> pure Assigned
        Just ride -> case ride.status of
          RideStatus.NEW -> pure Assigned
          RideStatus.INPROGRESS -> pure InProgress
          RideStatus.COMPLETED -> pure Completed
          RideStatus.CANCELLED -> pure Cancelled
          _ -> pure Assigned
    DBooking.COMPLETED -> pure Completed
    DBooking.CANCELLED -> pure Cancelled
