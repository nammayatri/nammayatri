module Storage.Queries.BusinessEventExtra where

import Domain.Types.Booking
import Domain.Types.BusinessEvent
import Domain.Types.Person (Driver)
import Domain.Types.Ride
import Domain.Types.VehicleVariant
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Storage.Queries.OrphanInstances.BusinessEvent ()

logBusinessEvent ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe (Id Driver) ->
  EventType ->
  Maybe (Id Booking) ->
  Maybe WhenPoolWasComputed ->
  Maybe VehicleVariant ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe (Id Ride) ->
  DistanceUnit ->
  m ()
logBusinessEvent driverId eventType bookingId whenPoolWasComputed variant distance duration rideId distanceUnit = do
  uuid <- generateGUID
  now <- getCurrentTime
  let bE =
        BusinessEvent
          { id = uuid,
            eventType = eventType,
            timeStamp = now,
            driverId = driverId,
            bookingId = bookingId,
            whenPoolWasComputed = whenPoolWasComputed,
            vehicleVariant = variant,
            distance = distance,
            distanceUnit,
            duration = duration,
            rideId = rideId
          }
  createWithKV bE

logDriverAssignedEvent :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Id Booking -> Id Ride -> DistanceUnit -> m ()
logDriverAssignedEvent driverId bookingId rideId = do
  logBusinessEvent
    (Just driverId)
    DRIVER_ASSIGNED
    (Just bookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)

logRideConfirmedEvent :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> DistanceUnit -> m ()
logRideConfirmedEvent bookingId = do
  logBusinessEvent
    Nothing
    RIDE_CONFIRMED
    (Just bookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

logRideCommencedEvent :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Id Booking -> Id Ride -> DistanceUnit -> m ()
logRideCommencedEvent driverId bookingId rideId = do
  logBusinessEvent
    (Just driverId)
    RIDE_COMMENCED
    (Just bookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)

-- Extra code goes here --
