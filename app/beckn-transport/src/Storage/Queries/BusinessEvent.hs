module Storage.Queries.BusinessEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.BusinessEvent
import Domain.Types.Ride
import Domain.Types.RideBooking
import Domain.Types.Vehicle (Variant)
import Storage.Queries.Person (DriverPoolResult)
import Storage.Tabular.BusinessEvent ()
import Types.App (Driver)
import Utils.Common

logBusinessEvent ::
  Maybe (Id Driver) ->
  EventType ->
  Maybe (Id RideBooking) ->
  Maybe WhenPoolWasComputed ->
  Maybe Variant ->
  Maybe Double ->
  Maybe Double ->
  Maybe (Id Ride) ->
  SqlDB ()
logBusinessEvent driverId eventType rideBookingId whenPoolWasComputed variant distance duration rideId = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create' $
    BusinessEvent
      { id = uuid,
        eventType = eventType,
        timeStamp = now,
        driverId = driverId,
        rideBookingId = rideBookingId,
        whenPoolWasComputed = whenPoolWasComputed,
        vehicleVariant = variant,
        distance = distance,
        duration = duration,
        rideId = rideId
      }

logDriverInPoolEvent :: WhenPoolWasComputed -> Maybe (Id RideBooking) -> DriverPoolResult -> SqlDB ()
logDriverInPoolEvent whenPoolWasComputed rideBookingId driverInPool = do
  logBusinessEvent
    (Just driverInPool.driverId)
    DRIVER_IN_POOL
    rideBookingId
    (Just whenPoolWasComputed)
    (Just driverInPool.variant)
    (Just driverInPool.distanceToDriver)
    driverInPool.durationToPickup
    Nothing

logDriverAssignetEvent :: Id Driver -> Id RideBooking -> Id Ride -> SqlDB ()
logDriverAssignetEvent driverId rideBookingId rideId = do
  logBusinessEvent
    (Just driverId)
    DRIVER_ASSIGNED
    (Just rideBookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)

logRideConfirmedEvent :: Id RideBooking -> SqlDB ()
logRideConfirmedEvent rideBookingId = do
  logBusinessEvent
    Nothing
    RIDE_CONFIRMED
    (Just rideBookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

logRideCommencedEvent :: Id Driver -> Id RideBooking -> Id Ride -> SqlDB ()
logRideCommencedEvent driverId rideBookingId rideId = do
  logBusinessEvent
    (Just driverId)
    RIDE_COMMENCED
    (Just rideBookingId)
    Nothing
    Nothing
    Nothing
    Nothing
    (Just rideId)
