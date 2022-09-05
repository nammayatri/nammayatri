module Storage.Queries.BusinessEvent where

import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as Google
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking
import Domain.Types.BusinessEvent
import Domain.Types.Person (Driver)
import Domain.Types.Ride
import Domain.Types.Vehicle.Variant (Variant)
import Storage.Queries.Person
import Storage.Tabular.BusinessEvent ()

logBusinessEvent ::
  Maybe (Id Driver) ->
  EventType ->
  Maybe (Id Booking) ->
  Maybe WhenPoolWasComputed ->
  Maybe Variant ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe (Id Ride) ->
  SqlDB ()
logBusinessEvent driverId eventType bookingId whenPoolWasComputed variant distance duration rideId = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create $
    BusinessEvent
      { id = uuid,
        eventType = eventType,
        timeStamp = now,
        driverId = driverId,
        bookingId = bookingId,
        whenPoolWasComputed = whenPoolWasComputed,
        vehicleVariant = variant,
        distance = distance,
        duration = duration,
        rideId = rideId
      }

logDriverInPoolEvent :: WhenPoolWasComputed -> Maybe (Id Booking) -> Google.GetDistanceResult DriverPoolResult a -> SqlDB ()
logDriverInPoolEvent whenPoolWasComputed bookingId getDistanceRes = do
  let driverInPool = getDistanceRes.origin
  logBusinessEvent
    (Just driverInPool.driverId)
    DRIVER_IN_POOL
    bookingId
    (Just whenPoolWasComputed)
    (Just driverInPool.vehicle.variant)
    (Just $ Meters $ floor driverInPool.distanceToDriver)
    (Just getDistanceRes.duration)
    Nothing

logDriverAssignedEvent :: Id Driver -> Id Booking -> Id Ride -> SqlDB ()
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

logRideConfirmedEvent :: Id Booking -> SqlDB ()
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

logRideCommencedEvent :: Id Driver -> Id Booking -> Id Ride -> SqlDB ()
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
