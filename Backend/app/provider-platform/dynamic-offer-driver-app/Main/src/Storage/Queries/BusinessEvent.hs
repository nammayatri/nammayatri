{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BusinessEvent where

import Domain.Types.Booking
import Domain.Types.BusinessEvent
import Domain.Types.Person (Driver)
import Domain.Types.Ride
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
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
  SqlDB m ()
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

logDriverAssignedEvent :: Id Driver -> Id Booking -> Id Ride -> SqlDB m ()
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

logRideConfirmedEvent :: Id Booking -> SqlDB m ()
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

logRideCommencedEvent :: Id Driver -> Id Booking -> Id Ride -> SqlDB m ()
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
