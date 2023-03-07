{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride
  ( GetDriverLocRes,
    GetRideInfoRes,
    getDriverLoc,
    getRideInfo,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps as Maps
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QRP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics
import qualified Tools.Notifications as Notify

type GetDriverLocRes = MapSearch.LatLong

getDriverLoc ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasField "rideCfg" r RideConfig
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetDriverLocRes
getDriverLoc rideId personId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  res <- CallBPP.callGetDriverLocation ride
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = Maps.getCoordinates booking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
  driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
  mbIsOnTheWayNotified <- Redis.get @() (driverOnTheWay rideId)
  mbHasReachedNotified <- Redis.get @() (driverHasReached rideId)
  when (ride.status == NEW && (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified)) $ do
    let distance = highPrecMetersToMeters $ distanceBetweenInMeters fromLocation res.currPoint
    mbStartDistance <- Redis.get @Meters (distanceUpdates rideId)
    case mbStartDistance of
      Nothing -> Redis.setExp (distanceUpdates rideId) distance 3600
      Just startDistance -> when (startDistance - 50 > distance) $ do
        unless (isJust mbIsOnTheWayNotified) $ do
          Notify.notifyDriverOnTheWay personId
          Redis.setExp (driverOnTheWay rideId) () driverOnTheWayNotifyExpiry
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached personId ride
          Redis.setExp (driverHasReached rideId) () 1500
  return res.currPoint

distanceUpdates :: Id SRide.Ride -> Text
distanceUpdates (Id rideId) = "BAP: DriverDistance " <> rideId

driverOnTheWay :: Id SRide.Ride -> Text
driverOnTheWay (Id rideId) = "BAP: DriverIsOnTheWay " <> rideId

driverHasReached :: Id SRide.Ride -> Text
driverHasReached (Id rideId) = "BAP: DriverHasReached " <> rideId

-- Action for ride info
type GetRideInfo = SRide.Ride

type GetRideInfoRes = SRide.ShareRideInfo

getRideInfo ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r
  ) =>
  Id GetRideInfo ->
  m GetRideInfoRes
getRideInfo rideId = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  person <- runInReplica $ QRP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  logInfo $ "bookingDetails: " <> show booking.bookingDetails
  let mbtoLocation = case booking.bookingDetails of
        DBooking.OneWayDetails x -> Just x.toLocation
        DBooking.DriverOfferDetails x -> Just x.toLocation
        _ -> Nothing
  return $
    ShareRideInfo
      { id = ride.id,
        bookingId = ride.bookingId,
        status = ride.status,
        driverName = ride.driverName,
        driverRating = ride.driverRating,
        driverMobileNumber = ride.driverMobileNumber,
        vehicleNumber = ride.vehicleNumber,
        vehicleModel = ride.vehicleModel,
        vehicleColor = ride.vehicleColor,
        trackingUrl = ride.trackingUrl,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        userFirstName = person.firstName,
        userLastName = person.lastName,
        fromLocation = booking.fromLocation,
        toLocation = mbtoLocation
      }