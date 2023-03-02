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
    getDriverLoc,
  )
where

import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics
import qualified Tools.Notifications as Notify

type GetDriverLocRes = MapSearch.LatLong

getDriverLoc ::
  forall m r.
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
  ride <- QRide.findById rideId (Proxy @m) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  res <- CallBPP.callGetDriverLocation ride
  booking <- QRB.findById ride.bookingId (Proxy @m) >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = booking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
  driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
  mbIsOnTheWayNotified <- Redis.get @() (driverOnTheWay rideId)
  mbHasReachedNotified <- Redis.get @() (driverHasReached rideId)
  when (ride.status == NEW && (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified)) $ do
    distance <-
      (.distance)
        <$> MapSearch.getDistance booking.merchantId
          MapSearch.GetDistanceReq
            { origin = fromLocation,
              destination = res.currPoint,
              travelMode = Just MapSearch.CAR
            }
    mbStartDistance <- Redis.get @Meters (distanceUpdates rideId)
    case mbStartDistance of
      Nothing -> Redis.setExp (distanceUpdates rideId) distance 3600
      Just startDistance -> when (startDistance - 100 > distance) $ do
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
