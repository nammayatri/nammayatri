module Domain.Action.UI.Ride
  ( GetDriverLocRes,
    getDriverLoc,
  )
where

import App.Types
import Beckn.External.GoogleMaps.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

type GetDriverLocRes = MapSearch.LatLong

getDriverLoc ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r,
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
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = rideBooking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
  driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
  mbIsOnTheWayNotified <- Redis.getKeyRedis @() (driverOnTheWay rideId)
  mbHasReachedNotified <- Redis.getKeyRedis @() (driverHasReached rideId)
  when (ride.status == NEW && (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified)) $ do
    distance <- (.distance) <$> MapSearch.getDistance (Just MapSearch.CAR) (GoogleMaps.getCoordinates fromLocation) res.currPoint
    mbStartDistance <- Redis.getKeyRedis @Meters (distanceUpdates rideId)
    case mbStartDistance of
      Nothing -> Redis.setExRedis (distanceUpdates rideId) distance 3600
      Just startDistance -> when (startDistance - 100 > distance) $ do
        unless (isJust mbIsOnTheWayNotified) $ do
          Notify.notifyDriverOnTheWay personId
          Redis.setExRedis (driverOnTheWay rideId) () driverOnTheWayNotifyExpiry
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached personId ride
          Redis.setExRedis (driverHasReached rideId) () 1500
  return res.currPoint

distanceUpdates :: Id SRide.Ride -> Text
distanceUpdates (Id rideId) = "BAP: DriverDistance " <> rideId

driverOnTheWay :: Id SRide.Ride -> Text
driverOnTheWay (Id rideId) = "BAP: DriverIsOnTheWay " <> rideId

driverHasReached :: Id SRide.Ride -> Text
driverHasReached (Id rideId) = "BAP: DriverHasReached " <> rideId
