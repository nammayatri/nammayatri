module Product.Ride where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Data.Aeson.Types ()
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as Euler
import Servant hiding (throwError)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Types.API.Location
import qualified Types.API.Ride as API
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

getDriverLoc :: Id SRide.Ride -> Id SPerson.Person -> FlowHandler API.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  trackingUrl <- ride.trackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  res <-
    callAPI trackingUrl eulerClient "BPP.driverTrackUrl"
      >>= fromEitherM (\err -> InternalError $ "Failed to call driverTrackUrl: " <> show err)
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = rideBooking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
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
          Redis.setExRedis (driverOnTheWay rideId) () 900
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached personId
          Redis.setExRedis (driverHasReached rideId) () 900
  return res.currPoint

distanceUpdates :: Id SRide.Ride -> Text
distanceUpdates (Id rideId) = "BAP: DriverDistance " <> rideId

driverOnTheWay :: Id SRide.Ride -> Text
driverOnTheWay (Id rideId) = "BAP: DriverIsOnTheWay " <> rideId

driverHasReached :: Id SRide.Ride -> Text
driverHasReached (Id rideId) = "BAP: DriverHasReached " <> rideId
