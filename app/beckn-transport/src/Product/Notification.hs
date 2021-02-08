{-# LANGUAGE OverloadedLabels #-}

module Product.Notification where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.RegistrationToken as RT
import Beckn.Utils.Common
import qualified Data.Time as Time
import EulerHS.Prelude
import qualified Storage.Queries.Location as SLocation
import qualified Storage.Queries.NotificationStatus as SNotificationStatus
import qualified Storage.Queries.ProductInstance as SProductInstance
import qualified Types.API.Location as API
import qualified Types.API.Notification as API
import Types.App
import qualified Types.Storage.NotificationStatus as NotificationStatus

getNotificationDetail :: RT.RegistrationToken -> RideId -> FlowHandler API.NotificationDetailResponse
getNotificationDetail RT.RegistrationToken {..} rideId = withFlowHandler $ do
  let driverId = DriverId _EntityId
  notificationStatus <- SNotificationStatus.fetchByRideIdAndDriverId rideId driverId >>= fromMaybeM400 "NO_NOTIFICATION_INFO"
  unless (notificationStatus ^. #_status == NotificationStatus.NOTIFIED) $ throwBecknError400 "NO_NOTIFICATION_INFO"
  notifiedAt <- notificationStatus ^. #_notifiedAt & fromMaybeM400 "NO_NOTIFICATION_INFO"
  productInstance <- SProductInstance.findById $ (ProductInstanceId . _getRideId) rideId
  pickupLocId <- LocationId <$> productInstance ^. #_fromLocation & fromMaybeM500 "NO_PICKUP_LOC"
  dropLocId <- LocationId <$> productInstance ^. #_toLocation & fromMaybeM500 "NO_DROP_LOC"
  pickupLoc <- SLocation.findLocationById pickupLocId >>= fromMaybeM500 "NO_PICKUP_LOC"
  dropLoc <- SLocation.findLocationById dropLocId >>= fromMaybeM500 "NO_DROP_LOC"
  _ <- SNotificationStatus.updateStatus rideId driverId NotificationStatus.RECEIVED
  now <- getCurrTime
  let eta = Time.diffUTCTime (productInstance ^. #_startTime) now
  driverNotificationExpiry <- asks $ driverNotificationExpiry . driverAllocationConfig
  let notificationExpiry = Time.addUTCTime driverNotificationExpiry notifiedAt
  pure $
    API.NotificationDetailResponse
      { productInstanceId = _getRideId rideId,
        pickupLoc = toLocationInfo pickupLoc,
        dropLoc = toLocationInfo dropLoc,
        etaForPickupLoc = eta / 60,
        distanceToPickupLoc = fromMaybe "0" $ productInstance ^. #_udf5,
        notificationExpiry = notificationExpiry
      }

toLocationInfo :: Location.Location -> API.LocationInfo
toLocationInfo Location.Location {..} =
  API.LocationInfo
    { locationType = Just _locationType,
      lat = _lat,
      long = _long,
      ward = _ward,
      district = _district,
      city = _city,
      state = _state,
      country = _country,
      pincode = _pincode,
      address = _address,
      durationInS = Nothing,
      distanceInM = Nothing,
      bbox = Nothing,
      waypoints = Nothing,
      snapped_waypoints = Nothing
    }
