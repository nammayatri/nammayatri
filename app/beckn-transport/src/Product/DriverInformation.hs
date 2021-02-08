{-# LANGUAGE OverloadedLabels #-}

module Product.DriverInformation where

import qualified App.Types as App
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App
import Beckn.Types.MapSearch
import Beckn.Types.Storage.RegistrationToken (RegistrationToken, RegistrationTokenT (..))
import Beckn.Utils.Common (fromMaybeM500, withFlowHandler)
import Data.List.NonEmpty (unzip)
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (unzip)
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.Registration as Registration
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Location as QLocation
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QueryPI
import qualified Types.API.DriverInformation as DriverInformationAPI
import Types.App

getInformation :: RegistrationToken -> App.FlowHandler DriverInformationAPI.DriverInformationResponse
getInformation RegistrationToken {..} = withFlowHandler $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = DriverId _EntityId
  person <- QPerson.findPersonById (PersonId _EntityId)
  personEntity <- Person.mkPersonRes person
  orgId <- person ^. #_organizationId & fromMaybeM500 "ORGANIZATION_ID_IS_NOT_PRESENT"
  organization <- QOrganization.findOrganizationById $ OrganizationId orgId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM500 "INVALID_DRIVER_ID"
  pure $
    DriverInformationAPI.DriverInformationResponse
      { transporter = organization,
        person = personEntity,
        driverInformation = driverInfo
      }

setActivity :: RegistrationToken -> Bool -> App.FlowHandler APIResult.APIResult
setActivity RegistrationToken {..} isActive = withFlowHandler $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = DriverId _EntityId
  QDriverInformation.updateActivity driverId isActive
  pure APIResult.Success

getRideInfo :: RegistrationToken -> Maybe ProductInstanceId -> App.FlowHandler DriverInformationAPI.GetRideInfoRes
getRideInfo RegistrationToken {..} mbProductInstanceId = withFlowHandler $ do
  let rideId = RideId . _getProductInstanceId <$> mbProductInstanceId
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideId
  case mbNotification of
    Nothing -> return emptyGetRideInfoRes
    Just notification -> do
      let productInstanceId = rideIdToProductInstanceId $ notification ^. #_rideId
          notificationTime = notification ^. #_notifiedAt
      driverNotificationExpiry <- getDriverNotificationExpiry
      productInstance <- QueryPI.findById productInstanceId
      driver <- QPerson.findPersonById personId
      driverLocation <- findLocationById $ driver ^. #_locationId
      fromLocation <- findLocationById $ productInstance ^. #_fromLocation
      toLocation <- findLocationById $ productInstance ^. #_toLocation
      let (fromLat, fromLong) = extractLatLong fromLocation
          (driverLat, driverLong) = extractLatLong driverLocation
      mbRoute <- getRoute driverLat driverLong fromLat fromLong
      return
        DriverInformationAPI.GetRideInfoRes
          { _productInstanceId = Just productInstanceId,
            _pickupLoc = fromLocation,
            _dropLoc = toLocation,
            _etaForPickupLoc = (`div` 60) . durationInS <$> mbRoute,
            _distanceToPickupLoc = distanceInM <$> mbRoute,
            _notificationExpiryTime = addUTCTime driverNotificationExpiry <$> notificationTime
          }
  where
    driverId = DriverId _EntityId
    personId = PersonId $ _getDriverId driverId
    rideIdToProductInstanceId rideId = ProductInstanceId $ rideId ^. #_getRideId
    getDriverNotificationExpiry = App.driverNotificationExpiry . App.driverAllocationConfig <$> ask
    findLocationById mbId = maybe (return Nothing) QLocation.findLocationById $ LocationId <$> mbId
    extractLatLong location = unzip (location >>= (\loc -> (,) <$> loc ^. #_lat <*> loc ^. #_long))
    getRoute fromLat fromLong toLat toLong = fromMaybe (return Nothing) $ Location.getRoute' <$> fromLat <*> fromLong <*> toLat <*> toLong
    emptyGetRideInfoRes =
      DriverInformationAPI.GetRideInfoRes
        { _productInstanceId = mbProductInstanceId,
          _pickupLoc = Nothing,
          _dropLoc = Nothing,
          _etaForPickupLoc = Nothing,
          _distanceToPickupLoc = Nothing,
          _notificationExpiryTime = Nothing
        }
