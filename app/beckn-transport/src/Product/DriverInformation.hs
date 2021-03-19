{-# LANGUAGE OverloadedLabels #-}

module Product.DriverInformation where

import qualified App.Types as App
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Amount (amountToString)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Types.Storage.RegistrationToken (RegistrationToken, RegistrationTokenT (..))
import Beckn.Utils.Common (fromMaybeM500, fromMaybeMWithInfo500, withFlowHandler)
import EulerHS.Prelude
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
import Types.Error

getInformation :: RegistrationToken -> App.FlowHandler DriverInformationAPI.DriverInformationResponse
getInformation RegistrationToken {..} = withFlowHandler $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = Id _EntityId
  person <- QPerson.findPersonById (Id _EntityId)
  personEntity <- Person.mkPersonRes person
  orgId <- person ^. #_organizationId & fromMaybeMWithInfo500 PersonInvalidState "_organizationId is null."
  organization <- QOrganization.findOrganizationById $ Id orgId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM500 DriverInfoNotFound
  pure $
    DriverInformationAPI.DriverInformationResponse
      { transporter = organization,
        person = personEntity,
        driverInformation = driverInfo
      }

setActivity :: RegistrationToken -> Bool -> App.FlowHandler APIResult.APIResult
setActivity RegistrationToken {..} isActive = withFlowHandler $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = Id _EntityId
  QDriverInformation.updateActivity driverId isActive
  pure APIResult.Success

getRideInfo :: RegistrationToken -> Maybe (Id Ride) -> App.FlowHandler DriverInformationAPI.GetRideInfoRes
getRideInfo RegistrationToken {..} rideId = withFlowHandler $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideId
  case mbNotification of
    Nothing -> return $ DriverInformationAPI.GetRideInfoRes Nothing
    Just notification -> do
      let productInstanceId = cast $ notification ^. #_rideId
      let notificationExpiryTime = notification ^. #_expiresAt
      productInstance <- QueryPI.findById productInstanceId
      driver <- QPerson.findPersonById $ cast driverId
      driverLocation <- findLocationById (driver ^. #_locationId) >>= fromMaybeMWithInfo500 PersonInvalidState "_locationId is null."
      fromLocation <- findLocationById (productInstance ^. #_fromLocation) >>= fromMaybeMWithInfo500 ProductInstanceInvalidState "_fromLocation is null."
      toLocation <- findLocationById (productInstance ^. #_toLocation) >>= fromMaybeMWithInfo500 ProductInstanceInvalidState "_toLocation is null."
      (fromLat, fromLong) <- extractLatLong fromLocation & fromMaybeMWithInfo500 LocationInvalidState "_lat or _long is null."
      (driverLat, driverLong) <- extractLatLong driverLocation & fromMaybeMWithInfo500 LocationInvalidState "_lat or _long is null."
      mbRoute <- Location.getRoute' driverLat driverLong fromLat fromLong
      return $
        DriverInformationAPI.GetRideInfoRes $
          Just $
            DriverInformationAPI.RideInfo
              { productInstanceId = productInstanceId,
                pickupLoc = fromLocation,
                dropLoc = toLocation,
                etaForPickupLoc = (`div` 60) . durationInS <$> mbRoute,
                distanceToPickupLoc = distanceInM <$> mbRoute,
                notificationExpiryTime = notificationExpiryTime,
                estimatedPrice = amountToString $ productInstance ^. #_price
              }
  where
    driverId = Id _EntityId
    findLocationById mbId = maybe (return Nothing) QLocation.findLocationById $ Id <$> mbId
    extractLatLong = \loc -> (,) <$> loc ^. #_lat <*> loc ^. #_long
