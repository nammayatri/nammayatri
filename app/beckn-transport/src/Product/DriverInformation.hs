{-# LANGUAGE OverloadedLabels #-}

module Product.DriverInformation where

import App.Types
import qualified App.Types as App
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount (amountToString)
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Types.Storage.RegistrationToken (RegistrationToken, RegistrationTokenT (..))
import Beckn.Utils.Common (fromMaybeM, withFlowHandlerAPI)
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
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Types.API.DriverInformation as DriverInformationAPI
import Types.App
import Types.Error

getInformation :: RegistrationToken -> App.FlowHandler DriverInformationAPI.DriverInformationResponse
getInformation RegistrationToken {..} = withFlowHandlerAPI $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = Id _EntityId
  person <- QPerson.findPersonById (Id _EntityId)
  personEntity <- Person.mkPersonRes person
  orgId <- person ^. #_organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  organization <- QOrganization.findOrganizationById $ Id orgId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  pure $
    DriverInformationAPI.DriverInformationResponse
      { transporter = organization,
        person = personEntity,
        driverInformation = driverInfo
      }

setActivity :: RegistrationToken -> Bool -> App.FlowHandler APISuccess.APISuccess
setActivity RegistrationToken {..} isActive = withFlowHandlerAPI $ do
  _ <- Registration.checkPersonExists _EntityId
  let driverId = Id _EntityId
  QDriverInformation.updateActivity driverId isActive
  pure APISuccess.Success

getRideInfo :: RegistrationToken -> Maybe (Id Ride) -> App.FlowHandler DriverInformationAPI.GetRideInfoRes
getRideInfo RegistrationToken {..} rideId = withFlowHandlerAPI $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideId
  case mbNotification of
    Nothing -> return $ DriverInformationAPI.GetRideInfoRes Nothing
    Just notification -> do
      let productInstanceId = cast $ notification ^. #_rideId
      let notificationExpiryTime = notification ^. #_expiresAt
      productInstance <- QueryPI.findById productInstanceId
      driver <- QPerson.findPersonById $ cast driverId
      driverLocation <- findLocationById (driver ^. #_locationId) >>= fromMaybeM (PersonFieldNotPresent "location_id")
      fromLocation <- findLocationById (productInstance ^. #_fromLocation) >>= fromMaybeM (PIFieldNotPresent "location_id")
      toLocation <- findLocationById (productInstance ^. #_toLocation) >>= fromMaybeM (PIFieldNotPresent "to_location_id")
      (fromLat, fromLong) <- extractLatLong fromLocation & fromMaybeM (LocationFieldNotPresent "from")
      (driverLat, driverLong) <- extractLatLong driverLocation & fromMaybeM (LocationFieldNotPresent "driver")
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
                estimatedPrice = amountToString <$> productInstance ^. #_price
              }
  where
    driverId = Id _EntityId
    findLocationById mbId = maybe (return Nothing) QLocation.findLocationById $ Id <$> mbId
    extractLatLong = \loc -> (,) <$> loc ^. #_lat <*> loc ^. #_long

listDriver :: Text -> Maybe Integer -> Maybe Integer -> FlowHandler DriverInformationAPI.ListDriverRes
listDriver orgId mbLimit mbOffset = withFlowHandlerAPI $ do
  personList <- QDriverInformation.findAllWithLimitOffsetByOrgIds mbLimit mbOffset [orgId]
  respPersonList <- traverse convertToRes personList
  return $ DriverInformationAPI.ListDriverRes respPersonList
  where
    convertToRes (person, driverInfo) = do
      vehicle <- maybe (return Nothing) QVehicle.findVehicleById $ Id <$> person ^. #_udf1
      return $
        DriverInformationAPI.DriverEntityRes
          { id = person ^. #_id,
            firstName = person ^. #_firstName,
            middleName = person ^. #_middleName,
            lastName = person ^. #_lastName,
            mobileNumber = person ^. #_mobileNumber,
            linkedVehicle = vehicle,
            active = driverInfo ^. #_active,
            onRide = driverInfo ^. #_onRide
          }
