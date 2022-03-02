module Product.RideBooking where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP (buildRideReq)
import qualified Product.Location as Location
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.RideBooking as API
import Types.Error
import Types.Storage.AllocationEvent
import qualified Types.Storage.AllocationEvent as AllocationEvent
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import Types.Storage.RideRequest
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Types.Storage.SearchReqLocation as SLoc
import qualified Types.Storage.Vehicle as SV
import Utils.Common

rideBookingStatus :: Id SRB.RideBooking -> Id SP.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId _ = withFlowHandlerAPI $ do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
  buildRideBookingStatusRes rideBooking

rideBookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList person mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  rbList <- QRB.findAllByOrg orgId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rbList

rideBookingCancel ::
  Id SRB.RideBooking ->
  SP.Person ->
  FlowHandler APISuccess
rideBookingCancel rideBookingId admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <-
    QOrg.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  now <- getCurrentTime
  RideRequest.createFlow =<< buildRideReq rideBookingId (org.shortId) SRideRequest.CANCELLATION now
  return Success

getRideInfo :: Id SRB.RideBooking -> Id SP.Person -> FlowHandler API.GetRideInfoRes
getRideInfo rideBookingId personId = withFlowHandlerAPI $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideBookingId
  case mbNotification of
    Nothing -> return $ API.GetRideInfoRes Nothing
    Just notification -> do
      let notificationExpiryTime = notification.expiresAt
      rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingNotFound
      driver <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let driverLatLong = Location.locationToLatLong driverLocation
      fromLocation <-
        QLoc.findLocationById rideBooking.fromLocationId
          >>= fromMaybeM LocationNotFound
      let fromLatLong = Location.locationToLatLong fromLocation
      toLocation <-
        QLoc.findLocationById rideBooking.toLocationId
          >>= fromMaybeM LocationNotFound
      mbRoute <- Location.getRoute' [driverLatLong, fromLatLong]
      return $
        API.GetRideInfoRes $
          Just $
            API.RideInfo
              { bookingId = rideBooking.id,
                pickupLoc = SLoc.makeSearchReqLocationAPIEntity fromLocation,
                dropLoc = SLoc.makeSearchReqLocationAPIEntity toLocation,
                etaForPickupLoc = (`div` 60) . (.durationInS) <$> mbRoute,
                distanceToPickupLoc = (.distanceInM) <$> mbRoute,
                notificationExpiryTime = notificationExpiryTime,
                estimatedFare = rideBooking.estimatedFare,
                discount = rideBooking.discount,
                estimatedTotalFare = rideBooking.estimatedTotalFare
              }
  where
    driverId = cast personId

responseToEventType :: API.NotificationStatus -> AllocationEventType
responseToEventType API.ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType API.REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance :: Id SRB.RideBooking -> Id SP.Person -> API.SetDriverAcceptanceReq -> FlowHandler API.SetDriverAcceptanceRes
setDriverAcceptance rideBookingId personId req = withFlowHandlerAPI $ do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  rideBooking <-
    QRB.findById rideBookingId
      >>= fromMaybeM RideBookingDoesNotExist
  transporterOrg <-
    QOrg.findOrganizationById rideBooking.providerId
      >>= fromMaybeM OrgDoesNotExist
  guid <- generateGUID
  let driverResponse =
        API.DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            rideBookingId = rideBookingId,
            shortOrgId = transporterOrg.shortId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just $ encodeToText driverResponse
          }
  RideRequest.createFlow rideRequest
  AllocationEvent.logAllocationEvent
    (responseToEventType response)
    rideBookingId
    (Just driverId)
  pure Success
  where
    response = req.response
    driverId = cast personId
    logMessage =
      "beckn:" <> rideBookingId.getId <> ":"
        <> getId driverId
        <> ":response"
        <> " "
        <> show response

buildRideBookingStatusRes :: (DBFlow m r, EncFlow m r) => SRB.RideBooking -> m API.RideBookingStatusRes
buildRideBookingStatusRes rideBooking = do
  fromLocation <- QLoc.findLocationById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findLocationById rideBooking.toLocationId >>= fromMaybeM LocationNotFound
  let rbStatus = rideBooking.status
  mbRide <- QRide.findActiveByRBId rideBooking.id
  mbRideAPIEntity <- case mbRide of
    Just ride -> do
      now <- getCurrentTime
      vehicleM <- QVeh.findVehicleById ride.vehicleId
      let vehicle = fromMaybe (vehicleDefault now) vehicleM
      driver <- QP.findPersonById ride.driverId
      decDriver <- case driver of
        Just encDriver ->
          decrypt encDriver
        Nothing ->
          return $ driverDefault now
      return . Just $ SRide.makeRideAPIEntity ride decDriver vehicle
    Nothing -> return Nothing
  return $
    API.RideBookingStatusRes
      { id = rideBooking.id,
        status = rbStatus,
        estimatedFare = rideBooking.estimatedFare,
        discount = rideBooking.discount,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        toLocation = SLoc.makeSearchReqLocationAPIEntity toLocation,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        ride = mbRideAPIEntity,
        createdAt = rideBooking.createdAt,
        updatedAt = rideBooking.updatedAt
      }
  where
    driverDefault now =
      SP.Person
        { id = Id "[Driver deleted]",
          firstName = Nothing,
          middleName = Nothing,
          lastName = Nothing,
          fullName = Nothing,
          role = SP.DRIVER,
          gender = SP.FEMALE,
          identifierType = SP.EMAIL,
          email = Nothing,
          mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          passwordHash = Nothing,
          identifier = Nothing,
          rating = Nothing,
          isNew = False,
          udf1 = Nothing,
          udf2 = Nothing,
          organizationId = Nothing,
          deviceToken = Nothing,
          description = Nothing,
          createdAt = now,
          updatedAt = now
        }
    vehicleDefault now =
      SV.Vehicle
        { id = Id "[Vehicle deleted]",
          organizationId = Id "N/A",
          variant = SV.SEDAN,
          model = "N/A",
          color = "N/A",
          registrationNo = "N/A",
          capacity = Nothing,
          category = Nothing,
          make = Nothing,
          size = Nothing,
          energyType = Nothing,
          registrationCategory = Nothing,
          createdAt = now,
          updatedAt = now
        }
