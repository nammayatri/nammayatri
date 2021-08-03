module Product.RideBooking where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.Types.APISuccess
import Beckn.Types.Amount (amountToString)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP (mkRideReq)
import qualified Product.Location as Location
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.RideBooking as API
import Types.Error
import Types.Storage.AllocationEvent
import qualified Types.Storage.AllocationEvent as AllocationEvent
import qualified Types.Storage.Person as SP
import qualified Types.Storage.OldRide as SRide
import qualified Types.Storage.RideBooking as SRideBooking
import Types.Storage.RideRequest
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Types.Storage.SearchReqLocation as SLoc
import Utils.Common
import qualified Storage.Queries.Ride as QRide

rideBookingStatus :: Id SRide.Ride -> Id SP.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId _ = withFlowHandlerAPI $ do
  ride <- QRide.findById rideBookingId >>= fromMaybeM RideDoesNotExist
  buildRideBookingStatusRes ride

rideBookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList person mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  rideList <- QRide.findAllByOrg orgId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rideList

rideBookingCancel ::
  Id SRide.Ride ->
  SP.Person ->
  FlowHandler APISuccess
rideBookingCancel rideBookingId admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <-
    QOrg.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  RideRequest.createFlow =<< mkRideReq rideBookingId (org.shortId) SRideRequest.CANCELLATION
  return Success

getRideInfo :: Id SRide.Ride -> Id SP.Person -> FlowHandler API.GetRideInfoRes
getRideInfo rideBookingId personId = withFlowHandlerAPI $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId (cast rideBookingId)
  case mbNotification of
    Nothing -> return $ API.GetRideInfoRes Nothing
    Just notification -> do
      let rideId = cast $ notification.rideId
      let notificationExpiryTime = notification.expiresAt
      ride <- QRide.findById rideId >>= fromMaybeM RideNotFound
      driver <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let driverLatLong = Location.locationToLatLong driverLocation
      fromLocation <-
        ride.fromLocation & fromMaybeM (RideFieldNotPresent "from_location_id")
          >>= QLoc.findLocationById
          >>= fromMaybeM LocationNotFound
      let fromLatLong = Location.locationToLatLong fromLocation
      toLocation <-
        ride.toLocation & fromMaybeM (RideFieldNotPresent "to_location_id")
          >>= QLoc.findLocationById
          >>= fromMaybeM LocationNotFound
      mbRoute <- Location.getRoute' [driverLatLong, fromLatLong]
      return $
        API.GetRideInfoRes $
          Just $
            API.RideInfo
              { bookingId = rideId,
                pickupLoc = fromLatLong,
                dropLoc = Location.locationToLatLong toLocation,
                etaForPickupLoc = (`div` 60) . (.durationInS) <$> mbRoute,
                distanceToPickupLoc = (.distanceInM) <$> mbRoute,
                notificationExpiryTime = notificationExpiryTime,
                estimatedPrice = amountToString <$> ride.price
              }
  where
    driverId = cast personId

responseToEventType :: API.NotificationStatus -> AllocationEventType
responseToEventType API.ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType API.REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance :: Id SRide.Ride -> Id SP.Person -> API.SetDriverAcceptanceReq -> FlowHandler API.SetDriverAcceptanceRes
setDriverAcceptance rideBookingId personId req = withFlowHandlerAPI $ do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  productInstance <-
    QRide.findById rideBookingId
      >>= fromMaybeM RideDoesNotExist
  transporterOrg <-
    QOrg.findOrganizationById productInstance.organizationId
      >>= fromMaybeM OrgDoesNotExist
  guid <- generateGUID
  let driverResponse =
        API.DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            rideId = rideBookingId,
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

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler API.RideBookingListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rideList <- QRide.findAllByDriver driverId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rideList

buildRideBookingStatusRes :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m API.RideBookingStatusRes
buildRideBookingStatusRes ride = do
  fromLocId <- ride.fromLocation & fromMaybeM (RideFieldNotPresent "fromLocation")
  toLocId <- ride.toLocation & fromMaybeM (RideFieldNotPresent "toLocation")
  fromLocation <- QLoc.findLocationById fromLocId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findLocationById toLocId >>= fromMaybeM LocationNotFound
  let rbStatus = case ride.status of
        SRide.CONFIRMED -> SRideBooking.CONFIRMED
        SRide.TRIP_ASSIGNED -> SRideBooking.TRIP_ASSIGNED
        SRide.COMPLETED -> SRideBooking.COMPLETED
        SRide.CANCELLED -> SRideBooking.CANCELLED
        _ -> SRideBooking.TRIP_ASSIGNED
  mbRide <-
    -- COMPLETED and TRIP_ASSIGNED means that there IS ride. But CANCELLED does not, it could be cancelled
    -- before TRIP_ASSIGNED. So technically there is a bug until we change our storage model.
    if rbStatus `elem` [SRideBooking.COMPLETED, SRideBooking.TRIP_ASSIGNED, SRideBooking.CANCELLED]
      then Just <$> buildRideAPIEntity ride
      else return Nothing

  return $
    API.RideBookingStatusRes
      { id = ride.id,
        status = rbStatus,
        estimatedPrice = ride.price,
        toLocation = SLoc.makeSearchReqLocationAPIEntity toLocation,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        ride = mbRide,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }

buildRideAPIEntity :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m SRide.RideAPIEntity
buildRideAPIEntity ride = do
  let rideStatus = case ride.status of
        SRide.INPROGRESS -> SRide.INPROGRESS
        SRide.COMPLETED -> SRide.COMPLETED
        SRide.CANCELLED -> SRide.CANCELLED
        _ -> SRide.NEW
  mbDriver <- join <$> traverse QP.findPersonById ride.personId
  mbDecMobNumber <- join <$> traverse decrypt (mbDriver <&> (.mobileNumber))
  mbVehicle <- join <$> traverse QVeh.findVehicleById (Id <$> ride.entityId)
  return $
    SRide.RideAPIEntity
      { id = ride.id,
        shortRideId = ride.shortId,
        status = rideStatus,
        driverName = mbDriver >>= (.firstName),
        driverNumber = (mbDriver >>= (.mobileCountryCode)) <> mbDecMobNumber,
        vehicleNumber = mbVehicle <&> (.registrationNo),
        vehicleColor = mbVehicle >>= (.color),
        vehicleVariant = mbVehicle >>= (.variant),
        vehicleModel = mbVehicle >>= (.model),
        computedPrice = ride.price,
        actualRideDistance = Nothing,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }
