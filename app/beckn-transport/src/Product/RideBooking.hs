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
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.Vehicle as QVeh
import qualified Types.API.RideBooking as API
import Types.Error
import Types.Storage.AllocationEvent
import qualified Types.Storage.AllocationEvent as AllocationEvent
import qualified Types.Storage.Person as SP
import Types.Storage.ProductInstance (ProductInstance)
import qualified Types.Storage.ProductInstance as SPI
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRideBooking
import Types.Storage.RideRequest
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Types.Storage.SearchReqLocation as SLoc
import Utils.Common

rideBookingStatus :: Id SPI.ProductInstance -> Id SP.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId _ = withFlowHandlerAPI $ do
  orderPI <- QPI.findById rideBookingId >>= fromMaybeM PIDoesNotExist
  buildRideBookingStatusRes orderPI

rideBookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList person mbLimit mbOffset mbIsOnlyActive = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  orderPIList <- QPI.findAllOrdersByOrg orgId mbLimit mbOffset mbIsOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes orderPIList

rideBookingCancel ::
  Id SPI.ProductInstance ->
  SP.Person ->
  FlowHandler APISuccess
rideBookingCancel rideBookingId admin = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  org <-
    QOrg.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  RideRequest.createFlow =<< mkRideReq rideBookingId (org.shortId) SRideRequest.CANCELLATION
  return Success

getRideInfo :: Id SPI.ProductInstance -> Id SP.Person -> FlowHandler API.GetRideInfoRes
getRideInfo rideBookingId personId = withFlowHandlerAPI $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId (cast rideBookingId)
  case mbNotification of
    Nothing -> return $ API.GetRideInfoRes Nothing
    Just notification -> do
      let orderPIId = cast $ notification.rideId
      let notificationExpiryTime = notification.expiresAt
      orderPI <- QPI.findById orderPIId >>= fromMaybeM PINotFound
      driver <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let driverLatLong = Location.locationToLatLong driverLocation
      fromLocation <-
        orderPI.fromLocation & fromMaybeM (PIFieldNotPresent "from_location_id")
          >>= QLoc.findLocationById
          >>= fromMaybeM LocationNotFound
      let fromLatLong = Location.locationToLatLong fromLocation
      toLocation <-
        orderPI.toLocation & fromMaybeM (PIFieldNotPresent "to_location_id")
          >>= QLoc.findLocationById
          >>= fromMaybeM LocationNotFound
      mbRoute <- Location.getRoute' [driverLatLong, fromLatLong]
      return $
        API.GetRideInfoRes $
          Just $
            API.RideInfo
              { bookingId = orderPIId,
                pickupLoc = fromLatLong,
                dropLoc = Location.locationToLatLong toLocation,
                etaForPickupLoc = (`div` 60) . (.durationInS) <$> mbRoute,
                distanceToPickupLoc = (.distanceInM) <$> mbRoute,
                notificationExpiryTime = notificationExpiryTime,
                estimatedPrice = amountToString <$> orderPI.price
              }
  where
    driverId = cast personId

responseToEventType :: API.NotificationStatus -> AllocationEventType
responseToEventType API.ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType API.REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance :: Id ProductInstance -> Id SP.Person -> API.SetDriverAcceptanceReq -> FlowHandler API.SetDriverAcceptanceRes
setDriverAcceptance rideBookingId personId req = withFlowHandlerAPI $ do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  productInstance <-
    QPI.findById productInstanceId
      >>= fromMaybeM PIDoesNotExist
  transporterOrg <-
    QOrg.findOrganizationById productInstance.organizationId
      >>= fromMaybeM OrgDoesNotExist
  guid <- generateGUID
  let driverResponse =
        API.DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            rideId = cast productInstanceId,
            shortOrgId = transporterOrg.shortId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just $ encodeToText driverResponse
          }
  RideRequest.createFlow rideRequest
  AllocationEvent.logAllocationEvent
    (responseToEventType response)
    (cast productInstanceId)
    (Just driverId)
  pure Success
  where
    response = req.response
    productInstanceId = rideBookingId
    driverId = cast personId
    logMessage =
      "beckn:" <> productInstanceId.getId <> ":"
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
listDriverRides driverId mbLimit mbOffset mbIsOnlyActive = withFlowHandlerAPI $ do
  orderPIList <- QPI.findAllOrdersByDriver driverId mbLimit mbOffset mbIsOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes orderPIList

buildRideBookingStatusRes :: (DBFlow m r, EncFlow m r) => SPI.ProductInstance -> m API.RideBookingStatusRes
buildRideBookingStatusRes orderPI = do
  fromLocId <- orderPI.fromLocation & fromMaybeM (PIFieldNotPresent "fromLocation")
  toLocId <- orderPI.toLocation & fromMaybeM (PIFieldNotPresent "toLocation")
  fromLocation <- QLoc.findLocationById fromLocId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findLocationById toLocId >>= fromMaybeM LocationNotFound
  let rbStatus = case orderPI.status of
        SPI.INSTOCK -> SRideBooking.NEW
        SPI.CONFIRMED -> SRideBooking.CONFIRMED
        SPI.TRIP_ASSIGNED -> SRideBooking.TRIP_ASSIGNED
        SPI.COMPLETED -> SRideBooking.COMPLETED
        SPI.CANCELLED -> SRideBooking.CANCELLED
        _ -> SRideBooking.TRIP_ASSIGNED
  mbRide <-
    -- COMPLETED and TRIP_ASSIGNED means that there IS ride. But CANCELLED does not, it could be cancelled
    -- before TRIP_ASSIGNED. So technically there is a bug until we change our storage model.
    if rbStatus `elem` [SRideBooking.COMPLETED, SRideBooking.TRIP_ASSIGNED, SRideBooking.CANCELLED]
      then Just <$> buildRideAPIEntity orderPI
      else return Nothing

  return $
    API.RideBookingStatusRes
      { id = orderPI.id,
        status = rbStatus,
        estimatedPrice = orderPI.price,
        toLocation = SLoc.makeSearchReqLocationAPIEntity toLocation,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        ride = mbRide,
        createdAt = orderPI.createdAt,
        updatedAt = orderPI.updatedAt
      }

buildRideAPIEntity :: (DBFlow m r, EncFlow m r) => SPI.ProductInstance -> m SRide.RideAPIEntity
buildRideAPIEntity orderPI = do
  let rideStatus = case orderPI.status of
        SPI.INPROGRESS -> SRide.INPROGRESS
        SPI.COMPLETED -> SRide.COMPLETED
        SPI.CANCELLED -> SRide.CANCELLED
        _ -> SRide.NEW
  mbDriver <- join <$> traverse QP.findPersonById orderPI.personId
  mbDecMobNumber <- join <$> traverse decrypt (mbDriver <&> (.mobileNumber))
  mbVehicle <- join <$> traverse QVeh.findVehicleById (Id <$> orderPI.entityId)
  return $
    SRide.RideAPIEntity
      { id = orderPI.id,
        shortRideId = orderPI.shortId,
        status = rideStatus,
        driverName = mbDriver >>= (.firstName),
        driverNumber = (mbDriver >>= (.mobileCountryCode)) <> mbDecMobNumber,
        vehicleNumber = mbVehicle <&> (.registrationNo),
        vehicleColor = mbVehicle >>= (.color),
        vehicleVariant = mbVehicle >>= (.variant),
        vehicleModel = mbVehicle >>= (.model),
        computedPrice = orderPI.price,
        actualRideDistance = Nothing,
        createdAt = orderPI.createdAt,
        updatedAt = orderPI.updatedAt
      }
