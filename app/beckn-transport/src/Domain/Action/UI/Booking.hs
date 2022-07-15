module Domain.Action.UI.Booking
  ( RideBookingListRes (..),
    GetRideInfoRes (..),
    RideInfo (..),
    SetDriverAcceptanceReq (..),
    SetDriverAcceptanceRes,
    NotificationStatus (..),
    DriverResponse (..),
    bookingStatus,
    bookingList,
    bookingCancel,
    getRideInfo,
    setDriverAcceptance,
  )
where

import Beckn.External.GoogleMaps.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Data.OpenApi (ToSchema (..))
import Domain.Types.AllocationEvent
import qualified Domain.Types.AllocationEvent as AllocationEvent
import Domain.Types.BookingLocation as DBLoc
import qualified Domain.Types.Person as SP
import Domain.Types.RideBooking (RideBooking)
import qualified Domain.Types.RideBooking as SRB
import Domain.Types.RideRequest
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude hiding (id)
import Product.BecknProvider.BP (buildRideReq)
import qualified Product.Location as Location
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Metrics
import Types.Error
import Utils.Common

newtype RideBookingListRes = RideBookingListRes
  { list :: [SRB.RideBookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data RideInfo = RideInfo
  { bookingId :: Id RideBooking,
    pickupLoc :: BookingLocationAPIEntity,
    dropLoc :: Maybe BookingLocationAPIEntity,
    etaForPickupLoc :: Minutes,
    distanceToPickupLoc :: Meters,
    notificationExpiryTime :: UTCTime,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

newtype SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { response :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type SetDriverAcceptanceRes = APISuccess

bookingStatus :: (EsqDBFlow m r, EncFlow m r) => Id SRB.RideBooking -> m SRB.RideBookingAPIEntity
bookingStatus bookingId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (RideBookingDoesNotExist bookingId.getId)
  SRB.buildRideBookingAPIEntity booking

bookingList :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m RideBookingListRes
bookingList person mbLimit mbOffset mbOnlyActive = do
  let Just orgId = person.organizationId
  rbList <- QRB.findAllByOrg orgId mbLimit mbOffset mbOnlyActive
  RideBookingListRes <$> traverse SRB.buildRideBookingAPIEntity rbList

bookingCancel ::
  (EsqDBFlow m r) =>
  Id SRB.RideBooking ->
  SP.Person ->
  m APISuccess
bookingCancel bookingId admin = do
  let Just orgId = admin.organizationId
  org <-
    QOrg.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
  now <- getCurrentTime
  rideReq <- buildRideReq bookingId (org.shortId) SRideRequest.CANCELLATION now
  Esq.runTransaction $ RideRequest.create rideReq
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> bookingCancel : ") (show rideReq)
  return Success

getRideInfo ::
  (EsqDBFlow m r, CoreMetrics m, HasGoogleMaps m r) => Id SRB.RideBooking -> Id SP.Person -> m GetRideInfoRes
getRideInfo bookingId personId = do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId bookingId
  case mbNotification of
    Nothing -> return $ GetRideInfoRes Nothing
    Just notification -> do
      let notificationExpiryTime = notification.expiresAt
      booking <- QRB.findById bookingId >>= fromMaybeM (RideBookingNotFound bookingId.getId)
      driver <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let driverLatLong = Location.locationToLatLong driverLocation
      fromLocation <-
        QBLoc.findById booking.fromLocationId
          >>= fromMaybeM LocationNotFound
      let fromLatLong = Location.locationToLatLong fromLocation
      toLocation <- case booking.rideBookingDetails of
        SRB.OneWayDetails details -> QBLoc.findById details.toLocationId >>= fromMaybeM LocationNotFound . Just
        SRB.RentalDetails _ -> pure Nothing
      distanceDuration <- MapSearch.getDistance (Just MapSearch.CAR) driverLatLong fromLatLong
      return $
        GetRideInfoRes $
          Just $
            RideInfo
              { bookingId = booking.id,
                pickupLoc = DBLoc.makeBookingLocationAPIEntity fromLocation,
                dropLoc = DBLoc.makeBookingLocationAPIEntity <$> toLocation,
                etaForPickupLoc = secondsToMinutes $ distanceDuration.duration,
                distanceToPickupLoc = distanceDuration.distance,
                notificationExpiryTime = notificationExpiryTime,
                estimatedFare = booking.estimatedFare,
                discount = booking.discount,
                estimatedTotalFare = booking.estimatedTotalFare
              }
  where
    driverId = cast personId

responseToEventType :: NotificationStatus -> AllocationEventType
responseToEventType ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance ::
  (EsqDBFlow m r) => Id SRB.RideBooking -> Id SP.Person -> SetDriverAcceptanceReq -> m SetDriverAcceptanceRes
setDriverAcceptance bookingId personId req = do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  booking <-
    QRB.findById bookingId
      >>= fromMaybeM (RideBookingDoesNotExist bookingId.getId)
  transporterOrg <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgDoesNotExist booking.providerId.getId)
  guid <- generateGUID
  let driverResponse =
        DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            rideBookingId = bookingId,
            shortOrgId = transporterOrg.shortId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just driverResponse
          }
  Esq.runTransaction $ do
    RideRequest.create rideRequest
    AllocationEvent.logAllocationEvent
      (responseToEventType response)
      bookingId
      (Just driverId)
  pure Success
  where
    response = req.response
    driverId = cast personId
    logMessage =
      "beckn:" <> bookingId.getId <> ":"
        <> getId driverId
        <> ":response"
        <> " "
        <> show response
