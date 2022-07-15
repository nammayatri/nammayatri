module Domain.Action.Beckn.Confirm where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Common
--import Beckn.Utils.Error.Throwing
--import Beckn.Utils.Logging
import Data.String.Conversions
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideBooking as DRB
import qualified Domain.Types.RideBooking.BookingLocation as DBL
import qualified Domain.Types.RiderDetails as DRD
--import qualified Product.BecknProvider.BP as BP

import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBooking.BookingLocation as QBL
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.Vehicle as QVeh
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.RideBooking,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DBL.LocationAddress,
    toAddress :: DBL.LocationAddress
  }

data DConfirmRes = DConfirmRes
  { booking :: DRB.RideBooking,
    ride :: DRide.Ride,
    fromLocation :: DBL.BookingLocation,
    toLocation :: DBL.BookingLocation,
    riderDetails :: DRD.RiderDetails,
    transporter :: DOrg.Organization
  }

handler ::
  ( FCMFlow m r,
    EsqDBFlow m r,
    HasPrettyLogger m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Subscriber.Subscriber ->
  Id DOrg.Organization ->
  DConfirmReq ->
  m DConfirmRes
handler subscriber transporterId req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (RideBookingDoesNotExist req.bookingId.getId)
  driverQuote <- QDQ.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
  driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
  let transporterId' = booking.providerId
  transporter <-
    QOrg.findById transporterId'
      >>= fromMaybeM (OrgNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapOrgId = booking.bapId
  unless (subscriber.subscriber_id == bapOrgId) $ throwError AccessDenied
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails req.customerMobileCountryCode req.customerPhoneNumber now
  vehicle <- QVeh.findByPersonId driver.id >>= fromMaybeM (DriverWithoutVehicle driver.id.getId) --FIXME
  ride <- buildRide driver.id vehicle.id booking
  Esq.runTransaction $ do
    when isNewRider $ QRD.create riderDetails
    QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
    QBL.updateAddress booking.fromLocation.id req.fromAddress
    QBL.updateAddress booking.toLocation.id req.toAddress
    QDI.updateOnRide (cast driver.id) True
    QRide.create ride
    QBE.logRideConfirmedEvent booking.id
    QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id

  uRideBooking <- QRB.findById booking.id >>= fromMaybeM (RideBookingNotFound booking.id.getId)
  Notify.notifyDriver notificationType notificationTitle (message uRideBooking) driver.id driver.deviceToken

  pure
    DConfirmRes
      { booking = uRideBooking,
        ride,
        riderDetails,
        transporter,
        fromLocation = uRideBooking.fromLocation,
        toLocation = uRideBooking.toLocation
      }
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message rideBooking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst rideBooking.startTime) <> ".",
            "Check the app for more details."
          ]
    buildRide driverId vehicleId rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      return
        DRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            shortId = shortId,
            status = DRide.INPROGRESS,
            driverId = cast driverId,
            vehicleId,
            otp = otp,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            fare = Nothing,
            traveledDistance = 0,
            createdAt = now,
            updatedAt = now
          }

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Text -> Text -> UTCTime -> m (DRD.RiderDetails, Bool)
getRiderDetails customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumber customerPhoneNumber >>= \case
    Nothing -> fmap (,True) . encrypt =<< buildRiderDetails
    Just a -> return (a, False)
  where
    buildRiderDetails = do
      id <- generateGUID
      return $
        DRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now
          }
