module Domain.Action.Beckn.Confirm where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Common
import Data.String.Conversions
import qualified Data.Text as T
import Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DRD
import Servant.Client (BaseUrl (..))
import qualified SharedLogic.CallBAP as BP
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.Merchant as QM
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.Booking.BookingLocation as QBL
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Tools.Notifications as Notify

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DBL.LocationAddress,
    toAddress :: DBL.LocationAddress,
    mbRiderName :: Maybe Text
  }

data DConfirmRes = DConfirmRes
  { booking :: DRB.Booking,
    ride :: DRide.Ride,
    fromLocation :: DBL.BookingLocation,
    toLocation :: DBL.BookingLocation,
    riderDetails :: DRD.RiderDetails,
    transporter :: DM.Merchant
  }

handler ::
  ( HasCacheConfig r,
    FCMFlow m r,
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasPrettyLogger m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Subscriber.Subscriber ->
  Id DM.Merchant ->
  DConfirmReq ->
  m DConfirmRes
handler subscriber transporterId req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  driverQuote <- QDQ.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
  driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
  let transporterId' = booking.providerId
  transporter <-
    QM.findById transporterId'
      >>= fromMaybeM (MerchantNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  now <- getCurrentTime
  unless (driverQuote.validTill > now || driverQuote.status == DDQ.Active) $ do
    cancelBooking booking driver transporter
    throwError $ QuoteExpired driverQuote.id.getId
  let bapMerchantId = booking.bapId
  unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
  (riderDetails, isNewRider) <- getRiderDetails req.customerMobileCountryCode req.customerPhoneNumber now
  ride <- buildRide driver.id booking
  driverSearchReqs <- QSRD.findAllByRequestId driverQuote.searchRequestId
  Esq.runTransaction $ do
    when isNewRider $ QRD.create riderDetails
    QRB.updateRiderId booking.id riderDetails.id
    QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
    QBL.updateAddress booking.fromLocation.id req.fromAddress
    QBL.updateAddress booking.toLocation.id req.toAddress
    whenJust req.mbRiderName $ QRB.updateRiderName booking.id
    QDI.updateOnRide (cast driver.id) True
    QRide.create ride
    QBE.logRideConfirmedEvent booking.id
    QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id
    QDQ.setInactiveByRequestId driverQuote.searchRequestId

  for_ driverSearchReqs $ \driverReq -> do
    let driverId = driverReq.driverId
    unless (driverId == driver.id) $ do
      driver_ <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      Notify.notifyDriverClearedFare driverId driverReq.searchRequestId driverQuote.estimatedFare driver_.deviceToken

  Esq.runTransaction $ do
    QSRD.removeAllBySearchId driverQuote.searchRequestId

  uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
  Notify.notifyDriver notificationType notificationTitle (message uBooking) driver.id driver.deviceToken

  pure
    DConfirmRes
      { booking = uBooking,
        ride,
        riderDetails,
        transporter,
        fromLocation = uBooking.fromLocation,
        toLocation = uBooking.toLocation
      }
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message booking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst booking.startTime) <> ".",
            "Check the app for more details."
          ]
    buildRide driverId booking = do
      guid <- Id <$> generateGUID
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      trackingUrl <- buildTrackingUrl guid
      return
        DRide.Ride
          { id = guid,
            bookingId = booking.id,
            shortId = shortId,
            status = DRide.NEW,
            driverId = cast driverId,
            otp = otp,
            trackingUrl = trackingUrl,
            fare = Nothing,
            traveledDistance = 0,
            chargeableDistance = Nothing,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            rideRating = Nothing,
            tripStartPos = Nothing,
            tripEndPos = Nothing,
            createdAt = now,
            updatedAt = now
          }
    buildTrackingUrl rideId = do
      bppUIUrl <- asks (.selfUIUrl)
      let rideid = T.unpack (getId rideId)
      return $
        bppUIUrl
          { --TODO: find a way to build it using existing types from Routes
            baseUrlPath = baseUrlPath bppUIUrl <> "/driver/location/" <> rideid
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

cancelBooking ::
  ( FCMFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  DPerson.Person ->
  DM.Merchant ->
  m ()
cancelBooking booking driver transporter = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  bookingCancellationReason <- buildBookingCancellationReason booking.id driver.id
  Esq.runTransaction $ do
    QRB.updateStatus booking.id DRB.CANCELLED
    QBCR.create bookingCancellationReason
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter DBCR.ByApplication
  fork "cancelRide - Notify driver" $ do
    Notify.notifyOnCancel booking driver.id driver.deviceToken DBCR.ByApplication

buildBookingCancellationReason ::
  MonadFlow m =>
  Id DRB.Booking ->
  Id DPerson.Person ->
  m DBCR.BookingCancellationReason
buildBookingCancellationReason bookingId driverId = do
  guid <- generateGUID
  return
    DBCR.BookingCancellationReason
      { id = guid,
        driverId = Just driverId,
        bookingId,
        rideId = Nothing,
        source = DBCR.ByApplication,
        reasonCode = Nothing,
        additionalInfo = Nothing
      }
