{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Confirm where

import Data.String.Conversions
import qualified Data.Text as T
import Domain.Action.Beckn.Search
import Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.QuoteSpecialZone as DQSZ
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as SRD
import Domain.Types.RideRoute
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchRequestForDriver as SReqD
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import Kernel.Storage.Hedis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.DriverMode as DMode
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.Ride as SRide
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CGHR
import Storage.CachedQueries.GoHomeConfig as QGHC
import Storage.CachedQueries.Merchant as QM
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.Booking.BookingLocation as QBL
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QuoteSpecialZone as QQSpecialZone
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideD
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Storage.Queries.Vehicle as QVeh
import Tools.Event
import qualified Tools.Notifications as Notify

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    vehicleVariant :: VehVar.Variant,
    driverId :: Maybe Text,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DBL.LocationAddress,
    toAddress :: DBL.LocationAddress,
    mbRiderName :: Maybe Text
  }

data DConfirmRes = DConfirmRes
  { booking :: DRB.Booking,
    ride :: Maybe DRide.Ride,
    fromLocation :: DBL.BookingLocation,
    toLocation :: DBL.BookingLocation,
    riderDetails :: DRD.RiderDetails,
    riderMobileCountryCode :: Text,
    riderPhoneNumber :: Text,
    riderName :: Maybe Text,
    vehicleVariant :: VehVar.Variant,
    transporter :: DM.Merchant,
    driverId :: Maybe Text,
    driverName :: Maybe Text
  }

handler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqLocDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasPrettyLogger m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    EventStreamFlow m r
  ) =>
  DM.Merchant ->
  DConfirmReq ->
  Either (DPerson.Person, DDQ.DriverQuote) DQSZ.QuoteSpecialZone ->
  m DConfirmRes
handler transporter req quote = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails transporter.id req.customerMobileCountryCode req.customerPhoneNumber now
  unless (booking.status == DRB.NEW) $
    throwError (BookingInvalidStatus $ show booking.status)
  case booking.bookingType of
    DRB.NormalBooking -> do
      case quote of
        Left (driver, driverQuote) -> do
          cfg <- QGHC.findByMerchantId transporter.id
          ghrId <- if cfg.enableGoHome then CGHR.getDriverGoHomeRequestInfo driver.id transporter.id (Just cfg) <&> (.driverGoHomeRequestId) else return Nothing
          ride <- buildRide driver.id booking ghrId req.customerPhoneNumber
          triggerRideCreatedEvent RideEventData {ride = ride, personId = cast driver.id, merchantId = transporter.id}
          rideDetails <- buildRideDetails ride driver
          driverSearchReqs <- QSRD.findAllActiveBySTId driverQuote.searchTryId
          routeInfo :: Maybe RouteInfo <- safeGet (searchRequestKey $ getId driverQuote.requestId)
          case routeInfo of
            Just route -> setExp (searchRequestKey $ getId ride.id) route 14400
            Nothing -> logDebug "Unable to get the key"
          when isNewRider $ void $ QRD.create riderDetails
          _ <- QRB.updateRiderId booking.id riderDetails.id
          _ <- QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
          _ <- QBL.updateAddress booking.fromLocation.id req.fromAddress
          _ <- QBL.updateAddress booking.toLocation.id req.toAddress
          whenJust req.mbRiderName $ QRB.updateRiderName booking.id
          _ <- QRide.create ride
          QDFS.updateStatus driver.id DDFS.RIDE_ASSIGNED {rideId = ride.id}
          _ <- QRideD.create rideDetails
          QBE.logRideConfirmedEvent booking.id
          QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id
          _ <- QDQ.setInactiveBySTId driverQuote.searchTryId
          _ <- QSRD.setInactiveBySTId driverQuote.searchTryId
          _ <- QDI.updateOnRide (cast driver.id) True
          DLoc.updateOnRideCache (cast driver.id)

          for_ driverSearchReqs $ \driverReq -> do
            let driverId = driverReq.driverId
            unless (driverId == driver.id) $ do
              DP.decrementTotalQuotesCount transporter.id (cast driverReq.driverId) driverReq.searchTryId
              DP.removeSearchReqIdFromMap transporter.id driverId driverReq.searchTryId
              _ <- QSRD.updateDriverResponse driverReq.id SReqD.Pulled
              driver_ <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
              Notify.notifyDriverClearedFare transporter.id driverId driverReq.searchTryId driverQuote.estimatedFare driver_.deviceToken

          uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
          Notify.notifyDriver transporter.id notificationType notificationTitle (message uBooking) driver.id driver.deviceToken

          pure
            DConfirmRes
              { booking = uBooking,
                ride = Just ride,
                riderDetails,
                riderMobileCountryCode = req.customerMobileCountryCode,
                riderPhoneNumber = req.customerPhoneNumber,
                riderName = req.mbRiderName,
                transporter,
                fromLocation = uBooking.fromLocation,
                toLocation = uBooking.toLocation,
                driverId = Just driver.id.getId,
                driverName = Just driver.firstName,
                vehicleVariant = req.vehicleVariant
              }
        Right _ -> throwError AccessDenied
    DRB.SpecialZoneBooking -> do
      case quote of
        Left _ -> throwError AccessDenied
        Right _ -> do
          otpCode <- generateOTPCode
          when isNewRider $ void $ QRD.create riderDetails
          _ <- QRB.updateRiderId booking.id riderDetails.id
          _ <- QRB.updateSpecialZoneOtpCode booking.id otpCode
          _ <- QBL.updateAddress booking.fromLocation.id req.fromAddress
          _ <- QBL.updateAddress booking.toLocation.id req.toAddress
          whenJust req.mbRiderName $ QRB.updateRiderName booking.id
          QBE.logRideConfirmedEvent booking.id
          uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)

          pure
            DConfirmRes
              { booking = uBooking,
                ride = Nothing,
                riderDetails,
                riderMobileCountryCode = req.customerMobileCountryCode,
                riderPhoneNumber = req.customerPhoneNumber,
                riderName = req.mbRiderName,
                transporter,
                fromLocation = uBooking.fromLocation,
                toLocation = uBooking.toLocation,
                driverId = Nothing,
                driverName = Nothing,
                vehicleVariant = req.vehicleVariant
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
    buildRide driverId booking ghrId customerPhoneNumber = do
      guid <- Id <$> generateGUID
      shortId <- generateShortId
      let otp = T.takeEnd 4 customerPhoneNumber
      now <- getCurrentTime
      trackingUrl <- buildTrackingUrl guid
      return
        DRide.Ride
          { id = guid,
            pickupDropOutsideOfThreshold = Nothing,
            bookingId = booking.id,
            shortId = shortId,
            merchantId = Just booking.providerId,
            status = DRide.NEW,
            driverId = cast driverId,
            otp = otp,
            trackingUrl = trackingUrl,
            fare = Nothing,
            traveledDistance = 0,
            chargeableDistance = Nothing,
            driverArrivalTime = Nothing,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            tripStartPos = Nothing,
            tripEndPos = Nothing,
            fareParametersId = Nothing,
            distanceCalculationFailed = Nothing,
            createdAt = now,
            updatedAt = now,
            driverDeviatedFromRoute = Just False,
            numberOfSnapToRoadCalls = Nothing,
            numberOfDeviation = Nothing,
            driverGoHomeRequestId = ghrId
          }

    buildTrackingUrl rideId = do
      bppUIUrl <- asks (.selfUIUrl)
      let rideid = T.unpack (getId rideId)
      return $
        bppUIUrl
          { --TODO: find a way to build it using existing types from Routes
            baseUrlPath = baseUrlPath bppUIUrl <> "/driver/location/" <> rideid
          }

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Text -> Text -> UTCTime -> m (DRD.RiderDetails, Bool)
getRiderDetails merchantId customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumberAndMerchant customerPhoneNumber merchantId >>= \case
    Nothing -> fmap (,True) . encrypt =<< buildRiderDetails
    Just a -> return (a, False)
  where
    buildRiderDetails = do
      id <- generateGUID
      return $
        DRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            merchantId,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now,
            referralCode = Nothing,
            referredByDriver = Nothing,
            referredAt = Nothing,
            hasTakenValidRide = False,
            hasTakenValidRideAt = Nothing
          }

buildRideDetails ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasPrettyLogger m r,
    EncFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRide.Ride ->
  DPerson.Person ->
  m SRD.RideDetails
buildRideDetails ride driver = do
  vehicle <-
    QVeh.findById ride.driverId
      >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  return
    SRD.RideDetails
      { id = ride.id,
        driverName = driver.firstName,
        driverNumber = driver.mobileNumber,
        driverCountryCode = driver.mobileCountryCode,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = Just vehicle.color,
        vehicleVariant = Just vehicle.variant,
        vehicleModel = Just vehicle.model,
        vehicleClass = Nothing
      }

cancelBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c
  ) =>
  DRB.Booking ->
  Maybe DPerson.Person ->
  DM.Merchant ->
  m ()
cancelBooking booking mbDriver transporter = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  let transporterId' = Just booking.providerId
  unless (transporterId' == Just transporter.id) $ throwError AccessDenied
  mbRide <- QRide.findActiveByRBId booking.id
  bookingCancellationReason <- case mbDriver of
    Nothing -> buildBookingCancellationReason booking.id Nothing mbRide transporterId'
    Just driver -> buildBookingCancellationReason booking.id (Just driver.id) mbRide transporterId'
  _ <- QRB.updateStatus booking.id DRB.CANCELLED
  QBCR.upsert bookingCancellationReason
  whenJust mbRide $ \ride -> do
    _ <- QRide.updateStatus ride.id DRide.CANCELLED
    driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    QDFS.updateStatus ride.driverId $ DMode.getDriverStatus driverInfo.mode driverInfo.active
  whenJust mbRide $ \ride -> do
    SRide.clearCache ride.driverId
    void $ DLoc.updateOnRide (cast ride.driverId) False booking.providerId
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter bookingCancellationReason.source
  whenJust mbRide $ \ride ->
    case mbDriver of
      Nothing -> throwError (PersonNotFound ride.driverId.getId)
      Just driver -> do
        fork "cancelRide - Notify driver" $ do
          Notify.notifyOnCancel transporter.id booking driver.id driver.deviceToken bookingCancellationReason.source
  where
    buildBookingCancellationReason bookingId driverId ride merchantId = do
      return $
        DBCR.BookingCancellationReason
          { driverId = driverId,
            bookingId,
            rideId = (.id) <$> ride,
            merchantId = merchantId,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing
          }

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    HasPrettyLogger m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c
  ) =>
  Subscriber.Subscriber ->
  Id DM.Merchant ->
  DConfirmReq ->
  UTCTime ->
  m (DM.Merchant, Either (DPerson.Person, DDQ.DriverQuote) DQSZ.QuoteSpecialZone)
validateRequest subscriber transporterId req now = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  transporter <-
    QM.findById transporterId'
      >>= fromMaybeM (MerchantNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapMerchantId = booking.bapId
  unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
  case booking.bookingType of
    DRB.NormalBooking -> do
      _ <- req.driverId & fromMaybeM (InvalidRequest "driverId Not Found for Normal Booking")
      driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
      unless (driverQuote.validTill > now || driverQuote.status == DDQ.Active) $ do
        cancelBooking booking (Just driver) transporter
        throwError $ QuoteExpired driverQuote.id.getId
      return (transporter, Left (driver, driverQuote))
    DRB.SpecialZoneBooking -> do
      quoteSpecialZone <- QQSpecialZone.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      unless (quoteSpecialZone.validTill > now) $ do
        cancelBooking booking Nothing transporter
        throwError $ QuoteExpired quoteSpecialZone.id.getId
      return (transporter, Right quoteSpecialZone)
