{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Confirm where

import qualified Data.HashMap.Strict as HM
import Data.String.Conversions
import Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQ
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Ride
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import Storage.CachedQueries.Merchant as QM
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Notifications as Notify

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    vehicleVariant :: DVeh.Variant,
    driverId :: Maybe Text,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DL.LocationAddress,
    toAddress :: DL.LocationAddress,
    mbRiderName :: Maybe Text,
    nightSafetyCheck :: Bool
  }

data ValidatedQuote = DriverQuote DPerson.Person DDQ.DriverQuote | StaticQuote DQ.Quote | RideOtpQuote DQ.Quote

data DConfirmResp = DConfirmResp
  { booking :: DRB.Booking,
    rideInfo :: Maybe RideInfo,
    fromLocation :: DL.Location,
    toLocation :: Maybe DL.Location,
    riderDetails :: DRD.RiderDetails,
    riderMobileCountryCode :: Text,
    riderPhoneNumber :: Text,
    riderName :: Maybe Text,
    transporter :: DM.Merchant,
    vehicleVariant :: DVeh.Variant,
    quoteType :: ValidatedQuote
  }

data RideInfo = RideInfo
  { ride :: DRide.Ride,
    vehicle :: DVeh.Vehicle,
    driver :: DPerson.Person
  }

handler :: DM.Merchant -> DConfirmReq -> ValidatedQuote -> Flow DConfirmResp
handler merchant req validatedQuote = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (booking.status == DRB.NEW) $ throwError (BookingInvalidStatus $ show booking.status)
  now <- getCurrentTime

  (riderDetails, isNewRider) <- SRD.getRiderDetails merchant.id req.customerMobileCountryCode req.customerPhoneNumber now req.nightSafetyCheck
  unless isNewRider $ QRD.updateNightSafetyChecks riderDetails.id req.nightSafetyCheck

  case validatedQuote of
    DriverQuote driver driverQuote -> handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails
    StaticQuote quote -> handleStaticOfferFlow isNewRider quote booking riderDetails
    RideOtpQuote quote -> handleRideOtpFlow isNewRider quote booking riderDetails
  where
    handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      (ride, _, vehicle) <- initializeRide merchant.id driver booking Nothing driverQuote.requestId.getId
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId driverQuote.estimatedFare
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      return $ mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking riderDetails

    handleRideOtpFlow isNewRider _ booking riderDetails = do
      otpCode <- generateOTPCode
      QRB.updateSpecialZoneOtpCode booking.id otpCode
      updateBookingDetails isNewRider booking riderDetails
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      return $ mkDConfirmResp Nothing uBooking riderDetails

    handleStaticOfferFlow isNewRider quote booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      initiateDriverSearchBatch merchant searchReq booking.tripCategory req.vehicleVariant quote.id.getId Nothing booking.id.getId
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      return $ mkDConfirmResp Nothing uBooking riderDetails

    updateBookingDetails isNewRider booking riderDetails = do
      when isNewRider $ QRD.create riderDetails
      QRB.updateRiderId booking.id riderDetails.id
      QL.updateAddress booking.fromLocation.id req.fromAddress
      whenJust booking.toLocation $ \toLocation -> QL.updateAddress toLocation.id req.toAddress
      whenJust req.mbRiderName $ QRB.updateRiderName booking.id
      QBE.logRideConfirmedEvent booking.id

    mkDConfirmResp mbRideInfo uBooking riderDetails =
      DConfirmResp
        { booking = uBooking,
          rideInfo = mbRideInfo,
          riderDetails,
          riderMobileCountryCode = req.customerMobileCountryCode,
          riderPhoneNumber = req.customerPhoneNumber,
          riderName = req.mbRiderName,
          transporter = merchant,
          fromLocation = uBooking.fromLocation,
          toLocation = uBooking.toLocation,
          vehicleVariant = req.vehicleVariant,
          quoteType = validatedQuote
        }

cancelBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    LT.HasLocationService m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
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
    Just driver -> do
      QDI.updateOnRide driver.id False
      buildBookingCancellationReason booking.id (Just driver.id) mbRide transporterId'

  QRB.updateStatus booking.id DRB.CANCELLED
  QBCR.upsert bookingCancellationReason
  whenJust mbRide $ \ride -> do
    void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
    QRide.updateStatus ride.id DRide.CANCELLED
    QDI.updateOnRide (cast ride.driverId) False
    void $ LF.rideDetails ride.id SRide.CANCELLED transporter.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon

  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter bookingCancellationReason.source
  whenJust mbRide $ \ride ->
    case mbDriver of
      Nothing -> throwError (PersonNotFound ride.driverId.getId)
      Just driver -> do
        fork "cancelRide - Notify driver" $ do
          Notify.notifyOnCancel booking.merchantOperatingCityId booking driver.id driver.deviceToken bookingCancellationReason.source
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
    HasPrettyLogger m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasLongDurationRetryCfg r c,
    LT.HasLocationService m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Subscriber.Subscriber ->
  Id DM.Merchant ->
  DConfirmReq ->
  UTCTime ->
  m (DM.Merchant, ValidatedQuote)
validateRequest subscriber transporterId req now = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  transporter <- QM.findById transporterId' >>= fromMaybeM (MerchantNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapMerchantId = booking.bapId
  unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
  case booking.tripCategory of
    DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    DTC.OneWay DTC.OneWayRideOtp -> getRideOtpQuoteDetails booking transporter
    DTC.RoundTrip DTC.RideOtp -> getRideOtpQuoteDetails booking transporter
    DTC.Rental DTC.RideOtp -> getRideOtpQuoteDetails booking transporter
    DTC.RideShare DTC.RideOtp -> getRideOtpQuoteDetails booking transporter
    DTC.OneWay DTC.OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    DTC.RoundTrip DTC.OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    DTC.Rental DTC.OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    DTC.RideShare DTC.OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
  where
    getDriverQuoteDetails booking transporter = do
      driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
      unless (driverQuote.validTill > now || driverQuote.status == DDQ.Active) $ do
        cancelBooking booking (Just driver) transporter
        throwError $ QuoteExpired driverQuote.id.getId
      return (transporter, DriverQuote driver driverQuote)

    getRideOtpQuoteDetails booking transporter = do
      quote <- getQuote booking transporter
      return (transporter, RideOtpQuote quote)

    getStaticQuoteDetails booking transporter = do
      quote <- getQuote booking transporter
      return (transporter, StaticQuote quote)

    getQuote booking transporter = do
      quote <- QQuote.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      unless (quote.validTill > now) $ do
        cancelBooking booking Nothing transporter
        throwError $ QuoteExpired quote.id.getId
      return quote
