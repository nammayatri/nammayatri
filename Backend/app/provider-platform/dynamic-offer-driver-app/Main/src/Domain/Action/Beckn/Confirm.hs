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
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import Domain.Types.Booking as DRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQ
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool.Types
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Ride
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import TransactionLogs.Types

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    vehicleVariant :: DVeh.Variant,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DL.LocationAddress,
    toAddress :: Maybe DL.LocationAddress,
    mbRiderName :: Maybe Text,
    nightSafetyCheck :: Bool,
    enableFrequentLocationUpdates :: Bool
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

  (riderDetails, isNewRider) <- SRD.getRiderDetails booking.currency merchant.id req.customerMobileCountryCode req.customerPhoneNumber now req.nightSafetyCheck
  unless isNewRider $ QRD.updateNightSafetyChecks riderDetails.id req.nightSafetyCheck

  case validatedQuote of
    DriverQuote driver driverQuote -> handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails
    StaticQuote quote -> handleStaticOfferFlow isNewRider quote booking riderDetails
    RideOtpQuote quote -> handleRideOtpFlow isNewRider quote booking riderDetails
  where
    handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      (ride, _, vehicle) <- initializeRide merchant.id driver uBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId $ mkPrice (Just driverQuote.currency) driverQuote.estimatedFare
      uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      return $ mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails

    handleRideOtpFlow isNewRider _ booking riderDetails = do
      otpCode <- generateOTPCode
      QRB.updateSpecialZoneOtpCode booking.id otpCode
      updateBookingDetails isNewRider booking riderDetails
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      return $ mkDConfirmResp Nothing uBooking riderDetails

    handleStaticOfferFlow isNewRider quote booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      let mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> quote.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> quote.farePolicy)
      tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge quote.id.getId
      let driverSearchBatchInput =
            DriverSearchBatchInput
              { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                merchant,
                searchReq,
                tripQuoteDetails = [tripQuoteDetail],
                customerExtraFee = Nothing,
                messageId = booking.id.getId,
                isRepeatSearch = False
              }
      initiateDriverSearchBatch driverSearchBatchInput
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      return $ mkDConfirmResp Nothing uBooking riderDetails

    updateBookingDetails isNewRider booking riderDetails = do
      when isNewRider $ QRD.create riderDetails
      QRB.updateRiderId booking.id riderDetails.id
      QL.updateAddress booking.fromLocation.id req.fromAddress
      whenJust booking.toLocation $ \toLocation -> do
        whenJust req.toAddress $ \toAddress -> QL.updateAddress toLocation.id toAddress
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

validateRequest ::
  ( KvDbFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasPrettyLogger m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
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
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  when (not isValueAddNP && booking.tripCategory /= DTC.OneWay DTC.OneWayOnDemandDynamicOffer) $
    throwError (InvalidRequest $ "Unserviceable trip category:-" <> show booking.tripCategory)
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
    DTC.InterCity DTC.OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    DTC.InterCity DTC.OneWayRideOtp -> getRideOtpQuoteDetails booking transporter
    DTC.InterCity DTC.OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    DTC.CrossCity DTC.OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    DTC.CrossCity DTC.OneWayRideOtp -> getRideOtpQuoteDetails booking transporter
    DTC.CrossCity DTC.OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
  where
    getDriverQuoteDetails booking transporter = do
      driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
      unless (driverQuote.validTill > now || driverQuote.status == DDQ.Active) $ do
        SBooking.cancelBooking booking (Just driver) transporter
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
        SBooking.cancelBooking booking Nothing transporter
        throwError $ QuoteExpired quote.id.getId
      return quote
