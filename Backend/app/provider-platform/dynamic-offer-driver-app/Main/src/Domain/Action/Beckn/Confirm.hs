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
import qualified Domain.Action.UI.DriverReferral as DUR
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import Domain.Types
import Domain.Types.Booking as DRB
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQ
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
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
import SharedLogic.MerchantPaymentMethod
import SharedLogic.Ride
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as QMPM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverQuote as QDQ
import Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RiderDetails as QRD
import Storage.Queries.RiderDriverCorrelation as SQR
import qualified Storage.Queries.SearchRequest as QSR
import TransactionLogs.Types

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    vehicleVariant :: DV.VehicleVariant,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DL.LocationAddress,
    toAddress :: Maybe DL.LocationAddress,
    mbRiderName :: Maybe Text,
    nightSafetyCheck :: Bool,
    enableFrequentLocationUpdates :: Bool,
    paymentId :: Maybe Text,
    enableOtpLessRide :: Bool
  }

data ValidatedQuote = DriverQuote DPerson.Person DDQ.DriverQuote | StaticQuote DQ.Quote | RideOtpQuote DQ.Quote | MeterRideQuote DPerson.Person DQ.Quote

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
    vehicleVariant :: DV.VehicleVariant,
    quoteType :: ValidatedQuote,
    cancellationFee :: Maybe PriceAPIEntity,
    paymentId :: Maybe Text,
    isAlreadyFav :: Maybe Bool,
    favCount :: Maybe Int
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
  let mbMerchantOperatingCityId = Just booking.merchantOperatingCityId

  (riderDetails, isNewRider) <- SRD.getRiderDetails booking.currency merchant.id mbMerchantOperatingCityId req.customerMobileCountryCode req.customerPhoneNumber booking.bapId req.nightSafetyCheck
  unless isNewRider $ QRD.updateNightSafetyChecks req.nightSafetyCheck riderDetails.id

  case validatedQuote of
    DriverQuote driver driverQuote -> handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails
    StaticQuote quote -> handleStaticOfferFlow isNewRider quote booking riderDetails
    RideOtpQuote quote -> handleRideOtpFlow isNewRider quote booking riderDetails
    MeterRideQuote driver quote -> handleMeterRideFlow isNewRider driver quote booking riderDetails
  where
    handleDynamicOfferFlow isNewRider driver driverQuote booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mFleetOwnerId <- QFDA.findByDriverId driver.id True
      (ride, _, vehicle) <- initializeRide merchant driver uBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide) (mFleetOwnerId <&> (.fleetOwnerId) <&> Id)
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId (mkPrice (Just driverQuote.currency) driverQuote.estimatedFare) Nothing
      uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails

    handleRideOtpFlow isNewRider _ booking riderDetails = do
      otpCode <- generateUniqueOTPCode booking.merchantOperatingCityId.getId (0 :: Integer)
      QRB.updateSpecialZoneOtpCode booking.id otpCode
      updateBookingDetails isNewRider booking riderDetails
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mkDConfirmResp Nothing uBooking riderDetails

    handleMeterRideFlow isNewRider driver _ booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      driverReferral <- QDR.findById driver.id
      dynamicReferralCode <-
        case driverReferral of
          Nothing -> do
            res <- DUR.generateReferralCode (Just driver.role) (driver.id, driver.merchantId, booking.merchantOperatingCityId)
            pure res.dynamicReferralCode
          Just dr -> pure dr.dynamicReferralCode
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mFleetOwnerId <- QFDA.findByDriverId driver.id True
      (ride, _, vehicle) <- initializeRide merchant driver uBooking dynamicReferralCode (Just req.enableFrequentLocationUpdates) Nothing (Just req.enableOtpLessRide) (mFleetOwnerId <&> (.fleetOwnerId) <&> Id)
      uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails

    generateUniqueOTPCode merchantOperatingCityId cnt = do
      when (cnt == 100) $ throwError (InternalError "Please try again in some time") -- Avoiding infinite loop (Todo: fix with something like LRU later)
      otpCode <- generateOTPCode
      isUnique <- checkAndStoreOTP merchantOperatingCityId otpCode
      if isUnique
        then return otpCode
        else generateUniqueOTPCode merchantOperatingCityId (cnt + 1)

    checkAndStoreOTP merchantOperatingCityId otpCode = do
      let otpKey = mkSpecialZoneOtpKey merchantOperatingCityId otpCode
      isPresent :: Maybe Bool <- Redis.get otpKey
      case isPresent of
        Nothing -> do
          Redis.setExp otpKey True 3600
          return True
        Just _ -> return False

    handleStaticOfferFlow isNewRider quote booking riderDetails = do
      updateBookingDetails isNewRider booking riderDetails
      searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      let mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> quote.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> quote.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> quote.farePolicy
      tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge quote.id.getId [] False booking.fareParams.congestionCharge booking.fareParams.petCharges booking.fareParams.priorityCharges booking.commission booking.fareParams.tollCharges
      merchantPaymentMethod <- maybe (return Nothing) QMPM.findById booking.paymentMethodId
      let paymentMethodInfo = mkPaymentMethodInfo <$> merchantPaymentMethod
      let driverSearchBatchInput =
            DriverSearchBatchInput
              { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                merchant,
                searchReq,
                tripQuoteDetails = [tripQuoteDetail],
                customerExtraFee = Nothing,
                messageId = booking.id.getId,
                billingCategory = booking.billingCategory,
                isRepeatSearch = False,
                isAllocatorBatch = False,
                paymentMethodInfo = paymentMethodInfo,
                emailDomain = booking.emailDomain
              }
      initiateDriverSearchBatch driverSearchBatchInput
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      mkDConfirmResp Nothing uBooking riderDetails

    updateBookingDetails isNewRider booking riderDetails = do
      when isNewRider $ QRD.create riderDetails
      QRB.updateRiderId booking.id riderDetails.id
      QL.updateAddress booking.fromLocation.id req.fromAddress
      whenJust booking.toLocation $ \toLocation -> do
        whenJust req.toAddress $ \toAddress -> QL.updateAddress toLocation.id toAddress
      whenJust req.mbRiderName $ QRB.updateRiderName booking.id
      whenJust req.paymentId $ QRB.updatePaymentId booking.id
      QBE.logRideConfirmedEvent booking.id booking.distanceUnit

    mkDConfirmResp mbRideInfo uBooking riderDetails = do
      mDriverStats <-
        if isNothing mbRideInfo
          then pure Nothing
          else QDriverStats.findById (fromJust mbRideInfo).driver.id
      isFav <-
        if isNothing mbRideInfo
          then pure Nothing
          else do
            let rideInfo = fromJust mbRideInfo
            isAlreadyFav' <- SQR.checkRiderFavDriver (fromMaybe "" uBooking.riderId) rideInfo.driver.id True
            case isAlreadyFav' of
              Just _ -> pure $ Just True
              Nothing -> pure $ Just False
      pure $
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
            quoteType = validatedQuote,
            cancellationFee = Nothing,
            paymentId = req.paymentId,
            isAlreadyFav = isFav,
            favCount = mDriverStats <&> (.favRiderCount)
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
    HasLongDurationRetryCfg r c,
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c
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
  let isAllowedForNonValueAddNP = case booking.tripCategory of
        OneWay OneWayOnDemandDynamicOffer -> True
        CrossCity OneWayOnDemandDynamicOffer _ -> True
        _ -> False
  when (not isValueAddNP && not isAllowedForNonValueAddNP) $
    throwError (InvalidRequest $ "Unserviceable trip category:-" <> show booking.tripCategory)
  case booking.tripCategory of
    OneWay OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    OneWay OneWayRideOtp -> getRideOtpQuoteDetails booking transporter
    Rental RideOtp -> getRideOtpQuoteDetails booking transporter
    RideShare RideOtp -> getRideOtpQuoteDetails booking transporter
    OneWay OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    Rental OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    RideShare OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    InterCity OneWayOnDemandDynamicOffer _ -> getDriverQuoteDetails booking transporter
    InterCity OneWayRideOtp _ -> getRideOtpQuoteDetails booking transporter
    InterCity OneWayOnDemandStaticOffer _ -> getStaticQuoteDetails booking transporter
    CrossCity OneWayOnDemandDynamicOffer _ -> getDriverQuoteDetails booking transporter
    CrossCity OneWayRideOtp _ -> getRideOtpQuoteDetails booking transporter
    CrossCity OneWayOnDemandStaticOffer _ -> getStaticQuoteDetails booking transporter
    Ambulance OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    Ambulance OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    Ambulance OneWayRideOtp -> getRideOtpQuoteDetails booking transporter -- should create new mode?
    Delivery OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    Delivery OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    Delivery OneWayRideOtp -> getRideOtpQuoteDetails booking transporter
    OneWay MeterRide -> getMeterRideQuoteDetails booking transporter
    _ -> throwError . InvalidRequest $ "UNSUPPORTED TYPE CATEGORY" <> show booking.tripCategory
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

    getMeterRideQuoteDetails booking transporter = do
      quote <- getQuote booking transporter
      searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      driverIdForSearch <- searchReq.driverIdForSearch & fromMaybeM (InvalidRequest $ "Driver Id for search not found for meter ride searchId: " <> quote.searchRequestId.getId)
      driver <- QPerson.findById driverIdForSearch >>= fromMaybeM (PersonNotFound driverIdForSearch.getId)
      return (transporter, MeterRideQuote driver quote)

    getQuote booking transporter = do
      quote <- QQuote.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      unless (quote.validTill > now) $ do
        SBooking.cancelBooking booking Nothing transporter
        throwError $ QuoteExpired quote.id.getId
      return quote

mkSpecialZoneOtpKey :: Text -> Text -> Text
mkSpecialZoneOtpKey merchantOperatingCityId otpCode = "SpecialZoneBooking:MerchantOperatingCityId:" <> show merchantOperatingCityId <> "Otp:" <> show otpCode
