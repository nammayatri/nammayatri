{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Confirm where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.DriverReferral as DUR
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import Domain.Types
import Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as DTCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.OnUpdate as DOU
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQ
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTMT
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified SharedLogic.AddOn as SAddOn
import qualified SharedLogic.Allocator as Alloc
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.DriverPool.Types
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FareCalculator (mkFareParamsBreakups)
import SharedLogic.MerchantPaymentMethod
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.QuickRetry (withQuickRetry)
import SharedLogic.Ride
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverQuote as QDQ
import Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Storage.Queries.RiderDriverCorrelation as SQR
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import qualified Tools.Metrics as Metrics
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
    consentToShareMobileNumber :: Maybe Bool,
    enableFrequentLocationUpdates :: Bool,
    paymentId :: Maybe Text,
    enableOtpLessRide :: Bool,
    driverPreference :: Maybe [Text],
    customerDiscountAmount :: Maybe HighPrecMoney,
    customerLanguage :: Maybe Maps.Language,
    bookingDepositSecured :: Maybe HighPrecMoney,
    -- | A BAP can select more than one add-on on the same item -- empty when
    -- none was echoed, never a single Maybe.
    addOns :: [Spec.AddOn]
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
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    isAlreadyFav :: Maybe Bool,
    favCount :: Maybe Int
  }

data RideInfo = RideInfo
  { ride :: DRide.Ride,
    vehicle :: DVeh.Vehicle,
    driver :: DPerson.Person
  }

cancelOldRideIfBetterDriverSwap :: DM.Merchant -> DDQ.DriverQuote -> DRide.Ride -> DPerson.Person -> DVeh.Vehicle -> DRB.Booking -> Flow ()
cancelOldRideIfBetterDriverSwap merchant driverQuote newRide newDriver newVehicle newBooking = do
  searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
  when (searchTry.searchRepeatType == DST.BETTER_DRIVER_SEARCH) $
    case searchTry.standInForBookingId of
      Nothing -> logWarning $ "BETTER_DRIVER_SEARCH searchTry " <> searchTry.id.getId <> " has no standInForBookingId"
      Just oldBookingIdText -> do
        mbOldBooking <- QRB.findById (Id oldBookingIdText)
        case mbOldBooking of
          Nothing -> logWarning $ "Better-driver-search swap: old booking " <> oldBookingIdText <> " not found"
          Just oldBooking -> do
            mbOldRide <- QRide.findActiveByRBId oldBooking.id
            case mbOldRide of
              Nothing -> logWarning $ "Better-driver-search swap: no active ride found for old booking " <> oldBookingIdText
              Just oldRide -> do
                now <- getCurrentTime
                let bookingCReason =
                      SBCR.BookingCancellationReason
                        { bookingId = oldBooking.id,
                          rideId = Just oldRide.id,
                          merchantId = Just merchant.id,
                          source = SBCR.ByAllocator,
                          reasonCode = Just (DTCR.CancellationReasonCode "RIDER_FOUND_BETTER_MATCH"),
                          driverId = Just oldRide.driverId,
                          additionalInfo = Just "Rider found a better driver match",
                          ondcCancellationReasonId = Nothing,
                          driverCancellationLocation = Nothing,
                          driverDistToPickup = Nothing,
                          distanceUnit = oldBooking.distanceUnit,
                          merchantOperatingCityId = Just oldBooking.merchantOperatingCityId,
                          createdAt = Just now,
                          updatedAt = Just now
                        }
                QBCR.upsert bookingCReason
                QRide.updateStatus oldRide.id DRide.CANCELLED
                logInfo $ "Better-driver-search swap: cancelled old ride " <> oldRide.id.getId <> " on booking " <> oldBookingIdText <> " - driverQuote " <> driverQuote.id.getId <> " was the accepted replacement"
                -- Tell the BAP about the replacement. The BAP never ran a search for this
                -- stand-by booking, so it has no way to find out otherwise - see
                -- SharedLogic.CallBAPInternal.betterDriverSwapAssign on the BAP side.
                -- Forked: the swap on this (BPP) side is already committed; a failed
                -- callback is only logged, same tolerance OneShotAssign's callback fork has.
                fork "better-driver-swap assign callback to BAP" $ do
                  callbackResult <- withTryCatch "betterDriverSwapAssignCallback" $ do
                    payload <- buildBetterDriverSwapAssignPayload oldBooking newBooking newRide newDriver newVehicle driverQuote
                    appBackendBapInternal <- asks (.appBackendBapInternal)
                    void $ withQuickRetry $ CallBAPInternal.betterDriverSwapAssign appBackendBapInternal.apiKey appBackendBapInternal.url payload
                  case callbackResult of
                    Right _ -> logInfo $ "Better-driver-swap assign callback delivered for new booking " <> newBooking.id.getId
                    Left err -> logError $ "Better-driver-swap assign callback failed for new booking " <> newBooking.id.getId <> ": " <> show err

-- | Mirrors SharedLogic.OneShotAssign.buildOneShotAssignPayload: reuses the same
-- rideAssignedCommon builder the Beckn RIDE_ASSIGNED paths use, so driver image,
-- birthday, favourites, tier upgrade and vehicle-model refill behave identically.
buildBetterDriverSwapAssignPayload :: DRB.Booking -> DRB.Booking -> DRide.Ride -> DPerson.Person -> DVeh.Vehicle -> DDQ.DriverQuote -> Flow CallBAPInternal.BetterDriverSwapAssignReq
buildBetterDriverSwapAssignPayload oldBooking booking ride driver vehicle driverQuote = do
  buildReq <- BP.rideAssignedCommon booking ride driver vehicle
  rideAssignedReq <- case buildReq of
    DOU.RideAssignedBuildReq r -> pure r
    DOU.ScheduledRideAssignedBuildReq r -> pure r
    _ -> throwError $ InternalError "rideAssignedCommon returned an unexpected build request"
  let bookingDetails = rideAssignedReq.bookingDetails
  driverName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
  driverMobile <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  let fareBreakups = mkFareParamsBreakups True identity CallBAPInternal.OneShotFareBreakupItem booking.fareParams
  pure
    CallBAPInternal.BetterDriverSwapAssignReq
      { oldBppBookingId = oldBooking.id.getId,
        bppEstimateId = driverQuote.estimateId.getId,
        bppQuoteId = driverQuote.id.getId,
        bppBookingId = booking.id.getId,
        bppRideId = ride.id.getId,
        currency = booking.currency,
        estimatedFare = booking.estimatedFare,
        commission = booking.commission,
        paymentCharge = booking.paymentCharge,
        paymentChargeBearer = booking.paymentChargeBearer,
        fareBreakups = fareBreakups,
        quoteValidTill = driverQuote.validTill,
        otp = fromMaybe ride.otp ride.endOtp,
        trackingUrl = ride.trackingUrl,
        driverDetails =
          CallBAPInternal.OneShotDriverDetails
            { name = driverName,
              mobileCountryCode = driver.mobileCountryCode,
              mobileNumber = driverMobile,
              rating = SP.roundToOneDecimal <$> bookingDetails.driverStats.rating,
              registeredAt = Just driver.createdAt,
              image = rideAssignedReq.image,
              isDriverBirthDay = rideAssignedReq.isDriverBirthDay,
              accountId = Nothing
            },
        vehicleDetails =
          CallBAPInternal.OneShotVehicleDetails
            { number = bookingDetails.vehicle.registrationNo,
              color = Just bookingDetails.vehicle.color,
              model = Just bookingDetails.vehicle.model,
              variant = bookingDetails.vehicle.variant,
              serviceTierType = booking.vehicleServiceTier,
              serviceTierName = Just booking.vehicleServiceTierName,
              vehicleAge = rideAssignedReq.vehicleAge
            },
        distanceToPickup = Just driverQuote.distanceToPickup,
        durationToPickup = Just driverQuote.durationToPickup,
        isAlreadyFav = rideAssignedReq.isAlreadyFav,
        favCount = Just rideAssignedReq.favCount,
        isSafetyPlus = rideAssignedReq.isSafetyPlus,
        isFreeRide = rideAssignedReq.isFreeRide,
        specialLocationTag = booking.specialLocationTag,
        assignedServiceTierName = rideAssignedReq.assignedServiceTierName,
        billingCategory = booking.billingCategory
      }

handler :: DM.Merchant -> DConfirmReq -> ValidatedQuote -> Flow DConfirmResp
handler merchant req validatedQuote = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (booking.status == DRB.NEW) $ throwError (BookingInvalidStatus $ show booking.status)
  let mbMerchantOperatingCityId = Just booking.merchantOperatingCityId

  (storedRiderDetails, isNewRider) <- SRD.getRiderDetails booking.currency merchant.id mbMerchantOperatingCityId req.customerMobileCountryCode req.customerPhoneNumber booking.bapId req.nightSafetyCheck req.consentToShareMobileNumber
  unless isNewRider $ do
    QRD.updateNightSafetyChecks req.nightSafetyCheck storedRiderDetails.id
    whenJust req.consentToShareMobileNumber $ \consent -> QRD.updateConsentToShareMobileNumber (Just consent) storedRiderDetails.id
  let riderDetails = storedRiderDetails {DRD.consentToShareMobileNumber = maybe storedRiderDetails.consentToShareMobileNumber Just req.consentToShareMobileNumber}

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
      (ride, _, vehicle) <- initializeRide merchant driver uBooking Nothing (Just req.enableFrequentLocationUpdates) driverQuote.clientId (Just req.enableOtpLessRide) (mFleetOwnerId <&> (.fleetOwnerId) <&> Id) True False Nothing
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId (mkPrice (Just driverQuote.currency) driverQuote.estimatedFare) Nothing
      uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      cancelOldRideIfBetterDriverSwap merchant driverQuote ride driver vehicle uBooking2
      -- Booking confirmed: decrement demand at this pickup gate AND complete any
      -- Accepted pickup-zone request for this driver (supply -1). Idempotent with StartRide.
      fork "specialZoneCompletePickupZoneOnConfirm" $
        SpecialZoneDriverDemand.completePickupZoneRequestsForDriver driver.id uBooking2.id.getId uBooking2.pickupGateId (show $ DV.castServiceTierToVariant uBooking2.vehicleServiceTier)
      mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails Nothing

    handleRideOtpFlow isNewRider _ booking riderDetails = do
      otpCode <- generateUniqueOTPCode booking.merchantOperatingCityId.getId (0 :: Integer)
      QRB.updateSpecialZoneOtpCode booking.id otpCode
      updateBookingDetails isNewRider booking riderDetails
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      -- Special-zone OTP: customer is committed at this gate (demand fulfilled).
      -- No driver assigned yet — supply is decremented later when a driver enters the
      -- OTP and StartRide fires. SETNX-idempotent on bookingId, so safe vs StartRide.
      fork "specialZoneDemandDecrementOnOtpConfirm" $
        SpecialZoneDriverDemand.runDemandDecrementForBooking uBooking.id.getId uBooking.pickupGateId (show $ DV.castServiceTierToVariant uBooking.vehicleServiceTier)
      mkDConfirmResp Nothing uBooking riderDetails Nothing

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
      (ride, _, vehicle) <- initializeRide merchant driver uBooking dynamicReferralCode (Just req.enableFrequentLocationUpdates) Nothing (Just req.enableOtpLessRide) (mFleetOwnerId <&> (.fleetOwnerId) <&> Id) False False Nothing
      uBooking2 <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      fork "specialZoneCompletePickupZoneOnMeterConfirm" $
        SpecialZoneDriverDemand.completePickupZoneRequestsForDriver driver.id uBooking2.id.getId uBooking2.pickupGateId (show $ DV.castServiceTierToVariant uBooking2.vehicleServiceTier)
      mkDConfirmResp (Just $ RideInfo {ride, driver, vehicle}) uBooking2 riderDetails Nothing

    generateUniqueOTPCode merchantOperatingCityId cnt = do
      when (cnt == 100) $ throwError (InternalError "Please try again in some time") -- Avoiding infinite loop (Todo: fix with something like LRU later)
      otpCode <- generateOTPCode
      isUnique <- checkAndStoreOTP merchantOperatingCityId otpCode
      if isUnique
        then return otpCode
        else generateUniqueOTPCode merchantOperatingCityId (cnt + 1)

    checkAndStoreOTP merchantOperatingCityId otpCode = do
      let otpKey = mkSpecialZoneOtpKey merchantOperatingCityId otpCode
      isPresent :: Maybe Bool <- Redis.runInMultiCloudRedisMaybeResult $ Redis.get otpKey
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
      tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge quote.id.getId [] False booking.fareParams.congestionCharge booking.fareParams.petCharges booking.fareParams.priorityCharges booking.commission booking.fareParams.tollCharges booking.fareParams.govtCharges booking.fareParams.driverCancellationNotAllowed booking.fareParams.bufferedFare
      paymentMethodInfo <- resolveBookingPaymentMethodInfo booking
      let driverSearchBatchInput =
            DriverSearchBatchInput
              { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                merchant,
                searchReq,
                tripQuoteDetails = [tripQuoteDetail],
                customerExtraFee = Nothing,
                negativeFareAdjustment = Nothing,
                messageId = booking.id.getId,
                billingCategory = booking.billingCategory,
                isRepeatSearch = False,
                isAllocatorBatch = False,
                paymentMethodInfo = paymentMethodInfo,
                riderDetails = Just riderDetails,
                emailDomain = booking.emailDomain,
                businessEmailDomain = booking.businessEmailDomain,
                driverPreference = req.driverPreference,
                addOnData = booking.addOnData,
                betterDriverSearchForBookingId = Nothing
              }
      searchTry <- initiateDriverSearchBatch driverSearchBatchInput
      QRB.updateSearchTryId booking.id searchTry.id
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      -- Listed on the driver board only once the rider has confirmed, not at init.
      when uBooking.isScheduled $ SBooking.addScheduledBookingInRedis uBooking
      -- Static offer Confirm: customer is committed at this gate (demand fulfilled).
      -- Driver gets matched later via the new search batch; supply tracking happens
      -- through that flow's StartRide. SETNX-idempotent on bookingId.
      fork "specialZoneDemandDecrementOnStaticConfirm" $
        SpecialZoneDriverDemand.runDemandDecrementForBooking uBooking.id.getId uBooking.pickupGateId (show $ DV.castServiceTierToVariant uBooking.vehicleServiceTier)
      mkDConfirmResp Nothing uBooking riderDetails paymentMethodInfo

    updateBookingDetails isNewRider booking riderDetails = do
      when isNewRider $ QRD.create riderDetails
      QRB.updateRiderIdAndConsentSnapshot booking.id riderDetails.id riderDetails.consentToShareMobileNumber
      QL.updateAddress booking.fromLocation.id req.fromAddress
      whenJust booking.toLocation $ \toLocation -> do
        whenJust req.toAddress $ \toAddress -> QL.updateAddress toLocation.id toAddress
      whenJust req.mbRiderName $ QRB.updateRiderName booking.id
      whenJust req.paymentId $ QRB.updatePaymentId booking.id
      whenJust req.customerDiscountAmount $ QRB.updateDiscountAmount booking.id
      whenJust req.customerLanguage $ QRB.updateCustomerLanguage booking.id
      when (req.bookingDepositSecured /= booking.bookingDeposit) $
        QRB.updateBookingDeposit req.bookingDepositSecured booking.id
      QBE.logRideConfirmedEvent booking.id booking.distanceUnit

    mkDConfirmResp mbRideInfo uBooking riderDetails mbPaymentMethodInfo = do
      cityLabel <- SML.getCityLabel uBooking.merchantOperatingCityId
      metricsDistanceBucketEdges <- SML.getDistanceBucketEdges uBooking.merchantOperatingCityId
      let (pickupZone, dropZone) = SML.specialZoneLabels uBooking.area
      Metrics.incrementBookingCreatedCount merchant.shortId.getShortId cityLabel (show uBooking.vehicleServiceTier) (SML.distanceBucketLabel metricsDistanceBucketEdges uBooking.estimatedDistance) pickupZone dropZone
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
      paymentMethodInfo <- case mbPaymentMethodInfo of
        Just info -> pure $ Just info
        Nothing -> resolveBookingPaymentMethodInfo uBooking
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
            paymentMethodInfo,
            isAlreadyFav = isFav,
            favCount = mDriverStats <&> (.favRiderCount)
          }

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    Metrics.HasBPPMetrics m r,
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
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasShortDurationRetryCfg r c,
    Redis.HedisLTSFlowEnv r,
    Finance.HasActorInfo m r,
    Alloc.SchedulerJobFlow r
  ) =>
  Subscriber.Subscriber ->
  Id DM.Merchant ->
  DConfirmReq ->
  UTCTime ->
  DTMT.TransporterConfig ->
  m (DM.Merchant, ValidatedQuote)
validateRequest subscriber transporterId req now transporterConfig = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  transporter <- QM.findById transporterId' >>= fromMaybeM (MerchantNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapMerchantId = booking.bapId
  unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  let isOndcScheduledRideSupportEnabled = fromMaybe False transporterConfig.enableOndcScheduledRideSupport
  -- OneWay OneWayOnDemandStaticOffer is the only category the pilot newly allows for non-value-add (external) BAPs -- everything else they were never validated for must stay blocked, even in a pilot-enabled city.
  -- This allows the two pre-existing dynamic-offer categories always, and OneWay OneWayOnDemandStaticOffer only when the city has the pilot enabled.
  let isAllowedForNonValueAddNP = case booking.tripCategory of
        OneWay OneWayOnDemandDynamicOffer -> True
        CrossCity OneWayOnDemandDynamicOffer _ -> True
        OneWay OneWayOnDemandStaticOffer -> isOndcScheduledRideSupportEnabled
        _ -> False
  when isOndcScheduledRideSupportEnabled $
    SAddOn.verifyAddOnEcho booking.addOnData booking.merchantOperatingCityId (Just booking.vehicleServiceTier) req.addOns
  when (not isValueAddNP && not isAllowedForNonValueAddNP) $
    throwError (InvalidRequest $ "Unserviceable trip category:-" <> show booking.tripCategory)
  case booking.tripCategory of
    OneWay OneWayOnDemandDynamicOffer -> getDriverQuoteDetails booking transporter
    OneWay OneWayRideOtp -> getRideOtpQuoteDetails booking transporter
    Rental RideOtp -> getRideOtpQuoteDetails booking transporter
    IntercityRental RideOtp _ -> getRideOtpQuoteDetails booking transporter
    RideShare RideOtp -> getRideOtpQuoteDetails booking transporter
    OneWay OneWayOnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    Rental OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
    IntercityRental OnDemandStaticOffer _ -> getStaticQuoteDetails booking transporter
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
    -- FIX: this case previously had no EasyBooking branch (only a catch-all), so it silently
    -- fell through to "UNSUPPORTED TYPE CATEGORY" at confirm time — same generic, static-quote
    -- handling as Rental's static-offer branch above (EasyBooking is QuoteBased just like it).
    -- RideOtp mode deliberately not handled yet (never produced at dispatch, see Search.hs).
    EasyBooking OnDemandStaticOffer -> getStaticQuoteDetails booking transporter
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
      searchReq <- QSRLite.findByIdLite quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
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
