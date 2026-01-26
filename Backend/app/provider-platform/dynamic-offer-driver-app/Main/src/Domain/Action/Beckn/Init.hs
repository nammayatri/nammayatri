{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init where

import qualified Domain.Action.UI.DemandHotspots as DemandHotspots
import Domain.Types
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.ConditionalCharges as DTCC
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.DeliveryPersonDetails as DTDPD
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Quote as DQ
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Trip as DTrip
import qualified Domain.Types.VehicleVariant as Veh
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis.Queries as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Booking
import SharedLogic.Cancel
import qualified SharedLogic.FareCalculatorV2 as FCV2
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.RiderDetails as SRD
import qualified SharedLogic.Type as SLT
import qualified Storage.Cac.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import Utils.Common.CacUtils

data FulfillmentId = QuoteId (Id DQ.Quote) | DriverQuoteId (Id DDQ.DriverQuote)

data InitReq = InitReq
  { fulfillmentId :: FulfillmentId,
    vehicleVariant :: Veh.VehicleVariant,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    bppSubscriberId :: Maybe Text,
    riderPhoneNumber :: Text,
    mbRiderName :: Maybe Text,
    estimateId :: Text,
    initReqDetails :: Maybe InitReqDetails,
    isAdvanceBookingEnabled :: Maybe Bool,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    paymentMode :: Maybe DMPM.PaymentMode,
    displayBookingId :: Maybe Text
  }

data InitReqDetails = InitReqDeliveryDetails DTDD.DeliveryDetails

data ValidatedInitQuote = ValidatedQuote DQ.Quote | ValidatedEstimate DDQ.DriverQuote DST.SearchTry

data ValidatedInitReq = ValidatedInitReq
  { searchRequest :: DSR.SearchRequest,
    quote :: ValidatedInitQuote
  }

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    driverName :: Maybe Text,
    driverId :: Maybe Text,
    bppSubscriberId :: Maybe Text,
    riderPhoneNumber :: Text,
    riderName :: Maybe Text,
    vehicleVariant :: Veh.VehicleVariant,
    paymentId :: Text,
    cancellationFee :: Maybe PriceAPIEntity,
    estimateId :: Text
  }

handler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  Id DM.Merchant ->
  InitReq ->
  ValidatedInitReq ->
  m InitRes
handler merchantId req validatedReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  paymentId <- generateGUID
  let searchRequest = validatedReq.searchRequest
      riderName = req.mbRiderName
      riderPhoneNumber = req.riderPhoneNumber
  whenJust req.isAdvanceBookingEnabled $ \isAdvanceBookingEnabled' -> do
    QSR.updateIsAdvancedBookingEnabled isAdvanceBookingEnabled' searchRequest.id
  (mbPaymentMethod, paymentUrl) <- fetchPaymentMethodAndUrl searchRequest.merchantOperatingCityId
  (booking, driverName, driverId) <-
    case validatedReq.quote of
      ValidatedEstimate driverQuote searchTry -> do
        booking <- buildBooking searchRequest driverQuote searchTry.billingCategory driverQuote.id.getId driverQuote.tripCategory now mbPaymentMethod paymentUrl (Just driverQuote.distanceToPickup) req.initReqDetails searchRequest.configInExperimentVersions driverQuote.coinsRewardedOnGoldTierRide
        triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = transporter.id}
        QRB.createBooking booking
        QST.updateStatus DST.COMPLETED (searchTry.id)
        return (booking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
      ValidatedQuote quote -> do
        booking <- buildBooking searchRequest quote SLT.PERSONAL quote.id.getId quote.tripCategory now mbPaymentMethod paymentUrl Nothing req.initReqDetails searchRequest.configInExperimentVersions Nothing -------------TO DO --------RITIKA
        QRB.createBooking booking
        when booking.isScheduled $ void $ addScheduledBookingInRedis booking
        return (booking, Nothing, Nothing)
  fork "Updating Demand Hotspots on booking" $ do
    let lat = searchRequest.fromLocation.lat
        lon = searchRequest.fromLocation.lon
        merchantOpCityId = searchRequest.merchantOperatingCityId
    transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    DemandHotspots.updateDemandHotspotsOnBooking searchRequest.id merchantOpCityId transporterConfig (Maps.LatLong lat lon)
  let paymentMethodInfo = req.paymentMethodInfo
      bppSubscriberId = req.bppSubscriberId
      estimateId = req.estimateId
      cancellationFee = Nothing
  pure InitRes {vehicleVariant = req.vehicleVariant, ..}
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EncFlow m r,
        HasField "vehicleServiceTier" q ServiceTierType,
        HasField "distance" q (Maybe Meters),
        HasField "estimatedFare" q HighPrecMoney,
        HasField "currency" q Currency,
        HasField "fareParams" q DFP.FareParameters,
        HasField "specialLocationTag" q (Maybe Text)
      ) =>
      DSR.SearchRequest ->
      q ->
      SLT.BillingCategory ->
      Text ->
      TripCategory ->
      UTCTime ->
      Maybe DMPM.MerchantPaymentMethod ->
      Maybe Text ->
      Maybe Meters ->
      Maybe InitReqDetails ->
      [LYT.ConfigVersionMap] ->
      Maybe Int ->
      m DRB.Booking
    buildBooking searchRequest driverQuote billingCategory quoteId tripCategory now mbPaymentMethod paymentUrl distanceToPickup initReqDetails configInExperimentVersions coinsRewardedOnGoldTierRide = do
      id <- Id <$> generateGUID
      let fromLocation = searchRequest.fromLocation
          toLocation = searchRequest.toLocation
          stops = searchRequest.stops
          isTollApplicable = isTollApplicableForTrip driverQuote.vehicleServiceTier tripCategory
      exophone <- findRandomExophone searchRequest.merchantOperatingCityId searchRequest DExophone.CALL_RIDE
      vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow driverQuote.vehicleServiceTier searchRequest.merchantOperatingCityId configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show driverQuote.vehicleServiceTier))
      mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback quoteId
      commission <- FCV2.calculateCommission driverQuote.fareParams mbFarePolicy
      let bapUri = showBaseUrl req.bapUri
          displayBookingId = req.displayBookingId
      (initiatedAs, senderDetails, receiverDetails) <- do
        case tripCategory of
          Delivery _ -> do
            initReqDetail' <- fromMaybeM (InternalError "Delivery details not found") initReqDetails
            case initReqDetail' of
              InitReqDeliveryDetails deliveryDetails -> makeBookingDeliveryDetails searchRequest deliveryDetails merchantId
          _ -> pure (Nothing, Nothing, Nothing)
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
            merchantOperatingCityId = searchRequest.merchantOperatingCityId,
            primaryExophone = exophone.primaryPhone,
            bapId = req.bapId,
            bapCity = Just req.bapCity,
            bapCountry = Just req.bapCountry,
            riderId = Nothing,
            estimatedCongestionCharge = driverQuote.fareParams.congestionCharge,
            vehicleServiceTier = driverQuote.vehicleServiceTier,
            vehicleServiceTierName = vehicleServiceTierItem.name,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierItem.seatingCapacity,
            vehicleServiceTierAirConditioned = vehicleServiceTierItem.airConditionedThreshold,
            isAirConditioned = vehicleServiceTierItem.isAirConditioned,
            estimatedDistance = driverQuote.distance,
            maxEstimatedDistance = req.maxEstimatedDistance,
            createdAt = now,
            updatedAt = now,
            isPetRide = isJust driverQuote.fareParams.petCharges,
            estimatedFare = driverQuote.estimatedFare,
            currency = driverQuote.currency,
            distanceUnit = searchRequest.distanceUnit,
            riderName = Nothing,
            billingCategory = billingCategory,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            specialLocationTag = driverQuote.specialLocationTag,
            specialZoneOtpCode = Nothing,
            disabilityTag = searchRequest.disabilityTag,
            area = searchRequest.area,
            isScheduled = searchRequest.isScheduled,
            paymentMethodId = mbPaymentMethod <&> (.id),
            paymentInstrument = mbPaymentMethod <&> (.paymentInstrument),
            distanceToPickup = distanceToPickup,
            stopLocationId = (.id) <$> toLocation,
            startTime = searchRequest.startTime,
            returnTime = searchRequest.returnTime,
            roundTrip = searchRequest.roundTrip,
            tollNames = if isTollApplicable then searchRequest.tollNames else Nothing,
            tollIds = if isTollApplicable then searchRequest.tollIds else Nothing,
            estimateId = Just $ Id req.estimateId,
            paymentId = Nothing,
            isDashboardRequest = searchRequest.isDashboardRequest,
            fromLocGeohash = searchRequest.fromLocGeohash,
            toLocGeohash = searchRequest.toLocGeohash,
            hasStops = searchRequest.hasStops,
            isReferredRide = searchRequest.driverIdForSearch $> True,
            dynamicPricingLogicVersion = searchRequest.dynamicPricingLogicVersion,
            parcelType = searchRequest.parcelType,
            parcelQuantity = searchRequest.parcelQuantity,
            isSafetyPlus = DTCC.SAFETY_PLUS_CHARGES `elem` map (.chargeCategory) driverQuote.fareParams.conditionalCharges,
            commission = commission,
            isInsured = fromMaybe False req.isInsured,
            insuredAmount = req.insuredAmount,
            exotelDeclinedCallStatusReceivingTime = Nothing,
            numberOfLuggages = searchRequest.numberOfLuggages,
            paymentMode = searchRequest.paymentMode,
            ..
          }
    makeBookingDeliveryDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DSR.SearchRequest -> DTDD.DeliveryDetails -> Id DM.Merchant -> m (Maybe TripParty, Maybe DTDPD.DeliveryPersonDetails, Maybe DTDPD.DeliveryPersonDetails)
    makeBookingDeliveryDetails searchReq deliveryDetails mId = do
      --update search req locations
      merchant <- QM.findById mId >>= fromMaybeM (MerchantNotFound mId.getId)
      let senderLocationId = searchReq.fromLocation.id
          mbMerchantOperatingCityId = Just searchReq.merchantOperatingCityId
      receiverLocationId <- (searchReq.toLocation <&> (.id)) & fromMaybeM (InternalError $ "To location not found for trip category delivery search request " <> show searchReq.id)
      QLoc.updateInstructionsAndExtrasById deliveryDetails.senderDetails.address.instructions deliveryDetails.senderDetails.address.extras senderLocationId
      QLoc.updateInstructionsAndExtrasById deliveryDetails.receiverDetails.address.instructions deliveryDetails.receiverDetails.address.extras receiverLocationId

      -- update Rider details
      (senderRiderDetails, isNewSender) <- SRD.getRiderDetails searchReq.currency mId mbMerchantOperatingCityId (fromMaybe "+91" merchant.mobileCountryCode) deliveryDetails.senderDetails.phoneNumber searchReq.bapId False
      (receiverRiderDetails, isNewReceiver) <- SRD.getRiderDetails searchReq.currency mId mbMerchantOperatingCityId (fromMaybe "+91" merchant.mobileCountryCode) deliveryDetails.receiverDetails.phoneNumber searchReq.bapId False
      when isNewSender $ QRD.create senderRiderDetails
      when isNewReceiver $ QRD.create receiverRiderDetails

      -- update trip category of search request
      QSR.updateTripCategory (Just (DTrip.Delivery DTrip.OneWayOnDemandDynamicOffer)) searchReq.id

      -- get the sender and receiver exophone number
      senderPrimaryExophone <- findRandomExophone searchReq.merchantOperatingCityId searchReq DExophone.CALL_DELIVERY_SENDER
      receiverPrimaryExophone <- findRandomExophone searchReq.merchantOperatingCityId searchReq DExophone.CALL_DELIVERY_RECEIVER

      let senderPersonDetails =
            DTDPD.DeliveryPersonDetails
              { DTDPD.name = deliveryDetails.senderDetails.name,
                DTDPD.primaryExophone = senderPrimaryExophone.primaryPhone,
                DTDPD.id = senderRiderDetails.id
              }
          receiverPersonDetails =
            DTDPD.DeliveryPersonDetails
              { DTDPD.name = deliveryDetails.receiverDetails.name,
                DTDPD.primaryExophone = receiverPrimaryExophone.primaryPhone,
                DTDPD.id = receiverRiderDetails.id
              }
      return (Just deliveryDetails.initiatedAs, Just senderPersonDetails, Just receiverPersonDetails)

    fetchPaymentMethodAndUrl merchantOpCityId = do
      mbPaymentMethod <- forM req.paymentMethodInfo $ \paymentMethodInfo -> do
        allPaymentMethods <-
          CQMPM.findAllByMerchantOpCityId merchantOpCityId
        let mbPaymentMethod = find (compareMerchantPaymentMethod paymentMethodInfo) allPaymentMethods
        mbPaymentMethod & fromMaybeM (InvalidRequest "Payment method not allowed")
      pure (mbPaymentMethod, Nothing) -- TODO : Remove paymentUrl from here altogether

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> DSR.SearchRequest -> DExophone.ExophoneType -> m DExophone.Exophone
findRandomExophone merchantOpCityId sr exoType = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id sr.transactionId))) >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  exophones <- CQExophone.findByMerchantOpCityIdServiceAndExophoneType merchantOpCityId merchantServiceUsageConfig.getExophone exoType
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantOpCityId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  InitReq ->
  m ValidatedInitReq
validateRequest merchantId req = do
  void $ QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  case req.fulfillmentId of
    DriverQuoteId driverQuoteId -> do
      driverQuote <- QDQuote.findById driverQuoteId >>= fromMaybeM (DriverQuoteNotFound driverQuoteId.getId)
      searchRequest <- QSR.findById driverQuote.requestId >>= fromMaybeM (SearchRequestNotFound driverQuote.requestId.getId)
      validatePaymentMode searchRequest
      callWithErrorHandling searchRequest.transactionId $ do
        -- Lock Description: This is a Lock held between Init and Cancel Search, if Cancel Search is OnGoing then the Driver Quote would be Marked Inactive post the lock release and Init will fail with `QuoteExpired`.
        -- Lock Release: If any errors or Post Init Beckn Action Handler is executed.
        isLockAcquired <- Redis.tryLockRedis (mkCancelSearchInitLockKey searchRequest.transactionId) 30
        searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
        updatedDriverQuote <- runInMasterDbAndRedis $ QDQuote.findById driverQuoteId >>= fromMaybeM (DriverQuoteNotFound driverQuoteId.getId)
        when (updatedDriverQuote.validTill < now || updatedDriverQuote.status == DDQ.Inactive || not isLockAcquired) $
          throwError $ QuoteExpired updatedDriverQuote.id.getId
        return $ ValidatedInitReq {searchRequest, quote = ValidatedEstimate updatedDriverQuote searchTry}
    QuoteId quoteId -> do
      quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
      when (quote.validTill < now) $
        throwError $ QuoteExpired quote.id.getId
      searchRequest <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      validatePaymentMode searchRequest
      return $ ValidatedInitReq {searchRequest, quote = ValidatedQuote quote}
  where
    callWithErrorHandling transactionId action = do
      exep <- withTryCatch "init:validateRequest:callWithErrorHandling" action
      case exep of
        Left e -> do
          Redis.unlockRedis (mkCancelSearchInitLockKey transactionId)
          someExceptionToAPIErrorThrow e
        Right a -> pure a
    someExceptionToAPIErrorThrow exc
      | Just (HTTPException err) <- fromException exc = throwError err
      | Just (BaseException err) <- fromException exc =
        throwError . InternalError . fromMaybe (show err) $ toMessage err
      | otherwise = throwError . InternalError $ show exc
    validatePaymentMode searchRequest = do
      let paymentMode = fromMaybe DMPM.LIVE req.paymentMode
          paymentMode' = fromMaybe DMPM.LIVE searchRequest.paymentMode
      unless (paymentMode == paymentMode') $ throwError (InvalidRequest "Wrong payment mode")

compareMerchantPaymentMethod :: DMPM.PaymentMethodInfo -> DMPM.MerchantPaymentMethod -> Bool
compareMerchantPaymentMethod providerPaymentMethod DMPM.MerchantPaymentMethod {..} =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
