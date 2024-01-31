{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Quote as DQ
import qualified Domain.Types.RideRoute as RI
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.Ride as SR
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as SZSRFallback
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event

data FulfillmentId = QuoteId (Id DQ.Quote) | EstimateId (Id DE.Estimate)

data InitReq = InitReq
  { fulfillmentId :: FulfillmentId,
    driverId :: Maybe Text,
    vehicleVariant :: Veh.Variant,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }

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
    driverId :: Maybe Text
  }

handler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r
  ) =>
  Id DM.Merchant ->
  InitReq ->
  ValidatedInitReq ->
  m InitRes
handler merchantId req validatedReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  let searchRequest = validatedReq.searchRequest
  (mbPaymentMethod, paymentUrl) <- fetchPaymentMethodAndUrl searchRequest.merchantOperatingCityId
  (booking, driverName, driverId) <-
    case validatedReq.quote of
      ValidatedEstimate driverQuote searchTry -> do
        booking <- buildBooking searchRequest driverQuote driverQuote.id.getId driverQuote.tripCategory now (mbPaymentMethod <&> (.id)) paymentUrl (Just driverQuote.distanceToPickup)
        triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = transporter.id}
        QRB.createBooking booking

        QST.updateStatus searchTry.id DST.COMPLETED
        return (booking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
      ValidatedQuote quote -> do
        booking <- buildBooking searchRequest quote quote.id.getId quote.tripCategory now (mbPaymentMethod <&> (.id)) paymentUrl Nothing
        QRB.createBooking booking
        -- moving route from search request id to booking id
        routeInfo :: Maybe RI.RouteInfo <- Redis.safeGet (SR.searchRequestKey $ getId searchRequest.id)
        case routeInfo of
          Just route -> Redis.setExp (SR.searchRequestKey $ getId booking.id) route 3600
          Nothing -> logDebug "Unable to get the key"

        return (booking, Nothing, Nothing)

  let paymentMethodInfo = req.paymentMethodInfo
  pure InitRes {..}
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        HasField "vehicleVariant" q Veh.Variant,
        HasField "distance" q (Maybe Meters),
        HasField "estimatedFare" q Money,
        HasField "fareParams" q DFP.FareParameters,
        HasField "specialLocationTag" q (Maybe Text)
      ) =>
      DSR.SearchRequest ->
      q ->
      Text ->
      DTC.TripCategory ->
      UTCTime ->
      Maybe (Id DMPM.MerchantPaymentMethod) ->
      Maybe Text ->
      Maybe Meters ->
      m DRB.Booking
    buildBooking searchRequest driverQuote quoteId tripCategory now mbPaymentMethodId paymentUrl distanceToPickup = do
      id <- Id <$> generateGUID
      let fromLocation = searchRequest.fromLocation
          toLocation = searchRequest.toLocation
      exophone <- findRandomExophone searchRequest.merchantOperatingCityId
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
            merchantOperatingCityId = searchRequest.merchantOperatingCityId,
            primaryExophone = exophone.primaryPhone,
            bapId = req.bapId,
            bapUri = req.bapUri,
            bapCity = Just req.bapCity,
            bapCountry = Just req.bapCountry,
            riderId = Nothing,
            vehicleVariant = driverQuote.vehicleVariant,
            estimatedDistance = driverQuote.distance,
            maxEstimatedDistance = req.maxEstimatedDistance,
            createdAt = now,
            updatedAt = now,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            specialLocationTag = driverQuote.specialLocationTag,
            specialZoneOtpCode = Nothing,
            disabilityTag = searchRequest.disabilityTag,
            area = searchRequest.area,
            isScheduled = searchRequest.isScheduled,
            paymentMethodId = mbPaymentMethodId,
            distanceToPickup = distanceToPickup,
            stopLocationId = (.id) <$> toLocation,
            startTime = searchRequest.startTime,
            ..
          }

    fetchPaymentMethodAndUrl merchantOpCityId = do
      mbPaymentMethod <- forM req.paymentMethodInfo $ \paymentMethodInfo -> do
        allPaymentMethods <-
          CQMPM.findAllByMerchantOpCityId merchantOpCityId
        let mbPaymentMethod = find (compareMerchantPaymentMethod paymentMethodInfo) allPaymentMethods
        mbPaymentMethod & fromMaybeM (InvalidRequest "Payment method not allowed")
      let paymentUrl = DMPM.getPrepaidPaymentUrl =<< mbPaymentMethod
      pure (mbPaymentMethod, paymentUrl)

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DExophone.Exophone
findRandomExophone merchantOpCityId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  exophones <- CQExophone.findByMerchantOpCityIdServiceAndExophoneType merchantOpCityId merchantServiceUsageConfig.getExophone DExophone.CALL_RIDE
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
    EstimateId estimateId -> do
      driverId <- req.driverId & fromMaybeM (InvalidRequest "driverId Not Found for Normal Booking")
      driverQuote <- QDQuote.findActiveQuoteByDriverIdAndVehVarAndEstimateId estimateId (Id driverId) req.vehicleVariant now >>= fromMaybeM (EstimateNotFound estimateId.getId)
      when (driverQuote.validTill < now || driverQuote.status == DDQ.Inactive) $
        throwError $ QuoteExpired driverQuote.id.getId
      searchRequest <- QSR.findById driverQuote.requestId >>= fromMaybeM (SearchRequestNotFound driverQuote.requestId.getId)
      searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
      return $ ValidatedInitReq {searchRequest, quote = ValidatedEstimate driverQuote searchTry}
    QuoteId quoteId -> do
      quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
      when (quote.validTill < now) $
        throwError $ QuoteExpired quote.id.getId
      -- Replace with this after the release
      -- searchRequest <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
      -- This is just to handle backward compatibility
      searchRequest <- do
        mbSearchReq <- QSR.findById quote.searchRequestId
        case mbSearchReq of
          Just sr -> return sr
          Nothing -> SZSRFallback.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)

      return $ ValidatedInitReq {searchRequest, quote = ValidatedQuote quote}

compareMerchantPaymentMethod :: DMPM.PaymentMethodInfo -> DMPM.MerchantPaymentMethod -> Bool
compareMerchantPaymentMethod providerPaymentMethod DMPM.MerchantPaymentMethod {..} =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
