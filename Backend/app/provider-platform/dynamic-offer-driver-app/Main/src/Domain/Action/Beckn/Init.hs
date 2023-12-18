{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init where

import qualified Data.HashMap as HM
import qualified Domain.Action.Beckn.Search as BS
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.QuoteSpecialZone as DQSZ
import qualified Domain.Types.RideRoute as RI
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
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
import qualified SharedLogic.CallBAP as BP
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.QuoteSpecialZone as QSZoneQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as QSRSpecialZone
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event

data InitReq = InitReq
  { estimateId :: Text,
    driverId :: Maybe Text,
    vehicleVariant :: Veh.Variant,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    initTypeReq :: InitTypeReq,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }

data InitTypeReq = InitSpecialZoneReq | InitNormalReq

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    driverName :: Maybe Text,
    driverId :: Maybe Text
  }

cancelBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  DRB.Booking ->
  Id DM.Merchant ->
  m AckResponse
cancelBooking booking transporterId = do
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  bookingCancellationReason <- buildBookingCancellationReason
  transporter <- QM.findById transporterId >>= fromMaybeM (MerchantNotFound transporterId.getId)
  _ <- QBCR.upsert bookingCancellationReason
  _ <- QRB.updateStatus booking.id DRB.CANCELLED
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter bookingCancellationReason.source
  pure Ack
  where
    buildBookingCancellationReason = do
      return $
        DBCR.BookingCancellationReason
          { driverId = Nothing,
            bookingId = booking.id,
            merchantId = Just booking.providerId,
            rideId = Nothing,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing
          }

handler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r
  ) =>
  Id DM.Merchant ->
  InitReq ->
  Either (DDQ.DriverQuote, DSR.SearchRequest, DST.SearchTry) (DQSZ.QuoteSpecialZone, DSRSZ.SearchRequestSpecialZone) ->
  m InitRes
handler merchantId req eitherReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  (booking, driverName, driverId) <- case req.initTypeReq of
    InitNormalReq -> do
      case eitherReq of
        Left (driverQuote, searchRequest, searchTry) -> do
          (mbPaymentMethod, paymentUrl) <- fetchPaymentMethodAndUrl searchRequest.merchantOperatingCityId
          booking <- buildBooking searchRequest driverQuote driverQuote.id.getId searchTry.startTime DRB.NormalBooking now (mbPaymentMethod <&> (.id)) paymentUrl searchRequest.disabilityTag searchRequest.merchantOperatingCityId (Just driverQuote.distanceToPickup)
          triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = transporter.id}
          QST.updateStatus searchTry.id DST.COMPLETED
          _ <- QRB.createBooking booking
          return (booking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
        Right _ -> throwError $ InvalidRequest "Can't have specialZoneQuote in normal booking"
    InitSpecialZoneReq -> do
      case eitherReq of
        Right (specialZoneQuote, searchRequest) -> do
          (mbPaymentMethod, paymentUrl) <- fetchPaymentMethodAndUrl searchRequest.merchantOperatingCityId
          booking <- buildBooking searchRequest specialZoneQuote specialZoneQuote.id.getId searchRequest.startTime DRB.SpecialZoneBooking now (mbPaymentMethod <&> (.id)) paymentUrl Nothing searchRequest.merchantOperatingCityId Nothing
          _ <- QRB.createBooking booking
          -- moving route from search request id to booking id
          routeInfo :: Maybe RI.RouteInfo <- Redis.safeGet (BS.searchRequestKey $ getId searchRequest.id)
          case routeInfo of
            Just route -> Redis.setExp (BS.searchRequestKey $ getId booking.id) route 3600
            Nothing -> logDebug "Unable to get the key"

          return (booking, Nothing, Nothing)
        Left _ -> throwError $ InvalidRequest "Can't have driverQuote in specialZone booking"
  let paymentMethodInfo = req.paymentMethodInfo
  pure InitRes {..}
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        HasField "transactionId" sr Text,
        HasField "fromLocation" sr DLoc.Location,
        HasField "toLocation" sr DLoc.Location,
        HasField "estimatedDuration" sr Seconds,
        HasField "area" sr (Maybe FareProductD.Area),
        HasField "vehicleVariant" q Veh.Variant,
        HasField "distance" q Meters,
        HasField "estimatedFare" q Money,
        HasField "fareParams" q DFP.FareParameters,
        HasField "specialLocationTag" q (Maybe Text)
      ) =>
      sr ->
      q ->
      Text ->
      UTCTime ->
      DRB.BookingType ->
      UTCTime ->
      Maybe (Id DMPM.MerchantPaymentMethod) ->
      Maybe Text ->
      Maybe Text ->
      Id DMOC.MerchantOperatingCity ->
      Maybe Meters ->
      m DRB.Booking
    buildBooking searchRequest driverQuote quoteId startTime bookingType now mbPaymentMethodId paymentUrl disabilityTag merchantOpCityId distanceToPickup = do
      id <- Id <$> generateGUID
      let fromLocation = searchRequest.fromLocation
          toLocation = searchRequest.toLocation
      exophone <- findRandomExophone merchantOpCityId
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
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
            fromLocation,
            toLocation,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            specialLocationTag = driverQuote.specialLocationTag,
            specialZoneOtpCode = Nothing,
            disabilityTag = disabilityTag,
            area = searchRequest.area,
            paymentMethodId = mbPaymentMethodId,
            distanceToPickup = distanceToPickup,
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

validateRequest :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> m (Either (DDQ.DriverQuote, DSR.SearchRequest, DST.SearchTry) (DQSZ.QuoteSpecialZone, DSRSZ.SearchRequestSpecialZone))
validateRequest merchantId req = do
  _ <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  case req.initTypeReq of
    InitNormalReq -> do
      driverId <- req.driverId & fromMaybeM (InvalidRequest "driverId Not Found for Normal Booking")
      driverQuote <- QDQuote.findActiveQuoteByDriverIdAndVehVarAndEstimateId (Id req.estimateId) (Id driverId) req.vehicleVariant now >>= fromMaybeM (QuoteNotFound req.estimateId)
      when (driverQuote.validTill < now || driverQuote.status == DDQ.Inactive) $
        throwError $ QuoteExpired driverQuote.id.getId
      searchRequest <- QSR.findById driverQuote.requestId >>= fromMaybeM (SearchRequestNotFound driverQuote.requestId.getId)
      searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
      return $ Left (driverQuote, searchRequest, searchTry)
    InitSpecialZoneReq -> do
      specialZoneQuote <- QSZoneQuote.findById (Id req.estimateId) >>= fromMaybeM (QuoteNotFound req.estimateId)
      when (specialZoneQuote.validTill < now) $
        throwError $ QuoteExpired specialZoneQuote.id.getId
      searchRequest <- QSRSpecialZone.findById specialZoneQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound specialZoneQuote.searchRequestId.getId)
      return $ Right (specialZoneQuote, searchRequest)

compareMerchantPaymentMethod :: DMPM.PaymentMethodInfo -> DMPM.MerchantPaymentMethod -> Bool
compareMerchantPaymentMethod providerPaymentMethod DMPM.MerchantPaymentMethod {..} =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
