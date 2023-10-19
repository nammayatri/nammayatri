{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init where

import qualified Domain.Action.Beckn.Search as BS
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
-- import qualified Domain.Types.Location as DLoc

import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.QuoteRental as DQR
import qualified Domain.Types.QuoteSpecialZone as DQSZ
import qualified Domain.Types.RideRoute as RI
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.SearchTry as DST
import Domain.Types.Vehicle.Variant
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
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.QuoteRental as QRQuote
import qualified Storage.Queries.QuoteSpecialZone as QSZoneQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as QSRSpecialZone
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event

-- import qualified Storage.Beam.SearchRequest as searchRequestForSpecialZone

data ValidateInitResponse
  = DRIVER_QUOTE (DDQ.DriverQuote, DSR.SearchRequest, DST.SearchTry)
  | SPECIAL_QUOTE (DQSZ.QuoteSpecialZone, DSRSZ.SearchRequestSpecialZone)
  | RENTAL_QUOTE (DQR.QuoteRental, DSR.SearchRequest)

data InitReq = InitReq
  { messageId :: Text,
    estimateId :: Text,
    driverId :: Maybe Text,
    vehicleVariant :: Veh.Variant,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    initTypeReq :: InitTypeReq,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    startTime :: UTCTime,
    rentalDuration :: Maybe Int
  }

data InitTypeReq = InitSpecialZoneReq | InitNormalReq | InitRentalReq deriving (Show, Eq)

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    driverName :: Maybe Text,
    driverId :: Maybe Text,
    startTime :: UTCTime
  }

cancelBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c
  ) =>
  DRB.Booking ->
  Id DM.Merchant ->
  m AckResponse
cancelBooking booking transporterId = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
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
    EventStreamFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DM.Merchant ->
  InitReq ->
  ValidateInitResponse ->
  m InitRes
handler merchantId req initReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime

  mbPaymentMethod <- forM req.paymentMethodInfo $ \paymentMethodInfo -> do
    allPaymentMethods <-
      CQMPM.findAllByMerchantId merchantId
    let mbPaymentMethod = find (compareMerchantPaymentMethod paymentMethodInfo) allPaymentMethods
    mbPaymentMethod & fromMaybeM (InvalidRequest "Payment method not allowed")
  let paymentUrl = DMPM.getPrepaidPaymentUrl =<< mbPaymentMethod
  (booking, driverName, driverId) <- case req.initTypeReq of
    InitNormalReq -> do
      case initReq of
        DRIVER_QUOTE (driverQuote, searchRequest, searchTry) -> do
          booking <- buildBooking searchRequest driverQuote driverQuote.id.getId searchTry.startTime DRB.NormalBooking now (mbPaymentMethod <&> (.id)) paymentUrl searchRequest.disabilityTag
          triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = transporter.id}
          QST.updateStatus searchTry.id DST.COMPLETED
          _ <- QRB.createBooking booking
          return (booking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
        _ -> throwError $ InvalidRequest "Can't have specialZoneQuote in normal booking"
    InitSpecialZoneReq -> do
      case initReq of
        SPECIAL_QUOTE (specialZoneQuote, searchRequest) -> do
          booking <- buildBookingSpecialzone specialZoneQuote searchRequest specialZoneQuote.id.getId searchRequest.startTime DRB.SpecialZoneBooking now (mbPaymentMethod <&> (.id)) paymentUrl Nothing
          _ <- QRB.createBooking booking
          -- moving route from search request id to booking id
          routeInfo :: Maybe RI.RouteInfo <- Redis.safeGet (BS.searchRequestKey $ getId searchRequest.id)
          case routeInfo of
            Just route -> Redis.setExp (BS.searchRequestKey $ getId booking.id) route 3600
            Nothing -> logDebug "Unable to get the key"

          return (booking, Nothing, Nothing)
        _ -> throwError $ InvalidRequest "Can't have driverQuote in specialZone booking"
    InitRentalReq -> do
      case initReq of
        RENTAL_QUOTE (rentalQuote, searchRequest) -> do
          (duration :: Int) <- case req.rentalDuration of
            Nothing -> throwError $ InvalidRequest "Request Invalid"
            Just duration' -> pure duration'

          farePolicy <- QFP.findById rentalQuote.farePolicyId >>= fromMaybeM NoFarePolicy
          -- let fullFarePolicy = FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant farePolicy
          let (sDistance, sDuration, sFare) = case farePolicy.farePolicyDetails of
                FarePolicyD.RentalDetails fPDetails -> (Meters $ duration * fPDetails.perHourFreeKms, Seconds duration, Money $ (getMoney fPDetails.perHourCharge) * duration)
                _ -> (0, 0, 0)
          _ <- QRQuote.updateBaseFields rentalQuote.id sDistance sDuration sFare
          updatedRentalQuotes <- QRQuote.findById (Id req.estimateId) >>= fromMaybeM (QuoteNotFound req.estimateId)

          --- TODO -- Update Rental Quote estimate fare and estimate duration
          --- make booking properly --with estimates and duration
          searchTry <- buildRentalSearchTry searchRequest.id req.startTime rentalQuote
          booking <- buildRentalBooking updatedRentalQuotes searchRequest req.startTime DRB.RentalBooking now (mbPaymentMethod <&> (.id)) paymentUrl searchRequest.disabilityTag
          _ <- QRB.createBooking booking
          QST.create searchTry
          return (booking, Nothing, Nothing)
        _ -> throwError $ InvalidRequest "Can't have driverQuote in specialZone booking"
  let paymentMethodInfo = req.paymentMethodInfo
  let startTime = req.startTime
  pure InitRes {..}
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        HasField "transactionId" sr Text,
        HasField "searchRequestDetails" sr DSR.SearchRequestDetails,
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
      m DRB.Booking
    buildBooking searchRequest driverQuote quoteId startTime bookingType now mbPaymentMethodId paymentUrl disabilityTag = do
      id <- Id <$> generateGUID

      exophone <- findRandomExophone merchantId
      let bookingDetails =
            case searchRequest.searchRequestDetails of
              DSR.SearchRequestDetailsOnDemand {..} ->
                DRB.BookingDetailsOnDemand
                  { specialZoneOtpCode = Nothing,
                    specialLocationTag,
                    toLocation
                  }
              DSR.SearchRequestDetailsRental {} ->
                DRB.BookingDetailsRental
                  { rentalToLocation = Nothing
                  }
      let fromLocation = case searchRequest.searchRequestDetails of
            DSR.SearchRequestDetailsOnDemand {} -> fromLocation
            DSR.SearchRequestDetailsRental {..} -> rentalFromLocation
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
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
            bookingDetails,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = 0,
            fareParams = driverQuote.fareParams,
            disabilityTag = disabilityTag,
            area = searchRequest.area,
            paymentMethodId = mbPaymentMethodId,
            ..
          }
    buildBookingSpecialzone ::
      ( CacheFlow m r,
        EsqDBFlow m r
      ) =>
      DQSZ.QuoteSpecialZone ->
      DSRSZ.SearchRequestSpecialZone ->
      Text ->
      UTCTime ->
      DRB.BookingType ->
      UTCTime ->
      Maybe (Id DMPM.MerchantPaymentMethod) ->
      Maybe Text ->
      Maybe Text ->
      m DRB.Booking
    buildBookingSpecialzone specialZoneQuote searchRequestForSpecialZone quoteId startTime bookingType now mbPaymentMethodId paymentUrl disabilityTag = do
      id <- Id <$> generateGUID
      let bookingOnDemand =
            DRB.BookingDetailsOnDemand
              { specialZoneOtpCode = Nothing,
                specialLocationTag = Nothing,
                toLocation = searchRequestForSpecialZone.toLocation
              }
      exophone <- findRandomExophone merchantId
      pure
        DRB.Booking
          { transactionId = searchRequestForSpecialZone.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
            primaryExophone = exophone.primaryPhone,
            bapId = req.bapId,
            bapUri = req.bapUri,
            bapCity = Just req.bapCity,
            bapCountry = Just req.bapCountry,
            riderId = Nothing,
            vehicleVariant = specialZoneQuote.vehicleVariant,
            estimatedDistance = specialZoneQuote.distance,
            maxEstimatedDistance = req.maxEstimatedDistance,
            createdAt = now,
            fromLocation = searchRequestForSpecialZone.fromLocation,
            updatedAt = now,
            bookingDetails = bookingOnDemand,
            estimatedFare = specialZoneQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = 0,
            fareParams = specialZoneQuote.fareParams,
            disabilityTag = disabilityTag,
            area = searchRequestForSpecialZone.area,
            paymentMethodId = mbPaymentMethodId,
            ..
          }
    buildRentalBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r
      ) =>
      DQR.QuoteRental ->
      DSR.SearchRequest ->
      UTCTime ->
      DRB.BookingType ->
      UTCTime ->
      Maybe (Id DMPM.MerchantPaymentMethod) ->
      Maybe Text ->
      Maybe Text ->
      m DRB.Booking
    buildRentalBooking rentalQuote searchRequest startTime bookingType now mbPaymentMethodId paymentUrl disabilityTag = do
      id <- Id <$> generateGUID
      exophone <- findRandomExophone merchantId
      bookingDetails <- do
        case searchRequest.searchRequestDetails of
          DSR.SearchRequestDetailsOnDemand {} -> do
            throwError $ InvalidRequest "On Demand is not allowed here"
          DSR.SearchRequestDetailsRental {} ->
            return $
              DRB.BookingDetailsRental
                { rentalToLocation = Nothing
                }
      fromLocation <- case searchRequest.searchRequestDetails of
        DSR.SearchRequestDetailsOnDemand {} -> throwError $ InvalidRequest "On Demand is not allowed here"
        DSR.SearchRequestDetailsRental {..} -> return rentalFromLocation
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
            primaryExophone = exophone.primaryPhone,
            bapId = req.bapId,
            bapUri = req.bapUri,
            bapCity = Just req.bapCity,
            bapCountry = Just req.bapCountry,
            riderId = Nothing,
            vehicleVariant = SEDAN,
            estimatedDistance = rentalQuote.baseDistance,
            maxEstimatedDistance = req.maxEstimatedDistance,
            createdAt = now,
            updatedAt = now,
            bookingDetails,
            estimatedFare = rentalQuote.baseFare,
            riderName = Nothing,
            estimatedDuration = rentalQuote.baseDuration,
            fareParams = rentalQuote.fareParams,
            disabilityTag = disabilityTag,
            area = searchRequest.area,
            paymentMethodId = mbPaymentMethodId,
            quoteId = rentalQuote.id.getId,
            ..
          }
    buildRentalSearchTry ::
      ( MonadTime m,
        MonadGuid m,
        MonadReader r m,
        HasField "searchRequestExpirationSeconds" r NominalDiffTime
      ) =>
      Id DSR.SearchRequest ->
      UTCTime ->
      DQR.QuoteRental ->
      m DST.SearchTry
    buildRentalSearchTry searchReqId startTime rentalQuote = do
      now <- getCurrentTime
      id_ <- Id <$> generateGUID
      searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
      let validTill_ = searchRequestExpirationSeconds `addUTCTime` startTime
          customerExtraFee = Nothing
      pure
        DST.SearchTry
          { id = id_,
            requestId = searchReqId,
            tag = DSR.RENTAL,
            estimateId = Nothing,
            merchantId = Just merchantId,
            messageId = req.messageId,
            startTime,
            validTill = validTill_,
            vehicleVariant = rentalQuote.vehicleVariant,
            status = DST.ACTIVE,
            createdAt = now,
            updatedAt = now,
            searchRepeatType = DST.INITIAL,
            searchRepeatCounter = 0,
            baseFare = rentalQuote.baseFare,
            ..
          }

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  exophones <- CQExophone.findByMerchantServiceAndExophoneType merchantId merchantServiceUsageConfig.getExophone DExophone.CALL_RIDE
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

validateRequest :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> m ValidateInitResponse
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
      return $ DRIVER_QUOTE (driverQuote, searchRequest, searchTry)
    InitSpecialZoneReq -> do
      specialZoneQuote <- QSZoneQuote.findById (Id req.estimateId) >>= fromMaybeM (QuoteNotFound req.estimateId)
      when (specialZoneQuote.validTill < now) $
        throwError $ QuoteExpired specialZoneQuote.id.getId
      searchRequest <- QSRSpecialZone.findById specialZoneQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound specialZoneQuote.searchRequestId.getId)
      return $ SPECIAL_QUOTE (specialZoneQuote, searchRequest)
    InitRentalReq -> do
      rentalQuotes <- QRQuote.findById (Id req.estimateId) >>= fromMaybeM (QuoteNotFound req.estimateId)
      searchRequest <- QSR.findById (rentalQuotes.searchRequestId) >>= fromMaybeM (SearchRequestNotFound rentalQuotes.searchRequestId.getId)
      return $ RENTAL_QUOTE (rentalQuotes, searchRequest)

compareMerchantPaymentMethod :: DMPM.PaymentMethodInfo -> DMPM.MerchantPaymentMethod -> Bool
compareMerchantPaymentMethod providerPaymentMethod DMPM.MerchantPaymentMethod {..} =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
