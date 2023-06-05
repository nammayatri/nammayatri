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
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.QuoteSpecialZone as DQSZ
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.QuoteSpecialZone as QSZoneQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as QSRSpecialZone
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

data InitReq = InitReq
  { driverQuoteId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    initTypeReq :: InitTypeReq,
    maxEstimatedDistance :: Maybe HighPrecMeters
  }

data InitTypeReq = InitSpecialZoneReq | InitNormalReq

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant
  }

buildBookingLocation :: (MonadGuid m) => DLoc.SearchReqLocation -> m DLoc.BookingLocation
buildBookingLocation DLoc.SearchReqLocation {..} = do
  let address = DLoc.LocationAddress {..}
  guid <- generateGUIDText
  pure
    DLoc.BookingLocation
      { id = Id guid,
        ..
      }

cancelBooking ::
  ( EsqDBFlow m r,
    HedisFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasCacheConfig r
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
            rideId = Nothing,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing
          }

handler :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> Either (DDQ.DriverQuote, DSR.SearchRequest, DST.SearchTry) (DQSZ.QuoteSpecialZone, DSRSZ.SearchRequestSpecialZone) -> m InitRes
handler merchantId req eitherReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  case req.initTypeReq of
    InitNormalReq -> do
      case eitherReq of
        Left (driverQuote, searchRequest, searchTry) -> do
          booking <- buildBooking searchRequest driverQuote searchTry.startTime DRB.NormalBooking now
          -- Esq.runTransaction $
          _ <- QRB.create booking
          pure InitRes {..}
        Right _ -> throwError $ InvalidRequest "Can't have specialZoneQuote in normal booking"
    InitSpecialZoneReq -> do
      case eitherReq of
        Right (specialZoneQuote, searchRequest) -> do
          booking <- buildBooking searchRequest specialZoneQuote searchRequest.startTime DRB.SpecialZoneBooking now
          -- Esq.runTransaction $
          _ <- QRB.create booking
          pure InitRes {..}
        Left _ -> throwError $ InvalidRequest "Can't have driverQuote in specialZone booking"
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        HasField "transactionId" sr Text,
        HasField "fromLocation" sr DLoc.SearchReqLocation,
        HasField "toLocation" sr DLoc.SearchReqLocation,
        HasField "estimatedDuration" sr Seconds,
        HasField "vehicleVariant" q Veh.Variant,
        HasField "distance" q Meters,
        HasField "estimatedFare" q Money,
        HasField "fareParams" q DFP.FareParameters
      ) =>
      sr ->
      q ->
      UTCTime ->
      DRB.BookingType ->
      UTCTime ->
      m DRB.Booking
    buildBooking searchRequest driverQuote startTime bookingType now = do
      id <- Id <$> generateGUID
      fromLocation <- buildBookingLocation searchRequest.fromLocation
      toLocation <- buildBookingLocation searchRequest.toLocation
      exophone <- findRandomExophone merchantId
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            quoteId = req.driverQuoteId,
            status = DRB.NEW,
            providerId = merchantId,
            primaryExophone = exophone.primaryPhone,
            bapId = req.bapId,
            bapUri = req.bapUri,
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
            specialZoneOtpCode = Nothing,
            ..
          }

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  exophones <- CQExophone.findAllByMerchantId merchantId
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

validateRequest :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> m (Either (DDQ.DriverQuote, DSR.SearchRequest, DST.SearchTry) (DQSZ.QuoteSpecialZone, DSRSZ.SearchRequestSpecialZone))
validateRequest merchantId req = do
  _ <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  case req.initTypeReq of
    InitNormalReq -> do
      driverQuote <- QDQuote.findById (Id req.driverQuoteId) >>= fromMaybeM (QuoteNotFound req.driverQuoteId)
      when (driverQuote.validTill < now) $
        throwError $ QuoteExpired driverQuote.id.getId
      searchRequest <- QSR.findById driverQuote.requestId >>= fromMaybeM (SearchRequestNotFound driverQuote.requestId.getId)
      searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
      return $ Left (driverQuote, searchRequest, searchTry)
    InitSpecialZoneReq -> do
      specialZoneQuote <- QSZoneQuote.findById (Id req.driverQuoteId) >>= fromMaybeM (QuoteNotFound req.driverQuoteId)
      when (specialZoneQuote.validTill < now) $
        throwError $ QuoteExpired specialZoneQuote.id.getId
      searchRequest <- QSRSpecialZone.findById specialZoneQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound specialZoneQuote.searchRequestId.getId)
      return $ Right (specialZoneQuote, searchRequest)
