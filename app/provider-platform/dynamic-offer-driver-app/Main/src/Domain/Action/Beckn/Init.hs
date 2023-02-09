module Domain.Action.Beckn.Init where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.SearchRequest as QSR

data InitReq = InitReq
  { driverQuoteId :: Id DQuote.DriverQuote,
    bapId :: Text,
    bapUri :: BaseUrl
  }

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
  Esq.runTransaction $ do
    QBCR.upsert bookingCancellationReason
    QRB.updateStatus booking.id DRB.CANCELLED
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
            additionalInfo = Nothing
          }

handler :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> m InitRes
handler merchantId req = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  driverQuote <- QDQuote.findById req.driverQuoteId >>= fromMaybeM (QuoteNotFound req.driverQuoteId.getId)
  when (driverQuote.validTill < now) $
    throwError $ QuoteExpired driverQuote.id.getId
  searchRequest <- QSR.findById driverQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound driverQuote.searchRequestId.getId)
  -- do we need to check searchRequest.validTill?
  booking <- buildBooking searchRequest driverQuote now
  Esq.runTransaction $
    QRB.create booking
  pure InitRes {..}
  where
    buildBooking searchRequest driverQuote now = do
      id <- Id <$> generateGUID
      fromLocation <- buildBookingLocation searchRequest.fromLocation
      toLocation <- buildBookingLocation searchRequest.toLocation
      pure
        DRB.Booking
          { quoteId = req.driverQuoteId,
            status = DRB.NEW,
            providerId = merchantId,
            bapId = req.bapId,
            bapUri = req.bapUri,
            startTime = searchRequest.startTime,
            riderId = Nothing,
            vehicleVariant = driverQuote.vehicleVariant,
            estimatedDistance = driverQuote.distance,
            createdAt = now,
            updatedAt = now,
            fromLocation,
            toLocation,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            ..
          }
