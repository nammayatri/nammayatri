{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init
  ( InitReq (..),
    InitTypeReq (..),
    InitRes (..),
    handler,
    cancelBooking,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
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
    merchant :: DM.Merchant
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

handler :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> (InitRes -> m ()) -> m ()
handler merchantId req callbackFunc = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case req.initTypeReq of
    InitNormalReq -> normalReqHandler merchant req callbackFunc
    InitSpecialZoneReq -> specLocReqHandler merchant req callbackFunc

normalReqHandler :: (CacheFlow m r, EsqDBFlow m r) => DM.Merchant -> InitReq -> (InitRes -> m ()) -> m ()
normalReqHandler merchant req callbackFunc = do
  now <- getCurrentTime
  driverQuote <- QDQuote.findById (Id req.driverQuoteId) >>= fromMaybeM (QuoteNotFound req.driverQuoteId)
  when (driverQuote.validTill < now) $
    throwError $ QuoteExpired driverQuote.id.getId
  searchRequest <- QSR.findById driverQuote.requestId >>= fromMaybeM (SearchRequestNotFound driverQuote.requestId.getId)
  searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)

  fork "normal init request processing" $ do
    booking <- buildBooking req merchant searchRequest driverQuote searchTry.startTime DRB.NormalBooking now
    Esq.runTransaction $
      QRB.create booking
    callbackFunc InitRes {..}

specLocReqHandler :: (CacheFlow m r, EsqDBFlow m r) => DM.Merchant -> InitReq -> (InitRes -> m ()) -> m ()
specLocReqHandler merchant req callbackFunc = do
  now <- getCurrentTime
  specialZoneQuote <- QSZoneQuote.findById (Id req.driverQuoteId) >>= fromMaybeM (QuoteNotFound req.driverQuoteId)
  when (specialZoneQuote.validTill < now) $
    throwError $ QuoteExpired specialZoneQuote.id.getId
  searchRequest <- QSRSpecialZone.findById specialZoneQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound specialZoneQuote.searchRequestId.getId)
  fork "spec loc init request processing" $ do
    booking <- buildBooking req merchant searchRequest specialZoneQuote searchRequest.startTime DRB.SpecialZoneBooking now
    Esq.runTransaction $
      QRB.create booking
    callbackFunc InitRes {..}

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  exophones <- CQExophone.findAllByMerchantId merchantId
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

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
  InitReq ->
  DM.Merchant ->
  sr ->
  q ->
  UTCTime ->
  DRB.BookingType ->
  UTCTime ->
  m DRB.Booking
buildBooking req merchant searchRequest driverQuote startTime bookingType now = do
  id <- Id <$> generateGUID
  fromLocation <- buildBookingLocation searchRequest.fromLocation
  toLocation <- buildBookingLocation searchRequest.toLocation
  exophone <- findRandomExophone merchant.id
  pure
    DRB.Booking
      { transactionId = searchRequest.transactionId,
        quoteId = req.driverQuoteId,
        status = DRB.NEW,
        providerId = merchant.id,
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
