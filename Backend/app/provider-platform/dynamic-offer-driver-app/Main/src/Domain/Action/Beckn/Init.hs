{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init where

import Data.Set (Set)
import Data.Time.Calendar (DayOfWeek)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (timeToTimeOfDay)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RecurringBooking as DRecurringBooking
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant (Variant)
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.GoogleMaps as Maps
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.FarePolicy as QFarePolicy
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.QuoteSpecialZone as QSZoneQuote
import qualified Storage.Queries.RecurringBooking as QRecurringBooking
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as QSRSpecialZone
import Tools.Error (FarePolicyError (NoFarePolicy))
import qualified Tools.Maps as Maps

data InitReq
  = InitOneWayTripReq InitOneWayTrip
  | InitRecurringBookingReq InitRecurringBooking

data InitOneWayTrip = InitOneWayTrip
  { driverQuoteId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    initTypeReq :: InitTypeReq
  }

data InitTypeReq = InitSpecialZoneReq | InitNormalReq

data InitRecurringBooking = InitRecurringBooking
  { bapId :: Text,
    bapUri :: BaseUrl,
    variant :: Variant,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    pickupSchedule :: Set DayOfWeek,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant
  }

data InitRecurringBookingRes = InitRecurringBookingRes
  { booking :: DRecurringBooking.RecurringBooking,
    transporter :: DM.Merchant,
    farePolicy :: DFarePolicy.FarePolicy
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

initOneWayTrip :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitOneWayTrip -> m InitRes
initOneWayTrip merchantId req = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  case req.initTypeReq of
    InitNormalReq -> do
      driverQuote <- QDQuote.findById (Id req.driverQuoteId) >>= fromMaybeM (QuoteNotFound req.driverQuoteId)
      when (driverQuote.validTill < now) $
        throwError $ QuoteExpired driverQuote.id.getId
      searchRequest <- QSR.findById driverQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound driverQuote.searchRequestId.getId)
      -- do we need to check searchRequest.validTill?
      booking <- buildBooking searchRequest driverQuote DRB.NormalBooking now
      Esq.runTransaction $
        QRB.create booking
      pure InitRes {..}
    InitSpecialZoneReq -> do
      specialZoneQuote <- QSZoneQuote.findById (Id req.driverQuoteId) >>= fromMaybeM (QuoteNotFound req.driverQuoteId)
      when (specialZoneQuote.validTill < now) $
        throwError $ QuoteExpired specialZoneQuote.id.getId
      searchRequest <- QSRSpecialZone.findById specialZoneQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound specialZoneQuote.searchRequestId.getId)
      booking <- buildBooking searchRequest specialZoneQuote DRB.SpecialZoneBooking now
      Esq.runTransaction $
        QRB.create booking
      pure InitRes {..}
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        HasField "transactionId" sr Text,
        HasField "fromLocation" sr DLoc.SearchReqLocation,
        HasField "toLocation" sr DLoc.SearchReqLocation,
        HasField "startTime" sr UTCTime,
        HasField "estimatedDuration" sr Seconds,
        HasField "vehicleVariant" q Veh.Variant,
        HasField "distance" q Meters,
        HasField "estimatedFare" q Money,
        HasField "fareParams" q DFP.FareParameters
      ) =>
      sr ->
      q ->
      DRB.BookingType ->
      UTCTime ->
      m DRB.Booking
    buildBooking searchRequest driverQuote bookingType now = do
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
            startTime = searchRequest.startTime,
            vehicleVariant = driverQuote.vehicleVariant,
            estimatedDistance = driverQuote.distance,
            createdAt = now,
            updatedAt = now,
            fromLocation,
            toLocation,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            riderId = Nothing,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            specialZoneOtpCode = Nothing,
            ..
          }

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  CQExophone.findRandomExophone merchantId
    >>= fromMaybeM (ExophoneNotFound merchantId.getId)

initRecurringBooking ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  Id DM.Merchant ->
  InitRecurringBooking ->
  m InitRecurringBookingRes
initRecurringBooking merchantId req = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  farePolicy <- QFarePolicy.findByMerchantIdAndVariant merchantId req.variant Nothing >>= fromMaybeM NoFarePolicy
  recurringBooking <- buildRecurringBooking farePolicy
  runTransaction $ QRecurringBooking.create recurringBooking
  pure $
    InitRecurringBookingRes
      { booking = recurringBooking,
        transporter = transporter,
        farePolicy = farePolicy
      }
  where
    buildRecurringBooking farePolicy = do
      id <- Id <$> generateGUID
      sessiontoken <- generateGUIDText
      fromLocation <- buildRecurringBookingLocation merchantId sessiontoken req.pickupLocation
      toLocation <- buildRecurringBookingLocation merchantId sessiontoken req.dropLocation

      let startDate = utctDay req.pickupTime
          pickupTime = timeToTimeOfDay $ utctDayTime req.pickupTime
      pure $
        DRecurringBooking.RecurringBooking
          { scheduledDays = req.pickupSchedule,
            endDate = Nothing,
            status = DRecurringBooking.Active,
            providerId = merchantId.getId,
            bapId = req.bapId,
            bapUri = req.bapUri,
            farePolicyId = farePolicy.id,
            ..
          }

buildRecurringBookingLocation ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  Id DM.Merchant ->
  Text ->
  DLoc.SearchReqLocationAPIEntity ->
  m DLoc.BookingLocation
buildRecurringBookingLocation merchantId sessionToken DLoc.SearchReqLocationAPIEntity {lat, lon} = do
  pickupRes <-
    Maps.getPlaceName merchantId $
      Maps.GetPlaceNameReq
        { getBy = Maps.ByLatLong Maps.LatLong {..},
          sessionToken = Just sessionToken,
          language = Nothing
        }
  Maps.Address {..} <- Maps.mkLocation pickupRes
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now

  let address = DLoc.LocationAddress {..}
  pure $
    DLoc.BookingLocation
      { ..
      }
