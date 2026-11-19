{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Rescheduling a scheduled ride to a new pickup time.
--
-- A reschedule is a cancel-and-rebook driven by the app through the normal
-- search -> select/confirm calls. The app marks the search with the booking it
-- reschedules ('DSearchReq.rescheduledFromBookingId'); from there:
--
--   * search      -> 'validateRescheduleSearch' (fail early, before showing a price)
--   * select      -> 'validateRescheduleAccept', then 'cancelRescheduleSourceAfterSelect'
--                    once the BPP took the select (dynamic offers: the old driver is
--                    released right away, the allocator runs for the new time)
--   * confirm     -> 'validateRescheduleAccept' (static quotes: rental / intercity)
--   * on_confirm  -> 'completeRescheduleOnConfirm': move the booking deposit onto the
--                    new booking, then cancel the old one
--
-- The old booking is cancelled with reason 'rescheduleCancellationReasonCode'. That
-- cancel carries no fee and does not count as a customer cancellation (see
-- 'isRescheduleCancellation' and the BPP's handling of the same reason code).
module SharedLogic.Reschedule
  ( rescheduleCancellationReasonCode,
    findActiveRescheduleSource,
    isRescheduleOf,
    validateRescheduleSearch,
    validateRescheduleAccept,
    hasDepositHoldToCarry,
    cancelRescheduleSourceAfterSelect,
    completeRescheduleOnConfirm,
    isRescheduleCancellation,
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Location as Fields
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Fields
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.ServiceTierType as DST
import qualified Domain.Types.Trip as Trip
import Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.RescheduleReason
import SharedLogic.Search (SearchReqLocation (..))
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Metrics (CoreMetrics)
import TransactionLogs.Types (KeyConfig, TokenConfig)

-- | How far the searched pickup/drop may be from the booking's, in metres. Only the
-- time may change; this only absorbs GPS/geocoding jitter in what the app re-sends.
locationMatchToleranceMeters :: Int
locationMatchToleranceMeters = 100

-- | Held from accept (select/confirm) until the old booking is cancelled, so two
-- reschedules of the same booking cannot both go through.
rescheduleLockKey :: Id DRB.Booking -> Text
rescheduleLockKey bookingId = "Customer:Reschedule:BookingId-" <> bookingId.getId

rescheduleLockExpirySeconds :: Int
rescheduleLockExpirySeconds = 60

-- | The booking this search reschedules, while it is still live. Nothing once that
-- booking is cancelled/completed, which is also the case when a dynamic-offer
-- reschedule is auto-confirmed long after its select already retired the old booking.
findActiveRescheduleSource ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DSearchReq.SearchRequest ->
  m (Maybe DRB.Booking)
findActiveRescheduleSource searchRequest =
  case searchRequest.rescheduledFromBookingId of
    Nothing -> pure Nothing
    Just oldBookingId -> do
      mbOldBooking <- QRB.findById (Id oldBookingId)
      pure $ mfilter (\b -> b.status `notElem` DRB.terminalBookingStatus) mbOldBooking

-- | Whether this search reschedules that booking: the active-booking guards must
-- let the replacement through.
isRescheduleOf :: DSearchReq.SearchRequest -> DRB.Booking -> Bool
isRescheduleOf searchRequest booking = searchRequest.rescheduledFromBookingId == Just booking.id.getId

-- | At search time: the booking can be rescheduled to this time, and the searched
-- pickup/drop are the booking's own. Returns the old booking's BPP booking id when a driver
-- is assigned to it, so the BPP can give that driver a head start on the new ride.
validateRescheduleSearch ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DM.Merchant ->
  Id DRB.Booking ->
  UTCTime ->
  SearchReqLocation ->
  [SearchReqLocation] ->
  m (Maybe Text)
validateRescheduleSearch personId merchant oldBookingId newStartTime origin stops = do
  oldBooking <- QRB.findById oldBookingId >>= fromMaybeM (RescheduleBookingNotFound oldBookingId.getId)
  validateEligibility personId merchant oldBooking newStartTime
  unless (isNear origin.gps (locationLatLong oldBooking.fromLocation)) $
    throwError $ RescheduleTripMismatch "pickup location"
  whenJust (bookingDropLocation oldBooking) $ \toLocation ->
    unless (maybe False (\drop' -> isNear drop'.gps (locationLatLong toLocation)) (lastMay stops)) $
      throwError $ RescheduleTripMismatch "drop location"
  hasDriver <- isJust <$> QRide.findActiveByRBId oldBooking.id
  pure $ if hasDriver then (.getId) <$> oldBooking.bppBookingId else Nothing
  where
    -- Drop of the reschedulable ride types; a rental may have none (then the search's last stop is free).
    bookingDropLocation booking = case booking.bookingDetails of
      DRB.DriverOfferDetails details -> Just details.toLocation
      DRB.OneWayDetails details -> Just details.toLocation
      DRB.InterCityDetails details -> Just details.toLocation
      DRB.RentalDetails details -> details.stopLocation
      _ -> Nothing
    locationLatLong loc = LatLong {lat = loc.lat, lon = loc.lon}
    isNear a b = (highPrecMetersToMeters (distanceBetweenInMeters a b)).getMeters <= locationMatchToleranceMeters

-- | At accept time (select for dynamic offers, confirm for static quotes): the
-- authoritative check, since the rider may have waited on the price screen. Also
-- takes the reschedule lock for the old booking.
validateRescheduleAccept ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DM.Merchant ->
  DSearchReq.SearchRequest ->
  DRB.Booking ->
  DST.ServiceTierType ->
  Maybe Trip.TripCategory ->
  m ()
validateRescheduleAccept merchant searchRequest oldBooking newServiceTier mbNewTripCategory = do
  validateEligibility searchRequest.riderId merchant oldBooking searchRequest.startTime
  unless (newServiceTier == oldBooking.vehicleServiceTierType) $
    throwError $ RescheduleTripMismatch "vehicle tier"
  case (oldBooking.tripCategory, mbNewTripCategory) of
    (Just oldCategory, Just newCategory) ->
      unless (tripKind oldCategory == tripKind newCategory) $
        throwError $ RescheduleTripMismatch "trip type"
    _ -> pure ()
  acquired <- Redis.tryLockRedis (rescheduleLockKey oldBooking.id) rescheduleLockExpirySeconds
  unless acquired $ throwError RescheduleInProgress
  where
    -- Destination city codes etc. are not compared: pickup/drop are already pinned at search.
    tripKind :: Trip.TripCategory -> Text
    tripKind = \case
      Trip.OneWay mode -> "ONE_WAY_" <> show mode
      Trip.Rental _ -> "RENTAL"
      Trip.InterCity _ _ -> "INTER_CITY"
      Trip.CrossCity _ _ -> "CROSS_CITY"
      Trip.IntercityRental _ _ -> "INTERCITY_RENTAL"
      other -> show other

-- | Rules shared by search and accept.
--
--   * own, scheduled, live booking of a reschedulable ride type, on a value-add BPP
--     (the BPP must understand the reschedule cancel to waive its consequences)
--   * allowed while more than scheduleRideBufferTime before the old pickup, or at any
--     time while no driver is assigned
--   * the new pickup is still a scheduled ride: more than scheduleRideBufferTime away
--     (same strict rule Confirm uses to mark a booking scheduled)
validateEligibility ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DM.Merchant ->
  DRB.Booking ->
  UTCTime ->
  m ()
validateEligibility personId merchant oldBooking newStartTime = do
  unless (oldBooking.riderId == personId) $ throwError RescheduleNotOwnBooking
  unless oldBooking.isScheduled $ throwError RescheduleNotScheduledBooking
  unless (oldBooking.status `elem` [DBS.NEW, DBS.CONFIRMED, DBS.AWAITING_REASSIGNMENT, DBS.TRIP_ASSIGNED]) $
    throwError $ RescheduleBookingNotActive (show oldBooking.status)
  unless (isReschedulableTrip oldBooking) $ throwError RescheduleNotSupportedForTripType
  isValueAddNP <- CQVAN.isValueAddNP oldBooking.providerId
  unless isValueAddNP $ throwError RescheduleNotSupportedForTripType
  -- NY Regular subscription rides are booked through RESERVE searches; they are out of scope.
  mbOldSearchRequest <- QSearchRequest.findById (Id oldBooking.transactionId)
  when ((mbOldSearchRequest >>= (.searchMode)) == Just DSearchReq.RESERVE) $
    throwError RescheduleNotSupportedForTripType
  mbRide <- QRide.findActiveByRBId oldBooking.id
  whenJust mbRide $ \ride ->
    unless (ride.status `elem` [DRide.NEW, DRide.UPCOMING]) $
      throwError $ RescheduleBookingNotActive (show ride.status)
  now <- getCurrentTime
  let buffer = merchant.scheduleRideBufferTime
      outsideCancellationWindow = now < addUTCTime (negate buffer) oldBooking.startTime
  unless (outsideCancellationWindow || isNothing mbRide) $ throwError RescheduleNotAllowedInWindow
  unless (addUTCTime buffer now < newStartTime) $ throwError RescheduleTimeTooSoon

-- | Rental, intercity (incl. intercity rental and cross city) and one-way. A scheduled one-way
-- ride is a static quote (the BPP offers OneWayOnDemandStaticOffer for scheduled searches);
-- dynamic-offer one-way is kept for bookings that became scheduled some other way.
isReschedulableTrip :: DRB.Booking -> Bool
isReschedulableTrip booking =
  case booking.tripCategory of
    Just (Trip.Rental _) -> True
    Just (Trip.IntercityRental _ _) -> True
    Just (Trip.InterCity _ _) -> True
    Just (Trip.CrossCity _ _) -> True
    Just (Trip.OneWay Trip.OneWayOnDemandStaticOffer) -> True
    Just (Trip.OneWay Trip.OneWayOnDemandDynamicOffer) -> True
    Just _ -> False
    Nothing -> case booking.bookingDetails of
      DRB.RentalDetails _ -> True
      DRB.InterCityDetails _ -> True
      DRB.DriverOfferDetails _ -> True
      DRB.OneWayDetails _ -> True
      _ -> False

-- | Whether the old booking holds a deposit that will be moved onto the new booking.
-- When it does, confirm must not reserve a second deposit for the new booking.
hasDepositHoldToCarry :: (CacheFlow m r, EsqDBFlow m r) => DRB.Booking -> m Bool
hasDepositHoldToCarry oldBooking =
  if isJust oldBooking.bookingDepositAmount
    then not . null <$> BookingDeposit.findHolds oldBooking.id
    else pure False

-- | Dynamic offers: once the BPP accepted the select for the new time, retire the
-- old booking so its driver is released now. If this fails the new booking's
-- on_confirm retries it ('completeRescheduleOnConfirm').
cancelRescheduleSourceAfterSelect ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  DSearchReq.SearchRequest ->
  m ()
cancelRescheduleSourceAfterSelect searchRequest = do
  mbOldBooking <- findActiveRescheduleSource searchRequest
  whenJust mbOldBooking $ \oldBooking -> do
    res <- withTryCatch "reschedule:cancelOldBookingAfterSelect" $ cancelRescheduledBooking oldBooking
    case res of
      Left err -> logError $ "Reschedule: could not cancel old booking " <> oldBooking.id.getId <> " after select; on_confirm will retry: " <> show err
      Right () -> pure ()

-- | On the new booking's on_confirm: move the deposit hold from the old booking to
-- the new one, then cancel the old booking. Static quotes reach here with the old
-- booking untouched; for dynamic offers the old booking is normally already gone and
-- this is only a safety net.
--
-- If the deposit cannot be moved, the new booking is cancelled instead and the rider
-- keeps the original ride and its deposit.
completeRescheduleOnConfirm :: DRB.Booking -> Flow ()
completeRescheduleOnConfirm confirmedBooking =
  whenJust confirmedBooking.rescheduledFromBookingId $ \oldBookingId -> do
    mbOldBooking <- QRB.findById (Id oldBookingId)
    whenJust (mfilter (\b -> b.status `notElem` DRB.terminalBookingStatus) mbOldBooking) $ \oldBooking -> do
      newBooking <- QRB.findById confirmedBooking.id >>= fromMaybeM (BookingDoesNotExist confirmedBooking.id.getId)
      carryDeposit <- hasDepositHoldToCarry oldBooking
      rekeyRes <-
        if carryDeposit
          then withTryCatch "reschedule:rekeyBookingDepositHold" $ BookingDeposit.rekeyBookingDepositHold oldBooking newBooking
          else pure (Right ())
      case rekeyRes of
        Left err -> do
          logError $ "Reschedule: deposit could not move from " <> oldBooking.id.getId <> " to " <> newBooking.id.getId <> "; cancelling the new booking instead: " <> show err
          cancelLogged "reschedule:cancelNewBookingAfterRekeyFailure" newBooking
        Right () -> cancelLogged "reschedule:cancelOldBookingOnConfirm" oldBooking
  where
    cancelLogged tag booking = do
      res <- withTryCatch tag $ cancelRescheduledBooking booking
      case res of
        Left err -> logError $ "Reschedule: cancel of booking " <> booking.id.getId <> " failed: " <> show err
        Right () -> pure ()

-- | Cancel with the reschedule reason and no reallocation, through the regular rider
-- cancel path (the BPP's on_cancel completes it, notifying rider and driver).
cancelRescheduledBooking ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  DRB.Booking ->
  m ()
cancelRescheduledBooking booking = do
  mbRide <- QRide.findActiveByRBId booking.id
  let cancelReq =
        DCancel.CancelReq
          { reasonCode = SCR.CancellationReasonCode rescheduleCancellationReasonCode,
            reasonStage = if isJust mbRide then SCR.OnAssign else SCR.OnConfirm,
            additionalInfo = Just "Rescheduled by rider",
            reallocate = Just False,
            blockOnCancellationRate = Nothing,
            abortPaytmEdc = Nothing
          }
  dCancelRes <- DCancel.cancel booking mbRide cancelReq SBCR.ByUser
  void . withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes cancelReq.reallocate
