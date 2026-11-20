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
-- search -> confirm calls (static quotes only). The app marks the search with the booking it
-- reschedules ('DSearchReq.rescheduledFromBookingId'); from there:
--
--   * search      -> 'withRescheduledBookingTrip' (the trip is the booking's; only the time is the
--                    app's), then 'validateRescheduleSearch' (fail early, before showing a price)
--   * confirm     -> 'validateRescheduleAccept' (the rider's accept: authoritative check,
--                    takes the reschedule lock of the old booking)
--   * on_confirm  -> 'completeRescheduleOnConfirm': claim the old booking for this replacement,
--                    move its booking deposit onto the new booking, then cancel it
--
-- Until the replacement is confirmed the old booking is untouched, so backing out at any point
-- before that leaves the rider's ride as it was.
--
-- The old booking is cancelled with reason 'rescheduleCancellationReasonCode'. That
-- cancel carries no fee and does not count as a customer cancellation (see
-- 'isRescheduleCancellation' and the BPP's handling of the same reason code).
module SharedLogic.Reschedule
  ( rescheduleCancellationReasonCode,
    findActiveRescheduleSource,
    isRescheduleOf,
    withRescheduledBookingTrip,
    validateRescheduleSearch,
    validateRescheduleAccept,
    releaseRescheduleLock,
    hasDepositHoldToCarry,
    completeRescheduleOnConfirm,
    cancelRescheduledBooking,
    isRescheduleCancellation,
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import Control.Applicative ((<|>))
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
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.JobScheduler (RescheduleCancelBookingJobData (..), RiderJobType (..))
import SharedLogic.Search (InterCitySearchReq (..), OneWaySearchReq (..), RentalSearchReq (..), SearchReq (..), SearchReqLocation (..))
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Metrics (CoreMetrics)
import TransactionLogs.Types (KeyConfig, TokenConfig)

-- | Reason code of the cancel that retires the old booking. Sent to the BPP as the
-- cancel descriptor's short_desc, which is how the BPP recognises a reschedule.
rescheduleCancellationReasonCode :: Text
rescheduleCancellationReasonCode = "RESCHEDULED"

-- | Whether the booking was cancelled to reschedule it. Such a cancel must not count
-- as a customer cancellation (fraud counters, cancellation rate, cancel tags).
isRescheduleCancellation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m Bool
isRescheduleCancellation bookingId = do
  mbReason <- QBCR.findByRideBookingId bookingId
  pure $ (mbReason >>= (.reasonCode)) == Just (SCR.CancellationReasonCode rescheduleCancellationReasonCode)

-- | Held by the reschedule search (its id is the value) from its confirm until its replacement
-- is confirmed or fails, so two reschedules of the same booking cannot run at once.
rescheduleLockKey :: Id DRB.Booking -> Text
rescheduleLockKey bookingId = "Customer:Reschedule:BookingId-" <> bookingId.getId

rescheduleLockExpirySeconds :: Int
rescheduleLockExpirySeconds = 60

-- | The replacement booking that took over this booking (its id is the value). Set once, on the
-- first replacement's on_confirm; any other replacement confirmed later is a duplicate.
replacedByKey :: Id DRB.Booking -> Text
replacedByKey bookingId = "Customer:Reschedule:ReplacedBy:BookingId-" <> bookingId.getId

-- | The booking this search reschedules, while it is still live.
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

-- | A reschedule moves only the pickup time: the search's trip is replaced by the rider's own
-- booking's (pickup, stops, drop, rental package, round trip with its return moved by as much as
-- the pickup), whatever the app sent. Any other search is returned unchanged.
withRescheduledBookingTrip ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  SearchReq ->
  m SearchReq
withRescheduledBookingTrip personId req = case req of
  OneWaySearch OneWaySearchReq {..} | Just oldBookingId <- rescheduledFromBookingId -> do
    oldBooking <- findRidersBooking oldBookingId
    -- A cross-city booking loads as DriverOfferDetails; whether it may move is eligibility's call.
    let oneWay details =
          pure $
            OneWaySearch
              OneWaySearchReq
                { origin = toSearchLocation oldBooking.fromLocation,
                  destination = Just (toSearchLocation details.toLocation),
                  stops = Just (toSearchLocation <$> details.stops),
                  ..
                }
    case oldBooking.bookingDetails of
      DRB.OneWayDetails details -> oneWay details
      DRB.DriverOfferDetails details -> oneWay details
      _ -> throwError $ RescheduleTripMismatch "trip type"
  RentalSearch RentalSearchReq {..} | Just oldBookingId <- rescheduledFromBookingId -> do
    oldBooking <- findRidersBooking oldBookingId
    case oldBooking.bookingDetails of
      DRB.RentalDetails details ->
        pure $
          RentalSearch
            RentalSearchReq
              { origin = toSearchLocation oldBooking.fromLocation,
                stops = Just (toSearchLocation <$> (details.stops <> maybeToList details.stopLocation)),
                estimatedRentalDuration = fromMaybe estimatedRentalDuration oldBooking.estimatedDuration,
                estimatedRentalDistance = maybe estimatedRentalDistance distanceToMeters oldBooking.estimatedDistance,
                ..
              }
      _ -> throwError $ RescheduleTripMismatch "trip type"
  InterCitySearch InterCitySearchReq {..} | Just oldBookingId <- rescheduledFromBookingId -> do
    oldBooking <- findRidersBooking oldBookingId
    case oldBooking.bookingDetails of
      DRB.InterCityDetails details ->
        -- The search's stops end with its drop.
        pure $
          InterCitySearch
            InterCitySearchReq
              { origin = toSearchLocation oldBooking.fromLocation,
                stops = Just (toSearchLocation <$> (details.stops <> [details.toLocation])),
                roundTrip = fromMaybe False oldBooking.roundTrip,
                returnTime = addUTCTime (diffUTCTime startTime oldBooking.startTime) <$> oldBooking.returnTime,
                ..
              }
      _ -> throwError $ RescheduleTripMismatch "trip type"
  _ -> pure req
  where
    findRidersBooking oldBookingId = do
      oldBooking <- QRB.findById oldBookingId >>= fromMaybeM (RescheduleBookingNotFound oldBookingId.getId)
      unless (oldBooking.riderId == personId) $ throwError (RescheduleBookingNotFound oldBookingId.getId)
      pure oldBooking
    toSearchLocation :: Fields.Location -> SearchReqLocation
    toSearchLocation loc = SearchReqLocation {gps = LatLong loc.lat loc.lon, address = loc.address}

-- | At search: fail before any price is shown if the booking cannot move to this time. Returns the
-- old booking's BPP booking id, which the BPP uses to offer the ride to its driver first.
validateRescheduleSearch ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DM.Merchant ->
  Id DRB.Booking ->
  UTCTime ->
  m (Maybe Text)
validateRescheduleSearch personId merchant oldBookingId newStartTime = do
  oldBooking <- QRB.findById oldBookingId >>= fromMaybeM (RescheduleBookingNotFound oldBookingId.getId)
  validateEligibility personId merchant oldBooking newStartTime
  pure $ (.getId) <$> oldBooking.bppBookingId

-- | At accept time (confirm): the authoritative check, since the rider may have waited on the
-- price screen. Also takes the reschedule lock for the old booking.
validateRescheduleAccept ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DM.Merchant ->
  DSearchReq.SearchRequest ->
  DRB.Booking ->
  Text ->
  DST.ServiceTierType ->
  Maybe Trip.TripCategory ->
  m ()
validateRescheduleAccept personId merchant searchRequest oldBooking newProviderId newServiceTier mbNewTripCategory = do
  -- The caller, not the search's rider: confirm re-points a search to whoever confirms it.
  validateEligibility personId merchant oldBooking searchRequest.startTime
  unless (newProviderId == oldBooking.providerId) $
    throwError $ RescheduleTripMismatch "provider"
  unless (newServiceTier == oldBooking.vehicleServiceTierType) $
    throwError $ RescheduleTripMismatch "vehicle tier"
  -- A booking from before trip categories is judged by its details; a trip type that cannot be
  -- compared is refused rather than let through.
  case (tripKind <$> oldBooking.tripCategory <|> legacyTripKind oldBooking.bookingDetails, tripKind <$> mbNewTripCategory) of
    (Just oldKind, Just newKind) | oldKind == newKind -> pure ()
    _ -> throwError $ RescheduleTripMismatch "trip type"
  -- Already replaced: its replacement is confirmed and the old booking is being cancelled.
  replacedBy <- Redis.safeGet @Text (replacedByKey oldBooking.id)
  when (isJust replacedBy) $ throwError RescheduleInProgress
  acquired <- Redis.setNxExpire (rescheduleLockKey oldBooking.id) rescheduleLockExpirySeconds searchRequest.id.getId
  unless acquired $ do
    -- The same search confirming again (a retried confirm) keeps its own lock.
    holder <- Redis.safeGet @Text (rescheduleLockKey oldBooking.id)
    unless (holder == Just searchRequest.id.getId) $ throwError RescheduleInProgress
  where
    -- Destination city codes etc. are not compared: the trip itself comes from the booking at search.
    tripKind :: Trip.TripCategory -> Text
    tripKind = \case
      Trip.OneWay mode -> "ONE_WAY_" <> show mode
      Trip.Rental _ -> "RENTAL"
      Trip.InterCity _ _ -> "INTER_CITY"
      Trip.CrossCity _ _ -> "CROSS_CITY"
      Trip.IntercityRental _ _ -> "INTERCITY_RENTAL"
      other -> show other
    legacyTripKind :: DRB.BookingDetails -> Maybe Text
    legacyTripKind = \case
      DRB.RentalDetails _ -> Just "RENTAL"
      DRB.InterCityDetails _ -> Just "INTER_CITY"
      DRB.OneWayDetails _ -> Just (tripKind (Trip.OneWay Trip.OneWayOnDemandStaticOffer))
      _ -> Nothing

-- | Free the old booking for another reschedule; only the search holding the lock frees it.
releaseRescheduleLock :: (MonadFlow m, CacheFlow m r) => DSearchReq.SearchRequest -> m ()
releaseRescheduleLock searchRequest =
  whenJust searchRequest.rescheduledFromBookingId $ \oldBookingId -> do
    let key = rescheduleLockKey (Id oldBookingId)
    holder <- Redis.safeGet @Text key
    when (holder == Just searchRequest.id.getId) $ Redis.del key

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
  -- Someone else's booking answers exactly like a missing one.
  unless (oldBooking.riderId == personId) $ throwError (RescheduleBookingNotFound oldBooking.id.getId)
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

-- | Rental, intercity (incl. intercity rental and cross city) and one-way, all as static quotes
-- (the BPP offers OneWayOnDemandStaticOffer for scheduled one-way searches). Driver offers are
-- out: their select would have to give the old booking up before a replacement exists.
isReschedulableTrip :: DRB.Booking -> Bool
isReschedulableTrip booking =
  case booking.tripCategory of
    Just (Trip.Rental _) -> True
    Just (Trip.IntercityRental _ _) -> True
    Just (Trip.InterCity _ _) -> True
    Just crossCity@(Trip.CrossCity _ _) -> not (Trip.isDynamicOfferTrip crossCity)
    Just (Trip.OneWay Trip.OneWayOnDemandStaticOffer) -> True
    Just _ -> False
    Nothing -> case booking.bookingDetails of
      DRB.RentalDetails _ -> True
      DRB.InterCityDetails _ -> True
      DRB.OneWayDetails _ -> True
      _ -> False

-- | Whether the old booking holds a deposit that will be moved onto the new booking.
-- When it does, confirm must not reserve a second deposit for the new booking.
hasDepositHoldToCarry :: (CacheFlow m r, EsqDBFlow m r) => DRB.Booking -> m Bool
hasDepositHoldToCarry oldBooking =
  if isJust oldBooking.bookingDepositAmount
    then not . null <$> BookingDeposit.findHolds oldBooking.id
    else pure False

-- | On the new booking's on_confirm: claim the old booking for this replacement, move its deposit
-- hold and payments onto the new booking, then cancel the old booking.
--
--   * A second replacement of the same booking (the first already claimed it) is a duplicate and
--     is cancelled itself.
--   * If the deposit cannot be moved, the booking that holds it survives: normally the old one
--     (the new booking is cancelled and the rider keeps the original ride); when the hold already
--     moved, the new one, once the payments have followed it.
completeRescheduleOnConfirm :: DRB.Booking -> Flow ()
completeRescheduleOnConfirm confirmedBooking = do
  -- The booking's transactionId is its search request's id; only that search knows the source.
  mbSearchRequest <- QSearchRequest.findById (Id confirmedBooking.transactionId)
  whenJust mbSearchRequest $ \searchRequest ->
    whenJust searchRequest.rescheduledFromBookingId $ \oldBookingIdText -> do
      let oldBookingId = Id oldBookingIdText
      newBooking <- QRB.findById confirmedBooking.id >>= fromMaybeM (BookingDoesNotExist confirmedBooking.id.getId)
      mbOldBooking <- QRB.findById oldBookingId
      unless (newBooking.status `elem` DRB.terminalBookingStatus) $
        whenJust mbOldBooking $ \oldBooking -> do
          now <- getCurrentTime
          let claimTtl = max 3600 (ceiling (diffUTCTime oldBooking.startTime now) + 86400) :: Int
          claimed <- Redis.setNxExpire (replacedByKey oldBookingId) claimTtl newBooking.id.getId
          replacement <- if claimed then pure (Just newBooking.id.getId) else Redis.safeGet @Text (replacedByKey oldBookingId)
          if replacement /= Just newBooking.id.getId
            then do
              logError $ "Reschedule: booking " <> oldBookingId.getId <> " was already replaced by " <> show replacement <> "; cancelling duplicate " <> newBooking.id.getId
              cancelOrRetryLater newBooking
            else
              if oldBooking.status `elem` DRB.terminalBookingStatus
                then when (isJust oldBooking.bookingDepositAmount) $ secureReplacementDeposit newBooking
                else retireOldBooking oldBooking newBooking
      releaseRescheduleLock searchRequest
  where
    retireOldBooking oldBooking newBooking = do
      carryDeposit <- hasDepositHoldToCarry oldBooking
      rekeyRes <-
        if carryDeposit
          then withTryCatch "reschedule:rekeyBookingDepositHold" $ BookingDeposit.rekeyBookingDepositHold oldBooking newBooking
          else pure (Right ())
      case rekeyRes of
        Right () -> do
          -- Nothing to carry (no hold left on the old booking): the replacement secures its own.
          unless carryDeposit $ when (isJust oldBooking.bookingDepositAmount) $ secureReplacementDeposit newBooking
          cancelOrRetryLater oldBooking
        Left err -> do
          oldHolds <- BookingDeposit.findHolds oldBooking.id
          newHolds <- BookingDeposit.findHolds newBooking.id
          if null oldHolds && not (null newHolds)
            then do
              -- The hold moved: the payments must follow it before the old booking is cancelled,
              -- or cancelling it would refund money the new booking relies on.
              void $ withTryCatch "reschedule:moveDepositPayments" $ BookingDeposit.moveDepositPayments oldBooking newBooking
              leftOnOld <- BookingDeposit.depositPaymentsToCarry oldBooking.id
              if null leftOnOld
                then do
                  logError $ "Reschedule: deposit move from " <> oldBooking.id.getId <> " to " <> newBooking.id.getId <> " failed after the hold moved; completed it and keeping the new booking: " <> show err
                  cancelOrRetryLater oldBooking
                else logError $ "Reschedule: deposit payments of " <> oldBooking.id.getId <> " could not be moved to " <> newBooking.id.getId <> "; both bookings left live for manual resolution: " <> show err
            else do
              logError $ "Reschedule: deposit could not move from " <> oldBooking.id.getId <> " to " <> newBooking.id.getId <> "; cancelling the new booking instead: " <> show err
              Redis.del (replacedByKey oldBooking.id)
              cancelOrRetryLater newBooking

    -- A cancel that fails now is retried by a scheduler job until it goes through.
    cancelOrRetryLater booking = do
      res <- withTryCatch "reschedule:cancelRescheduledBooking" $ cancelRescheduledBooking booking
      case res of
        Right () -> pure ()
        Left err -> do
          logError $ "Reschedule: cancel of booking " <> booking.id.getId <> " failed, retrying from the scheduler: " <> show err
          createJobIn @_ @'RescheduleCancelBooking (Just booking.merchantId) (Just booking.merchantOperatingCityId) 60 $
            RescheduleCancelBookingJobData {bookingId = booking.id}

-- | The replacement was confirmed without its own deposit (it expected the old booking's hold), but
-- there is no hold left to carry: secure it from the rider's balance now.
secureReplacementDeposit :: DRB.Booking -> Flow ()
secureReplacementDeposit newBooking =
  whenJust newBooking.bookingDepositAmount $ \fee -> do
    newHolds <- BookingDeposit.findHolds newBooking.id
    when (null newHolds) $ do
      res <- withTryCatch "reschedule:secureReplacementDeposit" $ BookingDeposit.decideAndSecureBookingDeposit newBooking fee
      case res of
        Right (BookingDeposit.FeeSecured, _) -> logInfo $ "Reschedule: secured the deposit of " <> newBooking.id.getId <> " from the rider's balance"
        Right (BookingDeposit.FeeShortfall shortfall, _) -> logError $ "Reschedule: no deposit to carry to " <> newBooking.id.getId <> " and the balance is short by " <> show shortfall
        Left err -> logError $ "Reschedule: could not secure the deposit of " <> newBooking.id.getId <> ": " <> show err

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
