-- | Claiming a cab for a finding booking under the plate then booking lock, and the alloc-key helpers the close path shares.
module SharedLogic.SharedCab.Allocation.Claim
  ( InternalEndpointFlow,
    AllocFlow,
    claimFirst,
    claimGate,
    ClaimMiss (..),
    liveBookingsOf,
    findLive,
    claimFallbackPush,
  )
where

import Control.Monad.Extra (whenJustM)
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.SharedCab.Allocation.Select (ClaimPush (..), FindingBooking (..), LiveBooking (..), RankedCandidate (..), claimPush, claimTimer, claimable, skippedWhileFinding)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (liveSeatsOnVehicle, shared, withBookingLock)
import SharedLogic.SharedCab.BookingState
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.Keys (fallbackPushedKey)
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

type InternalEndpointFlow m r = HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]

type AllocFlow m r =
  ( MonadFlow m,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadMask m,
    Log m,
    Redis.HedisLTSFlowEnv r,
    Metrics.CoreMetrics m,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Events.EventFlow m r,
    EncFlow m r,
    InternalEndpointFlow m r
  )

-- Lock order, Phase 2: target cab lock FIRST (`sharedcab:lock:{plate}`, shared with walk-up writes), then the booking
-- lock (`sharedcab:lock:booking:{id}`); NEVER reverse. The locks are non-reentrant (withWaitAndLockRedis deadlocks on a
-- key you hold), so never call releaseSharedCabAllocation or a re-bind from inside cancelLeg's booking lock.
-- frfs_ticket_booking is KV-enabled outside local dev, so a DB read can be stale mid-drain: the booking lock is the row
-- mutex and the updateAllocatedVehicle expected-value predicate is the belt-and-braces inside it.
-- allocation_created goes out AFTER the CAS + alloc-key write; a crash in between leaves an allocated booking with no
-- event, bounded by the alloc key TTL (no transactional bridge KV x Redis x Kafka).

-- | Tickets are read in one query per call, not one per booking.
liveBookingsOf :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [DFTB.FRFSTicketBooking] -> m [LiveBooking]
liveBookingsOf bookings = do
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  pure [LiveBooking b [t.status | t <- tickets, t.frfsTicketBookingId == b.id] | b <- bookings]

findLive :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DFTB.FRFSTicketBooking -> m (Maybe LiveBooking)
findLive bookingId = QFRFSTicketBooking.findById bookingId >>= traverse (\b -> LiveBooking b . map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id)

data ClaimMiss = ClaimSessionGone | ClaimSeatsGone | ClaimCasLost | ClaimSkipped
  deriving (Show, Eq)

-- | The claim's decision under both locks, pure: the plate was not skipped, the cab still has the seats, and the booking is still
-- waiting for a cab with no degrade marker.
claimGate :: Session -> FindingBooking -> [Text] -> Int -> Maybe LiveBooking -> Bool -> Either ClaimMiss ()
claimGate s booking skipped liveSeats mbLive markerAlive
  | s.vehicleNumber `elem` skipped = Left ClaimSkipped
  | s.capacity - s.walkupCount - liveSeats < booking.seats = Left ClaimSeatsGone
  | otherwise = case mbLive of
    Just (LiveBooking b statuses) | claimable b.status b.vehicleNumber statuses markerAlive -> Right ()
    _ -> Left ClaimCasLost

-- | One candidate under both locks: re-read the session, recompute seats, CAS vehicleNumber, write the alloc key. A miss counts no attempt.
attemptClaim ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, MonadMask m) =>
  Config.SharedCabTunables ->
  FindingBooking ->
  RankedCandidate ->
  m (Either ClaimMiss UTCTime)
attemptClaim cfg booking cand =
  Session.withPlateLock plate $
    Session.readSession plate >>= \case
      Just s | s.status == ACTIVE && s.routeCode == booking.routeCode -> withBookingLock booking.bookingId (claimUnderLocks cfg booking cand s)
      _ -> pure (Left ClaimSessionGone)
  where
    plate = cand.rcSession.vehicleNumber

claimUnderLocks ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Config.SharedCabTunables ->
  FindingBooking ->
  RankedCandidate ->
  Session ->
  m (Either ClaimMiss UTCTime)
claimUnderLocks cfg booking cand s = do
  now <- getCurrentTime
  attempts <- readAttempts booking.bookingId
  skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> readSkipped booking.bookingId
  liveSeats <- liveSeatsOnVehicle s.vehicleNumber
  mbLive <- findLive booking.bookingId
  markerAlive <- Degraded.isMarkerAlive booking.bookingId
  for (claimGate s booking skipped liveSeats mbLive markerAlive) $ \() -> do
    QFRFSTicketBooking.updateAllocatedVehicle (Just s.vehicleNumber) booking.bookingId Nothing
    -- the key's TTL is only a garbage-collection backstop and must outlive the stacked timers (allocKeyTtl)
    writeAlloc (allocKeyTtl cfg) booking.bookingId AllocationState {vehicleNumber = s.vehicleNumber, driverId = Just s.driverId, vehicleTripId = Just s.vehicleTripId.getId, allocatedAt = now, timer = claimTimer cfg now cand}
    pure now

claimFallbackPush :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> m Bool
claimFallbackPush cfg bookingId = shared $ Redis.tryLockRedis (fallbackPushedKey bookingId) cfg.findingTimeoutSec

notifyDriverOfAllocation ::
  AllocFlow m r =>
  FindingBooking ->
  RankedCandidate ->
  m ()
notifyDriverOfAllocation booking cand =
  withTryCatch "sharedCabNotifyDriver" call >>= \case
    Left e -> logWarning $ "notifyDriverOfAllocation failed driver=" <> cand.rcSession.driverId <> " booking=" <> booking.bookingId.getId <> ": " <> show e
    Right _ -> pure ()
  where
    call = do
      let s = cand.rcSession
      merchant <- CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId)
      void . CallBPPInternal.sharedCabAllocationFCM merchant.driverOfferApiKey merchant.driverOfferBaseUrl $
        CallBPPInternal.SharedCabAllocationReq
          { bookingId = booking.bookingId.getId,
            driverId = s.driverId,
            seats = Just booking.seats,
            boardingCode = Nothing,
            vehicleNumber = Just s.vehicleNumber,
            boardStopCode = Just booking.boardStopCode,
            etaSeconds = Just cand.rcEtaToBoardStopSec
          }

claimFirst ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  FindingBooking ->
  [RankedCandidate] ->
  m (Maybe RankedCandidate)
claimFirst cfg booking = go (0 :: Int)
  where
    go _ [] = pure Nothing
    go rank (c : cs) =
      attemptClaim cfg booking c >>= \case
        Right _ -> do
          now <- getCurrentTime
          Events.emit c.rcSession.merchantOperatingCityId . Events.withTrip (Just c.rcSession.vehicleTripId.getId) $
            Events.bookingEvent (Events.AllocationCreated (Just (c.rcEtaToBoardStopSec `div` 60)) rank) booking.bookingId.getId (Just c.rcSession.vehicleNumber) (Just booking.routeCode) now
          Invariants.checkBooking booking.bookingId
          Invariants.checkCab c.rcSession.vehicleNumber
          fork "sharedCabNotifyDriver" (notifyDriverOfAllocation booking c)
          whenJustM (QFRFSTicketBooking.findById booking.bookingId) $ case claimPush c of
            PushArriving -> Notify.notifyArriving c.rcSession.vehicleNumber cfg.standTimerSec
            PushAssigned -> Notify.notifyAssigned c.rcSession.vehicleNumber
          pure (Just c)
        Left miss -> do
          logDebug $ "shared-cab claim missed booking=" <> booking.bookingId.getId <> " cab=" <> c.rcSession.vehicleNumber <> " reason=" <> show miss
          go (rank + 1) cs
