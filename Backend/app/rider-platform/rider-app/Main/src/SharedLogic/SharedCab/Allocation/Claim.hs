-- | Claiming a cab for a finding booking under the plate then booking lock, and the alloc-key helpers the close path shares.
module SharedLogic.SharedCab.Allocation.Claim
  ( InternalEndpointFlow,
    AllocFlow,
    claimFirst,
    readFindingSince,
    readAttempts,
    claimFallbackPush,
    warnAllocKeyTtlOnce,
    clearAllocationKeys,
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
import SharedLogic.SharedCab.Allocation.Select (ClaimPush (..), FindingBooking (..), RankedCandidate (..), claimPush, claimTimerKind, claimTimerSec, claimable, isSkipped, skippedWhileFinding)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (liveSeatsOnVehicle, shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.Keys (allocKey, allocKeyTtlWarnKey, attemptsKey, fallbackPushedKey, findingSinceKey, skippedKey)
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (SessionStatus (..))
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
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

-- Lock order, Phase 2: target cab lock FIRST (`sharedcab:lock:{plate}`, shared with walk-up writes), then the booking
-- lock (`sharedcab:lock:booking:{id}`); NEVER reverse. The locks are non-reentrant (withWaitAndLockRedis deadlocks on a
-- key you hold), so never call releaseSharedCabAllocation or a re-bind from inside cancelLeg's booking lock.
-- frfs_ticket_booking is KV-enabled outside local dev, so a DB read can be stale mid-drain: the booking lock is the row
-- mutex and the updateAllocatedVehicle expected-value predicate is the belt-and-braces inside it.
-- allocation_created goes out AFTER the CAS + alloc-key write; a crash in between leaves an allocated booking with no
-- event, bounded by the alloc key TTL (no transactional bridge KV x Redis x Kafka).

data ClaimMiss = ClaimSessionGone | ClaimSeatsGone | ClaimCasLost | ClaimSkipped
  deriving (Show, Eq)

-- | One candidate under both locks: re-read the session, recompute seats, CAS vehicleNumber, write the alloc key. A miss counts no attempt.
attemptClaim ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, MonadMask m) =>
  Config.SharedCabTunables ->
  FindingBooking ->
  RankedCandidate ->
  m (Either ClaimMiss UTCTime)
attemptClaim cfg booking cand = do
  let plate = cand.rcSession.vehicleNumber
  Session.withPlateLock plate $ do
    mbSession <- Session.readSession plate
    case mbSession of
      Just s
        | s.status == ACTIVE && s.routeCode == booking.routeCode -> do
          withBookingLock booking.bookingId $ do
            now <- getCurrentTime
            attempts <- readAttempts (attemptsKey booking.bookingId)
            skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> shared (Redis.sMembers (skippedKey booking.bookingId))
            liveSeats <- liveSeatsOnVehicle plate
            let available = s.capacity - s.walkupCount - liveSeats
            if plate `isSkipped` skipped
              then pure (Left ClaimSkipped)
              else
                if available < booking.seats
                  then pure (Left ClaimSeatsGone)
                  else do
                    QFRFSTicketBooking.findById booking.bookingId >>= \case
                      Just b -> do
                        statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
                        markerAlive <- Degraded.isMarkerAlive b.id
                        if claimable b.status b.vehicleNumber statuses markerAlive
                          then do
                            QFRFSTicketBooking.updateAllocatedVehicle (Just plate) b.id Nothing
                            -- the key's TTL is only a garbage-collection backstop and must outlive the stacked timers
                            warnAllocKeyTtlOnce cfg "claim"
                            let deadline = (\sec -> addUTCTime (intToNominalDiffTime sec) now) <$> claimTimerSec cfg cand
                            shared $
                              Redis.setExp
                                (allocKey booking.bookingId)
                                AllocationState {vehicleNumber = plate, driverId = Just s.driverId, vehicleTripId = Just s.vehicleTripId.getId, allocatedAt = now, expiresAt = deadline, attempts, timerKind = claimTimerKind cand}
                                (allocKeyTtl cfg)
                            pure (Right now)
                          else pure (Left ClaimCasLost)
                      Nothing -> pure (Left ClaimCasLost)
      _ -> pure (Left ClaimSessionGone)

readFindingSince :: (Redis.HedisFlow m r, MonadFlow m) => Id DFTB.FRFSTicketBooking -> UTCTime -> m UTCTime
readFindingSince bookingId createdAt = shared $ fromMaybe createdAt <$> Redis.safeGet (findingSinceKey bookingId)

allocKeyTtlWarnIntervalSec :: Int
allocKeyTtlWarnIntervalSec = 3600

-- | Warn ops once per site per hour that the alloc key TTL was raised past findingTimeoutSec; such a short value shortens every other TTL it feeds.
warnAllocKeyTtlOnce :: (MonadFlow m, Redis.HedisFlow m r, Log m) => Config.SharedCabTunables -> Text -> m ()
warnAllocKeyTtlOnce cfg site =
  when (allocKeyTtlShort cfg) . whenM (shared $ Redis.tryLockRedis (allocKeyTtlWarnKey site) allocKeyTtlWarnIntervalSec) $
    logWarning $
      "shared-cab config: alloc key TTL raised past findingTimeoutSec to cover the stacked timers (site "
        <> site
        <> "): findingTimeoutSec="
        <> show cfg.findingTimeoutSec
        <> ", standTimerSec="
        <> show cfg.standTimerSec
        <> ", movingTimerSec="
        <> show cfg.movingTimerSec
        <> ", allocKeyTtlMarginSec="
        <> show allocKeyTtlMarginSec
        <> " -- a key this short dies before its timers, the close reads TimerLost (no blame, no attempt) and the no-show path is unreachable; raise findingTimeoutSec"

claimFallbackPush :: (Redis.HedisFlow m r, MonadFlow m) => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> m Bool
claimFallbackPush cfg bookingId = shared $ Redis.tryLockRedis (fallbackPushedKey bookingId) cfg.findingTimeoutSec

readAttempts :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m Int
readAttempts key = shared $ fromMaybe 0 <$> Redis.safeGet key

clearAllocationKeys :: (MonadFlow m, Redis.HedisFlow m r) => Id DFTB.FRFSTicketBooking -> m ()
clearAllocationKeys bookingId =
  shared . forM_ [allocKey, attemptsKey, skippedKey, findingSinceKey, fallbackPushedKey] $ \key -> Redis.del (key bookingId)

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
