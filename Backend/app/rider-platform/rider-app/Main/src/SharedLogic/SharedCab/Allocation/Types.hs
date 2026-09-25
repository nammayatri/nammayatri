-- M7.1-7.4 skeleton: shared-cab allocation types.
-- Plan anchors (all 05-allocation-plan.md):
--   §2  "Allocation keys (Redis, ephemeral)": sharedcab:alloc:{bookingId} -> {vehicleNumber, allocatedAt, expiresAt, attempts};
--        stand timer + moving timer semantics.
--   §3  outcomes TIMEOUT / DRIVER_CANCELLED / PASSED_STOP / SEAT_LOST; attempts counts allocations, not candidates;
--        closing is idempotent.
--   §7  rider_config tunables + defaults (the allocation_* Kafka events live in 7.6's SharedLogic.SharedCab.Events).
--   §8.4 blame rules: "consecutiveMisses counts only DRIVER_CANCELLED and stand TIMEOUT";
--        "PASSED_STOP with the rider away = rider no-show only".
module SharedLogic.SharedCab.Allocation.Types
  ( AllocationState (..),
    AllocationOutcome (..),
    Blame (..),
    blameFor,
    countsTowardAttempts,
    countsTowardDriverMisses,
    AllocationConfig (..),
    defaultAllocationConfig,
  )
where

import Kernel.Prelude

-- | The value stored under Redis key @sharedcab:alloc:{bookingId}@ (05 §2).
-- Field names match the plan JSON exactly: {vehicleNumber, allocatedAt, expiresAt, attempts}.
--
--   * expiresAt = allocatedAt + standTimerSec while the cab is still standing (stand timer).
--   * expiresAt is reset to <now> + movingTimerSec once the tick sees the cab at the board stop
--     (moving timer, PRD §11 "90 s after arriving"); Nothing until the first timer is armed.
--   * attempts mirrors the lifetime counter kept under sharedcab:attempts:{bookingId}
--     (see SharedLogic.SharedCab.Allocation.attemptsKey -- the alloc value is deleted on close,
--     so the counter cannot live here alone across successive allocations).
data AllocationState = AllocationState
  { vehicleNumber :: Text, -- canonical plate of the cab holding the booking right now
    allocatedAt :: UTCTime,
    expiresAt :: Maybe UTCTime,
    attempts :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Why a live allocation ended before boarding (05 §3 release reasons).
-- These map 1:1 onto the @outcome@ field of the @allocation_closed@ event (05 §7).
data AllocationOutcome
  = -- | stand timer expired: cab waited standTimerSec and the booking was still open (05 §3, §8.4)
    StandTimeout
  | -- | moving timer expired after the cab reached the board stop (05 §2, §3)
    MovingTimeout
  | -- | driver cancelled the allocation (05 §3)
    DriverCancelled
  | -- | tick saw the cab pass the board stop with the booking unboarded (05 §6 item 2)
    PassedStop
  | -- | capacity guard evicted the latest unboarded allocation (05 §3, §8.7 walk-ups win)
    SeatLost
  | -- | cab route-changed / queued route no longer serves the board stop (05 §11 "Cab changes route")
    RouteChanged
  | -- | session went PAUSED/ENDED: release without penalty (05 §8.7)
    SessionClosed
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | @blame@ field of the @allocation_closed@ event (05 §7) and the foundation for
-- §8.4: "Timers blame the right party."
data Blame = BlameDriver | BlameRider | BlameNone
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | 05 §8.4 blame rules:
--   * cab time running out at the stand and driver cancels are the driver's miss;
--   * the cab reaching/passing the stop with the rider not there is the rider's no-show;
--   * capacity-guard eviction and session lifecycle closes are nobody's fault.
--
-- RANKED QUESTION (report Q5): PRD §11 treats the 90 s moving timer as "rider didn't board" --
-- MovingTimeout is mapped to BlameRider. If product wants MovingTimeout neutral, change only here.
blameFor :: AllocationOutcome -> Blame
blameFor = \case
  StandTimeout -> BlameDriver
  DriverCancelled -> BlameDriver
  MovingTimeout -> BlameRider
  PassedStop -> BlameRider
  SeatLost -> BlameNone
  RouteChanged -> BlameNone
  SessionClosed -> BlameNone

-- | 05 §3: "@attempts@ counts allocations, not candidates. A phase-2 miss increments nothing;
-- attempts+1 only when a real allocation ends in TIMEOUT / DRIVER_CANCELLED / PASSED_STOP / SEAT_LOST."
-- RouteChanged / SessionClosed releases are explicitly "without penalty" (05 §8.7, sibling clause).
countsTowardAttempts :: AllocationOutcome -> Bool
countsTowardAttempts = \case
  StandTimeout -> True
  MovingTimeout -> True
  DriverCancelled -> True
  PassedStop -> True
  SeatLost -> True
  RouteChanged -> False
  SessionClosed -> False

-- | 05 §8.4 literal: "@consecutiveMisses@ counts only DRIVER_CANCELLED and stand TIMEOUT."
countsTowardDriverMisses :: AllocationOutcome -> Bool
countsTowardDriverMisses = \case
  DriverCancelled -> True
  StandTimeout -> True
  _ -> False

-- | Engine tunables. Every field maps to a rider_config row of 05 §7 with the same default.
-- TODO(config pilot / rider_config task): read from rider_config once the fields land;
-- this record is the target shape so call sites never change.
data AllocationConfig = AllocationConfig
  { -- | drop candidates whose ETA to the rider's board stop exceeds this (05 §7 allocationWindowMin 8)
    allocationWindowSec :: Int,
    -- | rider "at the stop" radius; also the no-show criterion (05 §7 atStopRadiusM 100)
    atStopRadiusM :: Int,
    -- | deferred-allocation walk buffer (05 §7 walkBufferMin 1)
    walkBufferSec :: Int,
    -- | stationary-at-stand timer (05 §7 standTimerSec 180)
    standTimerSec :: Int,
    -- | post-arrival timer (05 §7 movingTimerSec 90)
    movingTimerSec :: Int,
    -- | after this many counted closes -> PRD R10 fallback surface (05 §7 maxAttempts 2)
    maxAttempts :: Int,
    -- | FINDING older than this -> R10 fallback irrespective of attempts (05 §7 fallbackAfterMin 10)
    fallbackAfterSec :: Int,
    -- | last cab leaves between search and book (05 §7 noCabGraceMin 2, decision 9 race)
    noCabGraceSec :: Int,
    -- | FINDING bookings system-cancel after this (05 §8.11 / §7 findingTimeoutMin 20);
    -- also the TTL for the attempts counter key
    findingTimeoutSec :: Int,
    -- | tick period (05 §7 tickSec 3)
    tickSec :: Int,
    -- | LTS freshness gate: drop cabs whose last position is older than this (05 §7 ltsMaxAgeSec 60, §3)
    ltsMaxAgeSec :: Int,
    -- | drop-stop passed: auto-end clock (05 §7 autoEndAfterDropMin 10) -- consumed by stop-progress tick (7.5+)
    autoEndAfterDropSec :: Int,
    -- | degraded boarding marker TTL (05 §7 degradedTimeoutMin 60) -- consumed by degraded close path
    degradedTimeoutSec :: Int
  }
  deriving (Show, Eq, Generic)

-- | 05 §7 defaults, change-management home is rider_config; do not tune these here.
defaultAllocationConfig :: AllocationConfig
defaultAllocationConfig =
  AllocationConfig
    { allocationWindowSec = 8 * 60,
      atStopRadiusM = 100,
      walkBufferSec = 60,
      standTimerSec = 180,
      movingTimerSec = 90,
      maxAttempts = 2,
      fallbackAfterSec = 10 * 60,
      noCabGraceSec = 2 * 60,
      findingTimeoutSec = 20 * 60,
      tickSec = 3,
      ltsMaxAgeSec = 60,
      autoEndAfterDropSec = 10 * 60,
      degradedTimeoutSec = 60 * 60
    }
