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
    TimerKind (..),
    AllocationOutcome (..),
    SkipReason (..),
    RiderFix (..),
    passedStopBlame,
    timerExpiry,
    FindingTimeout (..),
    findingTimeoutAction,
    totalAgeMult,
    isMovingSpeed,
    parseLtsTimestamp,
    Blame (..),
    blameFor,
    outcomeText,
    countsTowardAttempts,
    countsTowardDriverMisses,
    AllocationConfig (..),
    defaultAllocationConfig,
  )
where

import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

-- | The value stored under Redis key @sharedcab:alloc:{bookingId}@ (05 §2).
-- Field names match the plan JSON exactly: {vehicleNumber, allocatedAt, expiresAt, attempts}.
--
--   * Timer mode follows the cab, not the stop (05 §2 "Timer mode"): claimed while stationary ->
--     stand timer, expiresAt = allocatedAt + standTimerSec (the cab must start moving); claimed while
--     moving -> expiresAt = Nothing, and the tick clears a stand timer once the cab moves.
--   * expiresAt is reset to <now> + movingTimerSec once the tick sees the cab at the board stop
--     (moving timer, PRD §11 "90 s after arriving"); Nothing until the first timer is armed.
--   * attempts mirrors the lifetime counter kept under sharedcab:attempts:{bookingId}
--     (see SharedLogic.SharedCab.Allocation.attemptsKey -- the alloc value is deleted on close,
--     so the counter cannot live here alone across successive allocations).
data AllocationState = AllocationState
  { vehicleNumber :: Text, -- canonical plate of the cab holding the booking right now
    driverId :: Maybe Text, -- who drove it at claim (the session's driver, read under the plate lock): who a driver miss is charged to
    vehicleTripId :: Maybe Text, -- the session's trip at claim: where a driver miss is counted, even if the plate is on another trip by close time
    allocatedAt :: UTCTime,
    expiresAt :: Maybe UTCTime,
    attempts :: Int,
    timerKind :: TimerKind -- which of the two timers expiresAt belongs to
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | AwayTimer is the bounded wait of a stationary cab still away from the board stop: it is nobody's miss, unlike the
-- stand timer of a cab that sat at the stop and never left.
data TimerKind = StandTimer | MovingTimer | AwayTimer
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Why a live allocation ended before boarding (05 §3 release reasons).
-- These map 1:1 onto the @outcome@ field of the @allocation_closed@ event (05 §7).
data AllocationOutcome
  = -- | stand timer expired: cab waited standTimerSec and the booking was still open (05 §3, §8.4)
    StandTimeout
  | -- | moving timer expired after the cab reached the board stop (05 §2, §3)
    MovingTimeout
  | -- | the bounded wait for a stationary cab away from the stop ran out: nobody's fault, and the cab stays eligible
    AwayTimeout
  | -- | driver cancelled the allocation (05 §3)
    DriverCancelled
  | -- | tick saw the cab pass the board stop with the booking unboarded (05 §6 item 2); blame per passedStopBlame (R15)
    PassedStop Blame
  | -- | capacity guard evicted the latest unboarded allocation (05 §3, §8.7 walk-ups win)
    SeatLost
  | -- | cab route-changed / queued route no longer serves the board stop (05 §11 "Cab changes route")
    RouteChanged
  | -- | session went PAUSED/ENDED: release without penalty (05 §8.7)
    SessionClosed
  | -- | the alloc key vanished before any release (crash after the CAS, or ticks missed past the key's TTL)
    TimerLost
  | -- | R38: the allocated cab sent no LTS fix for silentReleaseMult x ltsMaxAgeSec; nobody's fault
    CabSilent
  | -- | the rider skipped the cab (R19); that plate is then excluded from the booking's next claims
    RiderSkipped SkipReason
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | R19: a full cab is nobody's fault; any other skip is the rider's.
data SkipReason = SkipFull | SkipOther
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | The @outcome@ field of the @allocation_closed@ event (05 §7).
outcomeText :: AllocationOutcome -> Text
outcomeText = \case
  StandTimeout -> "STAND_TIMEOUT"
  MovingTimeout -> "MOVING_TIMEOUT"
  AwayTimeout -> "AWAY_TIMEOUT"
  DriverCancelled -> "DRIVER_CANCELLED"
  PassedStop _ -> "PASSED_STOP"
  SeatLost -> "SEAT_LOST"
  RouteChanged -> "ROUTE_CHANGED"
  SessionClosed -> "SESSION_CLOSED"
  TimerLost -> "TIMER_LOST"
  CabSilent -> "CAB_SILENT"
  RiderSkipped _ -> "RIDER_SKIPPED"

-- | @blame@ field of the @allocation_closed@ event (05 §7) and the foundation for
-- §8.4: "Timers blame the right party."
data Blame = BlameDriver | BlameRider | BlameNone
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | 05 §8.4 blame rules:
--   * driver cancels are the driver's miss; cab time running out at the stand (the cab waited, the rider never boarded)
--     is the RIDER's no-show -- user decision 2026-09-30 (R75), which reverses the plan's "stand TIMEOUT is the driver's";
--   * the cab reaching/passing the stop with the rider not there is the rider's no-show; passing a rider
--     who was waiting at the stop is the driver's miss (R15, passedStopBlame);
--   * capacity-guard eviction and session lifecycle closes are nobody's fault.
--
-- RANKED QUESTION (report Q5): PRD §11 treats the 90 s moving timer as "rider didn't board" --
-- MovingTimeout is mapped to BlameRider. If product wants MovingTimeout neutral, change only here.
blameFor :: AllocationOutcome -> Blame
blameFor = \case
  StandTimeout -> BlameRider
  DriverCancelled -> BlameDriver
  MovingTimeout -> BlameRider
  AwayTimeout -> BlameNone
  PassedStop blame -> blame
  SeatLost -> BlameNone
  RouteChanged -> BlameNone
  SessionClosed -> BlameNone
  TimerLost -> BlameNone
  CabSilent -> BlameNone
  RiderSkipped SkipFull -> BlameNone
  RiderSkipped SkipOther -> BlameRider

-- | 05 §3: "@attempts@ counts allocations, not candidates. A phase-2 miss increments nothing;
-- attempts+1 only when a real allocation ends in TIMEOUT / DRIVER_CANCELLED / PASSED_STOP / SEAT_LOST."
-- RouteChanged / SessionClosed releases are explicitly "without penalty" (05 §8.7, sibling clause).
countsTowardAttempts :: AllocationOutcome -> Bool
countsTowardAttempts = \case
  StandTimeout -> True
  MovingTimeout -> True
  AwayTimeout -> True
  DriverCancelled -> True
  PassedStop _ -> True
  SeatLost -> True
  RouteChanged -> False
  SessionClosed -> False
  TimerLost -> False
  CabSilent -> False
  RiderSkipped SkipFull -> False
  RiderSkipped SkipOther -> True

-- | 05 §8.4, amended by user decision 2026-09-30 (R75): "@consecutiveMisses@ counts only DRIVER_CANCELLED"; a stand
-- timeout is the rider's no-show now.
countsTowardDriverMisses :: AllocationOutcome -> Bool
countsTowardDriverMisses = \case
  DriverCancelled -> True
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
    -- | a booking whose rider no-shows reach this many is cancelled, not reallocated (R54)
    maxNoShows :: Int,
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
      maxNoShows = 2,
      fallbackAfterSec = 10 * 60,
      noCabGraceSec = 2 * 60,
      findingTimeoutSec = 20 * 60,
      tickSec = 3,
      ltsMaxAgeSec = 60,
      autoEndAfterDropSec = 10 * 60,
      degradedTimeoutSec = 60 * 60
    }

-- | The tick's timer decision for one ALLOCATED booking, given its alloc key (05 §2, §3).
timerExpiry :: UTCTime -> Maybe AllocationState -> Maybe AllocationOutcome
timerExpiry _ Nothing = Just TimerLost
timerExpiry now (Just st) = case st.expiresAt of
  Just deadline | now > deadline -> Just $ case st.timerKind of
    StandTimer -> StandTimeout
    MovingTimer -> MovingTimeout
    AwayTimer -> AwayTimeout
  _ -> Nothing

-- | Moving per the cab's latest LTS speed (m/s); no speed reads as stationary, which arms the
-- stand timer -- the conservative side.
isMovingSpeed :: Maybe Double -> Bool
isMovingSpeed = maybe False (> 1.0)

-- | LTS writes chrono DateTime<Utc> as RFC 3339 (`2026-09-25T10:15:30.123456789Z`); aeson's UTCTime
-- parser also takes offsets and a space separator, and bare epoch seconds are accepted as a fallback.
parseLtsTimestamp :: Text -> Maybe UTCTime
parseLtsTimestamp ts =
  maybe (posixSecondsToUTCTime . fromInteger <$> readMaybe (T.unpack ts)) Just (A.parseMaybe A.parseJSON (A.String ts))

-- | The rider's last known position and when it was taken (the journey's rider-location stream).
data RiderFix = RiderFix
  { position :: LatLong,
    takenAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | R15, when the cab passed the board stop unboarded: a fresh fix within `radiusM` of the stop means the
-- rider was waiting and the driver skipped them; a fresh fix elsewhere means the rider wasn't there; no fix
-- or a stale one blames nobody.
passedStopBlame :: Int -> Int -> UTCTime -> LatLong -> Maybe RiderFix -> Blame
passedStopBlame radiusM maxAgeSec now stop = \case
  Just riderFix
    | diffUTCTime now riderFix.takenAt <= fromIntegral maxAgeSec ->
      if distanceBetweenInMeters riderFix.position stop <= fromIntegral radiusM then BlameDriver else BlameRider
  _ -> BlameNone

-- | R63 (05 §8.11): what the tick does with a booking it reads as FINDING (CONFIRMED, no cab, every ticket ACTIVE).
data FindingTimeout = KeepFinding | CancelNoCab
  deriving (Show, Eq)

-- | A FINDING booking is cancelled once its current FINDING stint has run findingTimeoutSec (the findingSince clock, reset by
-- every release, the same one fallbackAfterSec uses), so a booking a cab released late (a no-show, a driver cancel) still
-- gets the reallocation R54 promises. A stint clock alone would let a parked cab that keeps timing out hold the rider forever
-- (attempts and the fallback push do not cancel), so the total age since creation is capped at `totalAgeMult` stints.
-- A system cancel with a full refund (R54: no cab took the rider) unless a no-show is booked (findingTimeoutRefund).
findingTimeoutAction :: UTCTime -> Int -> UTCTime -> UTCTime -> FindingTimeout
findingTimeoutAction now findingTimeoutSec findingSince createdAt
  | diffUTCTime now findingSince > fromIntegral findingTimeoutSec = CancelNoCab
  | diffUTCTime now createdAt > fromIntegral (totalAgeMult * findingTimeoutSec) = CancelNoCab
  | otherwise = KeepFinding

-- | How many findingTimeoutSec stints a booking may live in FINDING in total, releases included.
totalAgeMult :: Int
totalAgeMult = 3
