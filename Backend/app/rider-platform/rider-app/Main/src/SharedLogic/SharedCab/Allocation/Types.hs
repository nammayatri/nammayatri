-- | Shared-cab allocation types: alloc key value, outcomes, blame rules and the tick's decisions.
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
    allocKeyTtl,
    allocKeyTtlMarginSec,
    allocKeyTtlShort,
    isMovingSpeed,
    parseLtsTimestamp,
    Blame (..),
    blameFor,
    outcomeText,
    countsTowardAttempts,
    countsTowardDriverMisses,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import SharedLogic.SharedCab.Config (SharedCabTunables)

-- | The value stored under @sharedcab:alloc:{bookingId}@. Claimed while stationary -> stand timer; claimed while moving -> no
-- expiresAt, and the tick clears a stand timer once the cab moves. @attempts@ mirrors @sharedcab:attempts:{bookingId}@, since this value is deleted on close.
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

data TimerKind = StandTimer | MovingTimer | AwayTimer
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AllocationOutcome
  = -- | stand timer expired: cab waited standTimerSec and the booking was still open
    StandTimeout
  | -- | moving timer expired after the cab reached the board stop
    MovingTimeout
  | -- | the bounded wait for a stationary cab away from the stop ran out: nobody's fault, and the cab stays eligible
    AwayTimeout
  | -- | driver cancelled the allocation
    DriverCancelled
  | -- | tick saw the cab pass the board stop with the booking unboarded; blame per passedStopBlame
    PassedStop Blame
  | -- | capacity guard evicted the latest unboarded allocation (walk-ups win)
    SeatLost
  | -- | cab route-changed / queued route no longer serves the board stop
    RouteChanged
  | -- | session went PAUSED/ENDED: release without penalty
    SessionClosed
  | -- | the alloc key vanished before any release (crash after the CAS, or ticks missed past the key's TTL)
    TimerLost
  | -- | the allocated cab sent no LTS fix for silentReleaseMult x ltsMaxAgeSec; nobody's fault
    CabSilent
  | -- | the rider skipped the cab; that plate is then excluded from the booking's next claims
    RiderSkipped SkipReason
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SkipReason = SkipFull | SkipOther
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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

data Blame = BlameDriver | BlameRider | BlameNone
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Blame: driver cancels are the driver's miss; the stand timer running out (the cab waited) and the cab passing the stop with the
-- rider away are the rider's no-show; passing a waiting rider is the driver's; capacity eviction and session closes are nobody's.
-- MovingTimeout is BlameRider; change only here if product wants it neutral.
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

-- | @attempts@ counts allocations, not candidates: a phase-2 miss adds nothing; RouteChanged / SessionClosed are without penalty.
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

countsTowardDriverMisses :: AllocationOutcome -> Bool
countsTowardDriverMisses = \case
  DriverCancelled -> True
  StandTimeout -> False
  MovingTimeout -> False
  AwayTimeout -> False
  PassedStop _ -> False
  SeatLost -> False
  RouteChanged -> False
  SessionClosed -> False
  TimerLost -> False
  CabSilent -> False
  RiderSkipped _ -> False

timerExpiry :: UTCTime -> Maybe AllocationState -> Maybe AllocationOutcome
timerExpiry _ Nothing = Just TimerLost
timerExpiry now (Just st) = case st.expiresAt of
  Just deadline | now > deadline -> Just $ case st.timerKind of
    StandTimer -> StandTimeout
    MovingTimer -> MovingTimeout
    AwayTimer -> AwayTimeout
  _ -> Nothing

isMovingSpeed :: Maybe Double -> Bool
isMovingSpeed = maybe False (> 1.0)

-- | LTS writes RFC 3339; aeson's UTCTime parser also takes offsets and a space separator, with bare epoch seconds as a fallback.
parseLtsTimestamp :: Text -> Maybe UTCTime
parseLtsTimestamp ts =
  A.parseMaybe A.parseJSON (A.String ts) <|> (posixSecondsToUTCTime . fromInteger <$> readMaybe (T.unpack ts))

data RiderFix = RiderFix
  { position :: LatLong,
    takenAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A fresh fix within `radiusM` of the stop means the rider was waiting (driver's miss); a fresh fix elsewhere, the rider's; none or stale, nobody's.
passedStopBlame :: Int -> Int -> UTCTime -> LatLong -> Maybe RiderFix -> Blame
passedStopBlame radiusM maxAgeSec now stop = \case
  Just riderFix
    | diffUTCTime now riderFix.takenAt <= fromIntegral maxAgeSec ->
      if distanceBetweenInMeters riderFix.position stop <= fromIntegral radiusM then BlameDriver else BlameRider
  _ -> BlameNone

data FindingTimeout = KeepFinding | CancelNoCab
  deriving (Show, Eq)

-- | Cancelled once the current FINDING stint has run findingTimeoutSec; total age is capped at `totalAgeMult` stints so a parked cab
-- that keeps timing out cannot hold the rider forever. Full refund unless a no-show is booked.
findingTimeoutAction :: UTCTime -> Int -> UTCTime -> UTCTime -> FindingTimeout
findingTimeoutAction now findingTimeoutSec findingSince createdAt
  | diffUTCTime now findingSince > fromIntegral findingTimeoutSec = CancelNoCab
  | diffUTCTime now createdAt > fromIntegral (totalAgeMult * findingTimeoutSec) = CancelNoCab
  | otherwise = KeepFinding

totalAgeMult :: Int
totalAgeMult = 3

-- | The TTL every armer of `sharedcab:alloc:{bookingId}` must use: the stand and moving timers stack under one key, so a TTL below
-- their sum closes as TimerLost (no attempt) and the no-show path is unreachable. Only a GC backstop, so widening is safe.
allocKeyTtl :: SharedCabTunables -> Int
allocKeyTtl cfg = max cfg.findingTimeoutSec (cfg.standTimerSec + cfg.movingTimerSec + allocKeyTtlMarginSec)

allocKeyTtlMarginSec :: Int
allocKeyTtlMarginSec = 10

allocKeyTtlShort :: SharedCabTunables -> Bool
allocKeyTtlShort cfg = cfg.findingTimeoutSec < cfg.standTimerSec + cfg.movingTimerSec + allocKeyTtlMarginSec
