-- | Shared-cab allocation types: alloc key value, outcomes, blame rules and the tick's decisions.
module SharedLogic.SharedCab.Allocation.Types
  ( AllocationState (..),
    Timer (..),
    timerDeadline,
    allocationDeadline,
    clearOnMove,
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
    skipsPlateOnClose,
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

-- | The value stored under @sharedcab:alloc:{bookingId}@. Claimed while stationary -> stand or away timer; claimed while moving ->
-- no timer, and the tick clears a stand or away timer once the cab moves. @attempts@ mirrors @sharedcab:attempts:{bookingId}@, since this value is deleted on close.
data AllocationState = AllocationState
  { vehicleNumber :: Text, -- canonical plate of the cab holding the booking right now
    driverId :: Maybe Text, -- who drove it at claim (the session's driver, read under the plate lock): who a driver miss is charged to
    vehicleTripId :: Maybe Text, -- the session's trip at claim: where a driver miss is counted, even if the plate is on another trip by close time
    allocatedAt :: UTCTime,
    timer :: Timer,
    attempts :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | The armed timer and its deadline, as one value: a deadline always knows what it times out as.
data Timer = NoTimer | StandTimer UTCTime | MovingTimer UTCTime | AwayTimer UTCTime
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

timerDeadline :: Timer -> Maybe UTCTime
timerDeadline = \case
  NoTimer -> Nothing
  StandTimer d -> Just d
  MovingTimer d -> Just d
  AwayTimer d -> Just d

allocationDeadline :: AllocationState -> Maybe UTCTime
allocationDeadline = timerDeadline . (.timer)

-- | A cab seen moving no longer waits at a stop: its stand and away timers are cleared; the moving timer is not.
clearOnMove :: Timer -> Timer
clearOnMove = \case
  StandTimer _ -> NoTimer
  AwayTimer _ -> NoTimer
  other -> other

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

data Blame = BlameDriver | BlameRider | BlameNone
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | What an outcome means, in one place: adding an outcome forces one complete record, with no wildcard to swallow it.
data OutcomePolicy = OutcomePolicy
  { policyBlame :: Blame,
    -- | @attempts@ counts allocations, not candidates: a phase-2 miss adds nothing; RouteChanged / SessionClosed are without penalty.
    countsAttempt :: Bool,
    countsDriverMiss :: Bool,
    -- | A cab the rider failed to board is skipped for this booking too, or its stand timer would charge a second no-show.
    skipsPlate :: Bool,
    wireName :: Text
  }

-- | Blame: driver cancels are the driver's miss; the stand timer running out (the cab waited) and the cab passing the stop with the
-- rider away are the rider's no-show; passing a waiting rider is the driver's; capacity eviction and session closes are nobody's.
-- MovingTimeout is BlameRider; change only here if product wants it neutral.
outcomePolicy :: AllocationOutcome -> OutcomePolicy
outcomePolicy = \case
  StandTimeout -> OutcomePolicy BlameRider True False True "STAND_TIMEOUT"
  MovingTimeout -> OutcomePolicy BlameRider True False True "MOVING_TIMEOUT"
  AwayTimeout -> OutcomePolicy BlameNone True False False "AWAY_TIMEOUT"
  DriverCancelled -> OutcomePolicy BlameDriver True True False "DRIVER_CANCELLED"
  PassedStop blame -> OutcomePolicy blame True False False "PASSED_STOP"
  SeatLost -> OutcomePolicy BlameNone True False False "SEAT_LOST"
  RouteChanged -> OutcomePolicy BlameNone False False False "ROUTE_CHANGED"
  SessionClosed -> OutcomePolicy BlameNone False False False "SESSION_CLOSED"
  TimerLost -> OutcomePolicy BlameNone False False False "TIMER_LOST"
  CabSilent -> OutcomePolicy BlameNone False False False "CAB_SILENT"
  RiderSkipped SkipFull -> OutcomePolicy BlameNone False False False "RIDER_SKIPPED"
  RiderSkipped SkipOther -> OutcomePolicy BlameRider True False False "RIDER_SKIPPED"

blameFor :: AllocationOutcome -> Blame
blameFor = policyBlame . outcomePolicy

outcomeText :: AllocationOutcome -> Text
outcomeText = wireName . outcomePolicy

countsTowardAttempts, countsTowardDriverMisses, skipsPlateOnClose :: AllocationOutcome -> Bool
countsTowardAttempts = countsAttempt . outcomePolicy
countsTowardDriverMisses = countsDriverMiss . outcomePolicy
skipsPlateOnClose = skipsPlate . outcomePolicy

timerExpiry :: UTCTime -> Maybe AllocationState -> Maybe AllocationOutcome
timerExpiry _ Nothing = Just TimerLost
timerExpiry now (Just st) = case st.timer of
  StandTimer d | now > d -> Just StandTimeout
  MovingTimer d | now > d -> Just MovingTimeout
  AwayTimer d | now > d -> Just AwayTimeout
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
