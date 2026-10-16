-- | Pure allocation decisions: who is eligible, in what order, and which timer a claim arms.
module SharedLogic.SharedCab.Allocation.Select
  ( FindingBooking (..),
    findingOf,
    allocatedPlate,
    closable,
    claimable,
    RankedCandidate (..),
    isFreshPosition,
    eligibleCandidates,
    standTimerOnClaim,
    claimTimerSec,
    claimTimerKind,
    clearsWhenMoving,
    ClaimPush (..),
    claimPush,
    silentReleaseMult,
    silentCab,
    movingCab,
    rankCandidates,
    planRouteAllocation,
    withoutSkipped,
    isSkipped,
    skippedWhileFinding,
    ticketsCountedAtConfirm,
    crossedMaxAttempts,
    groupAllOn,
  )
where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.Person as DP
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (diffUTCTime)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Allocation.Types
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.LegState (fallbackReached)
import SharedLogic.SharedCab.Plate (findByPlate, samePlate)
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))

data FindingBooking = FindingBooking
  { bookingId :: Id DFTB.FRFSTicketBooking,
    riderId :: Id DP.Person,
    routeCode :: Text,
    boardStopCode :: Text, -- == frfs_ticket_booking.fromStationCode
    dropStopCode :: Text, -- == frfs_ticket_booking.toStationCode
    seats :: Int, -- number of ticket rows (quantity = seats); 1-4
    findingSince :: UTCTime -- drives fallbackAfterSec / findingTimeoutSec
  }
  deriving (Show, Eq, Generic)

findingOf :: (DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus]) -> Maybe FindingBooking
findingOf (b, statuses) = do
  guard (isNothing b.vehicleNumber)
  route <- b.routeCode
  let seatCount = length (filter (== DFRFSTicket.ACTIVE) statuses)
  guard (seatCount > 0)
  pure
    FindingBooking
      { bookingId = b.id,
        riderId = b.riderId,
        routeCode = route,
        boardStopCode = b.fromStationCode,
        dropStopCode = b.toStationCode,
        seats = seatCount,
        findingSince = b.createdAt
      }

allocatedPlate :: (DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus]) -> Maybe Text
allocatedPlate (b, statuses) = plateIfUnboarded b.vehicleNumber statuses

plateIfUnboarded :: Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Maybe Text
plateIfUnboarded mbPlate statuses = do
  plate <- mbPlate
  guard (not (null statuses) && all (== DFRFSTicket.ACTIVE) statuses)
  pure plate

-- | The under-lock gate of every close: still live, still holding the plate the closer believes, nobody boarded since.
closable :: Text -> FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool
closable expectedPlate status mbPlate statuses =
  status == CONFIRMED && plateIfUnboarded mbPlate statuses == Just expectedPlate

-- | The under-lock gate of a claim: still live, no cab, nobody boarded, no degrade marker (a degraded boarding can land after the tick read FINDING).
claimable :: FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool -> Bool
claimable status mbPlate statuses markerAlive =
  status == CONFIRMED && isNothing mbPlate && not (null statuses) && all (== DFRFSTicket.ACTIVE) statuses && not markerAlive

data RankedCandidate = RankedCandidate
  { rcSession :: Session,
    rcEtaToBoardStopSec :: Int,
    rcUpcomingStop :: LT.UpcomingStop,
    rcMoving :: Bool, -- per its fresh LTS position: decides the claim's timer mode
    rcAtStop :: Bool -- within atStopRadiusM of the board stop: only then is a stationary cab waiting for the rider
  }
  deriving (Show, Generic)

-- | Freshness gate: LTS never drops a silent cab by age, so anything older than the gate is dropped. A missing or
-- unparseable timestamp is NOT fresh. TODO: the city-wide LTS outage rule must suspend this gate.
isFreshPosition :: UTCTime -> Int -> LT.VehicleInfo -> Bool
isFreshPosition now maxAgeSec vi = case vi.timestamp >>= parseLtsTimestamp of
  Nothing -> False -- silent-by-absence must not read as fresh
  Just ts -> let age = diffUTCTime now ts in 0 <= age && age <= fromIntegral maxAgeSec

-- | Cab not past the board stop (LTS keeps the stop Upcoming while the cab stands at it), an optimistic seat screen
-- (Phase 2 recounts under the lock), and ETA within allocationWindowSec.
eligibleCandidates ::
  UTCTime ->
  Config.SharedCabTunables ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
eligibleCandidates now cfg tracking sessions booking =
  [ RankedCandidate {rcSession = s, rcEtaToBoardStopSec = etaSec, rcUpcomingStop = stop, rcMoving = isMovingSpeed veh.vehicleInfo.speed, rcAtStop = distanceBetweenInMeters (LatLong veh.vehicleInfo.latitude veh.vehicleInfo.longitude) stop.stop.coordinate <= fromIntegral cfg.atStopRadiusM}
    | s <- sessions,
      s.status == ACTIVE,
      queuedRouteStillServes s booking.boardStopCode,
      s.capacity - s.walkupCount >= booking.seats,
      Just veh <- [findByPlate (.vehicleNumber) s.vehicleNumber tracking],
      isFreshPosition now cfg.ltsMaxAgeSec veh.vehicleInfo,
      Just stop <- [find (\u -> u.stop.stopCode == booking.boardStopCode && u.status == LT.Upcoming) =<< veh.vehicleInfo.upcomingStops],
      let etaSec = max 0 (floor (diffUTCTime stop.eta now)),
      etaSec <= cfg.allocationWindowSec
  ]

standTimerOnClaim :: RankedCandidate -> Bool
standTimerOnClaim c = not c.rcMoving && c.rcAtStop

-- | The timer a claim arms: a moving cab none, a stationary cab at the stop the stand timer, a stationary cab away from the stop
-- a bounded wait (allocationWindowSec), so a parked cab that keeps pinging cannot hold the rider until the key's TTL.
claimTimerSec :: Config.SharedCabTunables -> RankedCandidate -> Maybe Int
claimTimerSec cfg c
  | c.rcMoving = Nothing
  | c.rcAtStop = Just cfg.standTimerSec
  | otherwise = Just cfg.allocationWindowSec

claimTimerKind :: RankedCandidate -> TimerKind
claimTimerKind c
  | c.rcMoving || c.rcAtStop = StandTimer
  | otherwise = AwayTimer

clearsWhenMoving :: TimerKind -> Bool
clearsWhenMoving = (/= MovingTimer)

data ClaimPush = PushArriving | PushAssigned
  deriving (Show, Eq)

claimPush :: RankedCandidate -> ClaimPush
claimPush c = if standTimerOnClaim c then PushArriving else PushAssigned

silentReleaseMult :: Int
silentReleaseMult = 5

-- | Silent for silentReleaseMult x ltsMaxAgeSec while the route's other cabs report; if nobody is fresh it is an LTS outage and decides nothing.
silentCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
silentCab now maxAgeSec plate tracking =
  any (isFreshPosition now maxAgeSec . (.vehicleInfo)) tracking
    && maybe True (not . isFreshPosition now (silentReleaseMult * maxAgeSec) . (.vehicleInfo)) (findByPlate (.vehicleNumber) plate tracking)

-- | TODO: a queued route must still serve the board stop; needs its stop list from OTPRest. Tolerant for one tick (Phase 2 re-reads).
queuedRouteStillServes :: Session -> Text -> Bool
queuedRouteStillServes _ _ = True

rankCandidates :: [RankedCandidate] -> [RankedCandidate]
rankCandidates = sortOn rcEtaToBoardStopSec

planRouteAllocation ::
  UTCTime ->
  Config.SharedCabTunables ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
planRouteAllocation now cfg tracking sessions booking =
  rankCandidates (eligibleCandidates now cfg tracking sessions booking)

withoutSkipped :: [Text] -> [RankedCandidate] -> [RankedCandidate]
withoutSkipped skipped = filter (not . (`isSkipped` skipped) . (.rcSession.vehicleNumber))

isSkipped :: Text -> [Text] -> Bool
isSkipped = elem

skippedWhileFinding :: Config.SharedCabTunables -> UTCTime -> Int -> UTCTime -> [Text] -> [Text]
skippedWhileFinding cfg now attempts findingSince skipped
  | fallbackReached now cfg.maxAttempts attempts findingSince cfg.fallbackAfterSec = []
  | otherwise = skipped

ticketsCountedAtConfirm :: Maybe parent -> Bool
ticketsCountedAtConfirm = isNothing

crossedMaxAttempts :: Int -> Int -> Int -> Bool
crossedMaxAttempts maxAttempts before now = before < maxAttempts && now >= maxAttempts

groupAllOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupAllOn f = map (\grp -> (f (NE.head grp), NE.toList grp)) . NE.groupBy ((==) `on` f) . sortOn f

-- | A fresh, moving report for the plate: its stand timer is cleared rather than expired.
movingCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
movingCab now maxAgeSec plate =
  any (\vt -> samePlate vt.vehicleNumber plate && isFreshPosition now maxAgeSec vt.vehicleInfo && isMovingSpeed vt.vehicleInfo.speed)
