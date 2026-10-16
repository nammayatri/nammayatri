-- | Pure allocation decisions: who is eligible, in what order, and which timer a claim arms.
module SharedLogic.SharedCab.Allocation.Select
  ( FindingBooking (..),
    LiveBooking (..),
    Phase (..),
    phase,
    liveBookingPhase,
    findingOf,
    isFinding,
    allocatedPlate,
    closable,
    claimable,
    RankedCandidate (..),
    CabStance (..),
    isFreshPosition,
    eligibleCandidates,
    etaPlausible,
    claimTimer,
    ClaimPush (..),
    claimPush,
    silentReleaseMult,
    silentCab,
    movingCab,
    withoutSkipped,
    skippedWhileFinding,
    crossedMaxAttempts,
    groupAllOn,
    isRescheduledChild,
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
import Kernel.Utils.Common (addUTCTime, diffUTCTime)
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
    seats :: Int, -- the party's size: every rider booked together needs a seat in the same cab
    findingSince :: UTCTime -- drives fallbackAfterSec / findingTimeoutSec
  }
  deriving (Show, Eq, Generic)

-- | A CONFIRMED shared-cab booking with the status of each of its tickets.
data LiveBooking = LiveBooking
  { booking :: DFTB.FRFSTicketBooking,
    ticketStatuses :: [DFRFSTicket.FRFSTicketStatus]
  }

-- | Where a booking stands: needing a cab, holding one nobody has boarded, or neither (boarded, cancelled, no live ticket).
data Phase = Finding | AllocatedTo Text | NotWaiting
  deriving (Show, Eq)

-- | The one definition of the phase; every gate below reads it. A cab holds a booking only while every ticket is still ACTIVE.
phase :: FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Phase
phase status mbPlate statuses
  | status /= CONFIRMED = NotWaiting
  | otherwise = case mbPlate of
    Nothing | DFRFSTicket.ACTIVE `elem` statuses -> Finding
    Just plate | all (== DFRFSTicket.ACTIVE) statuses, not (null statuses) -> AllocatedTo plate
    _ -> NotWaiting

liveBookingPhase :: LiveBooking -> Phase
liveBookingPhase lb = phase lb.booking.status lb.booking.vehicleNumber lb.ticketStatuses

-- | A live booking the tick should find a cab for: it also needs a route to look on.
isFinding :: LiveBooking -> Bool
isFinding lb = liveBookingPhase lb == Finding && isJust lb.booking.routeCode

findingOf :: (DFTB.FRFSTicketBooking -> Int) -> LiveBooking -> Maybe FindingBooking
findingOf seatsOf lb = do
  guard (isFinding lb)
  let b = lb.booking
  route <- b.routeCode
  pure
    FindingBooking
      { bookingId = b.id,
        riderId = b.riderId,
        routeCode = route,
        boardStopCode = b.fromStationCode,
        dropStopCode = b.toStationCode,
        seats = seatsOf b,
        findingSince = b.createdAt
      }

allocatedPlate :: LiveBooking -> Maybe Text
allocatedPlate lb = case liveBookingPhase lb of
  AllocatedTo plate -> Just plate
  Finding -> Nothing
  NotWaiting -> Nothing

-- | The under-lock gate of every close: still live, still holding the plate the closer believes, nobody boarded since.
closable :: Text -> FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool
closable expectedPlate status mbPlate statuses = phase status mbPlate statuses == AllocatedTo expectedPlate

-- | The under-lock gate of a claim: still live, no cab, nobody boarded, no degrade marker (a degraded boarding can land after the tick read FINDING).
claimable :: FRFSTicketBookingStatus -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Bool -> Bool
claimable status mbPlate statuses markerAlive = phase status mbPlate statuses == Finding && all (== DFRFSTicket.ACTIVE) statuses && not markerAlive

data RankedCandidate = RankedCandidate
  { rcSession :: Session,
    rcEtaToBoardStopSec :: Int,
    rcUpcomingStop :: LT.UpcomingStop,
    rcStance :: CabStance
  }
  deriving (Show, Generic)

-- | Per the cab's fresh LTS position; decides the claim's timer. Only a stationary cab within atStopRadiusM of the board stop is waiting for the rider.
data CabStance = CabMoving | CabAtStop | CabAway
  deriving (Show, Eq)

cabStance :: Config.SharedCabTunables -> LT.VehicleInfo -> LT.UpcomingStop -> CabStance
cabStance cfg vi stop
  | isMovingSpeed vi.speed = CabMoving
  | distanceBetweenInMeters (LatLong vi.latitude vi.longitude) stop.stop.coordinate <= fromIntegral cfg.atStopRadiusM = CabAtStop
  | otherwise = CabAway

-- | Freshness gate: LTS never drops a silent cab by age, so anything older than the gate is dropped. A missing or
-- unparseable timestamp is NOT fresh. Known limitation: a city-wide LTS outage is not told apart from every cab being stale.
isFreshPosition :: UTCTime -> Int -> LT.VehicleInfo -> Bool
isFreshPosition now maxAgeSec vi = case vi.timestamp >>= parseLtsTimestamp of
  Nothing -> False -- silent-by-absence must not read as fresh
  Just ts -> let age = diffUTCTime now ts in 0 <= age && age <= fromIntegral maxAgeSec

-- | Nearest first. A cab not past the board stop (LTS keeps the stop Upcoming while the cab stands at it), an optimistic seat screen
-- (Phase 2 recounts under the lock), and ETA within allocationWindowSec.
eligibleCandidates ::
  UTCTime ->
  Config.SharedCabTunables ->
  [LT.VehicleTrackingOnRouteResp] ->
  [Session] ->
  FindingBooking ->
  [RankedCandidate]
eligibleCandidates now cfg tracking sessions booking =
  sortOn
    rcEtaToBoardStopSec
    [ RankedCandidate {rcSession = s, rcEtaToBoardStopSec = etaSec, rcUpcomingStop = stop, rcStance = cabStance cfg veh.vehicleInfo stop}
      | s <- sessions,
        s.status == ACTIVE,
        s.capacity - s.walkupCount >= booking.seats,
        Just veh <- [findByPlate (.vehicleNumber) s.vehicleNumber tracking],
        isFreshPosition now cfg.ltsMaxAgeSec veh.vehicleInfo,
        Just stop <- [find (\u -> u.stop.stopCode == booking.boardStopCode && u.status == LT.Upcoming) =<< veh.vehicleInfo.upcomingStops],
        let etaSec = max 0 (floor (diffUTCTime stop.eta now)),
        etaSec <= cfg.allocationWindowSec,
        etaPlausible cfg etaSec (LatLong veh.vehicleInfo.latitude veh.vehicleInfo.longitude) stop.stop.coordinate
    ]

-- | LTS snaps any cab, however far off the route, to the nearest route point and reports a static ETA from there, so a parked cab
-- 10 km away can read "1 minute". A candidate must be able to cover the straight-line distance to the stop in its ETA at
-- 'maxCabSpeedMps', plus the at-stop radius (a cab at the stop has ETA 0).
etaPlausible :: Config.SharedCabTunables -> Int -> LatLong -> LatLong -> Bool
etaPlausible cfg etaSec cab stopPoint =
  distanceBetweenInMeters cab stopPoint <= fromIntegral (etaSec * cfg.maxCabSpeedMps + cfg.atStopRadiusM)

-- | The timer a claim arms: a moving cab none, a stationary cab at the stop the stand timer, a stationary cab away from the stop
-- a bounded wait (allocationWindowSec), so a parked cab that keeps pinging cannot hold the rider until the key's TTL.
claimTimer :: Config.SharedCabTunables -> UTCTime -> RankedCandidate -> Timer
claimTimer cfg now c = case c.rcStance of
  CabMoving -> NoTimer
  CabAtStop -> StandTimer (after cfg.standTimerSec)
  CabAway -> AwayTimer (after cfg.allocationWindowSec)
  where
    after sec = addUTCTime (intToNominalDiffTime sec) now

data ClaimPush = PushArriving | PushAssigned
  deriving (Show, Eq)

claimPush :: RankedCandidate -> ClaimPush
claimPush c = case c.rcStance of
  CabAtStop -> PushArriving
  CabMoving -> PushAssigned
  CabAway -> PushAssigned

silentReleaseMult :: Int
silentReleaseMult = 5

-- | Silent for silentReleaseMult x ltsMaxAgeSec while the route's other cabs report; if nobody is fresh it is an LTS outage and decides nothing.
silentCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
silentCab now maxAgeSec plate tracking =
  any (isFreshPosition now maxAgeSec . (.vehicleInfo)) tracking
    && maybe True (not . isFreshPosition now (silentReleaseMult * maxAgeSec) . (.vehicleInfo)) (findByPlate (.vehicleNumber) plate tracking)

withoutSkipped :: [Text] -> [RankedCandidate] -> [RankedCandidate]
withoutSkipped skipped = filter ((`notElem` skipped) . (.rcSession.vehicleNumber))

skippedWhileFinding :: Config.SharedCabTunables -> UTCTime -> Int -> UTCTime -> [Text] -> [Text]
skippedWhileFinding cfg now attempts findingSince skipped
  | fallbackReached now cfg.maxAttempts attempts findingSince cfg.fallbackAfterSec = []
  | otherwise = skipped

-- | A rescheduled booking is a child of the one it replaced; the parent already counted the rider's tickets.
isRescheduledChild :: Maybe parent -> Bool
isRescheduledChild = isJust

crossedMaxAttempts :: Int -> Int -> Int -> Bool
crossedMaxAttempts maxAttempts before now = before < maxAttempts && now >= maxAttempts

groupAllOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupAllOn f = map (\grp -> (f (NE.head grp), NE.toList grp)) . NE.groupBy ((==) `on` f) . sortOn f

-- | A fresh, moving report for the plate: its stand timer is cleared rather than expired.
movingCab :: UTCTime -> Int -> Text -> [LT.VehicleTrackingOnRouteResp] -> Bool
movingCab now maxAgeSec plate =
  any (\vt -> samePlate vt.vehicleNumber plate && isFreshPosition now maxAgeSec vt.vehicleInfo && isMovingSpeed vt.vehicleInfo.speed)
