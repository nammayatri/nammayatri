module SharedLogic.SharedCab.SessionState
  ( SessionStatus (..),
    PauseReason (..),
    SessionMovement (..),
    Session (..),
    RouteSetMoves (..),
    WalkupSource (..),
    walkupsToCount,
    shouldDropOnDeadSession,
    ownedSession,
    setWalkup,
    fillCab,
    routeSetMoves,
    copyFrom,
    isStale,
    ExpiryAction (..),
    expiryAction,
    Ping (..),
    readPing,
    lastSeenFor,
    sessionTtlSec,
    SessionRead (..),
    RouteMember (..),
    routeMember,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType (SHARED_CAB))
import Data.Time (diffUTCTime)
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SharedCabSession (CabState, PauseReason (..), SessionMovement (..), SessionStatus (..))
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Error (SharedCabSessionError (..))

-- | How long a session lives without a write; a plate's bookings are written during its session.
sessionTtlSec :: Int
sessionTtlSec = 16 * 60 * 60

-- | A session key's read that tells a missing key from a failed read: a Redis error or an undecodable value is not "no session".
data SessionRead = SessionAbsent | SessionAt Session | SessionUnreadable

data RouteMember = LiveMember Session | StaleMember | UnknownMember
  deriving (Show, Eq)

-- | A route-set member is dropped only on a positive answer: no session key, or a session that isn't ACTIVE on this route. A read
-- that failed keeps the member (an overloaded Redis must not take live cabs off their route).
routeMember :: Text -> SessionRead -> RouteMember
routeMember route = \case
  SessionAt s
    | s.status == ACTIVE && s.routeCode == route -> LiveMember s
    | otherwise -> StaleMember
  SessionAbsent -> StaleMember
  SessionUnreadable -> UnknownMember

-- | Rider-app's copy of a cab's run, which driver-app owns (vehicle_trip, the route, the status). Walk-ups are rider-app's own: they
-- are seats, decided against the bookings. `tripVersion` is driver-app's version of the run, so a late copy never replaces a newer
-- one; `version` is this copy's, bumped on every change the driver's view shows (walk-up writes are checked against it).
data Session = Session
  { driverId :: Text,
    vehicleNumber :: Text,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    integratedBppConfigId :: Id DIBC.IntegratedBPPConfig,
    serviceTierType :: ServiceTierType,
    routeCode :: Text,
    queuedRouteCode :: Maybe Text,
    capacity :: Int,
    walkupCount :: Int,
    status :: SessionStatus,
    pauseReason :: Maybe PauseReason,
    version :: Int,
    tripVersion :: Int,
    startedAt :: UTCTime,
    vehicleTripId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RouteSetMoves = RouteSetMoves
  { removeFrom :: [Text],
    addTo :: [Text]
  }
  deriving (Show, Eq)

isLive :: Session -> Bool
isLive s = s.status /= ENDED

ownedSession :: Text -> Maybe Session -> Either SharedCabSessionError Session
ownedSession driver = \case
  Just s
    | not (isLive s) -> Left SessionNotFound
    | s.driverId /= driver -> Left SessionHeldByAnotherDriver
    | otherwise -> Right s
  Nothing -> Left SessionNotFound

-- | A copy from the same run that is older than the one held.
isStale :: CabState -> Session -> Bool
isStale cab prior = cab.vehicleTripId == prior.vehicleTripId && cab.version < prior.tripVersion

-- | The new copy from driver-app's run. Walk-ups ride on with the cab, a route change included; a copy with no live prior (a flush,
-- or a cab rider-app has not seen) seeds them high from the run's offline boardings, so allocation won't claim seats walk-ups may
-- fill, and the driver corrects them. The copy's own version moves only when something the driver sees changed.
copyFrom :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DIBC.IntegratedBPPConfig -> CabState -> Maybe Session -> Session
copyFrom merchantId cityId ibcId cab prior =
  let walkupCount = case prior of
        Just p | p.status /= ENDED, p.driverId == cab.driverId -> p.walkupCount
        _ -> min cab.capacity cab.offlineBoardings
      next =
        Session
          { driverId = cab.driverId,
            vehicleNumber = cab.vehicleNumber,
            merchantId,
            merchantOperatingCityId = cityId,
            integratedBppConfigId = ibcId,
            serviceTierType = SHARED_CAB,
            routeCode = cab.routeCode,
            queuedRouteCode = cab.queuedRouteCode,
            capacity = cab.capacity,
            walkupCount,
            status = cab.status,
            pauseReason = cab.pauseReason,
            version = maybe 1 (.version) prior,
            tripVersion = cab.version,
            startedAt = cab.startedAt,
            vehicleTripId = cab.vehicleTripId
          }
   in case prior of
        Just p | sameView p next -> next
        Just p -> withVersion (p.version + 1) next
        Nothing -> next

-- | Everything the driver's view shows of the copy, its versions aside.
sameView :: Session -> Session -> Bool
sameView a b =
  (a.vehicleTripId, a.routeCode, a.queuedRouteCode, a.capacity, a.walkupCount, a.status, a.pauseReason)
    == (b.vehicleTripId, b.routeCode, b.queuedRouteCode, b.capacity, b.walkupCount, b.status, b.pauseReason)

withVersion :: Int -> Session -> Session
withVersion v s = s {version = v}

setWalkup :: Int -> Int -> Session -> Either SharedCabSessionError Session
setWalkup expectedVersion count s
  | s.version /= expectedVersion = Left SessionVersionMismatch
  | count < 0 || count > s.capacity = Left InvalidWalkupCount
  | otherwise = Right $ withVersion (s.version + 1) s {walkupCount = count}

-- | The walk-ups now hold `walkups` seats: every seat left when the cab was marked full.
fillCab :: Int -> Session -> Session
fillCab walkups s = withVersion (s.version + 1) s {walkupCount = walkups}

data WalkupSource = DriverCounted | CabFullFill
  deriving (Show, Eq)

walkupsToCount :: WalkupSource -> Session -> Session -> Int
walkupsToCount source before after = case source of
  DriverCounted -> max 0 (after.walkupCount - before.walkupCount)
  CabFullFill -> 0

shouldDropOnDeadSession :: Maybe SessionStatus -> Bool -> [TicketStatus.FRFSTicketStatus] -> Bool
shouldDropOnDeadSession mbStatus hasLiveTrip statuses =
  TicketStatus.INPROGRESS `elem` statuses && case mbStatus of
    Just ENDED -> True
    Just _ -> False
    Nothing -> not hasLiveTrip

routeSetMoves :: Maybe Session -> Session -> RouteSetMoves
routeSetMoves old new =
  RouteSetMoves
    { removeFrom = filter (`notElem` newRoutes) (maybe [] allocatableOn old),
      addTo = newRoutes
    }
  where
    newRoutes = allocatableOn new
    allocatableOn s = [s.routeCode | s.status == ACTIVE]

-- | A cab's LTS ping as the expiry job sees it: a readable time, or one it can't trust (missing, unparseable, or >5 min ahead, which would keep it alive forever).
data Ping = SeenAt UTCTime | Unreadable
  deriving (Show, Eq)

readPing :: UTCTime -> Maybe UTCTime -> Ping
readPing now = \case
  Just ts | diffUTCTime ts now <= 5 * 60 -> SeenAt ts
  _ -> Unreadable

-- | Last heard from, or Nothing to skip this tick; an unreadable ping is skipped until the trip is older than `endAfter`.
lastSeenFor :: NominalDiffTime -> UTCTime -> UTCTime -> Maybe Ping -> Maybe UTCTime
lastSeenFor endAfter now startedAt = \case
  Just (SeenAt ts) -> Just (max startedAt ts)
  Just Unreadable | diffUTCTime now startedAt < endAfter -> Nothing
  _ -> Just startedAt

data ExpiryAction = PauseSilent | EndSilent
  deriving (Show, Eq)

expiryAction :: NominalDiffTime -> NominalDiffTime -> UTCTime -> UTCTime -> SessionStatus -> Maybe ExpiryAction
expiryAction pauseAfter endAfter now lastSeen status
  | status == ENDED = Nothing
  | silentFor >= endAfter = Just EndSilent
  | silentFor >= pauseAfter && status == ACTIVE = Just PauseSilent
  | otherwise = Nothing
  where
    silentFor = diffUTCTime now lastSeen
