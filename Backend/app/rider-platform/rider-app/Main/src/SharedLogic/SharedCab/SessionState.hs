module SharedLogic.SharedCab.SessionState
  ( SessionStatus (..),
    PauseReason (..),
    SessionMovement (..),
    SelectRouteMode (..),
    Session (..),
    OpenSessionReq (..),
    EndRouteAction (..),
    SelectPlan (..),
    RouteSetMoves (..),
    planSelect,
    WalkupSource (..),
    walkupsToCount,
    shouldDropOnDeadSession,
    ownedSession,
    newSession,
    switchRoute,
    queueRoute,
    endSession,
    pauseSession,
    resumeSession,
    setWalkup,
    fillCab,
    endActionReason,
    closedTripStatus,
    routeSetMoves,
    tripFor,
    returnRouteOf,
    sessionFromTrip,
    ExpiryAction (..),
    expiryAction,
    Ping (..),
    readPing,
    lastSeenFor,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType)
import Data.Aeson (Options (..), defaultOptions)
import qualified Data.Char as Char
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import Tools.Error (SharedCabSessionError (..))

data SessionStatus = ACTIVE | PAUSED | ENDED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data PauseReason = NO_LOCATION | DRIVER_OFFLINE | ABSENT | OFF_ROUTE
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Encodes as {"tag": "MOVING"} or {"tag": "AT_STOP", "stopName", "sinceMin"} (aeson's default tagged object).
data SessionMovement = MOVING | AT_STOP {stopName :: Text, sinceMin :: Int}
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | How a route change treats riders on board; JSON "afterLastDrop" | "force".
data SelectRouteMode = AfterLastDrop | Force
  deriving (Show, Eq, Generic)

selectRouteModeOptions :: Options
selectRouteModeOptions = defaultOptions {constructorTagModifier = \case c : cs -> Char.toLower c : cs; [] -> []}

instance ToJSON SelectRouteMode where
  toJSON = genericToJSON selectRouteModeOptions

instance FromJSON SelectRouteMode where
  parseJSON = genericParseJSON selectRouteModeOptions

instance ToSchema SelectRouteMode where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions selectRouteModeOptions

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
    consecutiveMisses :: Int,
    version :: Int,
    startedAt :: UTCTime,
    vehicleTripId :: Id DVT.VehicleTrip
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OpenSessionReq = OpenSessionReq
  { driverId :: Text,
    vehicleNumber :: Text,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    integratedBppConfigId :: Id DIBC.IntegratedBPPConfig,
    serviceTierType :: ServiceTierType,
    capacity :: Int,
    routeCode :: Text
  }
  deriving (Show, Eq, Generic)

data EndRouteAction = StartReturn | EndRoute | EndForNow
  deriving (Show, Eq)

data SelectPlan = OpenSession | ChangeRoute Session | KeepRoute Session
  deriving (Show, Eq)

-- | Route sets list only ACTIVE sessions, so `addTo` is re-asserted on every write (heals a lost member).
data RouteSetMoves = RouteSetMoves
  { removeFrom :: [Text],
    addTo :: [Text]
  }
  deriving (Show, Eq)

isLive :: Session -> Bool
isLive s = s.status /= ENDED

planSelect :: Text -> Text -> Maybe Session -> Either SharedCabSessionError SelectPlan
planSelect driver route = \case
  Just s
    | not (isLive s) -> Right OpenSession
    | s.driverId /= driver -> Left SessionHeldByAnotherDriver
    | s.routeCode == route -> Right (KeepRoute s)
    | otherwise -> Right (ChangeRoute s)
  Nothing -> Right OpenSession

ownedSession :: Text -> Maybe Session -> Either SharedCabSessionError Session
ownedSession driver = \case
  Just s
    | not (isLive s) -> Left SessionNotFound
    | s.driverId /= driver -> Left SessionHeldByAnotherDriver
    | otherwise -> Right s
  Nothing -> Left SessionNotFound

-- | Versions keep rising across sessions on a plate so a client holding an old session's version can't CAS the new one.
newSession :: OpenSessionReq -> Id DVT.VehicleTrip -> UTCTime -> Maybe Session -> Session
newSession req tripId now prior =
  Session
    { driverId = req.driverId,
      vehicleNumber = canonicalisePlate req.vehicleNumber,
      merchantId = req.merchantId,
      merchantOperatingCityId = req.merchantOperatingCityId,
      integratedBppConfigId = req.integratedBppConfigId,
      serviceTierType = req.serviceTierType,
      routeCode = req.routeCode,
      queuedRouteCode = Nothing,
      capacity = req.capacity,
      walkupCount = 0,
      status = ACTIVE,
      pauseReason = Nothing,
      consecutiveMisses = 0,
      version = maybe 1 ((+ 1) . (.version)) prior,
      startedAt = now,
      vehicleTripId = tripId
    }

bump :: Session -> Session
bump s = s {version = s.version + 1}

switchRoute :: Text -> Id DVT.VehicleTrip -> Session -> Session
switchRoute newRoute tripId s =
  bump s {routeCode = newRoute, vehicleTripId = tripId, queuedRouteCode = Nothing, status = ACTIVE, pauseReason = Nothing}

-- | `afterLastDrop`: the change waits in `queuedRouteCode` until the cab is empty; `switchRoute` clears it.
queueRoute :: Text -> Session -> Session
queueRoute route s = bump s {queuedRouteCode = Just route}

endSession :: Session -> Session
endSession s = bump s {status = ENDED, pauseReason = Nothing}

pauseSession :: PauseReason -> Session -> Either SharedCabSessionError Session
pauseSession reason s
  | s.status /= ACTIVE = Left SessionNotActive
  | otherwise = Right $ bump s {status = PAUSED, pauseReason = Just reason}

resumeSession :: Session -> Either SharedCabSessionError Session
resumeSession s
  | s.status /= PAUSED = Left SessionNotPaused
  | otherwise = Right $ bump s {status = ACTIVE, pauseReason = Nothing, consecutiveMisses = 0}

setWalkup :: Int -> Int -> Session -> Either SharedCabSessionError Session
setWalkup expectedVersion count s
  | s.version /= expectedVersion = Left SessionVersionMismatch
  | count < 0 || count > s.capacity = Left InvalidWalkupCount
  | otherwise = Right $ bump s {walkupCount = count}

-- | R19 "cab full": walk-ups take every seat the riders kept on board don't, so `available` reads 0.
-- Never lowers the walk-up count: declaring the cab full must not free a seat.
fillCab :: Int -> Session -> Session
fillCab seatsKept s = bump s {walkupCount = max s.walkupCount (s.capacity - seatsKept)}

-- | Which walk-ups count as offline boardings: a cab-full fill is the driver saying "no seats left", not people who boarded,
-- so it must not inflate the metric (R24).
data WalkupSource = DriverCounted | CabFullFill
  deriving (Show, Eq)

walkupsToCount :: WalkupSource -> Session -> Session -> Int
walkupsToCount source before after = case source of
  DriverCounted -> max 0 (after.walkupCount - before.walkupCount)
  CabFullFill -> 0

-- | R51: a rider still INPROGRESS on a plate whose run is over (session ENDED, or none and no live trip) is stranded
-- (Session.finish's per-rider drop failed); the sweep ends them.
shouldDropOnDeadSession :: Maybe SessionStatus -> Bool -> [TicketStatus.FRFSTicketStatus] -> Bool
shouldDropOnDeadSession mbStatus hasLiveTrip statuses =
  TicketStatus.INPROGRESS `elem` statuses && case mbStatus of
    Just ENDED -> True
    Just _ -> False
    Nothing -> not hasLiveTrip

endActionReason :: EndRouteAction -> DVT.VehicleTripEndReason
endActionReason = \case
  StartReturn -> DVT.RETURN
  EndRoute -> DVT.END_ROUTE
  EndForNow -> DVT.END_FOR_NOW

closedTripStatus :: DVT.VehicleTripEndReason -> DVT.VehicleTripStatus
closedTripStatus = \case
  DVT.END_ROUTE -> DVT.COMPLETED
  DVT.RETURN -> DVT.COMPLETED
  DVT.ROUTE_CHANGED -> DVT.COMPLETED
  DVT.END_FOR_NOW -> DVT.COMPLETED
  DVT.SESSION_TIMEOUT -> DVT.ABANDONED
  DVT.OPS_FORCED -> DVT.ABANDONED

routeSetMoves :: Maybe Session -> Session -> RouteSetMoves
routeSetMoves old new =
  RouteSetMoves
    { removeFrom = filter (`notElem` newRoutes) (maybe [] allocatableOn old),
      addTo = newRoutes
    }
  where
    newRoutes = allocatableOn new
    allocatableOn s = [s.routeCode | s.status == ACTIVE]

tripFor :: Session -> UTCTime -> DVT.VehicleTrip
tripFor s now =
  DVT.VehicleTrip
    { id = s.vehicleTripId,
      serviceTierType = s.serviceTierType,
      driverId = s.driverId,
      vehicleNumber = s.vehicleNumber,
      capacity = s.capacity,
      merchantId = s.merchantId,
      merchantOperatingCityId = s.merchantOperatingCityId,
      integratedBppConfigId = s.integratedBppConfigId,
      routeCode = s.routeCode,
      status = DVT.ACTIVE,
      startedAt = now,
      movingAt = Nothing,
      reachedEndAt = Nothing,
      endedAt = Nothing,
      endReason = Nothing,
      offlineBoardings = 0,
      missedPickups = 0,
      createdAt = now,
      updatedAt = now
    }

-- | Routes are one per direction, named SC-<CORRIDOR>-F / SC-<CORRIDOR>-R by the feed generator.
returnRouteOf :: Text -> Either SharedCabSessionError Text
returnRouteOf code
  | Just corridor <- T.stripSuffix "-F" code = Right (corridor <> "-R")
  | Just corridor <- T.stripSuffix "-R" code = Right (corridor <> "-F")
  | otherwise = Left NoReturnRoute

-- | Flush recovery: the live trip row restores route, driver and capacity. A PAUSED trip comes back paused — pause
-- is non-terminal — while a `findActiveByVehicleNumber` miss means ENDED: no session. Walk-ups are seeded from
-- `offlineBoardings`, capped at capacity, and the driver corrects them down: it counts every walk-up of the run, not
-- who is on board now (04 §3), so it errs high and allocation won't claim seats walk-ups may fill. The pause reason
-- and `consecutiveMisses` were Redis-only and restart empty. The version restarts above any pre-flush counter so a
-- client's stale version can't CAS it.
sessionFromTrip :: DVT.VehicleTrip -> UTCTime -> Session
sessionFromTrip trip now =
  Session
    { driverId = trip.driverId,
      vehicleNumber = trip.vehicleNumber,
      merchantId = trip.merchantId,
      merchantOperatingCityId = trip.merchantOperatingCityId,
      integratedBppConfigId = trip.integratedBppConfigId,
      serviceTierType = trip.serviceTierType,
      routeCode = trip.routeCode,
      queuedRouteCode = Nothing,
      capacity = trip.capacity,
      walkupCount = min trip.capacity trip.offlineBoardings,
      -- unreachable fallback: findActiveByVehicleNumber only returns live rows
      status = case trip.status of
        DVT.ACTIVE -> ACTIVE
        DVT.PAUSED -> PAUSED
        _ -> ACTIVE,
      pauseReason = Nothing,
      consecutiveMisses = 0,
      version = floor (utcTimeToPOSIXSeconds now),
      startedAt = trip.startedAt,
      vehicleTripId = trip.id
    }

-- | A cab's LTS ping as the expiry job sees it: a readable time, or one it can't trust (missing, unparseable, or
-- more than 5 min ahead, e.g. a millisecond epoch or a skewed clock, which would otherwise keep the cab alive forever).
data Ping = SeenAt UTCTime | Unreadable
  deriving (Show, Eq)

readPing :: UTCTime -> Maybe UTCTime -> Ping
readPing now = \case
  Just ts | diffUTCTime ts now <= 5 * 60 -> SeenAt ts
  _ -> Unreadable

-- | When the cab was last heard from, or Nothing to skip it this tick. No ping on the route means silent since the
-- trip began; an unreadable one is skipped, until the trip is older than `endAfter`, so it can't stay live forever.
lastSeenFor :: NominalDiffTime -> UTCTime -> UTCTime -> Maybe Ping -> Maybe UTCTime
lastSeenFor endAfter now startedAt = \case
  Just (SeenAt ts) -> Just (max startedAt ts)
  Just Unreadable | diffUTCTime now startedAt < endAfter -> Nothing
  _ -> Just startedAt

data ExpiryAction = PauseSilent | EndSilent
  deriving (Show, Eq)

-- | `lastSeen` = the latest LTS ping, or the trip's start if the cab hasn't pinged on this route yet.
expiryAction :: NominalDiffTime -> NominalDiffTime -> UTCTime -> UTCTime -> SessionStatus -> Maybe ExpiryAction
expiryAction pauseAfter endAfter now lastSeen status
  | status == ENDED = Nothing
  | silentFor >= endAfter = Just EndSilent
  | silentFor >= pauseAfter && status == ACTIVE = Just PauseSilent
  | otherwise = Nothing
  where
    silentFor = diffUTCTime now lastSeen
