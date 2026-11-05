module SharedLogic.SharedCab.SessionState
  ( SessionFlow,
    SessionStatus (..),
    PauseReason (..),
    SessionMovement (..),
    SelectRouteMode (..),
    Session (..),
    OpenSessionReq (..),
    EndRouteAction (..),
    SelectPlan (..),
    RouteSetMoves (..),
    planSelect,
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
    routeSetMoves,
    returnRouteOf,
    defaultCapacity,
    sessionFromTrip,
    ExpiryAction (..),
    expiryAction,
    Ping (..),
    readPing,
    lastSeenFor,
    sessionTtlSec,
    SessionRead (..),
    RouteMember (..),
    routeMember,
    cityMember,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType (SHARED_CAB))
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
import Domain.Types.SharedCabSession (PauseReason (..), SessionMovement (..), SessionStatus (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, Forkable, HasRequestId, HasShortDurationRetryCfg, MonadFlow)
import SharedLogic.SharedCab.TripClient (DriverTrip (..))
import Tools.Error (SharedCabSessionError (..))

-- | How long a session lives without a write; a plate's bookings are written during its session.
sessionTtlSec :: Int
sessionTtlSec = 16 * 60 * 60

-- | A session key's read that tells a missing key from a failed read: a Redis error or an undecodable value is not "no session".
-- | What a session change needs: Redis, the DB and metrics.
type SessionFlow m r c =
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    MonadReader r m,
    Forkable m
  )

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

-- | The same rule for the city's set of live plates: a member stays while its session is live in this city.
cityMember :: Id DMOC.MerchantOperatingCity -> SessionRead -> RouteMember
cityMember cityId = \case
  SessionAt s
    | s.status /= ENDED && s.merchantOperatingCityId == cityId -> LiveMember s
    | otherwise -> StaleMember
  SessionAbsent -> StaleMember
  SessionUnreadable -> UnknownMember

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
    version :: Int,
    startedAt :: UTCTime,
    vehicleTripId :: Text
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

-- | StartReturn carries driver-app's id for the return run.
data EndRouteAction = StartReturn Text | EndRoute | EndForNow
  deriving (Show, Eq)

data SelectPlan = OpenSession | ChangeRoute Session | KeepRoute Session
  deriving (Show, Eq)

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

newSession :: OpenSessionReq -> Text -> UTCTime -> Maybe Session -> Session
newSession req tripId now prior =
  Session
    { driverId = req.driverId,
      vehicleNumber = req.vehicleNumber,
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
      version = maybe 1 ((+ 1) . (.version)) prior,
      startedAt = now,
      vehicleTripId = tripId
    }

bump :: Session -> Session
bump s = s {version = s.version + 1}

-- | A new run on the new route: startedAt is the run's start, as the expiry pass reads it.
switchRoute :: Text -> Text -> UTCTime -> Session -> Session
switchRoute newRoute tripId now s =
  bump s {routeCode = newRoute, vehicleTripId = tripId, startedAt = now, queuedRouteCode = Nothing, status = ACTIVE, pauseReason = Nothing}

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
  | otherwise = Right $ bump s {status = ACTIVE, pauseReason = Nothing}

setWalkup :: Int -> Int -> Session -> Either SharedCabSessionError Session
setWalkup expectedVersion count s
  | s.version /= expectedVersion = Left SessionVersionMismatch
  | count < 0 || count > s.capacity = Left InvalidWalkupCount
  | otherwise = Right $ bump s {walkupCount = count}

-- | The walk-ups now hold `walkups` seats: every seat left when the cab was marked full.
fillCab :: Int -> Session -> Session
fillCab walkups s = bump s {walkupCount = walkups}

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

returnRouteOf :: Text -> Either SharedCabSessionError Text
returnRouteOf code
  | Just corridor <- T.stripSuffix "-F" code = Right (corridor <> "-R")
  | Just corridor <- T.stripSuffix "-R" code = Right (corridor <> "-F")
  | otherwise = Left NoReturnRoute

defaultCapacity :: Int
defaultCapacity = 6

-- | Flush recovery from driver-app's ACTIVE trip, under rider-app's config of its feed (merchant, city, config ids). Pause lived only in the lost session, so the
-- run comes back ACTIVE (the expiry pass pauses a silent cab again). Walk-ups seed from the count the driver last set, capped at
-- capacity, and the driver corrects them; the version restarts above any pre-flush counter.
sessionFromTrip :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DIBC.IntegratedBPPConfig -> DriverTrip -> UTCTime -> Session
sessionFromTrip merchantId merchantOperatingCityId integratedBppConfigId trip now =
  Session
    { driverId = trip.driverId,
      vehicleNumber = trip.vehicleNumber,
      merchantId,
      merchantOperatingCityId,
      integratedBppConfigId,
      serviceTierType = SHARED_CAB,
      routeCode = trip.routeCode,
      queuedRouteCode = Nothing,
      capacity = seats,
      walkupCount = min seats trip.walkupCount,
      status = ACTIVE,
      pauseReason = Nothing,
      version = floor (utcTimeToPOSIXSeconds now),
      startedAt = trip.startedAt,
      vehicleTripId = trip.id
    }
  where
    seats = fromMaybe defaultCapacity trip.capacity

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
