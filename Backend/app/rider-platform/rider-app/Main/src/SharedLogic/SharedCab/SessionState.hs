module SharedLogic.SharedCab.SessionState
  ( SessionStatus (..),
    PauseReason (..),
    Session (..),
    OpenSessionReq (..),
    EndRouteAction (..),
    SelectPlan (..),
    RouteSetMoves (..),
    planSelect,
    ownedSession,
    newSession,
    switchRoute,
    endSession,
    pauseSession,
    resumeSession,
    setWalkup,
    endActionReason,
    closedTripStatus,
    routeSetMoves,
    tripFor,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType)
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

-- | `StartReturn Nothing` runs the same route code back: the feed models both directions as one route (direction_id 0/1).
data EndRouteAction = StartReturn (Maybe Text) | EndRoute | EndForNow
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

endActionReason :: EndRouteAction -> DVT.VehicleTripEndReason
endActionReason = \case
  StartReturn _ -> DVT.RETURN
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
      createdAt = now,
      updatedAt = now
    }
