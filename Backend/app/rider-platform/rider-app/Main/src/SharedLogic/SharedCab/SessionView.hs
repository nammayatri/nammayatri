-- | Driver-app contract types (`04` §4) kept out of spec/API/SharedCabInternal.yaml: the generated module
-- imports Servant and EulerHS.Prelude, whose `route` and `force` clash with these field names.
module SharedLogic.SharedCab.SessionView where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import SharedLogic.SharedCab.SessionState (PauseReason, SessionMovement, SessionStatus)

data SharedCabRoute = SharedCabRoute
  { code :: Text,
    name :: Text,
    direction :: Text,
    fromStop :: Text,
    toStop :: Text,
    distanceKm :: Maybe Double,
    isStandRoute :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SessionRoute = SessionRoute
  { code :: Text,
    name :: Text,
    direction :: Text,
    nextStops :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RiderStatus = AT_STOP | MINUTES_AWAY
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data BoardingRider = BoardingRider
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    dropStop :: Text,
    fare :: HighPrecMoney,
    riderStatus :: RiderStatus,
    minutesAway :: Maybe Int,
    expiresAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AlightingRider = AlightingRider
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RidersAtStop = RidersAtStop
  { stopName :: Text,
    boarding :: [BoardingRider],
    alighting :: [AlightingRider]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DemandAtStop = DemandAtStop
  { stopName :: Text,
    waiting :: Int,
    searching :: Int,
    windowMin :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data LowDemandAlternative = LowDemandAlternative
  { routeCode :: Text,
    name :: Text,
    riders :: Int,
    windowMin :: Int,
    distanceKm :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype LowDemandCard = LowDemandCard {alternatives :: [LowDemandAlternative]}
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype OffRoute = OffRoute {nearestRoutes :: [SharedCabRoute]}
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSession = SharedCabSession
  { route :: SessionRoute,
    queuedRoute :: Maybe SessionRoute,
    status :: SessionStatus,
    pauseReason :: Maybe PauseReason,
    movement :: SessionMovement,
    capacity :: Int,
    walkupCount :: Int,
    available :: Int,
    version :: Int,
    ridersByStop :: [RidersAtStop],
    demandAhead :: [DemandAtStop],
    lowDemandCard :: Maybe LowDemandCard,
    offRoute :: Maybe OffRoute
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data EndRouteNext = RETURN | CHANGE | END
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | END closes the run as END_ROUTE when `atLastStop` (ended at the route's last stop), else END_FOR_NOW.
-- RETURN / END are refused (SHARED_CAB_RIDERS_ON_BOARD) while riders are on board unless `force`.
data EndRouteReq = EndRouteReq
  { driverId :: Text,
    vehicleNumber :: Text,
    next :: EndRouteNext,
    atLastStop :: Maybe Bool,
    force :: Maybe Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
