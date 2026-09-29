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

data RiderStatus = AT_STOP | MINUTES_AWAY | ARRIVING | BOARDED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | The real states a rider row can be in, in precedence order: a flipped ticket wins (the cab carries them
-- already), an open allocation (`sharedcab:alloc:{bookingId}`) is next, else they are simply walking over.
riderStatusOf :: Bool -> Bool -> RiderStatus
riderStatusOf boarded allocated
  | boarded = BOARDED
  | allocated = ARRIVING
  | otherwise = MINUTES_AWAY

-- | ~5 km/h on foot (5000 m / 60 min), rounded up so the driver never under-waits by the conversion.
walkMinutesAway :: Double -> Int
walkMinutesAway meters = ceiling (meters / (5000 / 60))

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

-- | One live booking on the plate, resolved: `seats` is what its tickets still hold, `boarded` whether someone is
-- on board. Stop fields are stop codes. `riderStatus`, `minutesAway` and `expiresAt` carry the booking's real
-- allocation/walk state (SharedCabInternal.liveRiderRows fills them; groupRidersByStop only relays them).
data RiderRow = RiderRow
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    boardStopCode :: Text,
    dropStopCode :: Text,
    boarded :: Bool,
    fare :: HighPrecMoney,
    riderStatus :: RiderStatus,
    minutesAway :: Maybe Int,
    expiresAt :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | Per stop of the route, in route order, the riders still to board there and the boarded riders getting off
-- there; stops with neither are left out, as are rows holding no seat. A rider whose stop the route does not have
-- (a corridor-sibling or re-bound booking) is not dropped: they go in a trailing "Other stops" group, keyed by the
-- stop that decides where they show (board stop when waiting, drop stop when boarded), so the driver still sees them.
groupRidersByStop :: [(Text, Text)] -> [RiderRow] -> [RidersAtStop]
groupRidersByStop stops rows =
  [group stopName (== code) | (code, stopName) <- stops, nonEmpty (group stopName (== code))]
    ++ [other | let other = group otherStopsName (`notElem` map fst stops), nonEmpty other]
  where
    held = filter ((> 0) . (.seats)) rows
    group stopName atStop =
      RidersAtStop
        { stopName,
          boarding = [BoardingRider {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats, dropStop = nameOf r.dropStopCode, fare = r.fare, riderStatus = r.riderStatus, minutesAway = r.minutesAway, expiresAt = r.expiresAt} | r <- held, not r.boarded, atStop r.boardStopCode],
          alighting = [AlightingRider {bookingId = r.bookingId, firstName = r.firstName, seats = r.seats} | r <- held, r.boarded, atStop r.dropStopCode]
        }
    nonEmpty g = not (null g.boarding && null g.alighting)
    nameOf code = fromMaybe code (lookup code stops)

otherStopsName :: Text
otherStopsName = "Other stops"
