-- | The shared-cab session view rider-app serves on /internal/sharedCab/* and driver-app proxies to the driver app. One
-- definition for both apps, so the two cannot drift.
module Domain.Types.SharedCabSession where

import Data.Aeson (object, withObject, withText, (.:), (.=))
import qualified Data.Text as T
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data SessionStatus = ACTIVE | PAUSED | ENDED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data PauseReason = NO_LOCATION | DRIVER_OFFLINE | ABSENT | OFF_ROUTE
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Wire form: { tag: 'AT_STOP', sinceMin, stopName } | { tag: 'MOVING' }.
data SessionMovement
  = SessionMovementAtStop {sinceMin :: Int, stopName :: Text}
  | SessionMovementMoving
  deriving (Show, Eq, Generic, ToSchema)

instance ToJSON SessionMovement where
  toJSON (SessionMovementAtStop sinceMin stopName) =
    object ["tag" .= ("AT_STOP" :: Text), "sinceMin" .= sinceMin, "stopName" .= stopName]
  toJSON SessionMovementMoving = object ["tag" .= ("MOVING" :: Text)]

instance FromJSON SessionMovement where
  parseJSON = withObject "SessionMovement" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "AT_STOP" -> SessionMovementAtStop <$> o .: "sinceMin" <*> o .: "stopName"
      "MOVING" -> pure SessionMovementMoving
      other -> fail $ "Unknown session movement tag: " <> show other

-- | A status a newer rider-app adds decodes as UNKNOWN_STATUS instead of failing the driver's whole session view.
data RiderStatus = AT_STOP | MINUTES_AWAY | ARRIVING | BOARDED | UNKNOWN_STATUS
  deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON RiderStatus where
  parseJSON = withText "RiderStatus" $ \t ->
    pure $ case t of
      "AT_STOP" -> AT_STOP
      "MINUTES_AWAY" -> MINUTES_AWAY
      "ARRIVING" -> ARRIVING
      "BOARDED" -> BOARDED
      _ -> UNKNOWN_STATUS

-- | A stop with its coordinates, for the driver's map.
data SessionStop = SessionStop
  { code :: Text,
    name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SharedCabRoute = SharedCabRoute
  { code :: Text,
    name :: Text,
    direction :: Text,
    fromStop :: Text,
    toStop :: Text,
    -- | The route's stops in order, and its encoded shape, for the pre-start map.
    stops :: [SessionStop],
    shapePolyline :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | A rider the driver has not yet accepted or declined: the accept sheet's content.
data PendingOffer = PendingOffer
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    fare :: HighPrecMoney,
    boardStop :: Text,
    dropStop :: Text,
    minutesAway :: Maybe Int,
    offerExpiresAt :: UTCTime,
    riderRating :: Maybe Double,
    boardStopAddress :: Maybe Text,
    dropStopAddress :: Maybe Text,
    distanceMeters :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SessionRoute = SessionRoute
  { code :: Text,
    name :: Text,
    direction :: Text,
    nextStops :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | A rider holding seats on the cab, from the driver's accept until the drop; `riderStatus` says whether they are aboard yet.
data SharedCabRider = SharedCabRider
  { bookingId :: Text,
    firstName :: Text,
    seats :: Int,
    boardStop :: Text,
    dropStop :: Text,
    fare :: HighPrecMoney,
    riderStatus :: RiderStatus,
    minutesAway :: Maybe Int,
    expiresAt :: Maybe UTCTime,
    chatChannelId :: Maybe Text,
    callable :: Bool,
    -- | The rider's own number, only on a callable row in a city whose calls are DIRECT: the driver app dials it instead of the masked call.
    callNumber :: Maybe Text,
    -- | The rider's rating, one decimal; none when unrated.
    riderRating :: Maybe Double,
    boardStopAddress :: Maybe Text,
    dropStopAddress :: Maybe Text,
    -- | Metres along the route from the board stop to the drop stop; none when it cannot be worked out.
    distanceMeters :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DemandAtStop = DemandAtStop
  { stopCode :: Text,
    stopName :: Text,
    waiting :: Int,
    searching :: Int,
    windowMin :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

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
    riders :: [SharedCabRider],
    pendingOffers :: [PendingOffer],
    -- | The route's stops in order, its encoded shape and the cab's latest position, for the driver's map.
    stops :: [SessionStop],
    shapePolyline :: Maybe Text,
    vehicle :: Maybe LatLong,
    demandAhead :: [DemandAtStop]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Demand at one stop of the requested routes; a rider counts at one stop only.
data StopDemandCount = StopDemandCount
  { stopCode :: Text,
    waiting :: Maybe Int,
    searching :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | `publicTransportVersion` tells the driver app when its cached route data is stale.
data SharedCabDemandResp = SharedCabDemandResp
  { publicTransportVersion :: Maybe Text,
    stops :: [StopDemandCount]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | The feed's routes for the driver app's local cache; nothing here changes between feed versions.
data SharedCabRouteDataResp = SharedCabRouteDataResp
  { publicTransportVersion :: Maybe Text,
    routes :: [SharedCabRoute]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | A shared cab's live trip as driver-app, which owns it (vehicle_trip and the session), tells rider-app. Rider-app keeps a read
-- copy for allocation, seats and the rider's map, and ignores a copy older than its own (by `version`). `agencyId` is the feed's
-- integrated_bpp_config.agency_key, the one identity the two apps' config rows share.
data CabState = CabState
  { agencyId :: Text,
    driverId :: Text,
    vehicleNumber :: Text,
    vehicleTripId :: Text,
    routeCode :: Text,
    queuedRouteCode :: Maybe Text,
    capacity :: Int,
    status :: SessionStatus,
    pauseReason :: Maybe PauseReason,
    version :: Int,
    startedAt :: UTCTime,
    offlineBoardings :: Int,
    driverName :: Maybe Text,
    vehicleModel :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | What happened to the cab, for rider-app's side effects: a route change or an end releases the unboarded riders, an end drops
-- the boarded ones, an open or a resume seeds the city's allocation chains.
data CabChange = CAB_OPENED | CAB_ROUTE_CHANGED | CAB_ROUTE_QUEUED | CAB_PAUSED | CAB_RESUMED | CAB_ENDED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Driver-app -> rider-app on every trip change and session poll. `notifyOnBoard` tells the riders on board of a route change.
data CabUpdate = CabUpdate
  { cab :: CabState,
    change :: Maybe CabChange,
    notifyOnBoard :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CabRider = CabRider
  { bookingId :: Text,
    firstName :: Text,
    dropStop :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | The driver's session view (absent once the cab has ended) and the riders on board, which gate a route change or an end.
data CabSyncResp = CabSyncResp
  { session :: Maybe SharedCabSession,
    ridersOnBoard :: [CabRider]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Rider-app -> driver-app: what rider-app saw that changes the trip (no LTS ping for a while, the cab off its route, the last rider
-- dropped). The answer is the update to apply, if the trip changed.
data CabEvent = CAB_SILENT_PAUSE | CAB_SILENT_END | CAB_OFF_ROUTE | CAB_EMPTIED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

newtype CabEventReq = CabEventReq {event :: CabEvent}
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype CabEventResp = CabEventResp {update :: Maybe CabUpdate}
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Walk-ups and cab full are seat inventory, decided by rider-app against the bookings; `walkupsAdded` goes onto the trip's
-- offline boardings in driver-app.
data SharedCabSeatsResp = SharedCabSeatsResp
  { session :: SharedCabSession,
    walkupsAdded :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | A corridor's other direction: "<corridor>-F" and "<corridor>-R" run the same road both ways.
returnRouteCode :: Text -> Maybe Text
returnRouteCode code
  | Just corridor <- T.stripSuffix "-F" code = Just (corridor <> "-R")
  | Just corridor <- T.stripSuffix "-R" code = Just (corridor <> "-F")
  | otherwise = Nothing
