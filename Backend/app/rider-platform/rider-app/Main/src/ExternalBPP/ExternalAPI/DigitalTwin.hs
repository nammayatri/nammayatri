module ExternalBPP.ExternalAPI.DigitalTwin
  ( FleetBusState (..),
    FleetSummary (..),
    HeadwayInfo (..),
    StopArrival (..),
    FleetAlert (..),
    CorridorStatus (..),
    fetchBusState,
    fetchRouteBuses,
    fetchRouteHeadway,
    fetchStopArrivals,
    fetchFleetSummary,
    fetchActiveAlerts,
    fetchCorridorStatus,
    fetchBusHistory,
  )
where

import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common

-- | Digital Twin 1: Bus state as returned by fleet-state-aggregator
data FleetBusState = FleetBusState
  { busId :: Text,
    vehicleNumber :: Text,
    lat :: Double,
    lon :: Double,
    speedKmh :: Double,
    heading :: Maybe Double,
    status :: Text, -- ON_ROUTE | DELAYED | DEVIATED | STATIONARY | NO_SIGNAL | AT_DEPOT
    movementType :: Text, -- REVENUE_SERVICE | DEPOT_PULLOUT | DEADHEADING | etc.
    routeId :: Maybe Text,
    routeName :: Maybe Text,
    direction :: Maybe Text,
    routeProgressPct :: Maybe Double,
    nextStopCode :: Maybe Text,
    nextStopName :: Maybe Text,
    etaNextStopSeconds :: Maybe Int,
    tripNumber :: Maybe Int,
    driverId :: Maybe Text,
    lastUpdate :: UTCTime,
    signalAgeSeconds :: Int,
    isStationary :: Bool,
    isDeviated :: Bool
  }
  deriving (Show, Generic)

instance FromJSON FleetBusState where
  parseJSON = withObject "FleetBusState" $ \v ->
    FleetBusState
      <$> v .: "bus_id"
      <*> v .: "vehicle_number"
      <*> v .: "lat"
      <*> v .: "lon"
      <*> v .: "speed_kmh"
      <*> v .:? "heading"
      <*> v .: "status"
      <*> v .: "movement_type"
      <*> v .:? "route_id"
      <*> v .:? "route_name"
      <*> v .:? "direction"
      <*> v .:? "route_progress_pct"
      <*> v .:? "next_stop_code"
      <*> v .:? "next_stop_name"
      <*> v .:? "eta_next_stop_seconds"
      <*> v .:? "trip_number"
      <*> v .:? "driver_id"
      <*> v .: "last_update"
      <*> v .: "signal_age_seconds"
      <*> v .: "is_stationary"
      <*> v .: "is_deviated"

instance ToJSON FleetBusState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

-- | Fleet summary statistics
data FleetSummary = FleetSummary
  { totalGpsBuses :: Int,
    activeOnRoute :: Int,
    delayed :: Int,
    deviated :: Int,
    stationary :: Int,
    noSignal :: Int,
    atDepot :: Int,
    coveragePct :: Double,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON FleetSummary where
  parseJSON = withObject "FleetSummary" $ \v ->
    FleetSummary
      <$> v .: "total_gps_buses"
      <*> v .: "active_on_route"
      <*> v .: "delayed"
      <*> v .: "deviated"
      <*> v .: "stationary"
      <*> v .: "no_signal"
      <*> v .: "at_depot"
      <*> v .: "coverage_pct"
      <*> v .: "last_update"

instance ToJSON FleetSummary where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

-- | Headway information for a route+direction
data HeadwayInfo = HeadwayInfo
  { routeId :: Text,
    direction :: Text,
    scheduledHeadwayMinutes :: Maybe Double,
    actualAvgHeadwayMinutes :: Maybe Double,
    maxGapMinutes :: Maybe Double,
    hasBunching :: Bool,
    bunchingCount :: Int,
    busCount :: Int,
    status :: Text -- Normal | Warning | Critical | InsufficientData
  }
  deriving (Show, Generic)

instance FromJSON HeadwayInfo where
  parseJSON = withObject "HeadwayInfo" $ \v ->
    HeadwayInfo
      <$> v .: "route_id"
      <*> v .: "direction"
      <*> v .:? "scheduled_headway_minutes"
      <*> v .:? "actual_avg_headway_minutes"
      <*> v .:? "max_gap_minutes"
      <*> v .: "has_bunching"
      <*> v .: "bunching_count"
      <*> v .: "bus_count"
      <*> v .: "status"

instance ToJSON HeadwayInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

-- | Stop arrival prediction
data StopArrival = StopArrival
  { busId :: Text,
    routeId :: Text,
    direction :: Text,
    serviceType :: Maybe Text,
    etaSeconds :: Int,
    distanceM :: Maybe Double,
    isLive :: Bool
  }
  deriving (Show, Generic)

instance FromJSON StopArrival where
  parseJSON = withObject "StopArrival" $ \v ->
    StopArrival
      <$> v .: "bus_id"
      <*> v .: "route_id"
      <*> v .: "direction"
      <*> v .:? "service_type"
      <*> v .: "eta_seconds"
      <*> v .:? "distance_m"
      <*> v .: "is_live"

instance ToJSON StopArrival where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

-- | Fleet alert
data FleetAlert = FleetAlert
  { alertId :: Text,
    alertType :: Text,
    severity :: Text,
    busId :: Text,
    routeId :: Maybe Text,
    title :: Text,
    description :: Text,
    createdAt :: UTCTime,
    status :: Text
  }
  deriving (Show, Generic)

instance FromJSON FleetAlert where
  parseJSON = withObject "FleetAlert" $ \v ->
    FleetAlert
      <$> v .: "alert_id"
      <*> v .: "alert_type"
      <*> v .: "severity"
      <*> v .: "bus_id"
      <*> v .:? "route_id"
      <*> v .: "title"
      <*> v .: "description"
      <*> v .: "created_at"
      <*> v .: "status"

instance ToJSON FleetAlert where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

-- | Twin 2: Corridor status
data CorridorStatus = CorridorStatus
  { corridorId :: Text,
    corridorName :: Text,
    direction :: Text,
    avgSpeedKmh :: Double,
    congestionLevel :: Text, -- FREE_FLOW | SLOW_MOVING | CONGESTED | SEVERE
    sampleCount :: Int,
    predictedSpeedKmh :: Maybe Double,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON CorridorStatus where
  parseJSON = withObject "CorridorStatus" $ \v ->
    CorridorStatus
      <$> v .: "corridor_id"
      <*> v .: "corridor_name"
      <*> v .: "direction"
      <*> v .: "avg_speed_kmh"
      <*> v .: "congestion_level"
      <*> v .: "sample_count"
      <*> v .:? "predicted_speed_kmh"
      <*> v .: "last_update"

instance ToJSON CorridorStatus where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

-- | Helper to convert camelCase to snake_case
camelToSnake :: String -> String
camelToSnake = concatMap (\c -> if isUpper c then ['_', toLower c] else [c])
  where
    isUpper c = c >= 'A' && c <= 'Z'
    toLower c = toEnum (fromEnum c + 32)

-- | Configuration for fleet-state-aggregator API
data DigitalTwinConfig = DigitalTwinConfig
  { baseUrl :: BaseUrl,
    timeoutMs :: Int
  }
  deriving (Show, Generic)

-- | Fetch a single bus's state
fetchBusState ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  m (Maybe FleetBusState)
fetchBusState baseUrl busId = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/buses/" <> T.unpack busId}
  result <- callAPI url GET mempty "fetchBusState" Nothing
  case result of
    Right bus -> pure (Just bus)
    Left _ -> pure Nothing

-- | Fetch all buses on a route
fetchRouteBuses ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  m [FleetBusState]
fetchRouteBuses baseUrl routeId = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/route/" <> T.unpack routeId <> "/buses"}
  result <- callAPI url GET mempty "fetchRouteBuses" Nothing
  case result of
    Right buses -> pure buses
    Left _ -> pure []

-- | Fetch headway info for a route
fetchRouteHeadway ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  m [HeadwayInfo]
fetchRouteHeadway baseUrl routeId = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/route/" <> T.unpack routeId <> "/headway"}
  result <- callAPI url GET mempty "fetchRouteHeadway" Nothing
  case result of
    Right headway -> pure headway
    Left _ -> pure []

-- | Fetch arrivals at a stop
fetchStopArrivals ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  m [StopArrival]
fetchStopArrivals baseUrl stopCode = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/stop/" <> T.unpack stopCode <> "/arrivals"}
  result <- callAPI url GET mempty "fetchStopArrivals" Nothing
  case result of
    Right arrivals -> pure arrivals
    Left _ -> pure []

-- | Fetch fleet summary
fetchFleetSummary ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  m (Maybe FleetSummary)
fetchFleetSummary baseUrl = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/summary"}
  result <- callAPI url GET mempty "fetchFleetSummary" Nothing
  case result of
    Right summary -> pure (Just summary)
    Left _ -> pure Nothing

-- | Fetch active alerts
fetchActiveAlerts ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  m [FleetAlert]
fetchActiveAlerts baseUrl = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/alerts"}
  result <- callAPI url GET mempty "fetchActiveAlerts" Nothing
  case result of
    Right alerts -> pure alerts
    Left _ -> pure []

-- | Fetch corridor status (Twin 2)
fetchCorridorStatus ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  m [CorridorStatus]
fetchCorridorStatus baseUrl = do
  let url = baseUrl {baseUrlPath = "/api/v1/corridors"}
  result <- callAPI url GET mempty "fetchCorridorStatus" Nothing
  case result of
    Right corridors -> pure corridors
    Left _ -> pure []

-- | Fetch bus position history
fetchBusHistory ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  m [Value] -- Returns raw JSON array of history entries
fetchBusHistory baseUrl busId = do
  let url = baseUrl {baseUrlPath = "/api/v1/fleet/buses/" <> T.unpack busId <> "/history"}
  result <- callAPI url GET mempty "fetchBusHistory" Nothing
  case result of
    Right history -> pure history
    Left _ -> pure []

-- | Placeholder for HTTP call - in production this uses Kernel's HTTP client
callAPI :: (MonadFlow m, FromJSON a) => BaseUrl -> HttpMethod -> Value -> Text -> Maybe Int -> m (Either Text a)
callAPI _url _method _body _tag _timeout = do
  logInfo $ "Digital Twin API call: " <> _tag
  pure $ Left "Not yet connected to fleet-state-aggregator"

data HttpMethod = GET | POST | PATCH
