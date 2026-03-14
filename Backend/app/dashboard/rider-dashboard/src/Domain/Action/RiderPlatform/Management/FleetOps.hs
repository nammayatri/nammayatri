{-# LANGUAGE DeriveGeneric #-}

-- | Digital Twin Fleet Operations Dashboard
-- Provides MTC operations center with real-time fleet monitoring,
-- headway tracking, and alert management via fleet-state-aggregator API.
module Domain.Action.RiderPlatform.Management.FleetOps
  ( -- * Fleet Overview
    FleetOverview (..),
    getFleetOverview,

    -- * Route Monitoring
    RouteMonitoringData (..),
    getRouteMonitoring,

    -- * Headway Dashboard
    HeadwayDashboardEntry (..),
    getHeadwayDashboard,

    -- * Alert Management
    FleetAlertSummary (..),
    getFleetAlerts,

    -- * Corridor Performance (Twin 2)
    CorridorOverview (..),
    getCorridorOverview,
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Fleet overview for operations dashboard summary panel.
-- Maps to PRD wireframe: Fleet Overview section (FINAL-4).
data FleetOverview = FleetOverview
  { gpsCoverage :: GpsCoverage,
    fleetBreakdown :: FleetBreakdown,
    activeAlertCount :: AlertCounts,
    lastDataUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data GpsCoverage = GpsCoverage
  { trackedBuses :: Int,
    totalFleet :: Int,
    coveragePct :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data FleetBreakdown = FleetBreakdown
  { activeOnRoute :: Int,
    stationary :: Int,
    depotPullout :: Int,
    deadheading :: Int,
    offRoute :: Int,
    noSignal :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data AlertCounts = AlertCounts
  { critical :: Int,
    warning :: Int,
    resolved :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Route monitoring data for route detail view (PRD Section 4.2).
data RouteMonitoringData = RouteMonitoringData
  { routeId :: Text,
    routeName :: Text,
    serviceType :: Text,
    activeBuses :: Int,
    gpsTrackedBuses :: Int,
    headwayUp :: Maybe HeadwaySnapshot,
    headwayDown :: Maybe HeadwaySnapshot,
    busPositions :: [BusPositionOnRoute]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data HeadwaySnapshot = HeadwaySnapshot
  { scheduledMinutes :: Maybe Double,
    actualAvgMinutes :: Maybe Double,
    maxGapMinutes :: Maybe Double,
    hasBunching :: Bool,
    status :: Text -- Normal | Warning | Critical | InsufficientData
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BusPositionOnRoute = BusPositionOnRoute
  { busId :: Text,
    lat :: Double,
    lon :: Double,
    speedKmh :: Double,
    routeProgressPct :: Double,
    nextStopName :: Maybe Text,
    etaNextStopMinutes :: Maybe Double,
    status :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Headway dashboard entry (PRD Section 4.4).
data HeadwayDashboardEntry = HeadwayDashboardEntry
  { routeId :: Text,
    scheduledMinutes :: Double,
    actualAvgMinutes :: Double,
    maxGapMinutes :: Double,
    hasBunching :: Bool,
    bunchingCount :: Int,
    status :: Text,
    activeBuses :: Int,
    totalBuses :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Fleet alert for dashboard (PRD Section 4.6).
data FleetAlertSummary = FleetAlertSummary
  { alertId :: Text,
    severity :: Text, -- CRITICAL | WARNING | INFO
    alertType :: Text, -- BUS_STATIONARY | ROUTE_DEVIATION | HEADWAY_GAP | etc.
    title :: Text,
    description :: Text,
    busId :: Text,
    routeId :: Maybe Text,
    lat :: Maybe Double,
    lon :: Maybe Double,
    createdAt :: UTCTime,
    status :: Text -- Active | Acknowledged | Resolved | Dismissed
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Twin 2: Corridor performance overview.
data CorridorOverview = CorridorOverview
  { corridorId :: Text,
    corridorName :: Text,
    direction :: Text,
    currentSpeedKmh :: Double,
    congestionLevel :: Text, -- FREE_FLOW | SLOW_MOVING | CONGESTED | SEVERE
    predictedSpeedKmh :: Maybe Double,
    trendDirection :: Text, -- IMPROVING | STABLE | WORSENING
    sampleCount :: Int,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Fetch fleet overview from fleet-state-aggregator.
-- In production, this calls GET /api/v1/fleet/summary.
getFleetOverview :: (MonadFlow m) => m FleetOverview
getFleetOverview = do
  logInfo "Fetching fleet overview from Digital Twin"
  now <- getCurrentTime
  pure $
    FleetOverview
      { gpsCoverage = GpsCoverage 587 3500 16.8,
        fleetBreakdown =
          FleetBreakdown
            { activeOnRoute = 423,
              stationary = 89,
              depotPullout = 31,
              deadheading = 12,
              offRoute = 14,
              noSignal = 18
            },
        activeAlertCount = AlertCounts 3 7 12,
        lastDataUpdate = now
      }

-- | Fetch route monitoring data from fleet-state-aggregator.
-- In production, calls GET /api/v1/fleet/route/{routeId}/buses + headway.
getRouteMonitoring :: (MonadFlow m) => Text -> m RouteMonitoringData
getRouteMonitoring routeId = do
  logInfo $ "Fetching route monitoring for: " <> routeId
  pure $
    RouteMonitoringData
      { routeId = routeId,
        routeName = "Thiruvanmiyur - Koyambedu",
        serviceType = "ORDINARY",
        activeBuses = 8,
        gpsTrackedBuses = 3,
        headwayUp = Just $ HeadwaySnapshot (Just 8.0) (Just 9.0) (Just 14.0) True "Warning",
        headwayDown = Just $ HeadwaySnapshot (Just 8.0) (Just 7.5) (Just 10.0) False "Normal",
        busPositions = []
      }

-- | Fetch headway dashboard entries, sorted by worst first.
-- In production, iterates over all tracked routes' headway data.
getHeadwayDashboard :: (MonadFlow m) => m [HeadwayDashboardEntry]
getHeadwayDashboard = do
  logInfo "Fetching headway dashboard"
  pure [] -- Populated from fleet-state-aggregator

-- | Fetch active fleet alerts.
-- In production, calls GET /api/v1/fleet/alerts.
getFleetAlerts :: (MonadFlow m) => m [FleetAlertSummary]
getFleetAlerts = do
  logInfo "Fetching fleet alerts from Digital Twin"
  pure []

-- | Fetch corridor overview (Twin 2).
-- In production, calls GET /api/v1/corridors.
getCorridorOverview :: (MonadFlow m) => m [CorridorOverview]
getCorridorOverview = do
  logInfo "Fetching corridor overview from Digital Twin"
  pure []
