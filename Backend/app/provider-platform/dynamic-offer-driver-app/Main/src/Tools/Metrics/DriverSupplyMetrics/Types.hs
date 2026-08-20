{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.DriverSupplyMetrics.Types
  ( HasDriverSupplyMetrics,
    DriverSupplyMetricsContainer (..),
    registerDriverSupplyMetricsContainer,
    setDriverSupplyGauge,
    setDriverSupplyRideGauge,
    setDriverSupplyFunnelGauge,
  )
where

import EulerHS.Prelude
import Kernel.Utils.Common
import Prometheus as P

type HasDriverSupplyMetrics m r =
  HasFlowEnv m r '["driverSupplyMetrics" ::: DriverSupplyMetricsContainer]

-- Driver-supply gauges: ABSOLUTE values set periodically from a source of truth,
-- never incremented/decremented on events (event-driven gauges drift on pod
-- restarts and split across pods).
--
-- No backend_version on any of these: gauges are published concurrently by every
-- pod, and during a rollout two deployment versions would publish the same truth
-- under different label sets, so any sum() would double-count supply. Concurrent
-- same-value writes without the version label are benign (last-writer-wins on
-- identical numbers).
-- merchant = merchant shortId, city = operating city name.
-- DASHBOARDS: every pod exports its own series — always aggregate with
-- max by (...), NEVER sum across instances.
type DriverSupplyGauge = P.Vector P.Label2 P.Gauge

-- The windowed metrics carry the funnel dimensions of the search that produced the
-- event, so driver-grain reach is comparable to the ping-grain counters in
-- Tools.Metrics.ARDUBPPMetrics on the same slice.
-- NOTE: these are DISTINCT-driver counts, so summing across ANY dimension
-- OVER-counts — a driver pinged for both a 3km and a 20km search appears in both
-- distance buckets, and one serving several tiers appears in each. Read a single
-- slice, or BPP_drivers_online for the city-wide figure.
--
-- on-ride stops at distance_bucket: a booking carries no pooling version.
type DriverSupplyRideGauge = P.Vector P.Label4 P.Gauge

type DriverSupplyFunnelGauge = P.Vector P.Label6 P.Gauge

data DriverSupplyMetricsContainer = DriverSupplyMetricsContainer
  { driversOnlineGauge :: DriverSupplyGauge,
    driversReceivingGauge :: DriverSupplyFunnelGauge,
    driversAcceptingGauge :: DriverSupplyFunnelGauge,
    driversOnRideGauge :: DriverSupplyRideGauge
  }

registerDriverSupplyMetricsContainer :: IO DriverSupplyMetricsContainer
registerDriverSupplyMetricsContainer = do
  -- No vehicle_service_tier here: driver_information carries no tier (a driver's tiers
  -- come from their vehicle and preferences), so this gauge is city-wide by necessity.
  -- "active" is the same predicate the dispatch filter uses — dispatch-eligible drivers,
  -- which spans ONLINE and SILENT mode and includes drivers currently on a ride.
  -- Subtract BPP_drivers_on_ride for a free-supply view.
  driversOnlineGauge <- reg "BPP_drivers_online" "Current dispatch-eligible drivers (driver_information.active, spans ONLINE/SILENT and includes on-ride), refreshed periodically from the read replica"
  driversReceivingGauge <- regFunnel "BPP_drivers_receiving_searches" "Distinct drivers sent at least one search request in the last completed 10-minute window, by service tier, distance bucket and pooling versions"
  driversAcceptingGauge <- regFunnel "BPP_drivers_accepting_searches" "Distinct drivers who accepted at least one search request in the last completed 10-minute window, by service tier, distance bucket and pooling versions"
  driversOnRideGauge <- regRide "BPP_drivers_on_ride" "Distinct drivers assigned to at least one ride in the last completed 10-minute window, by service tier and distance bucket"
  return $ DriverSupplyMetricsContainer {..}
  where
    reg name description = P.register . P.vector ("merchant", "city") . P.gauge $ P.Info name description
    regRide name description = P.register . P.vector ("merchant", "city", "vehicle_service_tier", "distance_bucket") . P.gauge $ P.Info name description
    regFunnel name description = P.register . P.vector ("merchant", "city", "vehicle_service_tier", "distance_bucket", "pooling_logic_version", "pooling_config_version") . P.gauge $ P.Info name description

-- (param is gaugeVec, not "gauge": Prometheus is imported unqualified too, and a
-- local named gauge shadows P.gauge — fatal under this package's -Wall -Werror)
setDriverSupplyGauge :: MonadIO m => DriverSupplyGauge -> Text -> Text -> Int -> m ()
setDriverSupplyGauge gaugeVec merchantLabel cityLabel value =
  liftIO $ P.withLabel gaugeVec (merchantLabel, cityLabel) (`P.setGauge` fromIntegral value)

setDriverSupplyRideGauge :: MonadIO m => DriverSupplyRideGauge -> Text -> Text -> Text -> Text -> Int -> m ()
setDriverSupplyRideGauge gaugeVec merchantLabel cityLabel tierLabel distanceBucket value =
  liftIO $ P.withLabel gaugeVec (merchantLabel, cityLabel, tierLabel, distanceBucket) (`P.setGauge` fromIntegral value)

setDriverSupplyFunnelGauge :: MonadIO m => DriverSupplyFunnelGauge -> Text -> Text -> Text -> Text -> Text -> Text -> Int -> m ()
setDriverSupplyFunnelGauge gaugeVec merchantLabel cityLabel tierLabel distanceBucket poolingLogicV poolingConfigV value =
  liftIO $ P.withLabel gaugeVec (merchantLabel, cityLabel, tierLabel, distanceBucket, poolingLogicV, poolingConfigV) (`P.setGauge` fromIntegral value)
