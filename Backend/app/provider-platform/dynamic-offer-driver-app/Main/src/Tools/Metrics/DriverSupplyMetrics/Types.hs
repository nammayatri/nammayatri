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
  )
where

import EulerHS.Prelude
import Kernel.Utils.Common
import Prometheus as P

type HasDriverSupplyMetrics m r =
  HasFlowEnv m r '["driverSupplyMetrics" ::: DriverSupplyMetricsContainer]

-- Driver-supply gauges: ABSOLUTE values, set periodically from Redis counters that the
-- state-changing events maintain. Never inc/dec'd here -- a gauge mutated on events
-- drifts on pod restarts and splits across pods.
--
-- No backend_version: every pod publishes, and during a rollout two deployment versions
-- would publish the same truth under different label sets, so any sum() would
-- double-count supply. Concurrent same-value writes are benign (last-writer-wins on
-- identical numbers).
--
-- No vehicle_service_tier either: driver_information carries no tier (a driver's tiers
-- come from their vehicle and preferences), so both gauges are city-wide by necessity.
-- Per-tier funnel volumes are already on the BPP_* counters in Tools.Metrics.ARDUBPPMetrics.
--
-- merchant = merchant shortId, city = operating city name.
-- DASHBOARDS: every pod exports its own series -- always aggregate with
-- max by (merchant, city), NEVER sum across instances.
type DriverSupplyGauge = P.Vector P.Label2 P.Gauge

data DriverSupplyMetricsContainer = DriverSupplyMetricsContainer
  { driversOnlineGauge :: DriverSupplyGauge,
    driversOnRideGauge :: DriverSupplyGauge
  }

registerDriverSupplyMetricsContainer :: IO DriverSupplyMetricsContainer
registerDriverSupplyMetricsContainer = do
  -- "active" is the same predicate the dispatch filter uses -- dispatch-eligible drivers,
  -- which spans ONLINE and SILENT mode and INCLUDES drivers currently on a ride.
  -- Free supply = BPP_drivers_online - BPP_drivers_on_ride.
  driversOnlineGauge <- reg "BPP_drivers_online" "Current dispatch-eligible drivers (driver_information.active, spans ONLINE/SILENT and includes on-ride)"
  driversOnRideGauge <- reg "BPP_drivers_on_ride" "Drivers currently on a ride (incremented at ride start, decremented at completion or cancellation)"
  return $ DriverSupplyMetricsContainer {..}
  where
    reg name description = P.register . P.vector ("merchant", "city") . P.gauge $ P.Info name description

-- (param is gaugeVec, not "gauge": Prometheus is imported unqualified too, and a
-- local named gauge shadows P.gauge -- fatal under this package's -Wall -Werror)
setDriverSupplyGauge :: MonadIO m => DriverSupplyGauge -> Text -> Text -> Int -> m ()
setDriverSupplyGauge gaugeVec merchantLabel cityLabel value =
  liftIO $ P.withLabel gaugeVec (merchantLabel, cityLabel) (`P.setGauge` fromIntegral value)
