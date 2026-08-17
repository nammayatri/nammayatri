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

-- Driver-supply gauges: ABSOLUTE values set periodically from a source of truth,
-- never incremented/decremented on events (event-driven gauges drift on pod
-- restarts and split across pods).
--
-- Labels are (merchant, city) ONLY — deliberately no backend_version: gauges are
-- published concurrently by every pod, and during a rollout two deployment
-- versions would publish the same truth under different label sets, so any sum()
-- would double-count supply. Concurrent same-value writes without the version
-- label are benign (last-writer-wins on identical numbers).
-- merchant = merchant shortId, city = operating city name.
-- DASHBOARDS: every pod exports its own series — always aggregate with
-- max by (merchant, city), NEVER sum across instances.
type DriverSupplyGauge = P.Vector P.Label2 P.Gauge

data DriverSupplyMetricsContainer = DriverSupplyMetricsContainer
  { driversOnlineGauge :: DriverSupplyGauge,
    driversReceivingGauge :: DriverSupplyGauge,
    driversAcceptingGauge :: DriverSupplyGauge,
    driversOnRideGauge :: DriverSupplyGauge
  }

registerDriverSupplyMetricsContainer :: IO DriverSupplyMetricsContainer
registerDriverSupplyMetricsContainer = do
  driversOnlineGauge <- reg "BPP_drivers_online" "Current on-duty drivers (active: ONLINE or SILENT mode, including on-ride) per driver_information, refreshed periodically from the read replica"
  driversReceivingGauge <- reg "BPP_drivers_receiving_searches" "Distinct drivers sent at least one search request in the last completed 10-minute window"
  driversAcceptingGauge <- reg "BPP_drivers_accepting_searches" "Distinct drivers who accepted at least one search request in the last completed 10-minute window"
  driversOnRideGauge <- reg "BPP_drivers_on_ride" "Distinct drivers assigned to at least one ride in the last completed 10-minute window"
  return $ DriverSupplyMetricsContainer {..}
  where
    reg name description = P.register . P.vector ("merchant", "city") . P.gauge $ P.Info name description

-- (param is gaugeVec, not "gauge": Prometheus is imported unqualified too, and a
-- local named gauge shadows P.gauge — fatal under this package's -Wall -Werror)
setDriverSupplyGauge :: MonadIO m => DriverSupplyGauge -> Text -> Text -> Int -> m ()
setDriverSupplyGauge gaugeVec merchantLabel cityLabel value =
  liftIO $ P.withLabel gaugeVec (merchantLabel, cityLabel) (`P.setGauge` fromIntegral value)
