{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.DriverSearchRequestResponseMetrics.Types
  ( HasDriverSearchRequestResponseMetrics,
    DriverSearchRequestResponseMetricsContainer (..),
    module CoreMetrics,
    registerDriverSearchRequestResponseMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasDriverSearchRequestResponseMetrics m r =
  HasFlowEnv m r ["driverSearchRequestResponseMetrics" ::: DriverSearchRequestResponseMetricsContainer, "version" ::: DeploymentVersion]

-- Labels: (version, batch_number, response)
type DriverResponseCounterMetric = P.Vector P.Label3 P.Counter

newtype DriverSearchRequestResponseMetricsContainer = DriverSearchRequestResponseMetricsContainer
  { driverResponseCounter :: DriverResponseCounterMetric
  }

registerDriverSearchRequestResponseMetricsContainer :: IO DriverSearchRequestResponseMetricsContainer
registerDriverSearchRequestResponseMetricsContainer = do
  driverResponseCounter <- registerDriverResponseCounter
  return $ DriverSearchRequestResponseMetricsContainer {..}

registerDriverResponseCounter :: IO DriverResponseCounterMetric
registerDriverResponseCounter =
  P.register . P.vector ("version", "batch_number", "response") . P.counter $
    P.Info "driver_search_request_response_count" "Count of driver responses to a search request, labelled by deployment version, batch number and response type"
