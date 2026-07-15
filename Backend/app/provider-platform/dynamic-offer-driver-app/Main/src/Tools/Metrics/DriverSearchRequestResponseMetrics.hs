{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.DriverSearchRequestResponseMetrics
  ( module Tools.Metrics.DriverSearchRequestResponseMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.DriverSearchRequestResponseMetrics.Types as Reexport

-- | Increment the driver-respond counter for a single response to a search request.
-- Labels emitted: (deployment version, batch number, response type e.g. "Accept"/"Reject"/"Pulled").
-- Both @batchNumber@ and @response@ are passed pre-rendered as Text by the caller so this
-- module stays decoupled from domain types.
incrementDriverResponseCounter :: HasDriverSearchRequestResponseMetrics m r => Text -> Text -> m ()
incrementDriverResponseCounter batchNumber response = do
  metricsContainer <- asks (.driverSearchRequestResponseMetrics)
  version <- asks (.version)
  incrementDriverResponseCounter' metricsContainer batchNumber response version

incrementDriverResponseCounter' :: L.MonadFlow m => DriverSearchRequestResponseMetricsContainer -> Text -> Text -> DeploymentVersion -> m ()
incrementDriverResponseCounter' metricsContainer batchNumber response version = do
  let driverResponseCounter = metricsContainer.driverResponseCounter
  L.runIO $ P.withLabel driverResponseCounter (version.getDeploymentVersion, batchNumber, response) P.incCounter
