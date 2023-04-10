{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.SendSearchRequestToDriverMetrics
  ( module Tools.Metrics.SendSearchRequestToDriverMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Types.Common (Milliseconds)
import Prometheus as P
import Tools.Metrics.SendSearchRequestToDriverMetrics.Types as Reexport

incrementTaskCounter :: HasSendSearchRequestToDriverMetrics m r => Text -> m ()
incrementTaskCounter agencyName = do
  bmContainer <- asks (.ssrMetrics)
  version <- asks (.version)
  incrementTaskCounter' bmContainer agencyName version

incrementFailedTaskCounter :: HasSendSearchRequestToDriverMetrics m r => Text -> m ()
incrementFailedTaskCounter agencyName = do
  bmContainer <- asks (.ssrMetrics)
  version <- asks (.version)
  incrementFailedTaskCounter' bmContainer agencyName version

putTaskDuration :: HasSendSearchRequestToDriverMetrics m r => Text -> Milliseconds -> m ()
putTaskDuration agencyName duration = do
  bmContainer <- asks (.ssrMetrics)
  version <- asks (.version)
  putTaskDuration' bmContainer agencyName version duration

incrementTaskCounter' :: L.MonadFlow m => SendSearchRequestToDriverMetricsContainer -> Text -> DeploymentVersion -> m ()
incrementTaskCounter' bmContainer agencyName version = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.withLabel taskCounter (agencyName, version.getDeploymentVersion) P.incCounter

incrementFailedTaskCounter' :: L.MonadFlow m => SendSearchRequestToDriverMetricsContainer -> Text -> DeploymentVersion -> m ()
incrementFailedTaskCounter' bmContainer agencyName version = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.withLabel failedTaskCounter (agencyName, version.getDeploymentVersion) P.incCounter

putTaskDuration' :: L.MonadFlow m => SendSearchRequestToDriverMetricsContainer -> Text -> DeploymentVersion -> Milliseconds -> m ()
putTaskDuration' bmContainer agencyName version duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.withLabel taskDuration (agencyName, version.getDeploymentVersion) (`P.observe` ((/ 1000) . fromIntegral $ duration))
