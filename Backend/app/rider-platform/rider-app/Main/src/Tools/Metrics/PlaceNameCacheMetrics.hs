{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.PlaceNameCacheMetrics
  ( module Tools.Metrics.PlaceNameCacheMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.PlaceNameCacheMetrics.Types as Reexport

incrementCacheHit :: HasPlaceNameCacheMetrics m r => m ()
incrementCacheHit = do
  metricsContainer <- asks (.placeNameCacheMetrics)
  version <- asks (.version)
  incrementCacheHit' metricsContainer version

incrementCacheMiss :: HasPlaceNameCacheMetrics m r => m ()
incrementCacheMiss = do
  metricsContainer <- asks (.placeNameCacheMetrics)
  version <- asks (.version)
  incrementCacheMiss' metricsContainer version

incrementCacheHit' :: L.MonadFlow m => PlaceNameCacheMetricsContainer -> DeploymentVersion -> m ()
incrementCacheHit' metricsContainer version = do
  let hitCounter = metricsContainer.cacheHitCounter
  L.runIO $ P.withLabel hitCounter (version.getDeploymentVersion) P.incCounter

incrementCacheMiss' :: L.MonadFlow m => PlaceNameCacheMetricsContainer -> DeploymentVersion -> m ()
incrementCacheMiss' metricsContainer version = do
  let missCounter = metricsContainer.cacheMissCounter
  L.runIO $ P.withLabel missCounter (version.getDeploymentVersion) P.incCounter
