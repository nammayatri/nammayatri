{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.PlaceNameCacheMetrics.Types
  ( HasPlaceNameCacheMetrics,
    PlaceNameCacheMetricsContainer (..),
    module CoreMetrics,
    registerPlaceNameCacheMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasPlaceNameCacheMetrics m r = HasFlowEnv m r ["placeNameCacheMetrics" ::: PlaceNameCacheMetricsContainer, "version" ::: DeploymentVersion]

type CacheHitCounterMetric = P.Vector P.Label1 P.Counter

type CacheMissCounterMetric = P.Vector P.Label1 P.Counter

data PlaceNameCacheMetricsContainer = PlaceNameCacheMetricsContainer
  { cacheHitCounter :: CacheHitCounterMetric,
    cacheMissCounter :: CacheMissCounterMetric
  }

registerPlaceNameCacheMetricsContainer :: IO PlaceNameCacheMetricsContainer
registerPlaceNameCacheMetricsContainer = do
  cacheHitCounter <- registerCacheHitCounter
  cacheMissCounter <- registerCacheMissCounter
  return $ PlaceNameCacheMetricsContainer {..}

registerCacheHitCounter :: IO CacheHitCounterMetric
registerCacheHitCounter = P.register . P.vector "version" . P.counter $ P.Info "place_name_cache_hits_total" "Total number of place name cache hits"

registerCacheMissCounter :: IO CacheMissCounterMetric
registerCacheMissCounter = P.register . P.vector "version" . P.counter $ P.Info "place_name_cache_misses_total" "Total number of place name cache misses"
