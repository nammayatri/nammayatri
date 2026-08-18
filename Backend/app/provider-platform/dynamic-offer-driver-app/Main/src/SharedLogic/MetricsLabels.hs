{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MetricsLabels
  ( getMetricsLabels,
    getCityLabel,
    distanceBucketLabel,
    poolingVersionLabel,
    funnelLabels,
    searchReqFunnelLabels,
    driverSearchReqFunnelLabels,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRD
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

-- | Human-readable Prometheus label values for the BPP funnel counters:
-- merchant shortId and operating city name, falling back to the raw ids when
-- the cached lookups miss so a metric is never silently dropped.
-- Prefer in-scope domain objects (merchant.shortId.getShortId / show city) over
-- these lookups; use 'getCityLabel' when only the merchant object is in scope.
getMetricsLabels :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m (Text, Text)
getMetricsLabels merchantId merchantOpCityId = do
  mbMerchant <- CQM.findById merchantId
  cityLabel <- getCityLabel merchantOpCityId
  pure (maybe merchantId.getId (.shortId.getShortId) mbMerchant, cityLabel)

getCityLabel :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m Text
getCityLabel merchantOpCityId = do
  mbCity <- CQMOC.findById merchantOpCityId
  pure $ maybe merchantOpCityId.getId (show . (.city)) mbCity

-- | Distance bucket label for funnel counters. Edges deliberately match the analytics
-- stack's trip_distance_bin (ClickHouse) so Grafana and warehouse views agree by
-- construction. "unknown" when no estimate exists — never drop a metric over a label.
-- NOT the same as SharedLogic.Pricing.getDistanceBin (fine-grained 2km bins for
-- dynamic-pricing Redis keys) — that granularity would blow up metric cardinality.
distanceBucketLabel :: Maybe Meters -> Text
distanceBucketLabel Nothing = "unknown"
distanceBucketLabel (Just d)
  | m < 5000 = "0-5km"
  | m < 12000 = "5-12km"
  | m < 30000 = "12-30km"
  | otherwise = ">30km"
  where
    m = d.getMeters

poolingVersionLabel :: Maybe Int -> Text
poolingVersionLabel = maybe "unknown" show

-- | The three allocation-funnel label values, in the order every counter expects:
-- (distance_bucket, pooling_logic_version, pooling_config_version).
funnelLabels :: Maybe Meters -> Maybe Int -> Maybe Int -> (Text, Text, Text)
funnelLabels mbDistance mbPoolingLogicV mbPoolingConfigV =
  ( distanceBucketLabel mbDistance,
    poolingVersionLabel mbPoolingLogicV,
    poolingVersionLabel mbPoolingConfigV
  )

-- | From the search request — for the allocator, which already has it in scope.
-- NOTE: pooling versions are assigned during the first driver-pool computation
-- (ensurePoolingLogicVersion / getDriverPoolConfig), so only pass search requests read
-- AFTER that point; earlier reads legitimately carry Nothing and label "unknown".
searchReqFunnelLabels :: DSR.SearchRequest -> (Text, Text, Text)
searchReqFunnelLabels searchReq =
  funnelLabels searchReq.estimatedDistance searchReq.poolingLogicVersion searchReq.poolingConfigVersion

-- | From the driver's ping row — preferred wherever a SearchRequestForDriver is in scope.
-- Needs no extra fetch, and its poolingLogicVersion is the one the pool actually ran with
-- (stamped at fan-out as dpwRes.poolingLogicVersion <|> searchReq.poolingLogicVersion),
-- whereas the search request carries only the pre-pool snapshot.
driverSearchReqFunnelLabels :: DSRD.SearchRequestForDriver -> (Text, Text, Text)
driverSearchReqFunnelLabels sReqFD =
  funnelLabels sReqFD.tripEstimatedDistance sReqFD.poolingLogicVersion sReqFD.poolingConfigVersion
