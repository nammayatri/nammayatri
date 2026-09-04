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
    DistanceBucketEdges,
    distanceBucketEdges,
    getDistanceBucketEdges,
    distanceBucketLabel,
    poolingVersionLabel,
    searchReqFunnelLabels,
    driverSearchReqFunnelLabels,
    specialZoneLabels,
  )
where

import Data.List (group, sort)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Types.SpecialLocation as SL
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

-- | Merchant shortId and city name, falling back to raw ids so a metric is never dropped.
-- Prefer in-scope domain objects over these lookups. Total: a lookup error degrades to
-- the raw id rather than failing the request.
getMetricsLabels :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m (Text, Text)
getMetricsLabels merchantId merchantOpCityId = do
  eMerchant <- withTryCatch "metricsLabels:merchant" $ CQM.findById merchantId
  cityLabel <- getCityLabel merchantOpCityId
  let merchantLabel = either (const Nothing) identity eMerchant
  pure (maybe merchantId.getId (.shortId.getShortId) merchantLabel, cityLabel)

getCityLabel :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m Text
getCityLabel merchantOpCityId = do
  eCity <- withTryCatch "metricsLabels:city" $ CQMOC.findById merchantOpCityId
  pure $ maybe merchantOpCityId.getId (show . (.city)) (either (const Nothing) identity eCity)

-- | Upper bounds in whole kilometres, ascending and deduplicated. Constructor not
-- exported, so the cardinality cap holds by construction.
newtype DistanceBucketEdges = DistanceBucketEdges [Int]
  deriving (Eq, Show)

-- | Matches ClickHouse trip_distance_bin, so Grafana and warehouse views agree.
defaultDistanceBucketEdges :: DistanceBucketEdges
defaultDistanceBucketEdges = DistanceBucketEdges [5, 12, 30]

-- | Bucket count multiplies the series count of every funnel counter.
maxDistanceBucketEdges :: Int
maxDistanceBucketEdges = 4

maxRawDistanceBucketEdges :: Int
maxRawDistanceBucketEdges = 64

-- | Prefer this over 'getDistanceBucketEdges' where a config is already in scope: it is
-- pure. An unusable config falls back to the defaults whole, never partially applied.
distanceBucketEdges :: DTC.TransporterConfig -> DistanceBucketEdges
distanceBucketEdges transporterConfig =
  case transporterConfig.metricsDistanceBucketsKm of
    Nothing -> defaultDistanceBucketEdges
    Just rawEdges
      -- Bail before sorting: the column is an unbounded integer[].
      | not (null (drop maxRawDistanceBucketEdges rawEdges)) -> defaultDistanceBucketEdges
      | otherwise ->
        -- Clean before capping, so duplicates or a stray 0 don't cost a valid config.
        case map head . group . sort $ filter (> 0) rawEdges of
          cleanEdges
            | null cleanEdges -> defaultDistanceBucketEdges
            | not (null (drop maxDistanceBucketEdges cleanEdges)) -> defaultDistanceBucketEdges
            | otherwise -> DistanceBucketEdges cleanEdges

-- | For emission sites with no TransporterConfig in scope. Total: getOneConfig can throw,
-- and a metrics label must never fail a booking.
getDistanceBucketEdges :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DistanceBucketEdges
getDistanceBucketEdges merchantOpCityId = do
  eTransporterConfig <- withTryCatch "metricsLabels:distanceBucketEdges" $ getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  pure $ either (const defaultDistanceBucketEdges) (maybe defaultDistanceBucketEdges distanceBucketEdges) eTransporterConfig

-- | e.g. "0-5km" / ">30km"; "unknown" when no estimate exists.
-- NOT SharedLogic.Pricing.getDistanceBin, which is finer-grained and for pricing keys.
distanceBucketLabel :: DistanceBucketEdges -> Maybe Meters -> Text
distanceBucketLabel _ Nothing = "unknown"
distanceBucketLabel (DistanceBucketEdges edgesKm) (Just distance) = go 0 edgesKm
  where
    go lowerKm [] = ">" <> show lowerKm <> "km"
    go lowerKm (upperKm : restKm)
      | distance < Meters (upperKm * 1000) = show lowerKm <> "-" <> show upperKm <> "km"
      | otherwise = go upperKm restKm

poolingVersionLabel :: Maybe Int -> Text
poolingVersionLabel = maybe "unknown" show

-- | Pickup and drop special-zone ids taken straight from the ride's in-memory Area;
-- "none" when the ride has no special zone at that end. PURE — no lookup, no added
-- compute; regular rides are just ("none","none"). The two ends are independent, so a ride
-- can be filtered by its pickup zone, drop zone, or both. The id is opaque: map it to a
-- readable name in Grafana (the same way the dashboard already maps city ids to names).
specialZoneLabels :: Maybe SL.Area -> (Text, Text)
specialZoneLabels Nothing = ("none", "none")
specialZoneLabels (Just area) =
  ( fromMaybe "none" (SL.pickupSpecialZoneIdFromArea area),
    fromMaybe "none" (SL.dropSpecialZoneIdFromArea area)
  )

-- | The three allocation-funnel label values, in the order every counter expects:
-- (distance_bucket, pooling_logic_version, pooling_config_version).
funnelLabels :: DistanceBucketEdges -> Maybe Meters -> Maybe Int -> Maybe Int -> (Text, Text, Text)
funnelLabels edges mbDistance mbPoolingLogicV mbPoolingConfigV =
  ( distanceBucketLabel edges mbDistance,
    poolingVersionLabel mbPoolingLogicV,
    poolingVersionLabel mbPoolingConfigV
  )

-- | From the search request — for the allocator, which already has it in scope.
-- NOTE: pooling versions are assigned during the first driver-pool computation
-- (ensurePoolingLogicVersion / getDriverPoolConfig), so only pass search requests read
-- AFTER that point; earlier reads legitimately carry Nothing and label "unknown".
searchReqFunnelLabels :: DistanceBucketEdges -> DSR.SearchRequest -> (Text, Text, Text)
searchReqFunnelLabels edges searchReq =
  funnelLabels edges searchReq.estimatedDistance searchReq.poolingLogicVersion searchReq.poolingConfigVersion

-- | From the driver's ping row — preferred wherever a SearchRequestForDriver is in scope.
-- Needs no extra fetch, and its poolingLogicVersion is the one the pool actually ran with
-- (stamped at fan-out as dpwRes.poolingLogicVersion <|> searchReq.poolingLogicVersion),
-- whereas the search request carries only the pre-pool snapshot.
driverSearchReqFunnelLabels :: DistanceBucketEdges -> DSRD.SearchRequestForDriver -> (Text, Text, Text)
driverSearchReqFunnelLabels edges sReqFD =
  funnelLabels edges sReqFD.tripEstimatedDistance sReqFD.poolingLogicVersion sReqFD.poolingConfigVersion
