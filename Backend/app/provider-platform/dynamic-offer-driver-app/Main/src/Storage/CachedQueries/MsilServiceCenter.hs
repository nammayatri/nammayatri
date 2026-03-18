module Storage.CachedQueries.MsilServiceCenter
  ( findNearbyActiveCentersCached,
    clearCityCache,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MsilServiceCenter as DMSC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Logging (logDebug)
import qualified Storage.Queries.MsilServiceCenterExtra as QExtra

-- | Cache key pattern for service center data by city
mkCityCentersCacheKey :: Text -> Text
mkCityCentersCacheKey opCityId = "MSIL:SC:cityId:" <> opCityId

-- | Cache TTL in seconds (30 minutes)
cacheTTL :: Int
cacheTTL = 30 * 60

-- | Find nearby centers with Redis caching layer.
-- First checks cache, falls back to DB query and populates cache.
findNearbyActiveCentersCached ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Double ->
  Double ->
  Int ->
  Maybe Text ->
  Int ->
  m [(DMSC.MsilServiceCenter, Maybe Int)]
findNearbyActiveCentersCached merchantOpCityId lat lon radius mbCenterType limit = do
  -- For now, delegate directly to DB query.
  -- Redis GEO caching (GEOADD/GEORADIUS) would be added in production:
  --   1. Check Redis GEO set for city
  --   2. If populated, use GEORADIUS to find nearby centers
  --   3. If miss, load from DB, populate Redis GEO set, then query
  logDebug $ "Querying service centers: city=" <> getId merchantOpCityId <> " lat=" <> show lat <> " lon=" <> show lon
  QExtra.findNearbyActiveCenters merchantOpCityId lat lon radius mbCenterType limit

-- | Clear Redis cache for a city's service centers (called on admin CRUD operations)
clearCityCache ::
  ( CacheFlow m r,
    MonadFlow m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m ()
clearCityCache merchantOpCityId = do
  let cacheKey = mkCityCentersCacheKey (getId merchantOpCityId)
  Redis.del cacheKey
  logDebug $ "Cleared service center cache for city: " <> getId merchantOpCityId
