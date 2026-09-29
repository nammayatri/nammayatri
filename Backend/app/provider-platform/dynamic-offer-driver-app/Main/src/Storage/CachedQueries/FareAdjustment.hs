{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | City-level cache holding ONLY the ACTIVE fare adjustments — the hot path
-- (arm decision at search) never needs history, so the cached value stays
-- bounded by concurrent ops activity, not by every experiment ever run. The
-- status filter is applied in Haskell after the DB read (status is mutable
-- and on the KV secondary-key deny list). Terminal rows referenced by a
-- transaction pin are re-fetched by primary key in SharedLogic.FareAdjustment.
-- Cleared on every mutation, so activation/abort take effect on the next NEW
-- search.
module Storage.CachedQueries.FareAdjustment
  ( findActiveByMerchantOperatingCityId,
    clearCache,
  )
where

import qualified Domain.Types.FareAdjustment as DFA
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FareAdjustment as Queries

findActiveByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [DFA.FareAdjustment]
findActiveByMerchantOperatingCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCityKey merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> do
      -- ACTIVE-only at the SQL level; elapsed spikes are still status ACTIVE
      -- (expiry is a window fact, not a status write) so they come back here
      activeRows <- Queries.findAllByCityAndStatus merchantOpCityId DFA.ACTIVE
      now <- getCurrentTime
      -- expiry is lazy (there is no sweeper): an elapsed spike keeps status
      -- ACTIVE in the DB until a reader lands here, so flip it to EXPIRED on
      -- the way through — without this every spike ever run would stay in the
      -- cached "active" list forever. Idempotent under concurrent refills.
      -- Pinned transactions are unaffected: EXPIRED rows replay via the
      -- by-primary-key fallback in SharedLogic.FareAdjustment.
      let hasElapsed a = maybe False (<= now) a.validTill
          stillActive = [a | a <- activeRows, not (hasElapsed a)]
          toExpire = [a | a <- activeRows, hasElapsed a]
      forM_ toExpire $ \a -> Queries.updateStatusById DFA.EXPIRED a.id
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeCityKey merchantOpCityId) stillActive expTime
      pure stillActive

clearCache :: Hedis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      Hedis.del (makeCityKey merchantOpCityId)

makeCityKey :: Id DMOC.MerchantOperatingCity -> Text
makeCityKey merchantOpCityId = "driver-offer:CachedQueries:FareAdjustment:Active:CityId-" <> merchantOpCityId.getId
