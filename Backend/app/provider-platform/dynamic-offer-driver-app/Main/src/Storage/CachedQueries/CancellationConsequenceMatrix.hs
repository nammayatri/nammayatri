{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.CancellationConsequenceMatrix where

import Domain.Types.CancellationConsequenceMatrix
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Storage.Queries.CancellationConsequenceMatrix as Queries

-- Any writer (dashboard CRUD) MUST call clearCacheByCity afterwards.
create :: (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r) => CancellationConsequenceMatrix -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [CancellationConsequenceMatrix]
findAllByMerchantOpCityId merchantOpCityId =
  Hedis.safeGet (makeCityKey merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheByCity merchantOpCityId /=<< Queries.findAllByMerchantOperatingCityId merchantOpCityId

cacheByCity :: (CacheFlow m r) => Id MerchantOperatingCity -> [CancellationConsequenceMatrix] -> m ()
cacheByCity merchantOpCityId rows = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCityKey merchantOpCityId) rows expTime

-- Deletes run on both clouds' Redis (primary + secondary), per the fare-cache convention:
-- reads may be served cross-cloud, so a single-cloud delete leaves stale rows resurrectable.
clearCacheByCity :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCacheByCity merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.del (makeCityKey merchantOpCityId)

makeCityKey :: Id MerchantOperatingCity -> Text
makeCityKey merchantOpCityId = "driver-offer:CachedQueries:CancellationConsequenceMatrix:MerchantOpCityId-" <> getId merchantOpCityId
