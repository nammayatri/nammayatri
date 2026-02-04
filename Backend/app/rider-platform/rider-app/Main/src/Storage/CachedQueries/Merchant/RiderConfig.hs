{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.RiderConfig
  ( create,
    clearCache,
    findByMerchantOperatingCityId,
    findByMerchantOperatingCityIdInRideFlow,
    updateByPrimaryKey,
  )
where

import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.RiderConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RiderConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RiderConfig -> m ()
create = Queries.create

findByMerchantOperatingCityIdInRideFlow ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  m (Maybe RiderConfig)
findByMerchantOperatingCityIdInRideFlow id =
  findByMerchantOperatingCityId id

findByMerchantOperatingCityId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  m (Maybe RiderConfig)
findByMerchantOperatingCityId id = do
  Hedis.safeGet (makeMerchantOperatingCityIdKey id) >>= \case
    Just config -> return $ Just config
    Nothing -> (\a -> (whenJust a) cacheRiderConfig *> pure a) =<< Queries.findByMerchantOperatingCityId id

cacheRiderConfig :: (CacheFlow m r) => RiderConfig -> m ()
cacheRiderConfig riderConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOperatingCityIdKey riderConfig.merchantOperatingCityId) riderConfig expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "rider-app:CachedQueries:RiderConfig:MerchantOperatingCityId-" <> id.getId

clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = do
  Hedis.del (makeMerchantOperatingCityIdKey merchantOperatingCityId)

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => RiderConfig -> m ()
updateByPrimaryKey riderConfig = do
  Queries.updateByPrimaryKey riderConfig
  clearCache riderConfig.merchantOperatingCityId
