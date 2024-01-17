{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.DriverPoolConfig
  ( clearCache,
    create,
    findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndTripDistance,
    findByMerchantOpCityIdAndTripDistanceAndDVeh,
    update,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.SearchRequest (SearchRequestTag)
import qualified Domain.Types.Vehicle.Variant as DVeh
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.DriverPoolConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> SearchRequestTag -> m [DriverPoolConfig]
findAllByMerchantOpCityId id rt =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return $ fmap (coerce @(DriverPoolConfigD 'Unsafe) @DriverPoolConfig) a
    Nothing -> cacheDriverPoolConfigs id /=<< Queries.findAllByMerchantOpCityId id rt

findByMerchantOpCityIdAndTripDistance :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> SearchRequestTag -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistance merchantOpCityId rt tripDistance = find (\config -> config.tripDistance == tripDistance) <$> findAllByMerchantOpCityId merchantOpCityId rt

findByMerchantOpCityIdAndTripDistanceAndDVeh :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> SearchRequestTag -> Meters -> Maybe DVeh.Variant -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistanceAndDVeh merchantOpCityId rt tripDistance variant = find (\config -> config.tripDistance == tripDistance && config.vehicleVariant == variant) <$> findAllByMerchantOpCityId merchantOpCityId rt

cacheDriverPoolConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [DriverPoolConfig] -> m ()
cacheDriverPoolConfigs merchantOpCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOpCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @[DriverPoolConfig] @[DriverPoolConfigD 'Unsafe] cfg) expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:DriverPoolConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
update = Queries.update
