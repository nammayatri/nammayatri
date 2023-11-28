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
import Domain.Types.Merchant.ConfigMapping (ConfigMapping)
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Vehicle.Variant (Variant)
import qualified Domain.Types.Vehicle.Variant as DVeh
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.ConfigMapping as CMQ
import qualified Storage.Queries.Merchant.DriverPoolConfig as Queries

-- CMTODO: Handle Dashboard calls
create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
create = Queries.create

findByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> m (Maybe DriverPoolConfig)
findByMerchantOpCityId id distance mbvt = do
  currTime <- getLocalCurrentTime 19800
  cmId <- CMQ.getConfigMapId id distance mbvt currTime "driver_pool_config" >>= fromMaybeM (InternalError $ "ConfigMapping not found for DriverPoolConfig : mocid, distance, mbvt, currTime" <> show id <> "," <> show distance <> ", " <> show mbvt <> ", " <> show currTime)
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeConfigMapKey cmId) >>= \case
    Just a -> return . Just $ coerce @(DriverPoolConfigD 'Unsafe) @DriverPoolConfig a
    Nothing -> cacheDriverPoolConfigs cmId /=<< Queries.findByConfigMapId cmId

findByMerchantOpCityIdAndTripDistance :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistance merchantOpCityId tripDistance = find (\config -> config.tripDistance == tripDistance) <$> findAllByMerchantOpCityId merchantOpCityId tripDistance

findByMerchantOpCityIdAndTripDistanceAndDVeh :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe DVeh.Variant -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistanceAndDVeh merchantOpCityId tripDistance variant = find (\config -> config.tripDistance == tripDistance && config.vehicleVariant == variant) <$> findAllByMerchantOpCityId merchantOpCityId

cacheDriverPoolConfigs :: (CacheFlow m r) => Id ConfigMapping -> DriverPoolConfig -> m ()
cacheDriverPoolConfigs cmId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeConfigMapKey cmId
  Hedis.withCrossAppRedis $ Hedis.setExp key (coerce @DriverPoolConfig @(DriverPoolConfigD 'Unsafe) cfg) expTime

makeConfigMapKey :: Id ConfigMapping -> Text
makeConfigMapKey id = "driver-offer:CachedQueries:ConfigMapping:ConfigMapId-" <> id.getId

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:DriverPoolConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
update = Queries.update
