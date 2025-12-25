{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.PayoutConfig
  ( create,
    findAllByMerchantOpCityId,
    findByPrimaryKey,
    findByMerchantOpCityIdAndIsPayoutEnabled,
    clearCache,
    clearCacheById,
    clearCacheByCategory,
    clearConfigCache,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
import Domain.Types.VehicleCategory
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.PayoutConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [PayoutConfig]
findAllByMerchantOpCityId cityId mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast cityId)
    (LYT.DRIVER_CONFIG LYT.PayoutConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId cityId)

findByPrimaryKey :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe PayoutConfig)
findByPrimaryKey id vehicleCategory mbConfigVersionMap =
  DynamicLogic.findOneConfigWithCacheKey
    (cast id)
    (LYT.DRIVER_CONFIG LYT.PayoutConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findByPrimaryKey id vehicleCategory)
    (makeMerchantOpCityIdKeyAndCategory id vehicleCategory)

findByMerchantOpCityIdAndIsPayoutEnabled :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> Maybe [LYT.ConfigVersionMap] -> m [PayoutConfig]
findByMerchantOpCityIdAndIsPayoutEnabled id isPayoutEnabled mbConfigVersionMap =
  DynamicLogic.findAllConfigsWithCacheKey
    (cast id)
    (LYT.DRIVER_CONFIG LYT.PayoutConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findByMerchantOpCityIdAndIsPayoutEnabled id isPayoutEnabled)
    (makeMerchantOpCityIdKey id isPayoutEnabled)

makeMerchantOpCityIdKeyAndCategory :: Id MerchantOperatingCity -> VehicleCategory -> Text
makeMerchantOpCityIdKeyAndCategory id vehicleCategory = "driver-offer:CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> ":Category-" <> show vehicleCategory

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Bool -> Text
makeMerchantOpCityIdKey id isPayoutEnabled = "driver-offer:CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> "isPayoutEnabled:" <> show isPayoutEnabled

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> m ()
clearCache merchantOpCityId isPayoutEnabled =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdKey merchantOpCityId isPayoutEnabled)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.PayoutConfig)
    Nothing

clearCacheById :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheById merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.PayoutConfig)
    Nothing

clearCacheByCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> m ()
clearCacheByCategory merchantOpCityId vehicleCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdKeyAndCategory merchantOpCityId vehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.PayoutConfig)
    Nothing

clearConfigCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> m ()
clearConfigCache merchantOperatingCityId vehicleCategory = do
  clearCacheById merchantOperatingCityId
  clearCacheByCategory merchantOperatingCityId vehicleCategory
  clearCache merchantOperatingCityId True
  clearCache merchantOperatingCityId False
