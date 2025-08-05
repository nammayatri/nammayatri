{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.SharedRideConfigs where

import qualified BecknV2.OnDemand.Enums as Enums
import Domain.Types.MerchantOperatingCity
import Domain.Types.SharedRideConfigs
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SharedRideConfigs as Queries

findByMerchantOperatingCityIdAndVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Enums.VehicleCategory -> m (Maybe SharedRideConfigs)
findByMerchantOperatingCityIdAndVehicleCategory merchantOperatingCityId vehicleCategory =
  Hedis.safeGet (makeMerchantOperatingCityIdAndVehicleCategoryKey merchantOperatingCityId vehicleCategory) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheMerchantOperatingCityIdAndVehicleCategory =<< Queries.findByMerchantOperatingCityIdAndVehicleCategory merchantOperatingCityId vehicleCategory

cacheMerchantOperatingCityIdAndVehicleCategory :: (CacheFlow m r) => SharedRideConfigs -> m ()
cacheMerchantOperatingCityIdAndVehicleCategory config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantOperatingCityIdAndVehicleCategoryKey = makeMerchantOperatingCityIdAndVehicleCategoryKey config.merchantOperatingCityId config.vehicleCategory
  Hedis.setExp merchantOperatingCityIdAndVehicleCategoryKey config expTime

makeMerchantOperatingCityIdAndVehicleCategoryKey :: Id MerchantOperatingCity -> Enums.VehicleCategory -> Text
makeMerchantOperatingCityIdAndVehicleCategoryKey merchantOperatingCityId vehicleCategory = "CachedQueries:SharedRideConfigs:MerchantOperatingCityId:" <> merchantOperatingCityId.getId <> ":VehicleCategory:" <> show vehicleCategory

-- Clear cache
clearCache :: (CacheFlow m r) => Id MerchantOperatingCity -> Enums.VehicleCategory -> m ()
clearCache merchantOperatingCityId vehicleCategory = do
  Hedis.del (makeMerchantOperatingCityIdAndVehicleCategoryKey merchantOperatingCityId vehicleCategory)
