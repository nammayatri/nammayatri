{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantMessage
  ( create,
    findAllByMerchantOpCityId,
    findAllByMerchantOpCityIdInRideFlow,
    findByMerchantOpCityIdAndMessageKeyVehicleCategory,
    findByMerchantOpCityIdAndMessageKeyVehicleCategoryInRideFlow,
    clearCache,
    clearCacheById,
  )
where

import Domain.Types.MerchantMessage
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantMessage as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantMessage -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantMessage]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs (cast id) (LYT.DRIVER_CONFIG LYT.MerchantMessage) mbConfigVersionMap Nothing (Queries.findAllByMerchantOpCityId id)

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantMessage]
findAllByMerchantOpCityIdInRideFlow id configVersionMap =
  findAllByMerchantOpCityId id (Just configVersionMap)

makeMerchantOpCityIdAndMessageKey :: Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> Text
makeMerchantOpCityIdAndMessageKey id messageKey mbVehicleCategory = "CachedQueries:MerchantMessage:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> show messageKey <> maybe "" ((":VehCat-" <>) . show) mbVehicleCategory

findByMerchantOpCityIdAndMessageKeyVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MerchantMessage)
findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey vehicleCategory mbConfigVersionMap = do
  res <-
    DynamicLogic.findOneConfigWithCacheKey
      (cast id)
      (LYT.DRIVER_CONFIG LYT.MerchantMessage)
      mbConfigVersionMap
      Nothing
      (Queries.findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey vehicleCategory)
      (makeMerchantOpCityIdAndMessageKey id messageKey vehicleCategory)
  case res of
    Just a -> return $ Just a
    Nothing -> do
      case vehicleCategory of
        Nothing -> return Nothing
        Just _ ->
          DynamicLogic.findOneConfigWithCacheKey
            (cast id)
            (LYT.DRIVER_CONFIG LYT.MerchantMessage)
            mbConfigVersionMap
            Nothing
            (Queries.findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey Nothing)
            (makeMerchantOpCityIdAndMessageKey id messageKey Nothing)

findByMerchantOpCityIdAndMessageKeyVehicleCategoryInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> [LYT.ConfigVersionMap] -> m (Maybe MerchantMessage)
findByMerchantOpCityIdAndMessageKeyVehicleCategoryInRideFlow id messageKey vehicleCategory configVersionMap =
  findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey vehicleCategory (Just configVersionMap)

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> m ()
clearCache merchantOpCityId messageKey mbVehicleCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdAndMessageKey merchantOpCityId messageKey mbVehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MerchantMessage)
    Nothing

clearCacheById :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheById merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MerchantMessage)
    Nothing
