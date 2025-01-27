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
    findByMerchantOpCityIdAndMessageKeyVehicleCategory,
    clearCache,
    clearCacheById,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.MerchantMessage
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantMessage as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantMessage -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [MerchantMessage]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return $ fmap (coerce @(MerchantMessageD 'Unsafe) @MerchantMessage) a
    Nothing -> cacheMerchantMessageForCity id /=<< Queries.findAllByMerchantOpCityId id

cacheMerchantMessageForCity :: CacheFlow m r => Id MerchantOperatingCity -> [MerchantMessage] -> m ()
cacheMerchantMessageForCity merchantOperatingCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (fmap (coerce @MerchantMessage @(MerchantMessageD 'Unsafe)) cfg) expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:MerchantMessage:MerchantOperatingCityId-" <> id.getId

cacheMerchantMessage :: CacheFlow m r => MerchantMessage -> m ()
cacheMerchantMessage merchantMessage = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdAndMessageKey merchantMessage.merchantOperatingCityId merchantMessage.messageKey merchantMessage.vehicleCategory
  Hedis.setExp idKey (coerce @MerchantMessage @(MerchantMessageD 'Unsafe) merchantMessage) expTime

makeMerchantOpCityIdAndMessageKey :: Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> Text
makeMerchantOpCityIdAndMessageKey id messageKey mbVehicleCategory = "CachedQueries:MerchantMessage:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> show messageKey <> maybe "" ((":VehCat-" <>) . show) mbVehicleCategory

findByMerchantOpCityIdAndMessageKeyVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> m (Maybe MerchantMessage)
findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey vehicleCategory =
  Hedis.safeGet (makeMerchantOpCityIdAndMessageKey id messageKey vehicleCategory) >>= \case
    Just a -> return . Just $ coerce @(MerchantMessageD 'Unsafe) @MerchantMessage a
    Nothing -> do
      res <- Queries.findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey vehicleCategory
      case res of
        Just a -> do
          cacheMerchantMessage a
          return $ Just a
        Nothing -> case vehicleCategory of
          Nothing -> return Nothing
          _ -> findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey Nothing

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> m ()
clearCache merchantOpCityId messageKey mbVehicleCategory = do
  Hedis.del (makeMerchantOpCityIdAndMessageKey merchantOpCityId messageKey mbVehicleCategory)

clearCacheById :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCacheById merchantOpCityId = do
  Hedis.del (makeMerchantOpCityIdKey merchantOpCityId)
