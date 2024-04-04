{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DocumentVerificationConfig
  ( findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndDocumentType,
    findByMerchantOpCityIdAndDocumentTypeAndCategory,
    findByMerchantOpCityIdAndCategory,
    updateSupportedVehicleClassesJSON,
    clearCache,
    create,
    update,
  )
where

import Domain.Types.DocumentVerificationConfig as DTO
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DocumentVerificationConfig as Queries

create :: KvDbFlow m r => DocumentVerificationConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: KvDbFlow m r => Id MerchantOperatingCity -> m [DTO.DocumentVerificationConfig]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheDocumentVerificationConfigs id /=<< Queries.findAllByMerchantOpCityId Nothing Nothing id

findByMerchantOpCityIdAndDocumentTypeAndCategory :: KvDbFlow m r => Id MerchantOperatingCity -> DocumentType -> Category -> m (Maybe DTO.DocumentVerificationConfig)
findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId documentType category = find (\config -> config.vehicleCategory == category && config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId

findByMerchantOpCityIdAndDocumentType :: KvDbFlow m r => Id MerchantOperatingCity -> DocumentType -> m [DTO.DocumentVerificationConfig]
findByMerchantOpCityIdAndDocumentType merchantOpCityId documentType = filter (\config -> config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId

findByMerchantOpCityIdAndCategory :: KvDbFlow m r => Id MerchantOperatingCity -> Category -> m [DTO.DocumentVerificationConfig]
findByMerchantOpCityIdAndCategory merchantOpCityId category = filter (\config -> config.vehicleCategory == category) <$> findAllByMerchantOpCityId merchantOpCityId

cacheDocumentVerificationConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [DTO.DocumentVerificationConfig] -> m ()
cacheDocumentVerificationConfigs merchantOpCityId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeMerchantOpCityIdKey merchantOpCityId
  Hedis.withCrossAppRedis $ Hedis.setExp key configs expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "driver-offer:CachedQueries:DocumentVerificationConfig:MerchantOpCityId-" <> merchantOpCityId.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

update :: KvDbFlow m r => DocumentVerificationConfig -> m ()
update = Queries.update

updateSupportedVehicleClassesJSON :: KvDbFlow m r => Id MerchantOperatingCity -> SupportedVehicleClasses -> m ()
updateSupportedVehicleClassesJSON = Queries.updateSupportedVehicleClassesJSON
