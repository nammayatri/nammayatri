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
    findByMerchantOpCityIdAndDocumentTypeAndDefaultEnabledOnManualVerification,
    updateSupportedVehicleClassesJSON,
    clearCache,
    create,
    update,
  )
where

import Domain.Types.DocumentVerificationConfig as DTO
  ( DocumentType,
    DocumentVerificationConfig,
    SupportedVehicleClasses,
  )
import Domain.Types.MerchantOperatingCity
import Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.DocumentVerificationConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DocumentVerificationConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [DTO.DocumentVerificationConfig]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId Nothing Nothing id)

findByMerchantOpCityIdAndDocumentTypeAndCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DocumentType -> VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe DTO.DocumentVerificationConfig)
findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId documentType category mbConfigVersionMap =
  find (\config -> config.vehicleCategory == category && config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

findByMerchantOpCityIdAndDocumentType :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DocumentType -> Maybe [LYT.ConfigVersionMap] -> m [DTO.DocumentVerificationConfig]
findByMerchantOpCityIdAndDocumentType merchantOpCityId documentType mbConfigVersionMap =
  filter (\config -> config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

findByMerchantOpCityIdAndCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m [DTO.DocumentVerificationConfig]
findByMerchantOpCityIdAndCategory merchantOpCityId category mbConfigVersionMap =
  filter (\config -> config.vehicleCategory == category) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

findByMerchantOpCityIdAndDocumentTypeAndDefaultEnabledOnManualVerification :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DocumentType -> Bool -> Maybe [LYT.ConfigVersionMap] -> m [DTO.DocumentVerificationConfig]
findByMerchantOpCityIdAndDocumentTypeAndDefaultEnabledOnManualVerification merchantOpCityId documentType isDefaultEnabledOnManualVerification mbConfigVersionMap =
  filter
    ( \config ->
        config.documentType == documentType
          && config.isDefaultEnabledOnManualVerification == isDefaultEnabledOnManualVerification
    )
    <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig)
    Nothing

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DocumentVerificationConfig -> m ()
update = Queries.update

updateSupportedVehicleClassesJSON :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> SupportedVehicleClasses -> DTV.VehicleCategory -> m ()
updateSupportedVehicleClassesJSON = Queries.updateSupportedVehicleClassesJSON
