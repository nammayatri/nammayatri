{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DocumentVerificationStagesConfig
  ( findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndCategory,
    findByMerchantOpCityIdAndDocumentCategory,
    clearCache,
    create,
    updateByPrimaryKey,
  )
where

import qualified Domain.Types.DocumentVerificationConfig as DVC
import Domain.Types.DocumentVerificationStagesConfig as DTO (DocumentVerificationStagesConfig)
import Domain.Types.MerchantOperatingCity
import Domain.Types.VehicleCategory
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.DocumentVerificationStagesConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DocumentVerificationStagesConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [DTO.DocumentVerificationStagesConfig]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.DRIVER_CONFIG LYT.DocumentVerificationStagesConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId Nothing Nothing id)

findByMerchantOpCityIdAndDocumentCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DVC.DocumentCategory -> Maybe [LYT.ConfigVersionMap] -> m [DTO.DocumentVerificationStagesConfig]
findByMerchantOpCityIdAndDocumentCategory merchantOpCityId documentCategory mbConfigVersionMap =
  filter (\config -> config.documentCategory == documentCategory) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.DocumentVerificationStagesConfig)
    Nothing

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DocumentVerificationStagesConfig -> m ()
updateByPrimaryKey = Queries.updateByPrimaryKey
