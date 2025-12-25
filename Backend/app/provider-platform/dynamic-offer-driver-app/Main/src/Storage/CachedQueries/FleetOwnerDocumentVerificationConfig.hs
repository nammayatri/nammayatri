{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FleetOwnerDocumentVerificationConfig
  ( findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndDocumentType,
    findAllByMerchantOpCityIdAndRole,
    clearCache,
    create,
    findAllMandatoryByMerchantOpCityIdAndRole,
  )
where

import Domain.Types.DocumentVerificationConfig
import Domain.Types.FleetOwnerDocumentVerificationConfig as FODTO
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.FleetOwnerDocumentVerificationConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FleetOwnerDocumentVerificationConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [FODTO.FleetOwnerDocumentVerificationConfig]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId Nothing Nothing id)

findByMerchantOpCityIdAndDocumentType :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DocumentType -> Maybe [LYT.ConfigVersionMap] -> m (Maybe FODTO.FleetOwnerDocumentVerificationConfig)
findByMerchantOpCityIdAndDocumentType merchantOpCityId documentType mbConfigVersionMap =
  find (\config -> config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

findAllByMerchantOpCityIdAndRole :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id MerchantOperatingCity -> Role -> Maybe [LYT.ConfigVersionMap] -> m [FODTO.FleetOwnerDocumentVerificationConfig]
findAllByMerchantOpCityIdAndRole merchantOpCityId role mbConfigVersionMap = filter (\config -> config.role == role) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

findAllMandatoryByMerchantOpCityIdAndRole :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id MerchantOperatingCity -> Role -> Maybe [LYT.ConfigVersionMap] -> m [FODTO.FleetOwnerDocumentVerificationConfig]
findAllMandatoryByMerchantOpCityIdAndRole merchantOpCityId role mbConfigVersionMap = filter (\config -> config.role == role && config.isMandatory) <$> findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig)
    Nothing
