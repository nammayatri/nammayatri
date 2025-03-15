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
  )
where

import Domain.Types.DocumentVerificationConfig
import Domain.Types.FleetOwnerDocumentVerificationConfig as FODTO
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOwnerDocumentVerificationConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FleetOwnerDocumentVerificationConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [FODTO.FleetOwnerDocumentVerificationConfig]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheFleetOwnerDocumentVerificationConfigs id /=<< Queries.findAllByMerchantOpCityId Nothing Nothing id

findByMerchantOpCityIdAndDocumentType :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DocumentType -> m (Maybe FODTO.FleetOwnerDocumentVerificationConfig)
findByMerchantOpCityIdAndDocumentType merchantOpCityId documentType = find (\config -> config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId

findAllByMerchantOpCityIdAndRole :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id MerchantOperatingCity -> Role -> m [FODTO.FleetOwnerDocumentVerificationConfig]
findAllByMerchantOpCityIdAndRole merchantOpCityId role = filter (\config -> config.role == role) <$> findAllByMerchantOpCityId merchantOpCityId

cacheFleetOwnerDocumentVerificationConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [FODTO.FleetOwnerDocumentVerificationConfig] -> m ()
cacheFleetOwnerDocumentVerificationConfigs merchantOpCityId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeMerchantOpCityIdKey merchantOpCityId
  Hedis.withCrossAppRedis $ Hedis.setExp key configs expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "driver-offer:CachedQueries:FleetOwnerFleetOwnerDocumentVerificationConfig:MerchantOpCityId-" <> merchantOpCityId.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey
