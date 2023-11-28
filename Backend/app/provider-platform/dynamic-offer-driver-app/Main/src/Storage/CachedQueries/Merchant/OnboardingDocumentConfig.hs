{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.OnboardingDocumentConfig
  ( findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndDocumentType,
    clearCache,
    create,
    update,
  )
where

import Domain.Types.Merchant.ConfigMapping (ConfigMapping)
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.OnboardingDocumentConfig as DTO
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.ConfigMapping as CMQ
import qualified Storage.Queries.Merchant.OnboardingDocumentConfig as Queries

--CMTODO: Handle Dashboard calls

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => OnboardingDocumentConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> m [DTO.OnboardingDocumentConfig]
findAllByMerchantOpCityId id distance mbvt = do
  currTime <- getLocalCurrentTime 19800
  cmId <- CMQ.getConfigMapId id distance mbvt currTime "onboarding_document_config" >>= fromMaybeM (InternalError $ "ConfigMapping not found for OnboardingDocumentConfig : mocid, distance, mbvt, currTime" <> show id <> "," <> show distance <> ", " <> show mbvt <> ", " <> show currTime)
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeConfigMapKey cmId) >>= \case
    Just a -> return a
    Nothing -> cacheOnboardingDocumentConfigs cmId /=<< Queries.findByConfigMapId cmId

findByMerchantOpCityIdAndDocumentType :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DocumentType -> m (Maybe DTO.OnboardingDocumentConfig)
findByMerchantOpCityIdAndDocumentType merchantOpCityId documentType distance mbvt = find (\config -> config.documentType == documentType) <$> findAllByMerchantOpCityId merchantOpCityId distance mbvt

cacheOnboardingDocumentConfigs :: (CacheFlow m r) => Id ConfigMapping -> [DTO.OnboardingDocumentConfig] -> m ()
cacheOnboardingDocumentConfigs cmId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeConfigMapKey cmId
  Hedis.withCrossAppRedis $ Hedis.setExp key configs expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "driver-offer:CachedQueries:OnboardingDocumentConfig:MerchantOpCityId-" <> merchantOpCityId.getId

makeConfigMapKey :: Id ConfigMapping -> Text
makeConfigMapKey id = "driver-offer:CachedQueries:ConfigMapping:ConfigMapId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => OnboardingDocumentConfig -> m ()
update = Queries.update
