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
  ( findAllByMerchantId,
    findByMerchantIdAndDocumentType,
    clearCache,
    create,
    update,
  )
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.OnboardingDocumentConfig as DTO
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.OnboardingDocumentConfig as Queries

create :: MonadFlow m => OnboardingDocumentConfig -> m ()
create = Queries.create

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [DTO.OnboardingDocumentConfig]
findAllByMerchantId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheOnboardingDocumentConfigs id /=<< Queries.findAllByMerchantId id

findByMerchantIdAndDocumentType :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> DocumentType -> m (Maybe DTO.OnboardingDocumentConfig)
findByMerchantIdAndDocumentType merchantId documentType = find (\config -> config.documentType == documentType) <$> findAllByMerchantId merchantId

cacheOnboardingDocumentConfigs :: (CacheFlow m r) => Id Merchant -> [DTO.OnboardingDocumentConfig] -> m ()
cacheOnboardingDocumentConfigs merchantId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeMerchantIdKey merchantId
  Hedis.withCrossAppRedis $ Hedis.setExp key configs expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey merchantId = "driver-offer:CachedQueries:OnboardingDocumentConfig:MerchantId-" <> merchantId.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantIdKey

update :: MonadFlow m => OnboardingDocumentConfig -> m ()
update = Queries.update
