{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.OnboardingDocumentConfig
  ( findByMerchantIdAndDocumentType,
    clearCache,
  )
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.OnboardingDocumentConfig as DTO
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.OnboardingDocumentConfig as Queries

findByMerchantIdAndDocumentType :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> DocumentType -> m (Maybe DTO.OnboardingDocumentConfig)
findByMerchantIdAndDocumentType merchantId documentType =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantDocTypeKey merchantId documentType) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheOnboardingDocumentConfig /=<< Queries.findByMerchantIdAndDocumentType merchantId documentType

cacheOnboardingDocumentConfig :: (CacheFlow m r) => DTO.OnboardingDocumentConfig -> m ()
cacheOnboardingDocumentConfig config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeMerchantDocTypeKey config.merchantId config.documentType
  Hedis.withCrossAppRedis $ Hedis.setExp key config expTime

makeMerchantDocTypeKey :: Id Merchant -> DTO.DocumentType -> Text
makeMerchantDocTypeKey merchantId documentType = "driver-offer:CachedQueries:OnboardingDocumentConfig:MerchantId-" <> merchantId.getId <> ":DocumentType-" <> show documentType

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> DTO.DocumentType -> m ()
clearCache merchantId documentType = Hedis.withCrossAppRedis . Hedis.del $ makeMerchantDocTypeKey merchantId documentType
