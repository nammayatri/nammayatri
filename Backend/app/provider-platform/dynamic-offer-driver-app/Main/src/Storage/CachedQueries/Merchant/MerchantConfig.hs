{-# LANGUAGE DerivingStrategies #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantConfig
  ( findByMerchantId,
    clearCache,
    update,
    updateFCMConfig,
    updateReferralLinkPassword,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantConfig as Queries

findByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe MerchantConfig)
findByMerchantId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantConfigD 'Unsafe) @MerchantConfig a
    Nothing -> flip whenJust cacheMerchantConfig /=<< Queries.findByMerchantId id

cacheMerchantConfig :: (CacheFlow m r) => MerchantConfig -> m ()
cacheMerchantConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey cfg.merchantId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @MerchantConfig @(MerchantConfigD 'Unsafe) cfg) expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey id = "driver-offer:CachedQueries:MerchantConfig:MerchantId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => MerchantConfig -> m ()
clearCache merchant = do
  Hedis.withCrossAppRedis $ do
    Hedis.del (makeMerchantIdKey merchant.merchantId)

updateFCMConfig :: MonadFlow m => Id Merchant -> BaseUrl -> Text -> m ()
updateFCMConfig = Queries.updateFCMConfig

updateReferralLinkPassword :: MonadFlow m => Id Merchant -> Text -> m ()
updateReferralLinkPassword = Queries.updateReferralLinkPassword

update :: MonadFlow m => MerchantConfig -> m ()
update = Queries.update
