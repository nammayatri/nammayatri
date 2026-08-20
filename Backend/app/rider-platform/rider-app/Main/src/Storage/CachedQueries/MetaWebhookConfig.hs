{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Cache-aside lookup by phoneNumberId — this runs on every inbound Meta
-- webhook POST, so a plain DB hit per request would be a real regression
-- versus the old in-memory Dhall list. Modeled on
-- Storage.CachedQueries.Merchant.MerchantServiceConfig: read-through cache,
-- invalidate (not populate) on write. Only ciphertext (appSecret/verifyToken/
-- accessToken) ever touches Redis — MetaWebhookConfig's ToJSON/FromJSON never sees plaintext.
module Storage.CachedQueries.MetaWebhookConfig
  ( findByPhoneNumberId,
    create,
    clearCache,
  )
where

import Domain.Types.MetaWebhookConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.MetaWebhookConfig as Queries

findByPhoneNumberId :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe MetaWebhookConfig)
findByPhoneNumberId phoneNumberId =
  Hedis.safeGet (makeKey phoneNumberId) >>= \case
    Just cfg -> pure (Just cfg)
    Nothing -> flip whenJust cacheIt /=<< Queries.findByPhoneNumberId phoneNumberId

cacheIt :: CacheFlow m r => MetaWebhookConfig -> m ()
cacheIt cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeKey cfg.phoneNumberId) cfg expTime

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MetaWebhookConfig -> m ()
create cfg = do
  Queries.create cfg
  clearCache cfg.phoneNumberId

makeKey :: Text -> Text
makeKey phoneNumberId = "CachedQueries:MetaWebhookConfig:PhoneNumberId-" <> phoneNumberId

-- | Call after rotating a row's secrets or flipping `enabled` — cache-aside,
-- invalidate-on-write, not populate-on-write.
clearCache :: Hedis.HedisFlow m r => Text -> m ()
clearCache phoneNumberId = Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeKey phoneNumberId)
