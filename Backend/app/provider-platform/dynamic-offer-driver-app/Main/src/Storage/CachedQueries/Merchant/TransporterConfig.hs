{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
  ( create,
    clearCache,
    update,
    updateFCMConfig,
    updateReferralLinkPassword,
    getTransporterConfigFromDB,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.MerchantOperatingCity
import Domain.Types.TransporterConfig
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.TransporterConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
create = Queries.create

getTransporterConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe TransporterConfig)
getTransporterConfigFromDB id = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findByMerchantOpCityId id

cacheTransporterConfig :: (CacheFlow m r) => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey cfg.merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:TransporterConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

updateFCMConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> BaseUrl -> Text -> m ()
updateFCMConfig = Queries.updateFCMConfig

updateReferralLinkPassword :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> m ()
updateReferralLinkPassword = Queries.updateReferralLinkPassword

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
update = Queries.update
