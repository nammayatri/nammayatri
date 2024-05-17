{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.CoinsConfig where

import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT (DriverCoinsEventType (..))
import qualified Storage.Queries.Coins.CoinsConfig as Queries

fetchFunctionsOnEventbasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m [CoinsConfig]
fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId =
  Hedis.safeGet (makeCoinConfigKey eventType merchantOpCityId) >>= \case
    Just a -> return a
    Nothing -> cacheCoinConfig eventType merchantOpCityId /=<< Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId

cacheCoinConfig :: CacheFlow m r => DCT.DriverCoinsEventType -> Id DMOC.MerchantOperatingCity -> [CoinsConfig] -> m ()
cacheCoinConfig eventType merchantOpCityId coinsConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCoinConfigKey eventType merchantOpCityId) coinsConfig expTime

makeCoinConfigKey :: DCT.DriverCoinsEventType -> Id DMOC.MerchantOperatingCity -> Text
makeCoinConfigKey eventType merchantOpCityId = "CQ:CC:MOCId-" <> merchantOpCityId.getId <> "-EventType-" <> show eventType
