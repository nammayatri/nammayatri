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

import Data.Text (pack)
import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Storage.Queries.Coins.CoinsConfig as Queries

fetchFunctionsOnEventbasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m [CoinsConfig]
fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory =
  Hedis.safeGet (makeCoinConfigKey eventType merchantOpCityId vehicleCategory) >>= \case
    Just a -> return a
    Nothing -> do
      result <- Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehicleCategory)
      result' <-
        if null result
          then Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId Nothing
          else return result
      void $ cacheCoinConfig eventType merchantOpCityId vehicleCategory result
      return result'

cacheCoinConfig :: CacheFlow m r => DCT.DriverCoinsEventType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> [CoinsConfig] -> m ()
cacheCoinConfig eventType merchantOpCityId vehicleCategory coinsConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCoinConfigKey eventType merchantOpCityId vehicleCategory) coinsConfig expTime

makeCoinConfigKey :: DCT.DriverCoinsEventType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Text
makeCoinConfigKey eventType merchantOpCityId vehicleCategory =
  "cachedQueries:Coins:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> pack (show eventType)
    <> ":vehicleCategory-"
    <> show vehicleCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------

fetchConfigOnEventAndFunctionBasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m (Maybe CoinsConfig)
fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory =
  Hedis.safeGet (makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory) >>= \case
    Just a -> return a
    Nothing -> cacheCoinConfigOnEventAndFunction eventType eventFunction merchantOpCityId vehicleCategory /=<< Queries.fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId (Just vehicleCategory)

cacheCoinConfigOnEventAndFunction :: CacheFlow m r => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe CoinsConfig -> m ()
cacheCoinConfigOnEventAndFunction eventType eventFunction merchantOpCityId vehicleCategory coinsConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory) coinsConfig expTime

makeCoinConfigOnEventAndFunctionKey :: DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Text
makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory =
  "cachedQueries:Coins:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> pack (show eventType)
    <> ":vehicleCategory-"
    <> (show vehicleCategory)
    <> ":eventFunction-"
    <> (show eventFunction)
