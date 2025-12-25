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
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.Coins.CoinsConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

fetchFunctionsOnEventbasisInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> [LYT.ConfigVersionMap] -> m [CoinsConfig]
fetchFunctionsOnEventbasisInRideFlow eventType merchantId merchantOpCityId vehicleCategory configVersionMap = fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory (Just configVersionMap)

fetchFunctionsOnEventbasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m [CoinsConfig]
fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory mbConfigVersionMap = do
  let eventTypeText = pack (show eventType)
  result <-
    DynamicLogic.findAllConfigsWithCacheKey
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.CoinsConfig)
      mbConfigVersionMap
      Nothing
      (Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehicleCategory))
      (makeCoinConfigKey eventTypeText merchantOpCityId vehicleCategory)
  if null result
    then
      DynamicLogic.findAllConfigsWithCacheKey
        (cast merchantOpCityId)
        (LYT.DRIVER_CONFIG LYT.CoinsConfig)
        mbConfigVersionMap
        Nothing
        (Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId Nothing)
        (makeCoinConfigKey eventTypeText merchantOpCityId vehicleCategory)
    else return result

makeCoinConfigKey :: Text -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Text
makeCoinConfigKey eventType merchantOpCityId vehicleCategory =
  "cachedQueries:Coins:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> eventType
    <> ":vehicleCategory-"
    <> show vehicleCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------

fetchConfigOnEventAndFunctionBasisInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> [LYT.ConfigVersionMap] -> m (Maybe CoinsConfig)
fetchConfigOnEventAndFunctionBasisInRideFlow eventType eventFunction merchantId merchantOpCityId vehicleCategory configVersionMap = fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory (Just configVersionMap)

fetchConfigOnEventAndFunctionBasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe CoinsConfig)
fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory mbConfigVersionMap = do
  let eventTypeText = pack (show eventType)
  DynamicLogic.findOneConfigWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    mbConfigVersionMap
    Nothing
    (Queries.fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId (Just vehicleCategory))
    (makeCoinConfigOnEventAndFunctionKey eventTypeText eventFunction merchantOpCityId vehicleCategory)

makeCoinConfigOnEventAndFunctionKey :: Text -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Text
makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory =
  "cachedQueries:Coins:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> eventType
    <> ":vehicleCategory-"
    <> (show vehicleCategory)
    <> ":eventFunction-"
    <> (show eventFunction)

-------------------------------------------------------------------------------------------------------------------------------------------------------------

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Text -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m ()
clearCache eventType eventFunction merchantOpCityId vehicleCategory = do
  DynamicLogic.clearConfigCacheWithPrefix
    (makeCoinConfigKey eventType merchantOpCityId vehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    Nothing
  DynamicLogic.clearConfigCacheWithPrefix
    (makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    Nothing
