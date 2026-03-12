{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.MonetaryRewardConfig where

import Data.Text (pack)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MonetaryRewardConfig
import Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MonetaryRewardConfigExtra as Queries
import qualified Tools.DynamicLogic as DynamicLogic

fetchMonetaryRewardFunctionsOnEventbasisInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> [LYT.ConfigVersionMap] -> m [MonetaryRewardConfig]
fetchMonetaryRewardFunctionsOnEventbasisInRideFlow eventType merchantId merchantOpCityId vehicleCategory configVersionMap = fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory (Just configVersionMap)

fetchMonetaryRewardFunctionsOnEventbasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m [MonetaryRewardConfig]
fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory mbConfigVersionMap = do
  let eventTypeText = pack (show eventType)
  result <-
    DynamicLogic.findAllConfigsWithCacheKey
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.MonetaryRewardConfig)
      mbConfigVersionMap
      Nothing
      (Queries.fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehicleCategory))
      (makeMonetaryRewardConfigKey eventTypeText merchantOpCityId vehicleCategory)
  if null result
    then
      DynamicLogic.findAllConfigsWithCacheKey
        (cast merchantOpCityId)
        (LYT.DRIVER_CONFIG LYT.MonetaryRewardConfig)
        mbConfigVersionMap
        Nothing
        (Queries.fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOpCityId Nothing)
        (makeMonetaryRewardConfigKey eventTypeText merchantOpCityId vehicleCategory)
    else return result

makeMonetaryRewardConfigKey :: Text -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Text
makeMonetaryRewardConfigKey eventType merchantOpCityId vehicleCategory =
  "cachedQueries:WalletIncentive:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> eventType
    <> ":vehicleCategory-"
    <> show vehicleCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------

fetchMonetaryRewardConfigOnEventAndFunctionBasisInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> [LYT.ConfigVersionMap] -> m (Maybe MonetaryRewardConfig)
fetchMonetaryRewardConfigOnEventAndFunctionBasisInRideFlow eventType eventFunction merchantId merchantOpCityId vehicleCategory configVersionMap = fetchMonetaryRewardConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory (Just configVersionMap)

fetchMonetaryRewardConfigOnEventAndFunctionBasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MonetaryRewardConfig)
fetchMonetaryRewardConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory mbConfigVersionMap = do
  let eventTypeText = pack (show eventType)
  DynamicLogic.findOneConfigWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MonetaryRewardConfig)
    mbConfigVersionMap
    Nothing
    (Queries.fetchMonetaryRewardConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId (Just vehicleCategory))
    (makeMonetaryRewardConfigOnEventAndFunctionKey eventTypeText eventFunction merchantOpCityId vehicleCategory)

makeMonetaryRewardConfigOnEventAndFunctionKey :: Text -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Text
makeMonetaryRewardConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory =
  "cachedQueries:WalletIncentive:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> eventType
    <> ":vehicleCategory-"
    <> show vehicleCategory
    <> ":eventFunction-"
    <> show eventFunction

-------------------------------------------------------------------------------------------------------------------------------------------------------------

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Text -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m ()
clearCache eventType eventFunction merchantOpCityId vehicleCategory = do
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMonetaryRewardConfigKey eventType merchantOpCityId vehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MonetaryRewardConfig)
    Nothing
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMonetaryRewardConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MonetaryRewardConfig)
    Nothing
