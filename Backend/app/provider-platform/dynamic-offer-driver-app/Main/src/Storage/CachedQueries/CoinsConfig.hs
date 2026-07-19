{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.CoinsConfig
  ( findAllByMerchantOptCityId,
    fetchFunctionsOnEventbasisInRideFlow,
    fetchFunctionsOnEventbasis,
    fetchConfigOnEventAndFunctionBasisInRideFlow,
    fetchConfigOnEventAndFunctionBasis,
    clearCache,
    clearCityCache,
    getDriverIncentiveConfigHash,
    setDriverIncentiveConfigHash,
    clearDriverIncentiveConfigHash,
  )
where

import Data.Text (pack)
import qualified Data.Text as T
import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.Coins.CoinsConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

findAllByMerchantOptCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [CoinsConfig]
findAllByMerchantOptCityId merchantOpCityId =
  DynamicLogic.findAllConfigsWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    Nothing
    Nothing
    (Queries.findAllByMerchantOptCityId merchantOpCityId)
    ("cachedQueries:Coins:MocId-" <> merchantOpCityId.getId)

fetchFunctionsOnEventbasisInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> [LYT.ConfigVersionMap] -> m [CoinsConfig]
fetchFunctionsOnEventbasisInRideFlow eventType merchantId merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType configVersionMap = fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType (Just configVersionMap)

fetchFunctionsOnEventbasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> Maybe [LYT.ConfigVersionMap] -> m [CoinsConfig]
fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType mbConfigVersionMap = do
  let eventTypeText = pack (show eventType)
  -- Try with serviceTierType first (if provided)
  result <- case mbServiceTierType of
    Just stt -> do
      res <-
        DynamicLogic.findAllConfigsWithCacheKey
          (cast merchantOpCityId)
          (LYT.DRIVER_CONFIG LYT.CoinsConfig)
          mbConfigVersionMap
          Nothing
          (Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehicleCategory) (Just stt) tripCategoryType)
          (makeCoinConfigKey eventTypeText merchantOpCityId vehicleCategory (Just stt) tripCategoryType)
      if null res
        then pure []
        else pure res
    Nothing -> pure []
  -- Fall back to vehicleCategory only (serviceTierType = Nothing)
  if null result
    then
      DynamicLogic.findAllConfigsWithCacheKey
        (cast merchantOpCityId)
        (LYT.DRIVER_CONFIG LYT.CoinsConfig)
        mbConfigVersionMap
        Nothing
        (Queries.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehicleCategory) Nothing tripCategoryType)
        (makeCoinConfigKey eventTypeText merchantOpCityId vehicleCategory Nothing tripCategoryType)
    else return result

makeCoinConfigKey :: Text -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> Text
makeCoinConfigKey eventType merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType =
  "cachedQueries:Coins:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> eventType
    <> ":vehicleCategory-"
    <> show vehicleCategory
    <> ":serviceTierType-"
    <> show mbServiceTierType
    <> ":tripType-"
    <> show tripCategoryType

-------------------------------------------------------------------------------------------------------------------------------------------------------------

fetchConfigOnEventAndFunctionBasisInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> [LYT.ConfigVersionMap] -> m (Maybe CoinsConfig)
fetchConfigOnEventAndFunctionBasisInRideFlow eventType eventFunction merchantId merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType configVersionMap = fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType (Just configVersionMap)

fetchConfigOnEventAndFunctionBasis :: (CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> Maybe [LYT.ConfigVersionMap] -> m (Maybe CoinsConfig)
fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType mbConfigVersionMap = do
  let eventTypeText = pack (show eventType)
  DynamicLogic.findOneConfigWithCacheKey
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    mbConfigVersionMap
    Nothing
    (Queries.fetchConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOpCityId (Just vehicleCategory) mbServiceTierType tripCategoryType)
    (makeCoinConfigOnEventAndFunctionKey eventTypeText eventFunction merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType)

makeCoinConfigOnEventAndFunctionKey :: Text -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> Text
makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType =
  "cachedQueries:Coins:MocId-"
    <> merchantOpCityId.getId
    <> ":EventType-"
    <> eventType
    <> ":vehicleCategory-"
    <> (show vehicleCategory)
    <> ":serviceTierType-"
    <> show mbServiceTierType
    <> ":eventFunction-"
    <> (show eventFunction)
    <> ":tripType-"
    <> show tripCategoryType

-------------------------------------------------------------------------------------------------------------------------------------------------------------

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Text -> DCT.DriverCoinsFunctionType -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> DCT.TripCategoryType -> m ()
clearCache eventType eventFunction merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType = do
  -- Clear cache for the specific serviceTierType
  DynamicLogic.clearConfigCacheWithPrefix
    (makeCoinConfigKey eventType merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    Nothing
  DynamicLogic.clearConfigCacheWithPrefix
    (makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory mbServiceTierType tripCategoryType)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    Nothing
  -- Also clear the fallback cache (serviceTierType = Nothing) since it may now be stale
  when (isJust mbServiceTierType) $ do
    DynamicLogic.clearConfigCacheWithPrefix
      (makeCoinConfigKey eventType merchantOpCityId vehicleCategory Nothing tripCategoryType)
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.CoinsConfig)
      Nothing
    DynamicLogic.clearConfigCacheWithPrefix
      (makeCoinConfigOnEventAndFunctionKey eventType eventFunction merchantOpCityId vehicleCategory Nothing tripCategoryType)
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.CoinsConfig)
      Nothing
  -- ConfigPilot getConfig loads via findAllByMerchantOptCityId (city-wide key). Must clear
  -- that too, otherwise EndRide keeps serving stale timeBounds after create/update.
  clearCityCache merchantOpCityId

-- | City-wide CoinsConfig cache used by ConfigPilot (and findAllByMerchantOptCityId).
clearCityCache :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
clearCityCache merchantOpCityId =
  DynamicLogic.clearConfigCacheWithPrefix
    ("cachedQueries:Coins:MocId-" <> merchantOpCityId.getId)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.CoinsConfig)
    Nothing

-------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ETag Redis for GET /coins/incentiveConfig (same pattern as SpecialLocation list).
-- Keyed by city + vehicleCategory + eventFunction because the payload is cohort-specific.

driverIncentiveConfigHashRedisKey :: Text -> DTV.VehicleCategory -> Text -> Text
driverIncentiveConfigHashRedisKey mocId vehicleCategory eventFunctionKey =
  "DriverIncentiveCoins:Config:Hash:MocId:"
    <> mocId
    <> ":VehicleCategory:"
    <> show vehicleCategory
    <> ":EventFunction:"
    <> eventFunctionKey

getDriverIncentiveConfigHash :: (CacheFlow m r) => Text -> DTV.VehicleCategory -> Text -> m (Maybe Text)
getDriverIncentiveConfigHash mocId vehicleCategory eventFunctionKey =
  Hedis.safeGet (driverIncentiveConfigHashRedisKey mocId vehicleCategory eventFunctionKey)

setDriverIncentiveConfigHash :: (CacheFlow m r) => Text -> DTV.VehicleCategory -> Text -> Text -> m ()
setDriverIncentiveConfigHash mocId vehicleCategory eventFunctionKey eTag =
  Hedis.set (driverIncentiveConfigHashRedisKey mocId vehicleCategory eventFunctionKey) eTag

-- | Clear ETag for a specific cohort after CoinsConfig create/update.
clearDriverIncentiveConfigHash :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> DCT.DriverCoinsFunctionType -> m ()
clearDriverIncentiveConfigHash merchantOpCityId mbVehicleCategory eventFunction =
  case mbVehicleCategory of
    Just vc ->
      void $
        Hedis.del
          ( driverIncentiveConfigHashRedisKey
              merchantOpCityId.getId
              vc
              (T.pack (show eventFunction))
          )
    Nothing -> pure ()
