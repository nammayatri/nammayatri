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
    clearDriverIncentiveConfigHashForDriver,
    getDriverIncentiveConfigGeneration,
    mkDriverIncentiveConfigETag,
  )
where

import Data.Text (pack)
import qualified Data.Text as T
import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
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
-- ETag Redis for GET /coins/incentiveConfig.
--
-- Payload depends on Person.driverTag, so content hashes are per-driver.
-- CoinsConfig create/update bumps a city+vehicleCategory generation so all
-- drivers refetch without deleting every per-driver key (bulk-safe).
-- Person.driverTag update only deletes that driver's hash keys.

allVehicleCategories :: [DTV.VehicleCategory]
allVehicleCategories =
  [ DTV.CAR,
    DTV.MOTORCYCLE,
    DTV.TRAIN,
    DTV.BUS,
    DTV.FLIGHT,
    DTV.AUTO_CATEGORY,
    DTV.AMBULANCE,
    DTV.TRUCK,
    DTV.BOAT,
    DTV.TOTO
  ]

driverIncentiveConfigGenRedisKey :: Text -> DTV.VehicleCategory -> Text
driverIncentiveConfigGenRedisKey mocId vehicleCategory =
  "DriverIncentiveCoins:Config:Gen:MocId:"
    <> mocId
    <> ":VehicleCategory:"
    <> show vehicleCategory
    <> ":EventName:EndRide"

driverIncentiveConfigHashRedisKey :: Text -> DTV.VehicleCategory -> Text -> Text
driverIncentiveConfigHashRedisKey mocId vehicleCategory driverId =
  "DriverIncentiveCoins:Config:Hash:MocId:"
    <> mocId
    <> ":VehicleCategory:"
    <> show vehicleCategory
    <> ":DriverId:"
    <> driverId
    <> ":EventName:EndRide"

getDriverIncentiveConfigGeneration :: (CacheFlow m r) => Text -> DTV.VehicleCategory -> m Integer
getDriverIncentiveConfigGeneration mocId vehicleCategory =
  fromMaybe 0 <$> Hedis.safeGet (driverIncentiveConfigGenRedisKey mocId vehicleCategory)

-- | Returns cached ETag only when it was written under the current generation.
getDriverIncentiveConfigHash :: (CacheFlow m r) => Text -> DTV.VehicleCategory -> Text -> m (Maybe Text)
getDriverIncentiveConfigHash mocId vehicleCategory driverId = do
  gen <- getDriverIncentiveConfigGeneration mocId vehicleCategory
  mbETag <- Hedis.safeGet (driverIncentiveConfigHashRedisKey mocId vehicleCategory driverId)
  pure $
    mbETag >>= \eTag ->
      if etagGeneration eTag == Just gen then Just eTag else Nothing

setDriverIncentiveConfigHash :: (CacheFlow m r) => Text -> DTV.VehicleCategory -> Text -> Text -> m ()
setDriverIncentiveConfigHash mocId vehicleCategory driverId eTag = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (driverIncentiveConfigHashRedisKey mocId vehicleCategory driverId) eTag expTime

-- | After CoinsConfig create/update: bump generation so all drivers miss cache
-- without a city-wide delete of per-driver keys.
clearDriverIncentiveConfigHash :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m ()
clearDriverIncentiveConfigHash merchantOpCityId mbVehicleCategory =
  case mbVehicleCategory of
    Just vc -> void $ Hedis.incr (driverIncentiveConfigGenRedisKey merchantOpCityId.getId vc)
    Nothing -> mapM_ (\vc -> void $ Hedis.incr (driverIncentiveConfigGenRedisKey merchantOpCityId.getId vc)) allVehicleCategories

-- | After Person.driverTag update: drop only this driver's cached ETags.
clearDriverIncentiveConfigHashForDriver :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> m ()
clearDriverIncentiveConfigHashForDriver merchantOpCityId driverId =
  mapM_
    ( \vc -> void $ Hedis.del (driverIncentiveConfigHashRedisKey merchantOpCityId.getId vc driverId.getId)
    )
    allVehicleCategories

-- | ETag format: "<generation>:<contentHash>"
mkDriverIncentiveConfigETag :: Integer -> Text -> Text
mkDriverIncentiveConfigETag gen contentHash = T.pack (show gen) <> ":" <> contentHash

etagGeneration :: Text -> Maybe Integer
etagGeneration eTag =
  case T.break (== ':') eTag of
    (genText, rest) | not (T.null rest) -> readMaybe (T.unpack genText)
    _ -> Nothing
