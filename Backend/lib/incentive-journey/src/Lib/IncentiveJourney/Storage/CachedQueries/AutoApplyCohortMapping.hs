{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.AutoApplyCohortMapping
  ( findApplicableByMerchantAndCity,
    clearCacheByMerchantAndCity,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping as DAuto
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMappingExtra as QAuto
import Lib.IncentiveJourney.Types.Actor (JourneyActor, actorCachePrefix)

-- | Enabled auto-apply rows for this merchant, city, and driver vehicle category.
-- Key matches CoinsConfig: city id plus vehicle category. A null category on a row
-- is part of every category lookup, so clearing a null category deletes every key.
findApplicableByMerchantAndCity ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  m [DAuto.AutoApplyCohortMapping]
findApplicableByMerchantAndCity actor merchantId merchantOperatingCityId mbDriverVehicleCategory =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByMerchantAndCityKey actor merchantId merchantOperatingCityId mbDriverVehicleCategory)) >>= \case
    Just cached -> pure cached
    Nothing -> do
      fetched <- QAuto.findApplicableByMerchantAndCity merchantId merchantOperatingCityId mbDriverVehicleCategory
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByMerchantAndCityKey actor merchantId merchantOperatingCityId mbDriverVehicleCategory) fetched expTime
      pure fetched

clearCacheByMerchantAndCity ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  [Maybe DTV.VehicleCategory] ->
  m ()
clearCacheByMerchantAndCity actor merchantId merchantOperatingCityId categories =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      mapM_
        (void . Hedis.del . makeByMerchantAndCityKey actor merchantId merchantOperatingCityId)
        (cacheKeyCategories categories)

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

cacheKeyCategories :: [Maybe DTV.VehicleCategory] -> [Maybe Text]
cacheKeyCategories categories
  | any isNothing categories = nub $ Nothing : map (Just . T.pack . show) allVehicleCategories
  | otherwise = nub $ map (fmap (T.pack . show)) categories

makeByMerchantAndCityKey ::
  JourneyActor ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  Text
makeByMerchantAndCityKey actor merchantId merchantOperatingCityId mbVehicleCategory =
  actorCachePrefix actor
    <> ":CachedQueries:AutoApplyCohortMapping:MerchantId-"
    <> merchantId.getId
    <> ":MerchantOperatingCityId-"
    <> merchantOperatingCityId.getId
    <> ":vehicleCategory-"
    <> fromMaybe "Nothing" mbVehicleCategory
