{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.PayoutConfig
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ Use Storage.ConfigPilot.Config.PayoutConfig (getConfig) instead for reads."
    #-}
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.PayoutConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutConfig -> m ()
create val = do
  Queries.create val
  clearCacheAll val.merchantOperatingCityId

findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> PayoutEntity -> Maybe [LYT.ConfigVersionMap] -> m (Maybe PayoutConfig)
findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity id isPayoutEnabled payoutEntity _mbConfigVersionMap =
  Hedis.safeGet (makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity) >>= \case
    Just config -> return $ Just config
    Nothing -> flip whenJust (cachePayoutConfig isPayoutEnabled payoutEntity) /=<< Queries.findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity id isPayoutEnabled payoutEntity

cachePayoutConfig :: (CacheFlow m r) => Bool -> PayoutEntity -> PayoutConfig -> m ()
cachePayoutConfig isPayoutEnabled payoutEntity config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdKey config.merchantOperatingCityId isPayoutEnabled payoutEntity) config expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Bool -> PayoutEntity -> Text
makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity = "CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> "-isPayoutEnabled:" <> show isPayoutEnabled <> "-payoutEntity:" <> show payoutEntity

findByCityIdAndVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe PayoutConfig)
findByCityIdAndVehicleCategory id vehicleCategory _mbConfigVersionMap =
  Hedis.safeGet (makeMerchantOpCityIdAndCategoryKey id vehicleCategory) >>= \case
    Just config -> return $ Just config
    Nothing -> flip whenJust (cachePayoutConfigByCategory id vehicleCategory) /=<< Queries.findByCityIdAndVehicleCategory id (Just vehicleCategory)

cachePayoutConfigByCategory :: (CacheFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> PayoutConfig -> m ()
cachePayoutConfigByCategory cityId vehicleCategory config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdAndCategoryKey cityId vehicleCategory) config expTime

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe [LYT.ConfigVersionMap] ->
  m [PayoutConfig]
findAllByMerchantOpCityId id _mbConfigVersionMap =
  Hedis.safeGet (makeMerchantOpCityIdAllKey id) >>= \case
    Just configs -> return configs
    Nothing -> cacheAllPayoutConfigs id /=<< Queries.findAllByMerchantOpCityId id

cacheAllPayoutConfigs :: (CacheFlow m r) => Id MerchantOperatingCity -> [PayoutConfig] -> m ()
cacheAllPayoutConfigs cityId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdAllKey cityId) configs expTime

makeMerchantOpCityIdAndCategoryKey :: Id MerchantOperatingCity -> VehicleCategory -> Text
makeMerchantOpCityIdAndCategoryKey id vehicleCategory = "CachedQueries:MerchantPayoutConfig:CityId-" <> id.getId <> "-vehicleCategory:" <> show vehicleCategory

makeMerchantOpCityIdAllKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdAllKey id = "CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> "-all"

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> PayoutEntity -> m ()
clearCache merchantOpCityId isPayoutEnabled payoutEntity =
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeMerchantOpCityIdKey merchantOpCityId isPayoutEnabled payoutEntity)

clearCacheByCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> m ()
clearCacheByCategory merchantOpCityId vehicleCategory =
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeMerchantOpCityIdAndCategoryKey merchantOpCityId vehicleCategory)

clearCacheAll :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheAll merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeMerchantOpCityIdAllKey merchantOpCityId)

clearConfigCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe VehicleCategory -> PayoutEntity -> m ()
clearConfigCache merchantOperatingCityId mbVehicleCategory payoutEntity = do
  clearCache merchantOperatingCityId True payoutEntity
  clearCache merchantOperatingCityId False payoutEntity
  whenJust mbVehicleCategory $ \vehicleCategory ->
    clearCacheByCategory merchantOperatingCityId vehicleCategory
  clearCacheAll merchantOperatingCityId

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutConfig -> m ()
updateByPrimaryKey cfg = do
  Queries.updateByPrimaryKey cfg
  clearConfigCache cfg.merchantOperatingCityId cfg.vehicleCategory cfg.payoutEntity
