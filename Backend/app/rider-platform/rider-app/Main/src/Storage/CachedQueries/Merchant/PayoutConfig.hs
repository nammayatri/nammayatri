module Storage.CachedQueries.Merchant.PayoutConfig where

import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PayoutConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutConfig -> m ()
create = Queries.create

findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> PayoutEntity -> m (Maybe PayoutConfig)
findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity id isPayoutEnabled payoutEntity =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity) >>= \case
    Just a -> return a
    Nothing -> cachePayoutConfigForCityByEnabled id isPayoutEnabled payoutEntity /=<< Queries.findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity id isPayoutEnabled payoutEntity

cachePayoutConfigForCityByEnabled :: CacheFlow m r => Id MerchantOperatingCity -> Bool -> PayoutEntity -> Maybe PayoutConfig -> m ()
cachePayoutConfigForCityByEnabled id isPayoutEnabled payoutEntity cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity
  Hedis.setExp idKey cfg expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Bool -> PayoutEntity -> Text
makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity = "CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> "-isPayoutEnabled:" <> show isPayoutEnabled <> "-payoutEntity:" <> show payoutEntity

findByCityIdAndVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> m (Maybe PayoutConfig)
findByCityIdAndVehicleCategory id vehicleCategory =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdAndCategoryKey id vehicleCategory) >>= \case
    Just a -> return a
    Nothing -> cachePayoutConfigForCityAndVehicleCategory id vehicleCategory /=<< Queries.findByCityIdAndVehicleCategory id (Just vehicleCategory)

cachePayoutConfigForCityAndVehicleCategory :: CacheFlow m r => Id MerchantOperatingCity -> VehicleCategory -> Maybe PayoutConfig -> m ()
cachePayoutConfigForCityAndVehicleCategory id vehicleCategory cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdAndCategoryKey id vehicleCategory
  Hedis.setExp idKey cfg expTime

makeMerchantOpCityIdAndCategoryKey :: Id MerchantOperatingCity -> VehicleCategory -> Text
makeMerchantOpCityIdAndCategoryKey id vehicleCategory = "CachedQueries:MerchantPayoutConfig:CityId-" <> id.getId <> "-vehicleCategory:" <> show vehicleCategory

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Bool -> PayoutEntity -> m ()
clearCache merchantOpCityId isPayoutEnabled payoutEntity = Hedis.del (makeMerchantOpCityIdKey merchantOpCityId isPayoutEnabled payoutEntity)
