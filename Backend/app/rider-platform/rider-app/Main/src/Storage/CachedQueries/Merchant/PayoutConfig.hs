module Storage.CachedQueries.Merchant.PayoutConfig where

import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.PayoutConfig as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutConfig -> m ()
create = Queries.create

findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> PayoutEntity -> Maybe [LYT.ConfigVersionMap] -> m (Maybe PayoutConfig)
findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity id isPayoutEnabled payoutEntity mbConfigVersionMap =
  DynamicLogic.findOneConfigWithCacheKey
    (cast id)
    (LYT.RIDER_CONFIG LYT.PayoutConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity id isPayoutEnabled payoutEntity)
    (makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity)

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Bool -> PayoutEntity -> Text
makeMerchantOpCityIdKey id isPayoutEnabled payoutEntity = "CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> "-isPayoutEnabled:" <> show isPayoutEnabled <> "-payoutEntity:" <> show payoutEntity

findByCityIdAndVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe PayoutConfig)
findByCityIdAndVehicleCategory id vehicleCategory mbConfigVersionMap =
  DynamicLogic.findOneConfigWithCacheKey
    (cast id)
    (LYT.RIDER_CONFIG LYT.PayoutConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findByCityIdAndVehicleCategory id (Just vehicleCategory))
    (makeMerchantOpCityIdAndCategoryKey id vehicleCategory)

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe [LYT.ConfigVersionMap] ->
  m [PayoutConfig]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.RIDER_CONFIG LYT.PayoutConfig)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId id)

makeMerchantOpCityIdAndCategoryKey :: Id MerchantOperatingCity -> VehicleCategory -> Text
makeMerchantOpCityIdAndCategoryKey id vehicleCategory = "CachedQueries:MerchantPayoutConfig:CityId-" <> id.getId <> "-vehicleCategory:" <> show vehicleCategory

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> PayoutEntity -> m ()
clearCache merchantOpCityId isPayoutEnabled payoutEntity =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdKey merchantOpCityId isPayoutEnabled payoutEntity)
    (cast merchantOpCityId)
    (LYT.RIDER_CONFIG LYT.PayoutConfig)
    Nothing

clearCacheByCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> m ()
clearCacheByCategory merchantOpCityId vehicleCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdAndCategoryKey merchantOpCityId vehicleCategory)
    (cast merchantOpCityId)
    (LYT.RIDER_CONFIG LYT.PayoutConfig)
    Nothing

clearConfigCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> PayoutEntity -> m ()
clearConfigCache merchantOperatingCityId vehicleCategory payoutEntity = do
  clearCache merchantOperatingCityId True payoutEntity
  clearCache merchantOperatingCityId False payoutEntity
  clearCacheByCategory merchantOperatingCityId vehicleCategory
