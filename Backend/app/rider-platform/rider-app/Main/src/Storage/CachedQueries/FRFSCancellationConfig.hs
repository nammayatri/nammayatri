module Storage.CachedQueries.FRFSCancellationConfig
  ( findAllByMerchantOpCityAndVehicleCategory,
    clearCache,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSCancellationConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSCancellationConfig as Queries

findAllByMerchantOpCityAndVehicleCategory ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  m [FRFSCancellationConfig]
findAllByMerchantOpCityAndVehicleCategory merchantOpCityId vehicleCategory =
  Hedis.safeGet (makeCacheKey merchantOpCityId vehicleCategory) >>= \case
    Just configs -> return configs
    Nothing -> do
      configs <- Queries.findAllByMerchantOpCityAndVehicleCategory merchantOpCityId vehicleCategory
      cacheConfigs merchantOpCityId vehicleCategory configs
      return configs

cacheConfigs ::
  (CacheFlow m r) =>
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  [FRFSCancellationConfig] ->
  m ()
cacheConfigs merchantOpCityId vehicleCategory configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCacheKey merchantOpCityId vehicleCategory) configs expTime

makeCacheKey :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Spec.VehicleCategory -> Text
makeCacheKey merchantOpCityId vehicleCategory =
  "CachedQueries:FRFSCancellationConfig:CityId-" <> merchantOpCityId.getId <> ":VehicleCategory-" <> show vehicleCategory

clearCache ::
  (CacheFlow m r) =>
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  m ()
clearCache merchantOpCityId vehicleCategory =
  Hedis.del (makeCacheKey merchantOpCityId vehicleCategory)
