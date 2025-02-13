{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Rollout where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Rollout
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Rollout as Queries

findAllByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> m ([Domain.Types.Rollout.Rollout]))
findAllByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType = do
  (Hedis.safeGet $ "CachedQueries:Rollout:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId <$> merchantOperatingCityId) <> ":VehicleType-" <> show vehicleType)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("CachedQueries:Rollout:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId <$> merchantOperatingCityId) <> ":VehicleType-" <> show vehicleType) dataToBeCached expTime
              )
                /=<< Queries.findAllByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType
        )
