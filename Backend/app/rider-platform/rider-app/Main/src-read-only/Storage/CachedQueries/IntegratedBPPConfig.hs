{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.IntegratedBPPConfig where

import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IntegratedBPPConfig as Queries

findAllByDomainAndCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  BecknV2.OnDemand.Enums.VehicleCategory ->
  Domain.Types.IntegratedBPPConfig.PlatformType ->
  m [Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig]
findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType = do
  Hedis.safeGet ("CachedQueries:IntegratedBPPConfig:" <> ":Domain-" <> show domain <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":VehicleCategory-" <> show vehicleCategory <> ":PlatformType-" <> show platformType) >>= \case
    Just a -> pure a
    Nothing ->
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp ("CachedQueries:IntegratedBPPConfig:" <> ":Domain-" <> show domain <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":VehicleCategory-" <> show vehicleCategory <> ":PlatformType-" <> show platformType) dataToBeCached expTime
      )
        /=<< Queries.findAllByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType
