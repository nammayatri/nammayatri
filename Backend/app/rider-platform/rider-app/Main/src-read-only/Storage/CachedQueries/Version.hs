{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Version where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Version
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Version as Queries

findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> BecknV2.FRFS.Enums.VehicleCategory -> Domain.Types.Extra.Rollout.RawDataType -> Kernel.Prelude.Bool -> m ([Domain.Types.Version.Version]))
findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType merchantOperatingCityId vehicleType inputDataType isReadyToApply = do
  (Hedis.safeGet $ "CachedQueries:Version:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId <$> merchantOperatingCityId) <> ":VehicleType-" <> show vehicleType <> ":InputDataType-" <> show inputDataType)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("CachedQueries:Version:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId <$> merchantOperatingCityId) <> ":VehicleType-" <> show vehicleType <> ":InputDataType-" <> show inputDataType) dataToBeCached expTime
              )
                /=<< Queries.findAllReadyToApplyByMerchantOperatingCityAndVehicleTypeAndDataType isReadyToApply merchantOperatingCityId vehicleType inputDataType
        )
