{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.RouteStopMapping where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RouteStopMapping
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RouteStopMapping as Queries

findAllByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.FRFS.Enums.VehicleCategory -> m [Domain.Types.RouteStopMapping.RouteStopMapping])
findAllByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType = do
  (Hedis.safeGet $ "CachedQueries:RouteStopMapping:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":VehicleType-" <> show vehicleType)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("CachedQueries:RouteStopMapping:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":VehicleType-" <> show vehicleType) dataToBeCached expTime
              )
                /=<< Queries.findAllByMerchantOperatingCityAndVehicleTypeWithoutOptions merchantOperatingCityId vehicleType
        )

findByRouteCodeAndStopCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Domain.Types.RouteStopMapping.RouteStopMapping))
findByRouteCodeAndStopCode routeCode stopCode = do
  (Hedis.safeGet $ "CachedQueries:RouteStopMapping:" <> ":RouteCode-" <> show routeCode <> ":StopCode-" <> show stopCode)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("CachedQueries:RouteStopMapping:" <> ":RouteCode-" <> show routeCode <> ":StopCode-" <> show stopCode) dataToBeCached expTime
                )
                /=<< Queries.findByRouteCodeAndStopCode routeCode stopCode
        )
