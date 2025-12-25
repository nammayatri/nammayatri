{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.SurgePricing where

import qualified Domain.Types.Common
import qualified Domain.Types.SurgePricing
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.SurgePricing as Queries

findByHexDayHourAndVehicleServiceTier ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> Domain.Types.Common.ServiceTierType -> m (Kernel.Prelude.Maybe Domain.Types.SurgePricing.SurgePricing))
findByHexDayHourAndVehicleServiceTier sourceHex dayOfWeek hourOfDay vehicleServiceTier = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:SurgePricing:" <> ":SourceHex-" <> show sourceHex <> ":DayOfWeek-" <> show dayOfWeek <> ":HourOfDay-" <> show hourOfDay <> ":VehicleServiceTier-" <> show vehicleServiceTier)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:SurgePricing:" <> ":SourceHex-" <> show sourceHex <> ":DayOfWeek-" <> show dayOfWeek <> ":HourOfDay-" <> show hourOfDay <> ":VehicleServiceTier-" <> show vehicleServiceTier) dataToBeCached expTime
                )
                /=<< Queries.findByHexDayHourAndVehicleServiceTier sourceHex dayOfWeek hourOfDay vehicleServiceTier
        )
