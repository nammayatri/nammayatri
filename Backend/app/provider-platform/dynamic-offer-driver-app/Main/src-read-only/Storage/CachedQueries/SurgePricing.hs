{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.SurgePricing where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SurgePricing
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SurgePricing as Queries

findByHexDayHourForMerchantOpCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Kernel.Prelude.Maybe Domain.Types.SurgePricing.SurgePricing))
findByHexDayHourForMerchantOpCity merchantOperatingCityId sourceHex dayOfWeek hourOfDay = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:SurgePricing:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":SourceHex-" <> show sourceHex <> ":DayOfWeek-" <> show dayOfWeek <> ":HourOfDay-" <> show hourOfDay)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:SurgePricing:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":SourceHex-" <> show sourceHex <> ":DayOfWeek-" <> show dayOfWeek <> ":HourOfDay-" <> show hourOfDay) dataToBeCached expTime
                )
                /=<< Queries.findByHexDayHourForMerchantOpCity merchantOperatingCityId sourceHex dayOfWeek hourOfDay
        )
