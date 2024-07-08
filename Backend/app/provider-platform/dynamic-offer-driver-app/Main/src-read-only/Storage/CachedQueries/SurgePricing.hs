{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.SurgePricing where

import qualified Domain.Types.SurgePricing
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.SurgePricing as Queries

findByHexDayAndHour ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Kernel.Prelude.Maybe Domain.Types.SurgePricing.SurgePricing))
findByHexDayAndHour sourceHex dayOfWeek hourOfDay = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:SurgePricing:" <> ":SourceHex-" <> show sourceHex <> ":DayOfWeek-" <> show dayOfWeek <> ":HourOfDay-" <> show hourOfDay)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:SurgePricing:" <> ":SourceHex-" <> show sourceHex <> ":DayOfWeek-" <> show dayOfWeek <> ":HourOfDay-" <> show hourOfDay) dataToBeCached expTime
                )
                /=<< Queries.findByHexDayAndHour sourceHex dayOfWeek hourOfDay
        )
