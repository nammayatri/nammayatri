{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.DeviceVehicleMapping where

import qualified Data.Text
import qualified Domain.Types.DeviceVehicleMapping
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.DeviceVehicleMapping as Queries

findByDeviceIdCached :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Kernel.Prelude.Maybe Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping))
findByDeviceIdCached deviceId = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "CachedQueries:DeviceVehicleMapping:" <> ":DeviceId-" <> show deviceId)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.withCrossAppRedis $ Hedis.setExp ("CachedQueries:DeviceVehicleMapping:" <> ":DeviceId-" <> show deviceId) dataToBeCached expTime
                )
                /=<< Queries.findByDeviceId deviceId
        )
