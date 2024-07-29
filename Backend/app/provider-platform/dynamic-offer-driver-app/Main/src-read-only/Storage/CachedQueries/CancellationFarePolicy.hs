{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.CancellationFarePolicy where

import qualified Domain.Types.CancellationFarePolicy
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.CancellationFarePolicy as Queries

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationFarePolicy.CancellationFarePolicy -> m (Kernel.Prelude.Maybe Domain.Types.CancellationFarePolicy.CancellationFarePolicy))
findById id = do
  (Hedis.safeGet $ "driverOfferCachedQueries:CancellationFarePolicy:" <> ":Id-" <> Kernel.Types.Id.getId id)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("driverOfferCachedQueries:CancellationFarePolicy:" <> ":Id-" <> Kernel.Types.Id.getId id) dataToBeCached expTime
                )
                /=<< Queries.findById id
        )
