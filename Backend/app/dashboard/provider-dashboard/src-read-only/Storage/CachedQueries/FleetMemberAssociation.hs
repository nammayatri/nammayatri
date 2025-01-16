{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.FleetMemberAssociation where

import qualified Domain.Types.FleetMemberAssociation
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.FleetMemberAssociation as Queries

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Domain.Types.FleetMemberAssociation.FleetMemberAssociation))
findByPrimaryKey fleetMemberId = do
  (Hedis.safeGet $ "providerDashboardCachedQueries:FleetMemberAssociation:" <> ":FleetMemberId-" <> show fleetMemberId)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("providerDashboardCachedQueries:FleetMemberAssociation:" <> ":FleetMemberId-" <> show fleetMemberId) dataToBeCached expTime
                )
                /=<< Queries.findByPrimaryKey fleetMemberId
        )
