{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.FleetBadgeAssociation where

import qualified Domain.Types.FleetBadgeAssociation
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetBadgeAssociation as Queries

findAllFleetBadgeAssociationByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m [Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation])
findAllFleetBadgeAssociationByFleetOwnerId fleetOwnerId isActive = do
  (Hedis.safeGet $ "driverOfferCachedQueries:FleetBadgeAssociation:" <> ":FleetOwnerId-" <> show fleetOwnerId <> ":IsActive-" <> show isActive)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("driverOfferCachedQueries:FleetBadgeAssociation:" <> ":FleetOwnerId-" <> show fleetOwnerId <> ":IsActive-" <> show isActive) dataToBeCached expTime
              )
                /=<< Queries.findAllFleetBadgeAssociationByFleetOwnerId fleetOwnerId isActive
        )

findOneFleetBadgeAssociationByFleetOwnerIdAndDriverIdAndBadgeTypeAndIsActive ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.FleetBadgeType.FleetBadgeType -> Kernel.Prelude.Bool -> m (Kernel.Prelude.Maybe Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation))
findOneFleetBadgeAssociationByFleetOwnerIdAndDriverIdAndBadgeTypeAndIsActive fleetOwnerId driverId badgeType isActive = do
  (Hedis.safeGet $ "driverOfferCachedQueries:FleetBadgeAssociation:" <> ":FleetOwnerId-" <> show fleetOwnerId <> ":DriverId-" <> Kernel.Types.Id.getId driverId <> ":BadgeType-" <> show badgeType <> ":IsActive-" <> show isActive)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("driverOfferCachedQueries:FleetBadgeAssociation:" <> ":FleetOwnerId-" <> show fleetOwnerId <> ":DriverId-" <> Kernel.Types.Id.getId driverId <> ":BadgeType-" <> show badgeType <> ":IsActive-" <> show isActive) dataToBeCached expTime
                )
                /=<< Queries.findOneFleetBadgeAssociationByFleetOwnerIdAndDriverIdAndBadgeTypeAndIsActive fleetOwnerId driverId badgeType isActive
        )
