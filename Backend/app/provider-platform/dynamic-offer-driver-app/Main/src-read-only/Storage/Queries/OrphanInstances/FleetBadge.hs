{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetBadge where

import qualified Domain.Types.FleetBadge
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetBadge as Beam

instance FromTType' Beam.FleetBadge Domain.Types.FleetBadge.FleetBadge where
  fromTType' (Beam.FleetBadgeT {..}) = do
    pure $
      Just
        Domain.Types.FleetBadge.FleetBadge
          { badgeName = badgeName,
            badgeRank = badgeRank,
            badgeType = badgeType,
            createdAt = createdAt,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id <$> personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetBadge Domain.Types.FleetBadge.FleetBadge where
  toTType' (Domain.Types.FleetBadge.FleetBadge {..}) = do
    Beam.FleetBadgeT
      { Beam.badgeName = badgeName,
        Beam.badgeRank = badgeRank,
        Beam.badgeType = badgeType,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId <$> personId,
        Beam.updatedAt = updatedAt
      }
