{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetRCAssociation where

import qualified Domain.Types.FleetRCAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetRCAssociation as Beam

instance FromTType' Beam.FleetRCAssociation Domain.Types.FleetRCAssociation.FleetRCAssociation where
  fromTType' (Beam.FleetRCAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetRCAssociation.FleetRCAssociation
          { associatedOn = associatedOn,
            associatedTill = associatedTill,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            rcId = Kernel.Types.Id.Id rcId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetRCAssociation Domain.Types.FleetRCAssociation.FleetRCAssociation where
  toTType' (Domain.Types.FleetRCAssociation.FleetRCAssociation {..}) = do
    Beam.FleetRCAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
