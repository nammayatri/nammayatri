{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetOperatorAssociation where

import qualified Domain.Types.FleetOperatorAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetOperatorAssociation as Beam

instance FromTType' Beam.FleetOperatorAssociation Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation where
  fromTType' (Beam.FleetOperatorAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation
          { associatedOn = associatedOn,
            associatedTill = associatedTill,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            operatorId = operatorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetOperatorAssociation Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation where
  toTType' (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation {..}) = do
    Beam.FleetOperatorAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.operatorId = operatorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
