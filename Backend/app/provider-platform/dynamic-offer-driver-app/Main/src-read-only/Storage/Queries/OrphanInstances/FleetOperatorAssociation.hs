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
            createdAt = createdAt,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            operatorId = operatorId,
            updatedAt = updatedAt,
            merchantId = fmap Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = fmap Kernel.Types.Id.Id merchantOperatingCityId
          }

instance ToTType' Beam.FleetOperatorAssociation Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation where
  toTType' (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation {..}) = do
    Beam.FleetOperatorAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.operatorId = operatorId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = fmap Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = fmap Kernel.Types.Id.getId merchantOperatingCityId
      }
