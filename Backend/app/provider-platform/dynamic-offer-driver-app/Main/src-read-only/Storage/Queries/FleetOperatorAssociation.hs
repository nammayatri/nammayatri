{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOperatorAssociation where

import qualified Domain.Types.FleetOperatorAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation] -> m ())
createMany = traverse_ create

findByFleetIdAndOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation))
findByFleetIdAndOperatorId fleetOwnerId operatorId isActive = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.operatorId $ Se.Eq operatorId,
          Se.Is Beam.isActive $ Se.Eq isActive
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m (Maybe Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.operatorId operatorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

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
