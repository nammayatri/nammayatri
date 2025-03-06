{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverOperatorAssociation where

import qualified Domain.Types.DriverOperatorAssociation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOperatorAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation] -> m ())
createMany = traverse_ create

deleteByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByOperatorId operatorId = do deleteWithKV [Se.Is Beam.operatorId $ Se.Eq operatorId]

findAllOperatorByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation]))
findAllOperatorByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation -> m (Maybe Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation -> m ())
updateByPrimaryKey (Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.isActive isActive,
      Se.Set Beam.operatorId operatorId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverOperatorAssociation Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation where
  fromTType' (Beam.DriverOperatorAssociationT {..}) = do
    pure $
      Just
        Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation
          { associatedOn = associatedOn,
            associatedTill = associatedTill,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            operatorId = operatorId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverOperatorAssociation Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation where
  toTType' (Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation {..}) = do
    Beam.DriverOperatorAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.operatorId = operatorId,
        Beam.updatedAt = updatedAt
      }
