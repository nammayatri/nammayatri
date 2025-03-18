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

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation))
findByDriverId driverId isActive = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.isActive $ Se.Eq isActive]]

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
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.isActive isActive,
      Se.Set Beam.operatorId operatorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
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
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            operatorId = operatorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverOperatorAssociation Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation where
  toTType' (Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation {..}) = do
    Beam.DriverOperatorAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.operatorId = operatorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
