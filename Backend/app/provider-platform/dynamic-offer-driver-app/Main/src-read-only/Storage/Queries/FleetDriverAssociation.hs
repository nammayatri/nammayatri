{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetDriverAssociation (module Storage.Queries.FleetDriverAssociation, module ReExport) where

import qualified Domain.Types.FleetDriverAssociation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetDriverAssociation as Beam
import Storage.Queries.FleetDriverAssociationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetDriverAssociation.FleetDriverAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetDriverAssociation.FleetDriverAssociation] -> m ())
createMany = traverse_ create

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findAllDriverByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.FleetDriverAssociation.FleetDriverAssociation])
findAllDriverByFleetOwnerId fleetOwnerId = do findAllWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetDriverAssociation.FleetDriverAssociation))
findByDriverId driverId isActive = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.isActive $ Se.Eq isActive]]

findByDriverIdAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetDriverAssociation.FleetDriverAssociation))
findByDriverIdAndFleetOwnerId driverId fleetOwnerId isActive = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.isActive $ Se.Eq isActive
        ]
    ]

updateFleetDriverActiveStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> m ())
updateFleetDriverActiveStatus isActive associatedTill driverId fleetOwnerId = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.isActive isActive, Se.Set Beam.associatedTill associatedTill, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetDriverAssociation.FleetDriverAssociation -> m (Maybe Domain.Types.FleetDriverAssociation.FleetDriverAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetDriverAssociation.FleetDriverAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetDriverAssociation.FleetDriverAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
