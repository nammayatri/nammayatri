{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetVehicleAssignment (module Storage.Queries.FleetVehicleAssignment, module ReExport) where

import qualified Domain.Types.FleetVehicleAssignment
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetVehicleAssignment as Beam
import Storage.Queries.FleetVehicleAssignmentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment] -> m ())
createMany = traverse_ create

findByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m ([Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment]))
findByFleetOwnerId fleetOwnerId = do findAllWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId <$> fleetOwnerId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment -> m (Maybe Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTicketBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment]))
findByTicketBookingId ticketBookingId = do findAllWithKV [Se.Is Beam.ticketBookingId $ Se.Eq ticketBookingId]

findByVehicleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Vehicle.Vehicle) -> m ([Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment]))
findByVehicleId vehicleId = do findAllWithKV [Se.Is Beam.vehicleId $ Se.Eq (Kernel.Types.Id.getId <$> vehicleId)]

updateAssignmentStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Vehicle.Vehicle) -> Domain.Types.FleetVehicleAssignment.AssignmentStatus -> Kernel.Types.Id.Id Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment -> m ())
updateAssignmentStatus fleetOwnerId vehicleId assignmentStatus id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId <$> fleetOwnerId),
      Se.Set Beam.vehicleId (Kernel.Types.Id.getId <$> vehicleId),
      Se.Set Beam.assignmentStatus assignmentStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment -> m (Maybe Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment -> m ())
updateByPrimaryKey (Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.assignedAt assignedAt,
      Se.Set Beam.assignmentStatus assignmentStatus,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId <$> fleetOwnerId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.ticketBookingId ticketBookingId,
      Se.Set Beam.ticketBookingServiceId ticketBookingServiceId,
      Se.Set Beam.ticketPlaceId ticketPlaceId,
      Se.Set Beam.vehicleId (Kernel.Types.Id.getId <$> vehicleId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
