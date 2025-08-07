{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleAssignment (module Storage.Queries.VehicleAssignment, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle
import qualified Domain.Types.VehicleAssignment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleAssignment as Beam
import Storage.Queries.VehicleAssignmentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleAssignment.VehicleAssignment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleAssignment.VehicleAssignment] -> m ())
createMany = traverse_ create

findByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.VehicleAssignment.VehicleAssignment]))
findByFleetOwnerId fleetOwnerId = do findAllWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId)]

findByTicketId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.VehicleAssignment.VehicleAssignment]))
findByTicketId ticketId = do findAllWithKV [Se.Is Beam.ticketId $ Se.Eq ticketId]

findByVehicleId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Vehicle.Vehicle -> m ([Domain.Types.VehicleAssignment.VehicleAssignment]))
findByVehicleId vehicleId = do findAllWithKV [Se.Is Beam.vehicleId $ Se.Eq (Kernel.Types.Id.getId vehicleId)]

updateAssignmentStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.VehicleAssignment.AssignmentStatus -> Kernel.Types.Id.Id Domain.Types.VehicleAssignment.VehicleAssignment -> m ())
updateAssignmentStatus assignmentStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.assignmentStatus assignmentStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleAssignment.VehicleAssignment -> m (Maybe Domain.Types.VehicleAssignment.VehicleAssignment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleAssignment.VehicleAssignment -> m ())
updateByPrimaryKey (Domain.Types.VehicleAssignment.VehicleAssignment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.assignedAt assignedAt,
      Se.Set Beam.assignmentStatus assignmentStatus,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.placeId placeId,
      Se.Set Beam.ticketId ticketId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleId (Kernel.Types.Id.getId vehicleId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
