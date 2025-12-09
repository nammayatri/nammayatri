{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBookingAssignments (module Storage.Queries.FleetBookingAssignments, module ReExport) where

import qualified Domain.Types.FleetBookingAssignments
import qualified Domain.Types.FleetBookingInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetBookingAssignments as Beam
import Storage.Queries.FleetBookingAssignmentsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBookingAssignments.FleetBookingAssignments -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetBookingAssignments.FleetBookingAssignments] -> m ())
createMany = traverse_ create

findAllByMainAssignmentId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetBookingInformation.FleetBookingInformation -> m ([Domain.Types.FleetBookingAssignments.FleetBookingAssignments]))
findAllByMainAssignmentId mainAssignmentId = do findAllWithKV [Se.Is Beam.mainAssignmentId $ Se.Eq (Kernel.Types.Id.getId mainAssignmentId)]

updateAssignmentTiming ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FleetBookingAssignments.FleetBookingAssignments -> m ())
updateAssignmentTiming assignmentStartTime assignmentEndTime skuDurationMins id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.assignmentStartTime assignmentStartTime,
      Se.Set Beam.assignmentEndTime assignmentEndTime,
      Se.Set Beam.skuDurationMins skuDurationMins,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetBookingAssignments.FleetBookingAssignments -> m (Maybe Domain.Types.FleetBookingAssignments.FleetBookingAssignments))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBookingAssignments.FleetBookingAssignments -> m ())
updateByPrimaryKey (Domain.Types.FleetBookingAssignments.FleetBookingAssignments {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.assignmentEndTime assignmentEndTime,
      Se.Set Beam.assignmentStartTime assignmentStartTime,
      Se.Set Beam.bookingId bookingId,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.mainAssignmentId (Kernel.Types.Id.getId mainAssignmentId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.paymentMethod paymentMethod,
      Se.Set Beam.placeName placeName,
      Se.Set Beam.serviceId serviceId,
      Se.Set Beam.serviceName serviceName,
      Se.Set Beam.skuDurationMins skuDurationMins,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleNo vehicleNo,
      Se.Set Beam.visitDate visitDate
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
