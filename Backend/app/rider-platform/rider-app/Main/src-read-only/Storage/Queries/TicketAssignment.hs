{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketAssignment where

import qualified Domain.Types.TicketAssignment
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketAssignment as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketAssignment.TicketAssignment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketAssignment.TicketAssignment] -> m ())
createMany = traverse_ create

findByAssignmentNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.TicketAssignment.TicketAssignment]))
findByAssignmentNumber assignmentNumber = do findAllWithKV [Se.Is Beam.assignmentNumber $ Se.Eq assignmentNumber]

findByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe Domain.Types.TicketAssignment.TicketAssignment))
findByBookingId ticketBookingId = do findOneWithKV [Se.Is Beam.ticketBookingId $ Se.Eq (Kernel.Types.Id.getId ticketBookingId)]

findByBookingServiceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketAssignment.TicketAssignment))
findByBookingServiceId ticketBookingServiceId = do findOneWithKV [Se.Is Beam.ticketBookingServiceId $ Se.Eq (Kernel.Types.Id.getId ticketBookingServiceId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketAssignment.TicketAssignment -> m (Maybe Domain.Types.TicketAssignment.TicketAssignment))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketAssignment.AssignmentStatus -> Kernel.Types.Id.Id Domain.Types.TicketAssignment.TicketAssignment -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketAssignment.TicketAssignment -> m (Maybe Domain.Types.TicketAssignment.TicketAssignment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketAssignment.TicketAssignment -> m ())
updateByPrimaryKey (Domain.Types.TicketAssignment.TicketAssignment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.assignedBy assignedBy,
      Se.Set Beam.assignmentNumber assignmentNumber,
      Se.Set Beam.assignmentType assignmentType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.status status,
      Se.Set Beam.ticketBookingId (Kernel.Types.Id.getId ticketBookingId),
      Se.Set Beam.ticketBookingServiceId (Kernel.Types.Id.getId ticketBookingServiceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketAssignment Domain.Types.TicketAssignment.TicketAssignment where
  fromTType' (Beam.TicketAssignmentT {..}) = do
    pure $
      Just
        Domain.Types.TicketAssignment.TicketAssignment
          { assignedBy = assignedBy,
            assignmentNumber = assignmentNumber,
            assignmentType = assignmentType,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            status = status,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            ticketBookingServiceId = Kernel.Types.Id.Id ticketBookingServiceId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.TicketAssignment Domain.Types.TicketAssignment.TicketAssignment where
  toTType' (Domain.Types.TicketAssignment.TicketAssignment {..}) = do
    Beam.TicketAssignmentT
      { Beam.assignedBy = assignedBy,
        Beam.assignmentNumber = assignmentNumber,
        Beam.assignmentType = assignmentType,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.status = status,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.ticketBookingServiceId = Kernel.Types.Id.getId ticketBookingServiceId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
