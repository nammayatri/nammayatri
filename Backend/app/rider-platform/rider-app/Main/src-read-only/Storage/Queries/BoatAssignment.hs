{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BoatAssignment where

import qualified Domain.Types.BoatAssignment
import qualified Domain.Types.TicketBookingService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BoatAssignment as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BoatAssignment.BoatAssignment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BoatAssignment.BoatAssignment] -> m ())
createMany = traverse_ create

findByBoatAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.UTCTime -> m ([Domain.Types.BoatAssignment.BoatAssignment]))
findByBoatAndDate boatNumber assignedAt = do findAllWithKV [Se.And [Se.Is Beam.boatNumber $ Se.Eq boatNumber, Se.Is Beam.assignedAt $ Se.Eq assignedAt]]

findByBookingServiceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.BoatAssignment.BoatAssignment))
findByBookingServiceId ticketBookingServiceId = do findOneWithKV [Se.Is Beam.ticketBookingServiceId $ Se.Eq (Kernel.Types.Id.getId ticketBookingServiceId)]

findByDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.UTCTime -> m ([Domain.Types.BoatAssignment.BoatAssignment]))
findByDate assignedAt = do findAllWithKV [Se.Is Beam.assignedAt $ Se.Eq assignedAt]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BoatAssignment.BoatAssignment -> m (Maybe Domain.Types.BoatAssignment.BoatAssignment))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BoatAssignment.AssignmentStatus -> Kernel.Types.Id.Id Domain.Types.BoatAssignment.BoatAssignment -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BoatAssignment.BoatAssignment -> m (Maybe Domain.Types.BoatAssignment.BoatAssignment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BoatAssignment.BoatAssignment -> m ())
updateByPrimaryKey (Domain.Types.BoatAssignment.BoatAssignment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.assignedAt assignedAt,
      Se.Set Beam.assignedBy assignedBy,
      Se.Set Beam.boatNumber boatNumber,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.status status,
      Se.Set Beam.ticketBookingServiceId (Kernel.Types.Id.getId ticketBookingServiceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BoatAssignment Domain.Types.BoatAssignment.BoatAssignment where
  fromTType' (Beam.BoatAssignmentT {..}) = do
    pure $
      Just
        Domain.Types.BoatAssignment.BoatAssignment
          { assignedAt = assignedAt,
            assignedBy = assignedBy,
            boatNumber = boatNumber,
            createdAt = createdAt,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            status = status,
            ticketBookingServiceId = Kernel.Types.Id.Id ticketBookingServiceId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.BoatAssignment Domain.Types.BoatAssignment.BoatAssignment where
  toTType' (Domain.Types.BoatAssignment.BoatAssignment {..}) = do
    Beam.BoatAssignmentT
      { Beam.assignedAt = assignedAt,
        Beam.assignedBy = assignedBy,
        Beam.boatNumber = boatNumber,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.status = status,
        Beam.ticketBookingServiceId = Kernel.Types.Id.getId ticketBookingServiceId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
