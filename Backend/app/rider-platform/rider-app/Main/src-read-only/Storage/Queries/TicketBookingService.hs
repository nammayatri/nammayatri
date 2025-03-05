{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingService (module Storage.Queries.TicketBookingService, module ReExport) where

import qualified Data.Aeson
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
import qualified Storage.Beam.TicketBookingService as Beam
import Storage.Queries.TicketBookingServiceExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingService.TicketBookingService -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketBookingService.TicketBookingService] -> m ())
createMany = traverse_ create

findAllByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m [Domain.Types.TicketBookingService.TicketBookingService])
findAllByBookingId ticketBookingId = do findAllWithKV [Se.Is Beam.ticketBookingId $ Se.Eq (Kernel.Types.Id.getId ticketBookingId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketBookingService.TicketBookingService))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByShortId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketBookingService.TicketBookingService))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

updateAllStatusByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ())
updateAllStatusByBookingId status ticketBookingId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.ticketBookingId $ Se.Eq (Kernel.Types.Id.getId ticketBookingId)]

updateStatusAndCancelledSeatsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m ())
updateStatusAndCancelledSeatsById status cancelledSeats id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.cancelledSeats cancelledSeats, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m ())
updateVerificationById status verificationCount id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.verificationCount verificationCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe Domain.Types.TicketBookingService.TicketBookingService))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBookingService.TicketBookingService -> m ())
updateByPrimaryKey (Domain.Types.TicketBookingService.TicketBookingService {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount ((.amount) amount),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.bHourId (Kernel.Types.Id.getId <$> bHourId),
      Se.Set Beam.bookedSeats bookedSeats,
      Se.Set Beam.btype btype,
      Se.Set Beam.cancelledSeats cancelledSeats,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.expiryDate expiryDate,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketBookingId (Kernel.Types.Id.getId ticketBookingId),
      Se.Set Beam.ticketServiceId (Kernel.Types.Id.getId ticketServiceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vendorSplitDetails (Data.Aeson.toJSON <$> vendorSplitDetails),
      Se.Set Beam.verificationCount verificationCount,
      Se.Set Beam.visitDate visitDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
