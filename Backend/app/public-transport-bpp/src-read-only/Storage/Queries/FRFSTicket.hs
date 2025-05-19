{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicket where

import qualified Domain.Types.FRFSSearchRequest
import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicket as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicket.FRFSTicket -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicket.FRFSTicket] -> m ())
createMany = traverse_ create

findAllByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ([Domain.Types.FRFSTicket.FRFSTicket]))
findAllByBookingId bookingId = do findAllWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findByBookingId bookingId = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTrasactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearchRequest.FRFSSearchRequest -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findByTrasactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq (Kernel.Types.Id.getId transactionId)]

updateTicketStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m ())
updateTicketStatusById id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicket.FRFSTicket -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicket.FRFSTicket {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bapId bapId,
      Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.bppId bppId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.ticketQr ticketQr,
      Se.Set Beam.ticketStatus ticketStatus,
      Se.Set Beam.transactionId (Kernel.Types.Id.getId transactionId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  fromTType' (Beam.FRFSTicketT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicket.FRFSTicket
          { bapId = bapId,
            bookingId = Kernel.Types.Id.Id bookingId,
            bppId = bppId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            ticketQr = ticketQr,
            ticketStatus = ticketStatus,
            transactionId = Kernel.Types.Id.Id transactionId,
            updatedAt = updatedAt,
            validTill = validTill,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  toTType' (Domain.Types.FRFSTicket.FRFSTicket {..}) = do
    Beam.FRFSTicketT
      { Beam.bapId = bapId,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.bppId = bppId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ticketQr = ticketQr,
        Beam.ticketStatus = ticketStatus,
        Beam.transactionId = Kernel.Types.Id.getId transactionId,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
