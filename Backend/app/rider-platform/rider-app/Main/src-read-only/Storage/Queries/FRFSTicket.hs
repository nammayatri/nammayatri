{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicket where

import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicket as Beam

create :: KvDbFlow m r => (Domain.Types.FRFSTicket.FRFSTicket -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FRFSTicket.FRFSTicket] -> m ())
createMany = traverse_ create

findAllByStatus :: KvDbFlow m r => (Domain.Types.FRFSTicket.FRFSTicketStatus -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findAllByStatus status = do findOneWithKV [Se.Is Beam.status $ Se.Eq status]

findAllByTicketBookingId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m [Domain.Types.FRFSTicket.FRFSTicket])
findAllByTicketBookingId (Kernel.Types.Id.Id frfsTicketBookingId) = do findAllWithKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq frfsTicketBookingId]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

updateAllStausByBookingId :: KvDbFlow m r => (Domain.Types.FRFSTicket.FRFSTicketStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateAllStausByBookingId status (Kernel.Types.Id.Id frfsTicketBookingId) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.frfsTicketBookingId $ Se.Eq frfsTicketBookingId]]

updateStatusByTBookingIdAndTicketNumber ::
  KvDbFlow m r =>
  (Domain.Types.FRFSTicket.FRFSTicketStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Text -> m ())
updateStatusByTBookingIdAndTicketNumber status (Kernel.Types.Id.Id frfsTicketBookingId) ticketNumber = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.frfsTicketBookingId $ Se.Eq frfsTicketBookingId, Se.Is Beam.ticketNumber $ Se.Eq ticketNumber]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FRFSTicket.FRFSTicket -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicket.FRFSTicket {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.frfsTicketBookingId (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.qrData qrData,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketNumber ticketNumber,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  fromTType' (Beam.FRFSTicketT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicket.FRFSTicket
          { frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            qrData = qrData,
            riderId = Kernel.Types.Id.Id riderId,
            status = status,
            ticketNumber = ticketNumber,
            validTill = validTill,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  toTType' (Domain.Types.FRFSTicket.FRFSTicket {..}) = do
    Beam.FRFSTicketT
      { Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.qrData = qrData,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.status = status,
        Beam.ticketNumber = ticketNumber,
        Beam.validTill = validTill,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
