{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicket where

import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicket as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicket.FRFSTicket -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicket.FRFSTicket] -> m ())
createMany = traverse_ create

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketStatus.FRFSTicketStatus -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findAllByStatus status = do findOneWithKV [Se.Is Beam.status $ Se.Eq status]

findAllByTicketBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m [Domain.Types.FRFSTicket.FRFSTicket])
findAllByTicketBookingId frfsTicketBookingId = do findAllWithKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTicketBookingIdTicketNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findByTicketBookingIdTicketNumber frfsTicketBookingId ticketNumber = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId),
          Se.Is Beam.ticketNumber $ Se.Eq ticketNumber
        ]
    ]

findOneByTicketNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findOneByTicketNumber ticketNumber = do findOneWithKV [Se.Is Beam.ticketNumber $ Se.Eq ticketNumber]

udpateQrDataAndValidTill ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Text -> m ())
udpateQrDataAndValidTill qrData validTill frfsTicketBookingId ticketNumber = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.qrData qrData, Se.Set Beam.validTill validTill, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId),
          Se.Is Beam.ticketNumber $ Se.Eq ticketNumber
        ]
    ]

updateAllStatusByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketStatus.FRFSTicketStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateAllStatusByBookingId status frfsTicketBookingId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId)]]

updateRefreshTicketQRByTBookingIdAndTicketNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Text -> m ())
updateRefreshTicketQRByTBookingIdAndTicketNumber qrData qrRefreshAt frfsTicketBookingId ticketNumber = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.qrData qrData,
      Se.Set Beam.qrRefreshAt qrRefreshAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId), Se.Is Beam.ticketNumber $ Se.Eq ticketNumber]]

updateStatusByTBookingIdAndTicketNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketStatus.FRFSTicketStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Kernel.Prelude.Text -> m ())
updateStatusByTBookingIdAndTicketNumber status scannedByVehicleNumber frfsTicketBookingId ticketNumber = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.scannedByVehicleNumber scannedByVehicleNumber,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.frfsTicketBookingId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingId), Se.Is Beam.ticketNumber $ Se.Eq ticketNumber]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe Domain.Types.FRFSTicket.FRFSTicket))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicket.FRFSTicket -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicket.FRFSTicket {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.commencingHours commencingHours,
      Se.Set Beam.description description,
      Se.Set Beam.frfsTicketBookingId (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.isTicketFree isTicketFree,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.qrData qrData,
      Se.Set Beam.qrRefreshAt qrRefreshAt,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.scannedByVehicleNumber scannedByVehicleNumber,
      Se.Set Beam.status status,
      Se.Set Beam.ticketNumber ticketNumber,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  fromTType' (Beam.FRFSTicketT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicket.FRFSTicket
          { commencingHours = commencingHours,
            description = description,
            frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            isTicketFree = isTicketFree,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            qrData = qrData,
            qrRefreshAt = qrRefreshAt,
            riderId = Kernel.Types.Id.Id riderId,
            scannedByVehicleNumber = scannedByVehicleNumber,
            status = status,
            ticketNumber = ticketNumber,
            validTill = validTill,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  toTType' (Domain.Types.FRFSTicket.FRFSTicket {..}) = do
    Beam.FRFSTicketT
      { Beam.commencingHours = commencingHours,
        Beam.description = description,
        Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isTicketFree = isTicketFree,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.qrData = qrData,
        Beam.qrRefreshAt = qrRefreshAt,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.scannedByVehicleNumber = scannedByVehicleNumber,
        Beam.status = status,
        Beam.ticketNumber = ticketNumber,
        Beam.validTill = validTill,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
