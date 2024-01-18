{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicket where

import qualified Domain.Types.FRFSTicket
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicket as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTicket.FRFSTicket -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSTicket.FRFSTicket] -> m ()
createMany = traverse_ createWithKV

findAllByStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicket.FRFSTicketStatus -> m (Maybe (Domain.Types.FRFSTicket.FRFSTicket))
findAllByStatus status = do
  findOneWithKV
    [ Se.Is Beam.status $ Se.Eq status
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe (Domain.Types.FRFSTicket.FRFSTicket))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByTicketBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe (Domain.Types.FRFSTicket.FRFSTicket))
findByTicketBookingId (Kernel.Types.Id.Id frfsTicketBookingId) = do
  findOneWithKV
    [ Se.Is Beam.frfsTicketBookingId $ Se.Eq frfsTicketBookingId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicket.FRFSTicket -> m (Maybe (Domain.Types.FRFSTicket.FRFSTicket))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicket.FRFSTicket -> m ()
updateByPrimaryKey Domain.Types.FRFSTicket.FRFSTicket {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.frfsTicketBookingId $ (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.qrData $ qrData,
      Se.Set Beam.status $ status,
      Se.Set Beam.ticketNumber $ ticketNumber,
      Se.Set Beam.validTill $ validTill,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  fromTType' Beam.FRFSTicketT {..} = do
    pure $
      Just
        Domain.Types.FRFSTicket.FRFSTicket
          { frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            qrData = qrData,
            status = status,
            ticketNumber = ticketNumber,
            validTill = validTill,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicket Domain.Types.FRFSTicket.FRFSTicket where
  toTType' Domain.Types.FRFSTicket.FRFSTicket {..} = do
    Beam.FRFSTicketT
      { Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.qrData = qrData,
        Beam.status = status,
        Beam.ticketNumber = ticketNumber,
        Beam.validTill = validTill,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
