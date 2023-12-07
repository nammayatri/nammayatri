{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingService where

import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingService as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBookingService.TicketBookingService -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketBookingService.TicketBookingService] -> m ()
createMany = traverse_ createWithKV

findAllByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ([Domain.Types.TicketBookingService.TicketBookingService])
findAllByBookingId (Kernel.Types.Id.Id ticketBookingId) = do
  findAllWithKV
    [ Se.Is Beam.ticketBookingId $ Se.Eq ticketBookingId
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe (Domain.Types.TicketBookingService.TicketBookingService))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe (Domain.Types.TicketBookingService.TicketBookingService))
findByShortId (Kernel.Types.Id.ShortId shortId) = do
  findOneWithKV
    [ Se.Is Beam.shortId $ Se.Eq shortId
    ]

updateAllStatusByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ()
updateAllStatusByBookingId status updatedAt (Kernel.Types.Id.Id ticketBookingId) = do
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.updatedAt updatedAt
    ]
    [ Se.Is Beam.ticketBookingId $ Se.Eq ticketBookingId
    ]

updateVerificationById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Prelude.Int -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m ()
updateVerificationById status verificationCount updatedAt (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.verificationCount verificationCount,
      Se.Set Beam.updatedAt updatedAt
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe (Domain.Types.TicketBookingService.TicketBookingService))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Common.HighPrecMoney -> Domain.Types.BusinessHour.BusinessHourType -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Domain.Types.TicketBookingService.ServiceStatus -> Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m ()
updateByPrimaryKey amount btype createdAt expiryDate (Kernel.Types.Id.Id merchantOperatingCityId) (Kernel.Types.Id.ShortId shortId) status (Kernel.Types.Id.Id ticketBookingId) (Kernel.Types.Id.Id ticketServiceId) updatedAt verificationCount (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.btype btype,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.expiryDate expiryDate,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.shortId shortId,
      Se.Set Beam.status status,
      Se.Set Beam.ticketBookingId ticketBookingId,
      Se.Set Beam.ticketServiceId ticketServiceId,
      Se.Set Beam.updatedAt updatedAt,
      Se.Set Beam.verificationCount verificationCount
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  fromTType' Beam.TicketBookingServiceT {..} = do
    pure $
      Just
        Domain.Types.TicketBookingService.TicketBookingService
          { amount = amount,
            btype = btype,
            createdAt = createdAt,
            expiryDate = expiryDate,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            ticketServiceId = Kernel.Types.Id.Id ticketServiceId,
            updatedAt = updatedAt,
            verificationCount = verificationCount
          }

instance ToTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  toTType' Domain.Types.TicketBookingService.TicketBookingService {..} = do
    Beam.TicketBookingServiceT
      { Beam.amount = amount,
        Beam.btype = btype,
        Beam.createdAt = createdAt,
        Beam.expiryDate = expiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.ticketServiceId = Kernel.Types.Id.getId ticketServiceId,
        Beam.updatedAt = updatedAt,
        Beam.verificationCount = verificationCount
      }
