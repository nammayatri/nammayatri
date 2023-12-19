{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingServiceCategory where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingServiceCategory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory] -> m ()
createMany = traverse_ createWithKV

findAllByTicketBookingServiceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m ([Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory])
findAllByTicketBookingServiceId (Kernel.Types.Id.Id ticketBookingServiceId) = do
  findAllWithKV
    [ Se.Is Beam.ticketBookingServiceId $ Se.Eq $ ticketBookingServiceId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m (Maybe (Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq $ id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ()
updateByPrimaryKey Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount $ amount,
      Se.Set Beam.bookedSeats $ bookedSeats,
      Se.Set Beam.name $ name,
      Se.Set Beam.serviceCategoryId $ serviceCategoryId,
      Se.Set Beam.ticketBookingServiceId $ (Kernel.Types.Id.getId ticketBookingServiceId),
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq $ (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  fromTType' Beam.TicketBookingServiceCategoryT {..} = do
    pure $
      Just
        Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory
          { amount = amount,
            bookedSeats = bookedSeats,
            id = Kernel.Types.Id.Id id,
            name = name,
            serviceCategoryId = serviceCategoryId,
            ticketBookingServiceId = Kernel.Types.Id.Id ticketBookingServiceId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  toTType' Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory {..} = do
    Beam.TicketBookingServiceCategoryT
      { Beam.amount = amount,
        Beam.bookedSeats = bookedSeats,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.serviceCategoryId = serviceCategoryId,
        Beam.ticketBookingServiceId = Kernel.Types.Id.getId ticketBookingServiceId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
