{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingPeopleCategory where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBookingPeopleCategory
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingPeopleCategory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory] -> m ()
createMany = traverse_ createWithKV

findAllByServiceCategoryId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ([Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory])
findAllByServiceCategoryId (Kernel.Types.Id.Id ticketBookingServiceCategoryId) = do
  findAllWithKV
    [ Se.Is Beam.ticketBookingServiceCategoryId $ Se.Eq ticketBookingServiceCategoryId
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m (Maybe (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m (Maybe (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory -> m ()
updateByPrimaryKey Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.name name,
      Se.Set Beam.numberOfUnits numberOfUnits,
      Se.Set Beam.pricePerUnit pricePerUnit,
      Se.Set Beam.ticketBookingServiceCategoryId (Kernel.Types.Id.getId ticketBookingServiceCategoryId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.TicketBookingPeopleCategory Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory where
  fromTType' Beam.TicketBookingPeopleCategoryT {..} = do
    pure $
      Just
        Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory
          { id = Kernel.Types.Id.Id id,
            name = name,
            numberOfUnits = numberOfUnits,
            pricePerUnit = pricePerUnit,
            ticketBookingServiceCategoryId = Kernel.Types.Id.Id ticketBookingServiceCategoryId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBookingPeopleCategory Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory where
  toTType' Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory {..} = do
    Beam.TicketBookingPeopleCategoryT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.numberOfUnits = numberOfUnits,
        Beam.pricePerUnit = pricePerUnit,
        Beam.ticketBookingServiceCategoryId = Kernel.Types.Id.getId ticketBookingServiceCategoryId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
