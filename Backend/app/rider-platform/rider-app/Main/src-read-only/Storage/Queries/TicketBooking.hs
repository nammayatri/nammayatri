{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBooking where

import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBooking.TicketBooking -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketBooking.TicketBooking] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe (Domain.Types.TicketBooking.TicketBooking))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq $ id
    ]

findByShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m (Maybe (Domain.Types.TicketBooking.TicketBooking))
findByShortId (Kernel.Types.Id.ShortId shortId) = do
  findOneWithKV
    [ Se.Is Beam.shortId $ Se.Eq $ shortId
    ]

getAllBookingsByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity) -> Domain.Types.TicketBooking.BookingStatus -> m ([Domain.Types.TicketBooking.TicketBooking])
getAllBookingsByPersonId limit offset (Kernel.Types.Id.Id personId) merchantOperatingCityId status = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq $ personId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.status $ Se.Eq $ status
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateStatusByShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketBooking.BookingStatus -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m ()
updateStatusByShortId status (Kernel.Types.Id.ShortId shortId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status $ status,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.shortId $ Se.Eq $ shortId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe (Domain.Types.TicketBooking.TicketBooking))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq $ id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.TicketBooking.TicketBooking -> m ()
updateByPrimaryKey Domain.Types.TicketBooking.TicketBooking {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount $ amount,
      Se.Set Beam.personId $ (Kernel.Types.Id.getId personId),
      Se.Set Beam.shortId $ (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status $ status,
      Se.Set Beam.ticketPlaceId $ (Kernel.Types.Id.getId ticketPlaceId),
      Se.Set Beam.visitDate $ visitDate,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq $ (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  fromTType' Beam.TicketBookingT {..} = do
    pure $
      Just
        Domain.Types.TicketBooking.TicketBooking
          { amount = amount,
            id = Kernel.Types.Id.Id id,
            personId = Kernel.Types.Id.Id personId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketPlaceId = Kernel.Types.Id.Id ticketPlaceId,
            visitDate = visitDate,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  toTType' Domain.Types.TicketBooking.TicketBooking {..} = do
    Beam.TicketBookingT
      { Beam.amount = amount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
        Beam.visitDate = visitDate,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
