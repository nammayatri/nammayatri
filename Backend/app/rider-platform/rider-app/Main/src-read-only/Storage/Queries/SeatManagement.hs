{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SeatManagement where

import qualified Data.Time
import qualified Domain.Types.SeatManagement
import qualified Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SeatManagement as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SeatManagement.SeatManagement -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SeatManagement.SeatManagement] -> m ())
createMany = traverse_ create

findByTicketServiceCategoryIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Day -> m (Maybe Domain.Types.SeatManagement.SeatManagement))
findByTicketServiceCategoryIdAndDate ticketServiceCategoryId date = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.ticketServiceCategoryId $ Se.Eq (Kernel.Types.Id.getId ticketServiceCategoryId),
          Se.Is Beam.date $ Se.Eq date
        ]
    ]

safeUpdateBookedSeats :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Day -> m ())
safeUpdateBookedSeats booked ticketServiceCategoryId date = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.booked booked, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.ticketServiceCategoryId $ Se.Eq (Kernel.Types.Id.getId ticketServiceCategoryId),
          Se.Is Beam.date $ Se.Eq date,
          Se.Is Beam.booked $ Se.LessThan booked
        ]
    ]

updateBlockedSeats :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Day -> m ())
updateBlockedSeats blocked ticketServiceCategoryId date = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.blocked blocked, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.ticketServiceCategoryId $ Se.Eq (Kernel.Types.Id.getId ticketServiceCategoryId),
          Se.Is Beam.date $ Se.Eq date
        ]
    ]

updateBookedAndBlockedSeats ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Day -> m ())
updateBookedAndBlockedSeats booked blocked ticketServiceCategoryId date = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.booked booked,
      Se.Set Beam.blocked blocked,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.ticketServiceCategoryId $ Se.Eq (Kernel.Types.Id.getId ticketServiceCategoryId), Se.Is Beam.date $ Se.Eq date]]

updateBookedSeats :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> Data.Time.Day -> m ())
updateBookedSeats booked ticketServiceCategoryId date = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.booked booked, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.ticketServiceCategoryId $ Se.Eq (Kernel.Types.Id.getId ticketServiceCategoryId),
          Se.Is Beam.date $ Se.Eq date
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SeatManagement.SeatManagement -> m (Maybe Domain.Types.SeatManagement.SeatManagement))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SeatManagement.SeatManagement -> m ())
updateByPrimaryKey (Domain.Types.SeatManagement.SeatManagement {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.blocked blocked,
      Se.Set Beam.booked booked,
      Se.Set Beam.date date,
      Se.Set Beam.ticketServiceCategoryId (Kernel.Types.Id.getId ticketServiceCategoryId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SeatManagement Domain.Types.SeatManagement.SeatManagement where
  fromTType' (Beam.SeatManagementT {..}) = do
    pure $
      Just
        Domain.Types.SeatManagement.SeatManagement
          { blocked = blocked,
            booked = booked,
            date = date,
            id = Kernel.Types.Id.Id id,
            ticketServiceCategoryId = Kernel.Types.Id.Id ticketServiceCategoryId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SeatManagement Domain.Types.SeatManagement.SeatManagement where
  toTType' (Domain.Types.SeatManagement.SeatManagement {..}) = do
    Beam.SeatManagementT
      { Beam.blocked = blocked,
        Beam.booked = booked,
        Beam.date = date,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ticketServiceCategoryId = Kernel.Types.Id.getId ticketServiceCategoryId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
