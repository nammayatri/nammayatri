{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBooking (module Storage.Queries.TicketBooking, module ReExport) where

import qualified Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Extra.TicketBooking
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBooking as Beam
import Storage.Queries.TicketBookingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBooking.TicketBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketBooking.TicketBooking] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe Domain.Types.TicketBooking.TicketBooking))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m (Maybe Domain.Types.TicketBooking.TicketBooking))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

getAllBookingsByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Extra.TicketBooking.BookingStatus -> m [Domain.Types.TicketBooking.TicketBooking])
getAllBookingsByPersonId limit offset personId merchantOperatingCityId status = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.status $ Se.Eq status
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

getAllBookingsByPlaceIdAndVisitDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Data.Time.Day -> Domain.Types.Extra.TicketBooking.BookingStatus -> m [Domain.Types.TicketBooking.TicketBooking])
getAllBookingsByPlaceIdAndVisitDate ticketPlaceId visitDate status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.ticketPlaceId $ Se.Eq (Kernel.Types.Id.getId ticketPlaceId),
          Se.Is Beam.visitDate $ Se.Eq visitDate,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

updateStatusAndCancelledSeatsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Extra.TicketBooking.BookingStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ())
updateStatusAndCancelledSeatsById status cancelledSeats id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.cancelledSeats cancelledSeats, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Extra.TicketBooking.BookingStatus -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m ())
updateStatusByShortId status shortId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe Domain.Types.TicketBooking.TicketBooking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBooking.TicketBooking -> m ())
updateByPrimaryKey (Domain.Types.TicketBooking.TicketBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount ((.amount) amount),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.blockExpirationTime blockExpirationTime,
      Se.Set Beam.bookedSeats bookedSeats,
      Se.Set Beam.cancelledSeats cancelledSeats,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketPlaceId (Kernel.Types.Id.getId ticketPlaceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vendorSplitDetails (Data.Aeson.toJSON <$> vendorSplitDetails),
      Se.Set Beam.visitDate visitDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
