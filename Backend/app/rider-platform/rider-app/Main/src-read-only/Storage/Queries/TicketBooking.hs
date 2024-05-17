{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBooking (module Storage.Queries.TicketBooking, module ReExport) where

import qualified Data.Time.Calendar
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
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m (Maybe Domain.Types.TicketBooking.TicketBooking))
findByShortId (Kernel.Types.Id.ShortId shortId) = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq shortId]

getAllBookingsByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.TicketBooking.BookingStatus -> m [Domain.Types.TicketBooking.TicketBooking])
getAllBookingsByPersonId limit offset (Kernel.Types.Id.Id personId) (Kernel.Types.Id.Id merchantOperatingCityId) status = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq personId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

getAllBookingsByPlaceIdAndVisitDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Data.Time.Calendar.Day -> Domain.Types.TicketBooking.BookingStatus -> m [Domain.Types.TicketBooking.TicketBooking])
getAllBookingsByPlaceIdAndVisitDate (Kernel.Types.Id.Id ticketPlaceId) visitDate status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.ticketPlaceId $ Se.Eq ticketPlaceId,
          Se.Is Beam.visitDate $ Se.Eq visitDate,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

updateStatusAndCancelledSeatsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TicketBooking.BookingStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ())
updateStatusAndCancelledSeatsById status cancelledSeats (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.cancelledSeats cancelledSeats, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateStatusByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBooking.BookingStatus -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m ())
updateStatusByShortId status (Kernel.Types.Id.ShortId shortId) = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.shortId $ Se.Eq shortId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe Domain.Types.TicketBooking.TicketBooking))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketBooking.TicketBooking -> m ())
updateByPrimaryKey (Domain.Types.TicketBooking.TicketBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount ((.amount) amount),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) amount),
      Se.Set Beam.bookedSeats bookedSeats,
      Se.Set Beam.cancelledSeats cancelledSeats,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketPlaceId (Kernel.Types.Id.getId ticketPlaceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.visitDate visitDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
