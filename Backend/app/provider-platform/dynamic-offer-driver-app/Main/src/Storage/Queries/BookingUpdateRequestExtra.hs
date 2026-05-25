module Storage.Queries.BookingUpdateRequestExtra where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingUpdateRequest as Beam
import Storage.Queries.BookingUpdateRequest ()

findAllActiveByBookingId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id.Id DBooking.Booking ->
  m [DBUR.BookingUpdateRequest]
findAllActiveByBookingId bookingId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.bookingId $ Se.Eq (Id.getId bookingId),
          Se.Is Beam.status $ Se.In [DBUR.SOFT, DBUR.USER_CONFIRMED]
        ]
    ]

expireActiveSiblings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id.Id DBooking.Booking ->
  Id.Id DBUR.BookingUpdateRequest ->
  m ()
expireActiveSiblings bookingId currentBurId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status DBUR.EXPIRED,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.bookingId $ Se.Eq (Id.getId bookingId),
          Se.Is Beam.status $ Se.In [DBUR.SOFT, DBUR.USER_CONFIRMED],
          Se.Is Beam.id $ Se.Not $ Se.Eq (Id.getId currentBurId)
        ]
    ]

expireActiveSoftSiblings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id.Id DBooking.Booking ->
  Id.Id DBUR.BookingUpdateRequest ->
  m ()
expireActiveSoftSiblings bookingId currentBurId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status DBUR.EXPIRED,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.bookingId $ Se.Eq (Id.getId bookingId),
          Se.Is Beam.status $ Se.Eq DBUR.SOFT,
          Se.Is Beam.id $ Se.Not $ Se.Eq (Id.getId currentBurId)
        ]
    ]
