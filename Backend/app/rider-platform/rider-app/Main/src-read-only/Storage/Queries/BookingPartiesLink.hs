{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingPartiesLink where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingPartiesLink
import qualified Domain.Types.Person
import qualified Domain.Types.Trip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingPartiesLink as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingPartiesLink.BookingPartiesLink -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BookingPartiesLink.BookingPartiesLink] -> m ())
createMany = traverse_ create

findAllActiveByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m [Domain.Types.BookingPartiesLink.BookingPartiesLink])
findAllActiveByBookingId bookingId = do findAllWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId), Se.Is Beam.isActive $ Se.Eq True]]

findAllByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m [Domain.Types.BookingPartiesLink.BookingPartiesLink])
findAllByBookingId bookingId = do findAllWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findOneActiveByBookingIdAndTripParty ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Domain.Types.Trip.TripParty -> m (Maybe Domain.Types.BookingPartiesLink.BookingPartiesLink))
findOneActiveByBookingIdAndTripParty bookingId partyType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId),
          Se.Is Beam.partyType $ Se.Eq partyType,
          Se.Is Beam.isActive $ Se.Eq True
        ]
    ]

findOneActivePartyByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.BookingPartiesLink.BookingPartiesLink))
findOneActivePartyByRiderId partyId = do findOneWithKV [Se.And [Se.Is Beam.partyId $ Se.Eq (Kernel.Types.Id.getId partyId), Se.Is Beam.isActive $ Se.Eq True]]

findOneByPartyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.BookingPartiesLink.BookingPartiesLink))
findOneByPartyId partyId = do findOneWithKV [Se.Is Beam.partyId $ Se.Eq (Kernel.Types.Id.getId partyId)]

makeAllInactiveByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m ())
makeAllInactiveByBookingId bookingId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.isActive False, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId), Se.Is Beam.isActive $ Se.Eq True]]

instance FromTType' Beam.BookingPartiesLink Domain.Types.BookingPartiesLink.BookingPartiesLink where
  fromTType' (Beam.BookingPartiesLinkT {..}) = do
    pure $
      Just
        Domain.Types.BookingPartiesLink.BookingPartiesLink
          { bookingId = Kernel.Types.Id.Id bookingId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            partyId = Kernel.Types.Id.Id partyId,
            partyName = partyName,
            partyType = partyType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BookingPartiesLink Domain.Types.BookingPartiesLink.BookingPartiesLink where
  toTType' (Domain.Types.BookingPartiesLink.BookingPartiesLink {..}) = do
    Beam.BookingPartiesLinkT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.partyId = Kernel.Types.Id.getId partyId,
        Beam.partyName = partyName,
        Beam.partyType = partyType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
