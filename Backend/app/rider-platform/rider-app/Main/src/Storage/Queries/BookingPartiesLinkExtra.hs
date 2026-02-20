module Storage.Queries.BookingPartiesLinkExtra where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingPartiesLink
import qualified Domain.Types.Person
import qualified Domain.Types.Trip
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingPartiesLink as Beam
import Storage.Queries.OrphanInstances.BookingPartiesLink ()

findOneActiveByBookingIdAndTripParty ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Domain.Types.Trip.TripParty -> m (Maybe Domain.Types.BookingPartiesLink.BookingPartiesLink))
findOneActiveByBookingIdAndTripParty bookingId partyType = do
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId),
          Se.Is Beam.partyType $ Se.Eq partyType,
          Se.Is Beam.isActive $ Se.Eq True
        ]
    ]
    Nothing
    <&> listToMaybe

findOneActivePartyByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.BookingPartiesLink.BookingPartiesLink))
findOneActivePartyByRiderId partyId = do findAllWithKVAndConditionalDB [Se.And [Se.Is Beam.partyId $ Se.Eq (Kernel.Types.Id.getId partyId), Se.Is Beam.isActive $ Se.Eq True]] Nothing <&> listToMaybe
