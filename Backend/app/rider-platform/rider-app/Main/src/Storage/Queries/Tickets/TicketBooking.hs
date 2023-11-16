{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Tickets.TicketBooking where

import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import Domain.Types.Tickets.TicketBooking as DomainTB
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Tickets.TicketBooking as BeamTB

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TicketBooking -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id TicketBooking -> m (Maybe TicketBooking)
findById id = findOneWithKV [Se.Is BeamTB.id $ Se.Eq id.getId]

findByShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => ShortId TicketBooking -> m (Maybe TicketBooking)
findByShortId shortId = findOneWithKV [Se.Is BeamTB.shortId $ Se.Eq shortId.getShortId]

getAllBookingsByPersonId ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DP.Person ->
  Id MerchantOperatingCity ->
  BookingStatus ->
  Maybe Int ->
  Maybe Int ->
  m [TicketBooking]
getAllBookingsByPersonId personId merchantOpCityId status =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamTB.personId $ Se.Eq personId.getId,
          Se.Is BeamTB.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
          Se.Is BeamTB.status $ Se.Eq status
        ]
    ]
    (Se.Desc BeamTB.createdAt)

updateStatusByShortId :: MonadFlow m => ShortId TicketBooking -> BookingStatus -> m ()
updateStatusByShortId (ShortId shortId) status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamTB.status status,
      Se.Set BeamTB.updatedAt now
    ]
    [Se.Is BeamTB.shortId $ Se.Eq shortId]

instance FromTType' BeamTB.TicketBooking TicketBooking where
  fromTType' BeamTB.TicketBookingT {..} = do
    pure $
      Just $
        TicketBooking
          { id = Id id,
            shortId = ShortId shortId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            ticketPlaceId = Id ticketPlaceId,
            personId = Id personId,
            ..
          }

instance ToTType' BeamTB.TicketBooking TicketBooking where
  toTType' TicketBooking {..} =
    BeamTB.TicketBookingT
      { BeamTB.id = getId id,
        BeamTB.shortId = getShortId shortId,
        BeamTB.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamTB.ticketPlaceId = getId ticketPlaceId,
        BeamTB.personId = getId personId,
        BeamTB.amount = amount,
        BeamTB.visitDate = visitDate,
        BeamTB.status = status,
        BeamTB.createdAt = createdAt,
        BeamTB.updatedAt = updatedAt
      }
