module Storage.Queries.TicketBookingExtra where

import qualified Domain.Types.TicketBooking as DTB
import qualified Domain.Types.TicketPlace as DTP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import Storage.Beam.TicketBooking as BeamR
import Storage.Queries.OrphanInstances.TicketBooking ()

-- Extra code goes here --
findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTB.TicketBooking] ->
  m [DTB.TicketBooking]
findByIds ticketBookingIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ getId <$> ticketBookingIds]

findAllByTicketPlaceIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id.Id DTP.TicketPlace ->
  DTB.BookingStatus ->
  m [DTB.TicketBooking]
findAllByTicketPlaceIdAndStatus mbLimit mbOffset ticketPlaceId status = do
  findAllWithOptionsKV
    [Se.And [Se.Is BeamR.ticketPlaceId $ Se.Eq (getId ticketPlaceId), Se.Is BeamR.status $ Se.Eq status]]
    (Se.Desc BeamR.createdAt)
    mbLimit
    mbOffset
