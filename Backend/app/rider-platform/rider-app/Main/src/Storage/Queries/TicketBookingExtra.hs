module Storage.Queries.TicketBookingExtra where

import qualified Domain.Types.TicketBooking as DTB
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
