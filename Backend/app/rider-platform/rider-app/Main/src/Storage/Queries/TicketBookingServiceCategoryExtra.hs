module Storage.Queries.TicketBookingServiceCategoryExtra where

import qualified Domain.Types.TicketBookingServiceCategory as DTBS
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingServiceCategory as BeamR
import Storage.Queries.OrphanInstances.TicketBookingServiceCategory ()

-- Extra code goes here --

findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTBS.TicketBookingServiceCategory] ->
  m [DTBS.TicketBookingServiceCategory]
findByIds ticketBookingServiceCategoryIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ Id.getId <$> ticketBookingServiceCategoryIds]
