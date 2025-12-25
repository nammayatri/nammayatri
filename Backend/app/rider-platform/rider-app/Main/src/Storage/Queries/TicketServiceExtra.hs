module Storage.Queries.TicketServiceExtra where

import qualified Domain.Types.TicketService as DTBS
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketService as BeamR
import Storage.Queries.OrphanInstances.TicketService ()

-- Extra code goes here --
findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTBS.TicketService] ->
  m [DTBS.TicketService]
findByIds ticketServiceIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ Id.getId <$> ticketServiceIds]
