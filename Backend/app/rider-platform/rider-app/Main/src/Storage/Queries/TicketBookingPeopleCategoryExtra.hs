module Storage.Queries.TicketBookingPeopleCategoryExtra where

import qualified Domain.Types.TicketBookingPeopleCategory as DTBP
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingPeopleCategory as BeamR
import Storage.Queries.OrphanInstances.TicketBookingPeopleCategory ()

-- Extra code goes here --

findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTBP.TicketBookingPeopleCategory] ->
  m [DTBP.TicketBookingPeopleCategory]
findByIds ticketBookingPeopleCategoryIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ Id.getId <$> ticketBookingPeopleCategoryIds]
