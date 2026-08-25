module Storage.Queries.PersonPTStatsExtra where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonPTStats as DPUS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PersonPTStats as Beam
import Storage.Queries.OrphanInstances.PersonPTStats ()

findAllStaleByPersonId ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id Person.Person ->
  Text ->
  m [DPUS.PersonPTStats]
findAllStaleByPersonId personId staticPersonId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.staticPersonId $ Se.Not $ Se.Eq staticPersonId
        ]
    ]
