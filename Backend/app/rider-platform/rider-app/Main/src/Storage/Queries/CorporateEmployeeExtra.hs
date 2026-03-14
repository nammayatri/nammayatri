module Storage.Queries.CorporateEmployeeExtra where

import Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateEntity as DCEnt
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateEmployee as Beam
import Storage.Queries.OrphanInstances.CorporateEmployee ()

-- Extra code goes here --

-- | Batch fetch employees by a list of IDs (avoids N+1 queries)
findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id CorporateEmployee] ->
  m [CorporateEmployee]
findByIds [] = pure []
findByIds ids = findAllWithKV [Se.Is Beam.id $ Se.In (map getId ids)]

-- | Paginated query for employees by corporate entity ID
findByCorporateEntityIdPaginated ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DCEnt.CorporateEntity ->
  Int ->
  Int ->
  m [CorporateEmployee]
findByCorporateEntityIdPaginated entityId limit offset =
  findAllWithOptionsKV
    [Se.Is Beam.corporateEntityId $ Se.Eq (getId entityId)]
    (Se.Asc Beam.createdAt)
    (Just limit)
    (Just offset)
