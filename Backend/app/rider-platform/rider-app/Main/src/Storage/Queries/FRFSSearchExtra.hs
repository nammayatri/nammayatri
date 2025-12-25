module Storage.Queries.FRFSSearchExtra where

import Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam
import Storage.Queries.OrphanInstances.FRFSSearch ()

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create frfsSearchReq = create' frfsSearchReq

updateOnSearchFailed :: (MonadFlow m, EsqDBFlow m r) => Id FRFSSearch -> Maybe Bool -> m ()
updateOnSearchFailed (Id reqId) onSearchFailed = do
  updateOneWithKV
    [Se.Set Beam.onSearchFailed onSearchFailed]
    [Se.Is Beam.id (Se.Eq reqId)]
