module Storage.Queries.TollExtra where

import qualified Domain.Types.Toll
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (EsqDBFlow, MonadFlow, CacheFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Toll as Beam

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Toll.Toll -> m ()
deleteById tollId = deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId tollId)]
