module Storage.Queries.LocationExtra where

import Domain.Types.Location
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Location as Beam
import Storage.Queries.Location ()

findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id Location] -> m [Location]
findAllByIds ids = findAllWithKV [Se.Is Beam.id $ Se.In (getId <$> ids)]
