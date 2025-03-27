module Storage.Queries.VehicleInfoExtra where

import Domain.Types.VehicleInfo
import Kernel.Beam.Functions
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Beam.VehicleInfo as Beam
import Storage.Queries.VehicleInfo ()

-- Extra code goes here --
findAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m ([Domain.Types.VehicleInfo.VehicleInfo])
findAll = do findAllWithKV @Beam.VehicleInfoT []
