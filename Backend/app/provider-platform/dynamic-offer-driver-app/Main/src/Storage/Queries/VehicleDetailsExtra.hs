module Storage.Queries.VehicleDetailsExtra where

import Domain.Types.VehicleDetails
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.VehicleDetails as BeamVD
import Storage.Queries.OrphanInstances.VehicleDetails ()

-- Extra code goes here --
findAllVehicleDetails ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  m [VehicleDetails]
findAllVehicleDetails = findAllWithKV [Se.Is BeamVD.id $ Se.Not $ Se.Eq ""]
