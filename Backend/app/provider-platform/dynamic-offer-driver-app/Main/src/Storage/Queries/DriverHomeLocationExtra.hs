module Storage.Queries.DriverHomeLocationExtra where

import qualified Domain.Types.DriverHomeLocation as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.DriverHomeLocation as BeamDHL
import Storage.Queries.OrphanInstances.DriverHomeLocation ()

-- Extra code goes here --

updateHomeLocationById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.DriverHomeLocation -> Domain.UpdateDriverHomeLocation -> m ()
updateHomeLocationById homeLocationId driverHomeLocation = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHL.lat driverHomeLocation.lat, Se.Set BeamDHL.lon driverHomeLocation.lon, Se.Set BeamDHL.address driverHomeLocation.address, Se.Set BeamDHL.tag driverHomeLocation.tag, Se.Set BeamDHL.updatedAt now]
    [Se.Is BeamDHL.id $ Se.Eq $ getId homeLocationId]
