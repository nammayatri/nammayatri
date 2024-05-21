{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverHomeLocationExtra where

import Domain.Types.DriverHomeLocation
import qualified Domain.Types.DriverHomeLocation as Domain
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id as ID
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import Storage.Beam.DriverHomeLocation as BeamDHL
import Storage.Queries.OrphanInstances.DriverHomeLocation

-- Extra code goes here --

updateHomeLocationById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.DriverHomeLocation -> Domain.UpdateDriverHomeLocation -> m ()
updateHomeLocationById homeLocationId driverHomeLocation = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHL.lat driverHomeLocation.lat, Se.Set BeamDHL.lon driverHomeLocation.lon, Se.Set BeamDHL.address driverHomeLocation.address, Se.Set BeamDHL.tag driverHomeLocation.tag, Se.Set BeamDHL.updatedAt now]
    [Se.Is BeamDHL.id $ Se.Eq $ getId homeLocationId]
