{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.VehicleRouteMappingExtra where

import qualified Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRouteMapping as Beam
import Storage.Queries.OrphanInstances.VehicleRouteMapping

-- Extra code goes here --

findAllByVehicleNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Text] -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping]
findAllByVehicleNumber vehicleNumbers = do
  findAllWithKV [Se.Is Beam.vehicleNo $ Se.In $ vehicleNumbers]

findByVehicleNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m (Maybe Domain.Types.VehicleRouteMapping.VehicleRouteMapping)
findByVehicleNumber vehicleNumber = do
  findOneWithKV [Se.Is Beam.vehicleNo $ Se.Eq vehicleNumber]
