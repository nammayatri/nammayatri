{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRouteMappingExtra where

import Data.Text
import Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRouteMapping as Beam
import Storage.Queries.OrphanInstances.VehicleRouteMapping

findAllRouteMappings :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Encryption.DbHash -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping])
findAllRouteMappings vehicleNumberHash = do findAllWithKV [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash]

findOneMapping :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Encryption.DbHash -> Data.Text.Text -> m (Maybe Domain.Types.VehicleRouteMapping.VehicleRouteMapping))
findOneMapping vehicleNumberHash routeCode = do findOneWithKV [Se.And [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash, Se.Is Beam.routeCode $ Se.Eq routeCode]]
