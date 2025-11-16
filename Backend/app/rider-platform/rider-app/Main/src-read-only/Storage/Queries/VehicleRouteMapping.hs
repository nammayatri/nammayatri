{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRouteMapping (module Storage.Queries.VehicleRouteMapping, module ReExport) where

import qualified Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRouteMapping as Beam
import Storage.Queries.VehicleRouteMappingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleRouteMapping.VehicleRouteMapping] -> m ())
createMany = traverse_ create

findByRouteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping])
findByRouteId routeId = do findAllWithDb [Se.Is Beam.routeId $ Se.Eq routeId]

findByService :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping])
findByService service = do findAllWithDb [Se.Is Beam.service $ Se.Eq service]

findByShift :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping])
findByShift shift = do findAllWithDb [Se.Is Beam.shift $ Se.Eq shift]

findByVehicleNo :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping])
findByVehicleNo vehicleNo = do findAllWithDb [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.VehicleRouteMapping.VehicleRouteMapping))
findByPrimaryKey vehicleNo = do findOneWithKV [Se.And [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
updateByPrimaryKey (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.routeId routeId,
      Se.Set Beam.service service,
      Se.Set Beam.shift shift,
      Se.Set Beam.typeOfService typeOfService,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo]]
