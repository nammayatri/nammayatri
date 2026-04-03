{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.VehicleRouteMapping (module Storage.Queries.VehicleRouteMapping, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.VehicleRouteMappingExtra as ReExport
import qualified Domain.Types.VehicleRouteMapping
import qualified Storage.Beam.VehicleRouteMapping as Beam
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleRouteMapping.VehicleRouteMapping] -> m ())
createMany = traverse_ create
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
updateByPrimaryKey (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do {_now <- getCurrentTime;
                                                                                     updateWithKV [Se.Set Beam.blocked blocked,
                                                                                                   Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
                                                                                                   Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                                   Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                   Se.Set Beam.vehicleNumberEncrypted (((vehicleNumber & unEncrypted . encrypted))),
                                                                                                   Se.Set Beam.vehicleNumberHash ((vehicleNumber & hash)),
                                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode]]}



