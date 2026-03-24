{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.RoutePolylines (module Storage.Queries.RoutePolylines, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.RoutePolylinesExtra as ReExport
import qualified Domain.Types.RoutePolylines
import qualified Storage.Beam.RoutePolylines as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified BecknV2.FRFS.Enums
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RoutePolylines.RoutePolylines -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RoutePolylines.RoutePolylines] -> m ())
createMany = traverse_ create
findByRouteIdAndCity :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                        (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RoutePolylines.RoutePolylines))
findByRouteIdAndCity routeId merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.routeId $ Se.Eq routeId,
                                                                                 Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]
findByRouteIdAndVehicleType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> m (Maybe Domain.Types.RoutePolylines.RoutePolylines))
findByRouteIdAndVehicleType routeId vehicleType = do findOneWithKV [Se.And [Se.Is Beam.routeId $ Se.Eq routeId, Se.Is Beam.vehicleType $ Se.Eq vehicleType]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RoutePolylines.RoutePolylines -> m (Maybe Domain.Types.RoutePolylines.RoutePolylines))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RoutePolylines.RoutePolylines -> m ())
updateByPrimaryKey (Domain.Types.RoutePolylines.RoutePolylines {..}) = do {_now <- getCurrentTime;
                                                                           updateWithKV [Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                         Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                         Se.Set Beam.polyline polyline,
                                                                                         Se.Set Beam.routeId routeId,
                                                                                         Se.Set Beam.updatedAt _now,
                                                                                         Se.Set Beam.vehicleType vehicleType] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



