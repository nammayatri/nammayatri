{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Route (module Storage.Queries.Route, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Route as Beam
import Storage.Queries.RouteExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Route.Route -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Route.Route] -> m ())
createMany = traverse_ create

deleteByRouteCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByRouteCode code = do deleteWithKV [Se.Is Beam.code $ Se.Eq code]

findAllByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleCategory.VehicleCategory -> [Kernel.Prelude.Text] -> m [Domain.Types.Route.Route])
findAllByMerchantOperatingCityAndVehicleType limit offset merchantOperatingCityId vehicleType code = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.code $ Se.In code
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByRouteCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Route.Route))
findByRouteCode code = do findOneWithKV [Se.Is Beam.code $ Se.Eq code]

findByRouteCodes :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Prelude.Text] -> m [Domain.Types.Route.Route])
findByRouteCodes code = do findAllWithKV [Se.And [Se.Is Beam.code $ Se.In code]]

findByRouteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Route.Route -> m (Maybe Domain.Types.Route.Route))
findByRouteId id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Route.Route -> m (Maybe Domain.Types.Route.Route))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Route.Route -> m ())
updateByPrimaryKey (Domain.Types.Route.Route {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.code code,
      Se.Set Beam.color color,
      Se.Set Beam.endLat ((.lat) endPoint),
      Se.Set Beam.endLon ((.lon) endPoint),
      Se.Set Beam.longName longName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.polyline polyline,
      Se.Set Beam.roundRouteCode roundRouteCode,
      Se.Set Beam.shortName shortName,
      Se.Set Beam.startLat ((.lat) startPoint),
      Se.Set Beam.startLon ((.lon) startPoint),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
