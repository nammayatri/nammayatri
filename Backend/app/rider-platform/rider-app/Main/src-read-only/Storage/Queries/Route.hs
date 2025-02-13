{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Route (module Storage.Queries.Route, module ReExport) where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route
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

deleteByRouteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Route.Route -> m ())
deleteByRouteId id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantOperatingCityAndVehicleTypeAndVersionTag ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> m [Domain.Types.Route.Route])
findAllByMerchantOperatingCityAndVehicleTypeAndVersionTag limit offset merchantOperatingCityId vehicleType versionTag = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.versionTag $ Se.Eq versionTag
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByRouteCodeAndVersionTag :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> m (Maybe Domain.Types.Route.Route))
findByRouteCodeAndVersionTag code versionTag = do findOneWithKV [Se.And [Se.Is Beam.code $ Se.Eq code, Se.Is Beam.versionTag $ Se.Eq versionTag]]

findByRouteCodesAndVersionTag :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> [Kernel.Prelude.Text] -> m [Domain.Types.Route.Route])
findByRouteCodesAndVersionTag versionTag code = do findAllWithKV [Se.And [Se.Is Beam.versionTag $ Se.Eq versionTag, Se.Is Beam.code $ Se.In code]]

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
      Se.Set Beam.shortName shortName,
      Se.Set Beam.startLat ((.lat) startPoint),
      Se.Set Beam.startLon ((.lon) startPoint),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.versionTag versionTag,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
