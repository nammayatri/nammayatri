{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Route where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Route as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Route.Route -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Route.Route] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.FRFS.Enums.VehicleCategory -> m [Domain.Types.Route.Route])
findAllByMerchantOperatingCityAndVehicleType merchantOperatingCityId vehicleType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

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
      Se.Set Beam.endLat ((.lat) endPoint),
      Se.Set Beam.endLon ((.lon) endPoint),
      Se.Set Beam.longName longName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.shortName shortName,
      Se.Set Beam.startLat ((.lat) startPoint),
      Se.Set Beam.startLon ((.lon) startPoint),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Route Domain.Types.Route.Route where
  fromTType' (Beam.RouteT {..}) = do
    pure $
      Just
        Domain.Types.Route.Route
          { code = code,
            endPoint = Kernel.External.Maps.Types.LatLong endLat endLon,
            id = Kernel.Types.Id.Id id,
            longName = longName,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            shortName = shortName,
            startPoint = Kernel.External.Maps.Types.LatLong startLat startLon,
            timeBounds = timeBounds,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Route Domain.Types.Route.Route where
  toTType' (Domain.Types.Route.Route {..}) = do
    Beam.RouteT
      { Beam.code = code,
        Beam.endLat = (.lat) endPoint,
        Beam.endLon = (.lon) endPoint,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.longName = longName,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.shortName = shortName,
        Beam.startLat = (.lat) startPoint,
        Beam.startLon = (.lon) startPoint,
        Beam.timeBounds = timeBounds,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
