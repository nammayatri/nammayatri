{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Route where

import qualified Domain.Types.Route
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude hiding (sequence)
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

findAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.Route.Route]))
findAll routeId = do findAllWithKV [Se.Is Beam.routeId $ Se.Eq routeId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Route.Route))
findById routeId = do findOneWithKV [Se.Is Beam.routeId $ Se.Eq routeId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Route.Route))
findByPrimaryKey routeId = do findOneWithKV [Se.And [Se.Is Beam.routeId $ Se.Eq routeId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Route.Route -> m ())
updateByPrimaryKey (Domain.Types.Route.Route {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.endLat ((.lat) endPoint),
      Se.Set Beam.endLon ((.lon) endPoint),
      Se.Set Beam.longName longName,
      Se.Set Beam.shortName shortName,
      Se.Set Beam.startLat ((.lat) startPoint),
      Se.Set Beam.startLon ((.lon) startPoint),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.routeId $ Se.Eq routeId]]

instance FromTType' Beam.Route Domain.Types.Route.Route where
  fromTType' (Beam.RouteT {..}) = do
    pure $
      Just
        Domain.Types.Route.Route
          { endPoint = Kernel.External.Maps.Types.LatLong endLat endLon,
            longName = longName,
            routeId = routeId,
            shortName = shortName,
            startPoint = Kernel.External.Maps.Types.LatLong startLat startLon,
            timeBounds = timeBounds,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Route Domain.Types.Route.Route where
  toTType' (Domain.Types.Route.Route {..}) = do
    Beam.RouteT
      { Beam.endLat = (.lat) endPoint,
        Beam.endLon = (.lon) endPoint,
        Beam.longName = longName,
        Beam.routeId = routeId,
        Beam.shortName = shortName,
        Beam.startLat = (.lat) startPoint,
        Beam.startLon = (.lon) startPoint,
        Beam.timeBounds = timeBounds,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
