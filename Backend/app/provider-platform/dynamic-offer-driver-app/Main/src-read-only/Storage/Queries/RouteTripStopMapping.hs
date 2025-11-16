{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteTripStopMapping where

import qualified Data.Time
import qualified Domain.Types.RouteTripStopMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteTripStopMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteTripStopMapping.RouteTripStopMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteTripStopMapping.RouteTripStopMapping] -> m ())
createMany = traverse_ create

findAllByRouteCodeForStops ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Int -> Data.Time.DayOfWeek -> m [Domain.Types.RouteTripStopMapping.RouteTripStopMapping])
findAllByRouteCodeForStops routeCode tripSequenceNum scheduledDay = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.tripSequenceNum $ Se.Eq tripSequenceNum,
          Se.Is Beam.scheduledDay $ Se.Eq scheduledDay
        ]
    ]

findAllByStopCodeAndStopSequenceAndRoutes ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Data.Time.DayOfWeek -> [Kernel.Prelude.Text] -> m [Domain.Types.RouteTripStopMapping.RouteTripStopMapping])
findAllByStopCodeAndStopSequenceAndRoutes stopCode stopSequenceNum tripSequenceNum scheduledDay routeCode = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.stopCode $ Se.Eq stopCode,
          Se.Is Beam.stopSequenceNum $ Se.Eq stopSequenceNum,
          Se.Is Beam.tripSequenceNum $ Se.Eq tripSequenceNum,
          Se.Is Beam.scheduledDay $ Se.Eq scheduledDay,
          Se.Is Beam.routeCode $ Se.In routeCode
        ]
    ]

findAllRTSMappingByRouteAndDay :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Data.Time.DayOfWeek -> m [Domain.Types.RouteTripStopMapping.RouteTripStopMapping])
findAllRTSMappingByRouteAndDay routeCode scheduledDay = do findAllWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.scheduledDay $ Se.Eq scheduledDay]]

findByLocation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Maps.Types.LatLong -> m (Maybe Domain.Types.RouteTripStopMapping.RouteTripStopMapping))
findByLocation stopPoint = do findOneWithKV [Se.Is Beam.stopLat $ Se.Eq ((.lat) stopPoint), Se.Is Beam.stopLon $ Se.Eq ((.lon) stopPoint)]

findByRouteCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.RouteTripStopMapping.RouteTripStopMapping])
findByRouteCode routeCode = do findAllWithKV [Se.Is Beam.routeCode $ Se.Eq routeCode]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Data.Time.DayOfWeek -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Maybe Domain.Types.RouteTripStopMapping.RouteTripStopMapping))
findByPrimaryKey routeCode scheduledDay stopCode tripCode tripSequenceNum = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.scheduledDay $ Se.Eq scheduledDay,
          Se.Is Beam.stopCode $ Se.Eq stopCode,
          Se.Is Beam.tripCode $ Se.Eq tripCode,
          Se.Is Beam.tripSequenceNum $ Se.Eq tripSequenceNum
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteTripStopMapping.RouteTripStopMapping -> m ())
updateByPrimaryKey (Domain.Types.RouteTripStopMapping.RouteTripStopMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.providerCode providerCode,
      Se.Set Beam.scheduledArrival scheduledArrival,
      Se.Set Beam.scheduledDeparture scheduledDeparture,
      Se.Set Beam.stopName stopName,
      Se.Set Beam.stopLat ((.lat) stopPoint),
      Se.Set Beam.stopLon ((.lon) stopPoint),
      Se.Set Beam.stopSequenceNum stopSequenceNum,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.scheduledDay $ Se.Eq scheduledDay,
          Se.Is Beam.stopCode $ Se.Eq stopCode,
          Se.Is Beam.tripCode $ Se.Eq tripCode,
          Se.Is Beam.tripSequenceNum $ Se.Eq tripSequenceNum
        ]
    ]

instance FromTType' Beam.RouteTripStopMapping Domain.Types.RouteTripStopMapping.RouteTripStopMapping where
  fromTType' (Beam.RouteTripStopMappingT {..}) = do
    pure $
      Just
        Domain.Types.RouteTripStopMapping.RouteTripStopMapping
          { enabled = enabled,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            providerCode = providerCode,
            routeCode = routeCode,
            scheduledArrival = scheduledArrival,
            scheduledDay = scheduledDay,
            scheduledDeparture = scheduledDeparture,
            stopCode = stopCode,
            stopName = stopName,
            stopPoint = Kernel.External.Maps.Types.LatLong stopLat stopLon,
            stopSequenceNum = stopSequenceNum,
            tripCode = tripCode,
            tripSequenceNum = tripSequenceNum,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteTripStopMapping Domain.Types.RouteTripStopMapping.RouteTripStopMapping where
  toTType' (Domain.Types.RouteTripStopMapping.RouteTripStopMapping {..}) = do
    Beam.RouteTripStopMappingT
      { Beam.enabled = enabled,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.providerCode = providerCode,
        Beam.routeCode = routeCode,
        Beam.scheduledArrival = scheduledArrival,
        Beam.scheduledDay = scheduledDay,
        Beam.scheduledDeparture = scheduledDeparture,
        Beam.stopCode = stopCode,
        Beam.stopName = stopName,
        Beam.stopLat = (.lat) stopPoint,
        Beam.stopLon = (.lon) stopPoint,
        Beam.stopSequenceNum = stopSequenceNum,
        Beam.tripCode = tripCode,
        Beam.tripSequenceNum = tripSequenceNum,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
