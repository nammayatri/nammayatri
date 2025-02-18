{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteStopMapping where

import qualified Domain.Types.RouteStopMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteStopMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopMapping.RouteStopMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteStopMapping.RouteStopMapping] -> m ())
createMany = traverse_ create

findByRouteCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.RouteStopMapping.RouteStopMapping])
findByRouteCode routeCode = do findAllWithKV [Se.Is Beam.routeCode $ Se.Eq routeCode]

findByRouteCodeAndStopCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RouteStopMapping.RouteStopMapping))
findByRouteCodeAndStopCode routeCode stopCode = do findOneWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.stopCode $ Se.Eq stopCode]]

findByRouteCodes :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Prelude.Text] -> m [Domain.Types.RouteStopMapping.RouteStopMapping])
findByRouteCodes routeCode = do findAllWithKV [Se.And [Se.Is Beam.routeCode $ Se.In routeCode]]

findByStopCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.RouteStopMapping.RouteStopMapping])
findByStopCode stopCode = do findAllWithKV [Se.Is Beam.stopCode $ Se.Eq stopCode]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RouteStopMapping.RouteStopMapping))
findByPrimaryKey routeCode stopCode = do findOneWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.stopCode $ Se.Eq stopCode]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopMapping.RouteStopMapping -> m ())
updateByPrimaryKey (Domain.Types.RouteStopMapping.RouteStopMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.estimatedTravelTimeFromPreviousStop estimatedTravelTimeFromPreviousStop,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.providerCode providerCode,
      Se.Set Beam.sequenceNum sequenceNum,
      Se.Set Beam.stopName stopName,
      Se.Set Beam.stopLat ((.lat) stopPoint),
      Se.Set Beam.stopLon ((.lon) stopPoint),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.stopCode $ Se.Eq stopCode]]

instance FromTType' Beam.RouteStopMapping Domain.Types.RouteStopMapping.RouteStopMapping where
  fromTType' (Beam.RouteStopMappingT {..}) = do
    pure $
      Just
        Domain.Types.RouteStopMapping.RouteStopMapping
          { estimatedTravelTimeFromPreviousStop = estimatedTravelTimeFromPreviousStop,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            providerCode = providerCode,
            routeCode = routeCode,
            sequenceNum = sequenceNum,
            stopCode = stopCode,
            stopName = stopName,
            stopPoint = Kernel.External.Maps.Types.LatLong stopLat stopLon,
            timeBounds = timeBounds,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteStopMapping Domain.Types.RouteStopMapping.RouteStopMapping where
  toTType' (Domain.Types.RouteStopMapping.RouteStopMapping {..}) = do
    Beam.RouteStopMappingT
      { Beam.estimatedTravelTimeFromPreviousStop = estimatedTravelTimeFromPreviousStop,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.providerCode = providerCode,
        Beam.routeCode = routeCode,
        Beam.sequenceNum = sequenceNum,
        Beam.stopCode = stopCode,
        Beam.stopName = stopName,
        Beam.stopLat = (.lat) stopPoint,
        Beam.stopLon = (.lon) stopPoint,
        Beam.timeBounds = timeBounds,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
