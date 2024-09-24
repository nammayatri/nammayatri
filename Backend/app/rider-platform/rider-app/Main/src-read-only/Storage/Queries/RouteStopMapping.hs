{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteStopMapping where

import qualified Domain.Types.RouteStopMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude hiding (sequence)
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteStopMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopMapping.RouteStopMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteStopMapping.RouteStopMapping] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RouteStopMapping.RouteStopMapping -> m (Maybe Domain.Types.RouteStopMapping.RouteStopMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopMapping.RouteStopMapping -> m ())
updateByPrimaryKey (Domain.Types.RouteStopMapping.RouteStopMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.routeId (Kernel.Types.Id.getId routeId),
      Se.Set Beam.sequence sequence,
      Se.Set Beam.stopId (Kernel.Types.Id.getId stopId),
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RouteStopMapping Domain.Types.RouteStopMapping.RouteStopMapping where
  fromTType' (Beam.RouteStopMappingT {..}) = do
    pure $
      Just
        Domain.Types.RouteStopMapping.RouteStopMapping
          { id = Kernel.Types.Id.Id id,
            routeId = Kernel.Types.Id.Id routeId,
            sequence = sequence,
            stopId = Kernel.Types.Id.Id stopId,
            timeBounds = timeBounds,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteStopMapping Domain.Types.RouteStopMapping.RouteStopMapping where
  toTType' (Domain.Types.RouteStopMapping.RouteStopMapping {..}) = do
    Beam.RouteStopMappingT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.routeId = Kernel.Types.Id.getId routeId,
        Beam.sequence = sequence,
        Beam.stopId = Kernel.Types.Id.getId stopId,
        Beam.timeBounds = timeBounds,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
