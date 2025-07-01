{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteStopTimeTable where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.RouteStopTimeTable
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteStopTimeTable as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopTimeTable.RouteStopTimeTable -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteStopTimeTable.RouteStopTimeTable] -> m ())
createMany = traverse_ create

findByRouteCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.RouteStopTimeTable.RouteStopTimeTable])
findByRouteCode routeCode integratedBppConfigId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.In routeCode,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByRouteCodeAndStopCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.Prelude.Text] -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.RouteStopTimeTable.RouteStopTimeTable])
findByRouteCodeAndStopCode routeCode stopCode integratedBppConfigId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.In routeCode,
          Se.Is Beam.stopCode $ Se.Eq stopCode,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> BecknV2.FRFS.Enums.ServiceTierType -> Kernel.Prelude.Text -> Kernel.Prelude.TimeOfDay -> Kernel.Types.Id.Id Domain.Types.RouteStopTimeTable.RouteStopTimeTable -> m (Maybe Domain.Types.RouteStopTimeTable.RouteStopTimeTable))
findByPrimaryKey integratedBppConfigId serviceTierType stopCode timeOfArrival tripId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId),
          Se.Is Beam.serviceTierType $ Se.Eq serviceTierType,
          Se.Is Beam.stopCode $ Se.Eq stopCode,
          Se.Is Beam.timeOfArrival $ Se.Eq timeOfArrival,
          Se.Is Beam.tripId $ Se.Eq (Kernel.Types.Id.getId tripId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopTimeTable.RouteStopTimeTable -> m ())
updateByPrimaryKey (Domain.Types.RouteStopTimeTable.RouteStopTimeTable {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.delay delay,
      Se.Set Beam.platformCode platformCode,
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.source (Kernel.Prelude.Just source),
      Se.Set Beam.stage stage,
      Se.Set Beam.timeOfDeparture timeOfDeparture,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId),
          Se.Is Beam.serviceTierType $ Se.Eq serviceTierType,
          Se.Is Beam.stopCode $ Se.Eq stopCode,
          Se.Is Beam.timeOfArrival $ Se.Eq timeOfArrival,
          Se.Is Beam.tripId $ Se.Eq (Kernel.Types.Id.getId tripId)
        ]
    ]

instance FromTType' Beam.RouteStopTimeTable Domain.Types.RouteStopTimeTable.RouteStopTimeTable where
  fromTType' (Beam.RouteStopTimeTableT {..}) = do
    pure $
      Just
        Domain.Types.RouteStopTimeTable.RouteStopTimeTable
          { delay = delay,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            platformCode = platformCode,
            routeCode = routeCode,
            serviceTierType = serviceTierType,
            source = Kernel.Prelude.fromMaybe Domain.Types.RouteStopTimeTable.GTFS source,
            stage = stage,
            stopCode = stopCode,
            timeOfArrival = timeOfArrival,
            timeOfDeparture = timeOfDeparture,
            tripId = Kernel.Types.Id.Id tripId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteStopTimeTable Domain.Types.RouteStopTimeTable.RouteStopTimeTable where
  toTType' (Domain.Types.RouteStopTimeTable.RouteStopTimeTable {..}) = do
    Beam.RouteStopTimeTableT
      { Beam.delay = delay,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.platformCode = platformCode,
        Beam.routeCode = routeCode,
        Beam.serviceTierType = serviceTierType,
        Beam.source = Kernel.Prelude.Just source,
        Beam.stage = stage,
        Beam.stopCode = stopCode,
        Beam.timeOfArrival = timeOfArrival,
        Beam.timeOfDeparture = timeOfDeparture,
        Beam.tripId = Kernel.Types.Id.getId tripId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
