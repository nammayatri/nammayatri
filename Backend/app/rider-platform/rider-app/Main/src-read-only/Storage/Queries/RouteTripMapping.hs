{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteTripMapping where

import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.RouteTripMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteTripMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteTripMapping.RouteTripMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteTripMapping.RouteTripMapping] -> m ())
createMany = traverse_ create

findAllTripIdByRouteCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.RouteTripMapping.RouteTripMapping])
findAllTripIdByRouteCode routeCode integratedBppConfigId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.RouteTripMapping.RouteTripMapping))
findByPrimaryKey tripCode = do findOneWithKV [Se.And [Se.Is Beam.tripCode $ Se.Eq tripCode]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteTripMapping.RouteTripMapping -> m ())
updateByPrimaryKey (Domain.Types.RouteTripMapping.RouteTripMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.tripEndTime tripEndTime,
      Se.Set Beam.tripStartTime tripStartTime,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleType vehicleType
    ]
    [Se.And [Se.Is Beam.tripCode $ Se.Eq tripCode]]

instance FromTType' Beam.RouteTripMapping Domain.Types.RouteTripMapping.RouteTripMapping where
  fromTType' (Beam.RouteTripMappingT {..}) = do
    pure $
      Just
        Domain.Types.RouteTripMapping.RouteTripMapping
          { createdAt = createdAt,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            tripCode = tripCode,
            tripEndTime = tripEndTime,
            tripStartTime = tripStartTime,
            updatedAt = updatedAt,
            vehicleType = vehicleType
          }

instance ToTType' Beam.RouteTripMapping Domain.Types.RouteTripMapping.RouteTripMapping where
  toTType' (Domain.Types.RouteTripMapping.RouteTripMapping {..}) = do
    Beam.RouteTripMappingT
      { Beam.createdAt = createdAt,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.tripCode = tripCode,
        Beam.tripEndTime = tripEndTime,
        Beam.tripStartTime = tripStartTime,
        Beam.updatedAt = updatedAt,
        Beam.vehicleType = vehicleType
      }
