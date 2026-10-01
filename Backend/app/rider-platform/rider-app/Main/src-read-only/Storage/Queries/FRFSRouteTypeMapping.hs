{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSRouteTypeMapping where

import qualified Domain.Types.FRFSRouteTypeMapping
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Domain.Types.IntegratedBPPConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSRouteTypeMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping] -> m ())
createMany = traverse_ create

findAllByRouteCodeAndIntegratedBppConfigId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ([Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping]))
findAllByRouteCodeAndIntegratedBppConfigId routeCode integratedBppConfigId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m (Maybe Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping))
findByPrimaryKey integratedBppConfigId routeCode vehicleServiceTierId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId),
          Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping -> m ())
updateByPrimaryKey (Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeType routeType,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId),
          Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId)
        ]
    ]

instance FromTType' Beam.FRFSRouteTypeMapping Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping where
  fromTType' (Beam.FRFSRouteTypeMappingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping
          { integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            routeType = routeType,
            vehicleServiceTierId = Kernel.Types.Id.Id vehicleServiceTierId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSRouteTypeMapping Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping where
  toTType' (Domain.Types.FRFSRouteTypeMapping.FRFSRouteTypeMapping {..}) = do
    Beam.FRFSRouteTypeMappingT
      { Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.routeType = routeType,
        Beam.vehicleServiceTierId = Kernel.Types.Id.getId vehicleServiceTierId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
