{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRouteMapping (module Storage.Queries.VehicleRouteMapping, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRouteMapping as Beam
import Storage.Queries.VehicleRouteMappingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleRouteMapping.VehicleRouteMapping] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.VehicleRouteMapping.VehicleRouteMapping))
findByPrimaryKey routeCode = do findOneWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
updateByPrimaryKey (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.blocked blocked,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleColor vehicleColor,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleNumberEncrypted (((vehicleNumber & unEncrypted . encrypted))),
      Se.Set Beam.vehicleNumberHash ((vehicleNumber & hash)),
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode]]
