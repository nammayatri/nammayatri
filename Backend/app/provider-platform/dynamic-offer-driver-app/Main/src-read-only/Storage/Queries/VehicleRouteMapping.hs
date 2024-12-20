{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRouteMapping where

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

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleRouteMapping.VehicleRouteMapping] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Data.Text.Text -> m (Maybe Domain.Types.VehicleRouteMapping.VehicleRouteMapping))
findByPrimaryKey routeCode vehicleNumber = do findOneWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.vehicleNumber $ Se.Eq vehicleNumber]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRouteMapping.VehicleRouteMapping -> m ())
updateByPrimaryKey (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowEndingMidRoute allowEndingMidRoute,
      Se.Set Beam.blocked blocked,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleColor vehicleColor,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.vehicleNumber $ Se.Eq vehicleNumber]]

instance FromTType' Beam.VehicleRouteMapping Domain.Types.VehicleRouteMapping.VehicleRouteMapping where
  fromTType' (Beam.VehicleRouteMappingT {..}) = do
    pure $
      Just
        Domain.Types.VehicleRouteMapping.VehicleRouteMapping
          { allowEndingMidRoute = allowEndingMidRoute,
            blocked = blocked,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            routeCode = routeCode,
            vehicleClass = vehicleClass,
            vehicleColor = vehicleColor,
            vehicleModel = vehicleModel,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = vehicleServiceTierType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleRouteMapping Domain.Types.VehicleRouteMapping.VehicleRouteMapping where
  toTType' (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do
    Beam.VehicleRouteMappingT
      { Beam.allowEndingMidRoute = allowEndingMidRoute,
        Beam.blocked = blocked,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.routeCode = routeCode,
        Beam.vehicleClass = vehicleClass,
        Beam.vehicleColor = vehicleColor,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
