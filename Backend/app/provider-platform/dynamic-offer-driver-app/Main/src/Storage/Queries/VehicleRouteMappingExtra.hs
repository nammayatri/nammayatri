{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRouteMappingExtra where

import Data.Text
import Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRouteMapping as Beam
import Storage.Queries.OrphanInstances.VehicleRouteMapping

findAllRouteMappings :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Encryption.DbHash -> m [Domain.Types.VehicleRouteMapping.VehicleRouteMapping])
findAllRouteMappings vehicleNumberHash = do findAllWithKV [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash, Se.Is Beam.blocked $ Se.Eq False]

findOneMapping :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.External.Encryption.DbHash -> Data.Text.Text -> m (Maybe Domain.Types.VehicleRouteMapping.VehicleRouteMapping))
findOneMapping vehicleNumberHash routeCode = do findOneWithKV [Se.And [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash, Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.blocked $ Se.Eq False]]

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => VehicleRouteMapping -> DbHash -> m ()
upsert a@VehicleRouteMapping {..} vehicleNumberHash = do
  res <- findOneWithKV [Se.And [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash, Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId.getId]]
  if isJust res
    then do
      now <- getCurrentTime
      updateOneWithKV
        [ Se.Set Beam.blocked blocked,
          Se.Set Beam.merchantId merchantId.getId,
          Se.Set Beam.merchantOperatingCityId merchantOperatingCityId.getId,
          Se.Set Beam.updatedAt now
        ]
        [Se.And [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash, Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId.getId]]
    else createWithKV a

updateBlockedByVehicleNumberHash :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DbHash -> Bool -> m ()
updateBlockedByVehicleNumberHash fleetOwnerId vehicleNumberHash blocked = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.blocked blocked,
      Se.Set Beam.updatedAt now
    ]
    [Se.And [Se.Is Beam.vehicleNumberHash $ Se.Eq vehicleNumberHash, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]
