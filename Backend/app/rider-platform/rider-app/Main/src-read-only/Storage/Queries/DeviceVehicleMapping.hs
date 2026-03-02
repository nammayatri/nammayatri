{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DeviceVehicleMapping where

import qualified Data.Text
import qualified Domain.Types.DeviceVehicleMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DeviceVehicleMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping] -> m ())
createMany = traverse_ create

findAllByGtfsId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m ([Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping]))
findAllByGtfsId gtfsId = do findAllWithKV [Se.Is Beam.gtfsId $ Se.Eq gtfsId]

findByDeviceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping))
findByDeviceId deviceId = do findOneWithKV [Se.Is Beam.deviceId $ Se.Eq deviceId]

findByDeviceIdAndGtfsId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Data.Text.Text -> m (Maybe Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping))
findByDeviceIdAndGtfsId deviceId gtfsId = do findOneWithKV [Se.Is Beam.deviceId $ Se.Eq deviceId, Se.Is Beam.gtfsId $ Se.Eq gtfsId]

findByVehicleNo :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m ([Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping]))
findByVehicleNo vehicleNo = do findAllWithKV [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo]

upsertMapping :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Data.Text.Text -> Data.Text.Text -> m ())
upsertMapping vehicleNo gtfsId deviceId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.vehicleNo vehicleNo, Se.Set Beam.gtfsId gtfsId, Se.Set Beam.updatedAt _now] [Se.Is Beam.deviceId $ Se.Eq deviceId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping))
findByPrimaryKey deviceId = do findOneWithKV [Se.And [Se.Is Beam.deviceId $ Se.Eq deviceId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping -> m ())
updateByPrimaryKey (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.gtfsId gtfsId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleNo vehicleNo,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.deviceId $ Se.Eq deviceId]]

instance FromTType' Beam.DeviceVehicleMapping Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping where
  fromTType' (Beam.DeviceVehicleMappingT {..}) = do
    pure $
      Just
        Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
          { createdAt = createdAt,
            deviceId = deviceId,
            gtfsId = gtfsId,
            updatedAt = updatedAt,
            vehicleNo = vehicleNo,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.DeviceVehicleMapping Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping where
  toTType' (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping {..}) = do
    Beam.DeviceVehicleMappingT
      { Beam.createdAt = createdAt,
        Beam.deviceId = deviceId,
        Beam.gtfsId = gtfsId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNo = vehicleNo,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
