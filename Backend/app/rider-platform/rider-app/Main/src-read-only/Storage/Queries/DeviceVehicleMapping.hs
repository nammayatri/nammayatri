{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.DeviceVehicleMapping where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DeviceVehicleMapping
import qualified Storage.Beam.DeviceVehicleMapping as Beam
import qualified Data.Text
import qualified Kernel.Types.Id
import qualified Sequelize as Se



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
upsertMapping vehicleNo gtfsId deviceId = do {_now <- getCurrentTime;
                                              updateWithKV [Se.Set Beam.vehicleNo vehicleNo, Se.Set Beam.gtfsId gtfsId, Se.Set Beam.updatedAt _now] [Se.Is Beam.deviceId $ Se.Eq deviceId]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Data.Text.Text -> m (Maybe Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping))
findByPrimaryKey deviceId gtfsId = do findOneWithKV [Se.And [Se.Is Beam.deviceId $ Se.Eq deviceId, Se.Is Beam.gtfsId $ Se.Eq gtfsId]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping -> m ())
updateByPrimaryKey (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping {..}) = do {_now <- getCurrentTime;
                                                                                       updateWithKV [Se.Set Beam.updatedAt _now,
                                                                                                     Se.Set Beam.vehicleNo vehicleNo,
                                                                                                     Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                     Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)] [Se.And [Se.Is Beam.deviceId $ Se.Eq deviceId, Se.Is Beam.gtfsId $ Se.Eq gtfsId]]}



instance FromTType' Beam.DeviceVehicleMapping Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
    where fromTType' (Beam.DeviceVehicleMappingT {..}) = do pure $ Just Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping{createdAt = createdAt,
                                                                                                                               deviceId = deviceId,
                                                                                                                               gtfsId = gtfsId,
                                                                                                                               updatedAt = updatedAt,
                                                                                                                               vehicleNo = vehicleNo,
                                                                                                                               merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                               merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId}
instance ToTType' Beam.DeviceVehicleMapping Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
    where toTType' (Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping {..}) = do Beam.DeviceVehicleMappingT{Beam.createdAt = createdAt,
                                                                                                                 Beam.deviceId = deviceId,
                                                                                                                 Beam.gtfsId = gtfsId,
                                                                                                                 Beam.updatedAt = updatedAt,
                                                                                                                 Beam.vehicleNo = vehicleNo,
                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                                 Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId}



