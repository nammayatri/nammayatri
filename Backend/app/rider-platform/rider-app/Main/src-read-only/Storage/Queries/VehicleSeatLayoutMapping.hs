{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleSeatLayoutMapping where

import qualified Domain.Types.SeatLayout
import qualified Domain.Types.VehicleSeatLayoutMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleSeatLayoutMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping] -> m ())
createMany = traverse_ create

deleteByVehicleNoAndGtfsId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
deleteByVehicleNoAndGtfsId vehicleNo gtfsId = do deleteWithKV [Se.And [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo, Se.Is Beam.gtfsId $ Se.Eq gtfsId]]

findAllByGtfsId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping]))
findAllByGtfsId gtfsId = do findAllWithKV [Se.Is Beam.gtfsId $ Se.Eq gtfsId]

findAllBySeatLayoutId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> m ([Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping]))
findAllBySeatLayoutId seatLayoutId = do findAllWithKV [Se.Is Beam.seatLayoutId $ Se.Eq (Kernel.Types.Id.getId seatLayoutId)]

findByVehicleNoAndGtfsId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping))
findByVehicleNoAndGtfsId vehicleNo gtfsId = do findOneWithKV [Se.And [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo, Se.Is Beam.gtfsId $ Se.Eq gtfsId]]

updateSeatLayoutIdByVehicleNoAndGtfsId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateSeatLayoutIdByVehicleNoAndGtfsId seatLayoutId vehicleNo gtfsId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.seatLayoutId (Kernel.Types.Id.getId seatLayoutId), Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo, Se.Is Beam.gtfsId $ Se.Eq gtfsId]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping -> m (Maybe Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping -> m ())
updateByPrimaryKey (Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.gtfsId gtfsId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.seatLayoutId (Kernel.Types.Id.getId seatLayoutId),
      Se.Set Beam.vehicleNo vehicleNo,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleSeatLayoutMapping Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping where
  fromTType' (Beam.VehicleSeatLayoutMappingT {..}) = do
    pure $
      Just
        Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping
          { gtfsId = gtfsId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            seatLayoutId = Kernel.Types.Id.Id seatLayoutId,
            vehicleNo = vehicleNo,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleSeatLayoutMapping Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping where
  toTType' (Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping {..}) = do
    Beam.VehicleSeatLayoutMappingT
      { Beam.gtfsId = gtfsId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.seatLayoutId = Kernel.Types.Id.getId seatLayoutId,
        Beam.vehicleNo = vehicleNo,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
