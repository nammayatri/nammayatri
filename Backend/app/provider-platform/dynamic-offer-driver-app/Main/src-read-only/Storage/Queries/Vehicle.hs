{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Vehicle (module Storage.Queries.Vehicle, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.Common
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Vehicle as Beam
import Storage.Queries.VehicleExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Vehicle.Vehicle -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Vehicle.Vehicle] -> m ())
createMany = traverse_ create

deleteByDriverid :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverid driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Vehicle.Vehicle))
findById driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByRegistrationNo :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Vehicle.Vehicle))
findByRegistrationNo registrationNo = do findOneWithKV [Se.Is Beam.registrationNo $ Se.Eq registrationNo]

updateAirConditioned ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAirConditioned airConditioned downgradeReason driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.airConditioned airConditioned, Se.Set Beam.downgradeReason downgradeReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateManufacturing :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateManufacturing mYManufacturing driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.mYManufacturing mYManufacturing, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateOxygen :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateOxygen oxygen driverId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.oxygen oxygen, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateSelectedServiceTiers :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Common.ServiceTierType] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateSelectedServiceTiers selectedServiceTiers driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.selectedServiceTiers selectedServiceTiers, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVariantAndServiceTiers ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.VehicleVariant.VehicleVariant -> [Domain.Types.Common.ServiceTierType] -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVariantAndServiceTiers variant selectedServiceTiers category driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.variant variant,
      Se.Set Beam.selectedServiceTiers selectedServiceTiers,
      Se.Set Beam.category category,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVehicleModel :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVehicleModel model driverId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.model model, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVehicleName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVehicleName vehicleName driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.vehicleName vehicleName, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVehicleVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.VehicleVariant.VehicleVariant -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVehicleVariant variant category driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.variant variant, Se.Set Beam.category category, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateVentilator :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateVentilator ventilator driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.ventilator ventilator, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Vehicle.Vehicle))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Vehicle.Vehicle -> m ())
updateByPrimaryKey (Domain.Types.Vehicle.Vehicle {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.airConditioned airConditioned,
      Se.Set Beam.capacity capacity,
      Se.Set Beam.category category,
      Se.Set Beam.color color,
      Se.Set Beam.downgradeReason downgradeReason,
      Se.Set Beam.energyType energyType,
      Se.Set Beam.luggageCapacity luggageCapacity,
      Se.Set Beam.mYManufacturing mYManufacturing,
      Se.Set Beam.make make,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.model model,
      Se.Set Beam.oxygen oxygen,
      Se.Set Beam.registrationCategory registrationCategory,
      Se.Set Beam.registrationNo registrationNo,
      Se.Set Beam.selectedServiceTiers selectedServiceTiers,
      Se.Set Beam.size size,
      Se.Set Beam.variant variant,
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleName vehicleName,
      Se.Set Beam.vehicleRating vehicleRating,
      Se.Set Beam.ventilator ventilator,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
