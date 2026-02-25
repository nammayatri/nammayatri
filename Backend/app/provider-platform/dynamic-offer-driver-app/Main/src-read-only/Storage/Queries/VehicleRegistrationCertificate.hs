{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRegistrationCertificate (module Storage.Queries.VehicleRegistrationCertificate, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleRegistrationCertificate
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRegistrationCertificate as Beam
import Storage.Queries.VehicleRegistrationCertificateExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate] -> m ())
createMany = traverse_ create

findAllByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate]))
findAllByFleetOwnerId limit offset fleetOwnerId = do findAllWithOptionsKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId] (Se.Desc Beam.updatedAt) limit offset

findAllByFleetOwnerIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> [Kernel.Prelude.Maybe Kernel.Prelude.Text] -> m ([Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate]))
findAllByFleetOwnerIds limit offset fleetOwnerId = do findAllWithOptionsKV [Se.Is Beam.fleetOwnerId $ Se.In fleetOwnerId] (Se.Desc Beam.createdAt) limit offset

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findByImageId documentImageId = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByRCIdAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findByRCIdAndFleetOwnerId id fleetOwnerId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]

updateAirConditioned ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateAirConditioned airConditioned id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.airConditioned airConditioned, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateApproved ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateApproved approved id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.approved approved, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateFleetOwnerId fleetOwnerId id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.fleetOwnerId fleetOwnerId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateManufacturing ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateManufacturing mYManufacturing id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.mYManufacturing mYManufacturing, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateMerchantIdAndCityIdById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateMerchantIdAndCityIdById merchantId merchantOperatingCityId id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOxygen ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateOxygen oxygen id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.oxygen oxygen, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVehicleImageId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile) -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateVehicleImageId vehicleImageId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.vehicleImageId (Kernel.Types.Id.getId <$> vehicleImageId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVentilator ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateVentilator ventilator id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.ventilator ventilator, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatus verificationStatus documentImageId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
updateByPrimaryKey (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.airConditioned airConditioned,
      Se.Set Beam.approved approved,
      Se.Set Beam.certificateNumberEncrypted (((certificateNumber & unEncrypted . encrypted))),
      Se.Set Beam.certificateNumberHash ((certificateNumber & hash)),
      Se.Set Beam.dateOfRegistration dateOfRegistration,
      Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.failedRules failedRules,
      Se.Set Beam.fitnessExpiry fitnessExpiry,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.insuranceValidity insuranceValidity,
      Se.Set Beam.luggageCapacity luggageCapacity,
      Se.Set Beam.mYManufacturing mYManufacturing,
      Se.Set Beam.manufacturerModel manufacturerModel,
      Se.Set Beam.oxygen oxygen,
      Se.Set Beam.permitExpiry permitExpiry,
      Se.Set Beam.pucExpiry pucExpiry,
      Se.Set Beam.rejectReason rejectReason,
      Se.Set Beam.reviewRequired reviewRequired,
      Se.Set Beam.reviewedAt reviewedAt,
      Se.Set Beam.unencryptedCertificateNumber unencryptedCertificateNumber,
      Se.Set Beam.userPassedVehicleCategory userPassedVehicleCategory,
      Se.Set Beam.vehicleCapacity vehicleCapacity,
      Se.Set Beam.vehicleClass vehicleClass,
      Se.Set Beam.vehicleColor vehicleColor,
      Se.Set Beam.vehicleDoors vehicleDoors,
      Se.Set Beam.vehicleEnergyType vehicleEnergyType,
      Se.Set Beam.vehicleImageId (Kernel.Types.Id.getId <$> vehicleImageId),
      Se.Set Beam.vehicleManufacturer vehicleManufacturer,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleModelYear vehicleModelYear,
      Se.Set Beam.vehicleRating vehicleRating,
      Se.Set Beam.vehicleSeatBelts vehicleSeatBelts,
      Se.Set Beam.vehicleVariant vehicleVariant,
      Se.Set Beam.ventilator ventilator,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
