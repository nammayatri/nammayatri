{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleRegistrationCertificateExtra where

import qualified Domain.Types.IdfyVerification as IV
import Domain.Types.Vehicle as Vehicle
import Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.OrphanInstances.VehicleRegistrationCertificate

-- Extra code goes here --
upsert :: KvDbFlow m r => VehicleRegistrationCertificate -> m ()
upsert a@VehicleRegistrationCertificate {..} = do
  res <- findOneWithKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash))]
  if isJust res
    then
      updateWithKV
        [ Se.Set BeamVRC.permitExpiry permitExpiry,
          Se.Set BeamVRC.pucExpiry pucExpiry,
          Se.Set BeamVRC.insuranceValidity insuranceValidity,
          Se.Set BeamVRC.vehicleClass vehicleClass,
          Se.Set BeamVRC.vehicleVariant vehicleVariant,
          Se.Set BeamVRC.vehicleManufacturer vehicleManufacturer,
          Se.Set BeamVRC.manufacturerModel manufacturerModel,
          Se.Set BeamVRC.vehicleCapacity vehicleCapacity,
          Se.Set BeamVRC.vehicleModel vehicleModel,
          Se.Set BeamVRC.vehicleColor vehicleColor,
          Se.Set BeamVRC.vehicleEnergyType vehicleEnergyType,
          Se.Set BeamVRC.verificationStatus verificationStatus,
          Se.Set BeamVRC.reviewedAt reviewedAt,
          Se.Set BeamVRC.failedRules failedRules,
          Se.Set BeamVRC.fleetOwnerId fleetOwnerId,
          Se.Set BeamVRC.fitnessExpiry fitnessExpiry,
          Se.Set BeamVRC.reviewRequired reviewRequired,
          Se.Set BeamVRC.airConditioned airConditioned,
          Se.Set BeamVRC.luggageCapacity luggageCapacity,
          Se.Set BeamVRC.userPassedVehicleCategory userPassedVehicleCategory,
          Se.Set BeamVRC.updatedAt updatedAt
        ]
        [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash))]
    else createWithKV a

findLastVehicleRC :: KvDbFlow m r => DbHash -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumberHash = do
  findAllWithOptionsKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash] (Se.Desc BeamVRC.fitnessExpiry) Nothing Nothing <&> listToMaybe

updateVehicleVariant :: KvDbFlow m r => Id VehicleRegistrationCertificate -> Maybe Vehicle.Variant -> Maybe Bool -> Maybe Bool -> m ()
updateVehicleVariant (Id vehicleRegistrationCertificateId) variant reviewDone reviewRequired = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamVRC.updatedAt now]
        <> [Se.Set BeamVRC.reviewedAt (Just now) | isJust reviewDone]
        <> [Se.Set BeamVRC.reviewRequired reviewRequired | isJust reviewRequired]
        <> [Se.Set BeamVRC.vehicleVariant variant | isJust variant]
        <> [Se.Set BeamVRC.verificationStatus IV.VALID | isJust variant]
    )
    [Se.Is BeamVRC.id (Se.Eq vehicleRegistrationCertificateId)]

findByRCAndExpiry :: KvDbFlow m r => EncryptedHashedField 'AsEncrypted Text -> UTCTime -> m (Maybe VehicleRegistrationCertificate)
findByRCAndExpiry certNumber expiry = do
  let certNumberHash = certNumber & (.hash)
  findOneWithKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fitnessExpiry $ Se.Eq expiry]]

findAllById :: KvDbFlow m r => [Id VehicleRegistrationCertificate] -> m [VehicleRegistrationCertificate]
findAllById rcIds = findAllWithKV [Se.Is BeamVRC.id $ Se.In $ map (.getId) rcIds]

findLastVehicleRCWrapper :: (EncFlow m r, KvDbFlow m r) => Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCWrapper certNumber = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRC certNumberHash

findLastVehicleRCFleet :: KvDbFlow m r => DbHash -> Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCFleet certNumberHash fleetOwnerId = do
  findAllWithOptionsKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fleetOwnerId $ Se.Eq $ Just fleetOwnerId]] (Se.Desc BeamVRC.updatedAt) Nothing Nothing <&> listToMaybe

findLastVehicleRCFleet' :: (EncFlow m r, KvDbFlow m r) => Text -> Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCFleet' certNumber fleetOwnerId = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRCFleet certNumberHash fleetOwnerId
