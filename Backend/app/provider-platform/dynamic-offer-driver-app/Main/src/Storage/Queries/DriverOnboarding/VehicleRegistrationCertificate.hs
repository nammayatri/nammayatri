{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate where

import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Vehicle as Vehicle
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC

create :: MonadFlow m => VehicleRegistrationCertificate -> m ()
create = createWithKV

upsert :: MonadFlow m => VehicleRegistrationCertificate -> m ()
upsert a@VehicleRegistrationCertificate {..} = do
  res <- findOneWithKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash)), Se.Is BeamVRC.fitnessExpiry $ Se.Eq a.fitnessExpiry]]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamVRC.permitExpiry permitExpiry,
          Se.Set BeamVRC.pucExpiry pucExpiry,
          Se.Set BeamVRC.insuranceValidity insuranceValidity,
          Se.Set BeamVRC.vehicleClass vehicleClass,
          Se.Set BeamVRC.vehicleVariant vehicleVariant,
          Se.Set BeamVRC.vehicleManufacturer vehicleManufacturer,
          Se.Set BeamVRC.vehicleCapacity vehicleCapacity,
          Se.Set BeamVRC.vehicleModel vehicleModel,
          Se.Set BeamVRC.vehicleColor vehicleColor,
          Se.Set BeamVRC.vehicleEnergyType vehicleEnergyType,
          Se.Set BeamVRC.verificationStatus verificationStatus,
          Se.Set BeamVRC.failedRules failedRules,
          Se.Set BeamVRC.fleetOwnerId fleetOwnerId,
          Se.Set BeamVRC.updatedAt updatedAt
        ]
        [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash)), Se.Is BeamVRC.fitnessExpiry $ Se.Eq a.fitnessExpiry]]
    else createWithKV a

findById :: MonadFlow m => Id VehicleRegistrationCertificate -> m (Maybe VehicleRegistrationCertificate)
findById (Id vrcID) = findOneWithKV [Se.Is BeamVRC.id $ Se.Eq vrcID]

updateFleetOwnerId :: MonadFlow m => Id VehicleRegistrationCertificate -> Maybe Text -> m ()
updateFleetOwnerId (Id rcId) fleetOwnerId =
  updateWithKV
    [Se.Set BeamVRC.fleetOwnerId fleetOwnerId]
    [Se.Is BeamVRC.id $ Se.Eq rcId]

findLastVehicleRC :: (MonadFlow m) => DbHash -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumberHash = do
  findAllWithOptionsKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash] (Se.Desc BeamVRC.fitnessExpiry) Nothing Nothing <&> listToMaybe

updateVehicleVariant :: (MonadFlow m) => Id VehicleRegistrationCertificate -> Maybe Vehicle.Variant -> m ()
updateVehicleVariant (Id vehicleRegistrationCertificateId) variant = do
  updateOneWithKV
    [Se.Set BeamVRC.vehicleVariant variant]
    [Se.Is BeamVRC.id (Se.Eq vehicleRegistrationCertificateId)]

findByRCAndExpiry :: MonadFlow m => EncryptedHashedField 'AsEncrypted Text -> UTCTime -> m (Maybe VehicleRegistrationCertificate)
findByRCAndExpiry certNumber expiry = do
  let certNumberHash = certNumber & (.hash)
  findOneWithKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fitnessExpiry $ Se.Eq expiry]]

findAllById :: MonadFlow m => [Id VehicleRegistrationCertificate] -> m [VehicleRegistrationCertificate]
findAllById rcIds = findAllWithKV [Se.Is BeamVRC.id $ Se.In $ map (.getId) rcIds]

findAllByFleetOwnerId :: MonadFlow m => Text -> Int -> Int -> m [VehicleRegistrationCertificate]
findAllByFleetOwnerId fleetOwnerId limit offset = do
  findAllWithOptionsKV
    [Se.Is BeamVRC.fleetOwnerId $ Se.Eq $ Just fleetOwnerId]
    (Se.Desc BeamVRC.updatedAt)
    (Just limit)
    (Just offset)

findLastVehicleRCWrapper :: (MonadFlow m, EncFlow m r) => Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCWrapper certNumber = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRC certNumberHash

instance FromTType' BeamVRC.VehicleRegistrationCertificate VehicleRegistrationCertificate where
  fromTType' BeamVRC.VehicleRegistrationCertificateT {..} = do
    pure $
      Just
        VehicleRegistrationCertificate
          { id = Id id,
            documentImageId = Id documentImageId,
            certificateNumber = EncryptedHashed (Encrypted certificateNumberEncrypted) certificateNumberHash,
            fitnessExpiry = fitnessExpiry,
            permitExpiry = permitExpiry,
            pucExpiry = pucExpiry,
            insuranceValidity = insuranceValidity,
            vehicleClass = vehicleClass,
            vehicleVariant = vehicleVariant,
            failedRules = failedRules,
            vehicleManufacturer = vehicleManufacturer,
            vehicleCapacity = vehicleCapacity,
            vehicleModel = vehicleModel,
            vehicleColor = vehicleColor,
            vehicleEnergyType = vehicleEnergyType,
            verificationStatus = verificationStatus,
            fleetOwnerId = fleetOwnerId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamVRC.VehicleRegistrationCertificate VehicleRegistrationCertificate where
  toTType' VehicleRegistrationCertificate {..} = do
    BeamVRC.VehicleRegistrationCertificateT
      { BeamVRC.id = getId id,
        BeamVRC.documentImageId = getId documentImageId,
        BeamVRC.certificateNumberEncrypted = certificateNumber & unEncrypted . (.encrypted),
        BeamVRC.certificateNumberHash = certificateNumber & (.hash),
        BeamVRC.fitnessExpiry = fitnessExpiry,
        BeamVRC.permitExpiry = permitExpiry,
        BeamVRC.pucExpiry = pucExpiry,
        BeamVRC.insuranceValidity = insuranceValidity,
        BeamVRC.vehicleClass = vehicleClass,
        BeamVRC.vehicleVariant = vehicleVariant,
        BeamVRC.failedRules = failedRules,
        BeamVRC.vehicleManufacturer = vehicleManufacturer,
        BeamVRC.vehicleCapacity = vehicleCapacity,
        BeamVRC.vehicleModel = vehicleModel,
        BeamVRC.vehicleColor = vehicleColor,
        BeamVRC.vehicleEnergyType = vehicleEnergyType,
        BeamVRC.verificationStatus = verificationStatus,
        BeamVRC.fleetOwnerId = fleetOwnerId,
        BeamVRC.createdAt = createdAt,
        BeamVRC.updatedAt = updatedAt
      }
