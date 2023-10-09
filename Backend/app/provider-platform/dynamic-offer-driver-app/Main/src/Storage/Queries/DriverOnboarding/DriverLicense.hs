{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.DriverLicense where

import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverLicense as BeamDL

create :: MonadFlow m => DriverLicense -> m ()
create = createWithKV

upsert :: MonadFlow m => DriverLicense -> m ()
upsert a@DriverLicense {..} = do
  res <- findOneWithKV [Se.Is BeamDL.licenseNumberHash $ Se.Eq (a.licenseNumber & (.hash))]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamDL.driverDob driverDob,
          Se.Set BeamDL.driverName driverName,
          Se.Set BeamDL.licenseExpiry licenseExpiry,
          Se.Set BeamDL.classOfVehicles classOfVehicles,
          Se.Set BeamDL.verificationStatus verificationStatus,
          Se.Set BeamDL.failedRules failedRules,
          Se.Set BeamDL.updatedAt updatedAt
        ]
        [Se.Is BeamDL.licenseNumberHash $ Se.Eq (a.licenseNumber & (.hash))]
    else createWithKV a

findByDriverId :: MonadFlow m => Id Person -> m (Maybe DriverLicense)
findByDriverId (Id personId) = findOneWithKV [Se.Is BeamDL.driverId $ Se.Eq personId]

findByDLNumber :: (MonadFlow m, EncFlow m r) => Text -> m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  dlNumberHash <- getDbHash dlNumber
  findOneWithKV [Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash]

deleteByDriverId :: MonadFlow m => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamDL.driverId (Se.Eq driverId)]

instance FromTType' BeamDL.DriverLicense DriverLicense where
  fromTType' BeamDL.DriverLicenseT {..} = do
    pure $
      Just
        DriverLicense
          { id = Id id,
            driverId = Id driverId,
            documentImageId1 = Id documentImageId1,
            documentImageId2 = Id <$> documentImageId2,
            driverDob = driverDob,
            driverName = driverName,
            licenseNumber = EncryptedHashed (Encrypted licenseNumberEncrypted) licenseNumberHash,
            licenseExpiry = licenseExpiry,
            classOfVehicles = classOfVehicles,
            failedRules = failedRules,
            verificationStatus = verificationStatus,
            createdAt = createdAt,
            updatedAt = updatedAt,
            consent = consent,
            consentTimestamp = consentTimestamp
          }

instance ToTType' BeamDL.DriverLicense DriverLicense where
  toTType' DriverLicense {..} = do
    BeamDL.DriverLicenseT
      { BeamDL.id = getId id,
        BeamDL.driverId = getId driverId,
        BeamDL.documentImageId1 = getId documentImageId1,
        BeamDL.documentImageId2 = getId <$> documentImageId2,
        BeamDL.driverDob = driverDob,
        BeamDL.driverName = driverName,
        BeamDL.licenseNumberEncrypted = licenseNumber & unEncrypted . (.encrypted),
        BeamDL.licenseNumberHash = licenseNumber & (.hash),
        BeamDL.licenseExpiry = licenseExpiry,
        BeamDL.classOfVehicles = classOfVehicles,
        BeamDL.failedRules = failedRules,
        BeamDL.verificationStatus = verificationStatus,
        BeamDL.createdAt = createdAt,
        BeamDL.updatedAt = updatedAt,
        BeamDL.consent = consent,
        BeamDL.consentTimestamp = consentTimestamp
      }
