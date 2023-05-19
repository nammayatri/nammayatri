{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.DriverLicense where

import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.Person (Person)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverLicense as BeamDL
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.Person ()

create :: DriverLicense -> SqlDB ()
create = Esq.create

upsert :: DriverLicense -> SqlDB ()
upsert a@DriverLicense {..} =
  Esq.upsert
    a
    [ DriverLicenseDriverDob =. val driverDob,
      DriverLicenseDriverName =. val driverName,
      DriverLicenseLicenseExpiry =. val licenseExpiry,
      DriverLicenseClassOfVehicles =. val (PostgresList classOfVehicles),
      DriverLicenseVerificationStatus =. val verificationStatus,
      DriverLicenseFailedRules =. val (PostgresList failedRules),
      DriverLicenseUpdatedAt =. val updatedAt
    ]

findById ::
  Transactionable m =>
  Id DriverLicense ->
  m (Maybe DriverLicense)
findById = Esq.findById

findByDriverId ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLicense)
findByDriverId driverId = do
  findOne $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseDriverId ==. val (toKey driverId)
    return dl

findByDLNumber ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  dlNumberHash <- getDbHash dlNumber
  findOne $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseLicenseNumberHash ==. val dlNumberHash
    return dl

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId driverId =
  Esq.delete $ do
    dl <- from $ table @DriverLicenseT
    where_ $ dl ^. DriverLicenseDriverId ==. val (toKey driverId)

transformBeamDriverLicenseToDomain :: BeamDL.DriverLicense -> DriverLicense
transformBeamDriverLicenseToDomain BeamDL.DriverLicenseT {..} = do
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

transformDomainDriverLicenseToBeam :: DriverLicense -> BeamDL.DriverLicense
transformDomainDriverLicenseToBeam DriverLicense {..} =
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
