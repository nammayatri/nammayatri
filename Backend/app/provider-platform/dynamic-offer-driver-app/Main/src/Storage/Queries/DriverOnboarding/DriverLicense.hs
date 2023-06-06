{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.DriverLicense where

import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.Person (Person)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverLicense as BeamDL
import Storage.Tabular.Person ()

create :: L.MonadFlow m => DriverLicense -> m (MeshResult ())
create driverLicense = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverLicenseToBeam driverLicense)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

upsert :: L.MonadFlow m => DriverLicense -> m ()
upsert a@DriverLicense {..} = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      res <- either (pure Nothing) (transformBeamDriverLicenseToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDL.id $ Se.Eq (getId a.id)]
      if isJust res
        then
          void $
            KV.updateWoReturningWithKVConnector
              dbCOnf'
              Mesh.meshConfig
              [ Se.Set BeamDL.driverDob driverDob,
                Se.Set BeamDL.driverName driverName,
                Se.Set BeamDL.licenseExpiry licenseExpiry,
                Se.Set BeamDL.classOfVehicles classOfVehicles,
                Se.Set BeamDL.verificationStatus verificationStatus,
                Se.Set BeamDL.failedRules failedRules,
                Se.Set BeamDL.updatedAt updatedAt
              ]
              [Se.Is BeamDL.id (Se.Eq $ getId a.id)]
        else void $ KV.createWoReturingKVConnector dbCOnf' Mesh.meshConfig (transformDomainDriverLicenseToBeam a)
    Nothing -> pure ()

findById :: L.MonadFlow m => Id DriverLicense -> m (Maybe DriverLicense)
findById (Id dlId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverLicenseToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDL.id $ Se.Eq dlId]
    Nothing -> pure Nothing

findByDriverId :: L.MonadFlow m => Id Person -> m (Maybe DriverLicense)
findByDriverId (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverLicenseToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDL.driverId $ Se.Eq personId]
    Nothing -> pure Nothing

findByDLNumber :: (L.MonadFlow m, EncFlow m r) => Text -> m (Maybe DriverLicense)
findByDLNumber dlNumber = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  dlNumberHash <- getDbHash dlNumber
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverLicenseToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDL.licenseNumberHash $ Se.Eq dlNumberHash]
    Nothing -> pure Nothing

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamDL.driverId (Se.Eq driverId)]
    Nothing -> pure ()

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
