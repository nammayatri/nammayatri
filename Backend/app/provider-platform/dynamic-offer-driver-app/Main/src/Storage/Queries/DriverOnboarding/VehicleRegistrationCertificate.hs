{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate where

import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
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
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person ()

-- create :: VehicleRegistrationCertificate -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => VehicleRegistrationCertificate -> m (MeshResult ())
create vehicleRegistrationCertificate = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainVehicleRegistrationCertificateToBeam vehicleRegistrationCertificate)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- upsert :: VehicleRegistrationCertificate -> SqlDB ()
-- upsert a@VehicleRegistrationCertificate {..} =
--   Esq.upsert
--     a
--     [ VehicleRegistrationCertificatePermitExpiry =. val permitExpiry,
--       VehicleRegistrationCertificatePucExpiry =. val pucExpiry,
--       VehicleRegistrationCertificateInsuranceValidity =. val insuranceValidity,
--       VehicleRegistrationCertificateVehicleClass =. val vehicleClass,
--       VehicleRegistrationCertificateVehicleManufacturer =. val vehicleManufacturer,
--       VehicleRegistrationCertificateVehicleCapacity =. val vehicleCapacity,
--       VehicleRegistrationCertificateVehicleModel =. val vehicleModel,
--       VehicleRegistrationCertificateVehicleColor =. val vehicleColor,
--       VehicleRegistrationCertificateVehicleEnergyType =. val vehicleEnergyType,
--       VehicleRegistrationCertificateVerificationStatus =. val verificationStatus,
--       VehicleRegistrationCertificateFailedRules =. val (PostgresList failedRules),
--       VehicleRegistrationCertificateUpdatedAt =. val updatedAt
--     ]

upsert :: L.MonadFlow m => VehicleRegistrationCertificate -> m ()
upsert a@VehicleRegistrationCertificate {..} = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      res <- either (pure Nothing) (transformBeamVehicleRegistrationCertificateToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamVRC.id $ Se.Eq (getId a.id)]
      if isJust res
        then
          void $
            KV.updateWoReturningWithKVConnector
              dbCOnf'
              Mesh.meshConfig
              [ Se.Set BeamVRC.permitExpiry permitExpiry,
                Se.Set BeamVRC.pucExpiry pucExpiry,
                Se.Set BeamVRC.insuranceValidity insuranceValidity,
                Se.Set BeamVRC.vehicleClass vehicleClass,
                Se.Set BeamVRC.vehicleManufacturer vehicleManufacturer,
                Se.Set BeamVRC.vehicleCapacity vehicleCapacity,
                Se.Set BeamVRC.vehicleModel vehicleModel,
                Se.Set BeamVRC.vehicleColor vehicleColor,
                Se.Set BeamVRC.vehicleEnergyType vehicleEnergyType,
                Se.Set BeamVRC.verificationStatus verificationStatus,
                Se.Set BeamVRC.failedRules failedRules,
                Se.Set BeamVRC.updatedAt updatedAt
              ]
              [Se.Is BeamVRC.id (Se.Eq $ getId a.id)]
        else void $ KV.createWoReturingKVConnector dbCOnf' Mesh.meshConfig (transformDomainVehicleRegistrationCertificateToBeam a)
    Nothing -> pure ()

-- findById ::
--   Transactionable m =>
--   Id VehicleRegistrationCertificate ->
--   m (Maybe VehicleRegistrationCertificate)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id VehicleRegistrationCertificate -> m (Maybe VehicleRegistrationCertificate)
findById (Id vrcID) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamVehicleRegistrationCertificateToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamVRC.id $ Se.Eq vrcID]
    Nothing -> pure Nothing

-- findLastVehicleRC ::
--   (Transactionable m, EncFlow m r) =>
--   Text ->
--   m (Maybe VehicleRegistrationCertificate)
-- findLastVehicleRC certNumber = do
--   certNumberHash <- getDbHash certNumber
--   rcs <- findAll $ do
--     rc <- from $ table @VehicleRegistrationCertificateT
--     where_ $ rc ^. VehicleRegistrationCertificateCertificateNumberHash ==. val certNumberHash
--     orderBy [desc $ rc ^. VehicleRegistrationCertificateFitnessExpiry]
--     return rc
--   pure $ headMaybe rcs
--   where
--     headMaybe [] = Nothing
--     headMaybe (x : _) = Just x

findLastVehicleRC :: (L.MonadFlow m, EncFlow m r) => Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumber = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  certNumberHash <- getDbHash certNumber
  case dbConf of
    Just dbConf' -> do
      vrcData <- KV.findAllWithOptionsKVConnector dbConf' Mesh.meshConfig [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash] (Se.Desc BeamVRC.fitnessExpiry) Nothing Nothing
      case vrcData of
        Left _ -> pure Nothing
        Right x -> pure $ transformBeamVehicleRegistrationCertificateToDomain <$> headMaybe x
    Nothing -> pure Nothing
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

-- findByRCAndExpiry ::
--   Transactionable m =>
--   EncryptedHashedField 'AsEncrypted Text ->
--   UTCTime ->
--   m (Maybe VehicleRegistrationCertificate)
-- findByRCAndExpiry certNumber expiry = do
--   let certNumberHash = certNumber & (.hash)
--   findOne $ do
--     rc <- from $ table @VehicleRegistrationCertificateT
--     where_ $
--       rc ^. VehicleRegistrationCertificateCertificateNumberHash ==. val certNumberHash
--         &&. rc ^. VehicleRegistrationCertificateFitnessExpiry ==. val expiry
--     return rc

findByRCAndExpiry :: L.MonadFlow m => EncryptedHashedField 'AsEncrypted Text -> UTCTime -> m (Maybe VehicleRegistrationCertificate)
findByRCAndExpiry certNumber expiry = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  let certNumberHash = certNumber & (.hash)
  case dbConf of
    Just dbCOnf' ->
      either (pure Nothing) (transformBeamVehicleRegistrationCertificateToDomain <$>)
        <$> KV.findWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fitnessExpiry $ Se.Eq expiry]]
    Nothing -> pure Nothing

transformBeamVehicleRegistrationCertificateToDomain :: BeamVRC.VehicleRegistrationCertificate -> VehicleRegistrationCertificate
transformBeamVehicleRegistrationCertificateToDomain BeamVRC.VehicleRegistrationCertificateT {..} = do
  VehicleRegistrationCertificate
    { id = Id id,
      documentImageId = Id documentImageId,
      certificateNumber = EncryptedHashed (Encrypted certificateNumberEncrypted) certificateNumberHash,
      fitnessExpiry = fitnessExpiry,
      permitExpiry = permitExpiry,
      pucExpiry = pucExpiry,
      insuranceValidity = insuranceValidity,
      vehicleClass = vehicleClass,
      failedRules = failedRules,
      vehicleManufacturer = vehicleManufacturer,
      vehicleCapacity = vehicleCapacity,
      vehicleModel = vehicleModel,
      vehicleColor = vehicleColor,
      vehicleEnergyType = vehicleEnergyType,
      verificationStatus = verificationStatus,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainVehicleRegistrationCertificateToBeam :: VehicleRegistrationCertificate -> BeamVRC.VehicleRegistrationCertificate
transformDomainVehicleRegistrationCertificateToBeam VehicleRegistrationCertificate {..} =
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
      BeamVRC.failedRules = failedRules,
      BeamVRC.vehicleManufacturer = vehicleManufacturer,
      BeamVRC.vehicleCapacity = vehicleCapacity,
      BeamVRC.vehicleModel = vehicleModel,
      BeamVRC.vehicleColor = vehicleColor,
      BeamVRC.vehicleEnergyType = vehicleEnergyType,
      BeamVRC.verificationStatus = verificationStatus,
      BeamVRC.createdAt = createdAt,
      BeamVRC.updatedAt = updatedAt
    }
