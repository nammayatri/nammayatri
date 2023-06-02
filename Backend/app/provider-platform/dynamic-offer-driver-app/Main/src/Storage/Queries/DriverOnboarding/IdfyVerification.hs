{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.IdfyVerification where

import Domain.Types.DriverOnboarding.IdfyVerification as DDIV
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person (Person)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.IdfyVerification as BeamIV

create :: L.MonadFlow m => IdfyVerification -> m ()
create idfyVerification = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainIdfyVerificationToBeam idfyVerification)
    Nothing -> pure ()

findById :: L.MonadFlow m => Id IdfyVerification -> m (Maybe IdfyVerification)
findById (Id idfvId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamIdfyVerificationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamIV.id $ Se.Eq idfvId]
    Nothing -> pure Nothing

findAllByDriverId :: L.MonadFlow m => Id Person -> m [IdfyVerification]
findAllByDriverId (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamIdfyVerificationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamIV.driverId $ Se.Eq driverId]
    Nothing -> pure []

findLatestByDriverIdAndDocType :: L.MonadFlow m => Id Person -> ImageType -> m (Maybe IdfyVerification)
findLatestByDriverIdAndDocType (Id driverId) imgType = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      idvData <- KV.findAllWithOptionsKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamIV.driverId $ Se.Eq driverId, Se.Is BeamIV.docType $ Se.Eq imgType]] (Se.Desc BeamIV.createdAt) Nothing Nothing
      case idvData of
        Left _ -> pure Nothing
        Right x -> pure $ transformBeamIdfyVerificationToDomain <$> headMaybe x
    Nothing -> pure Nothing
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

findByRequestId :: L.MonadFlow m => Text -> m (Maybe IdfyVerification)
findByRequestId requestId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamIdfyVerificationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamIV.requestId $ Se.Eq requestId]
    Nothing -> pure Nothing

updateResponse :: (L.MonadFlow m, MonadTime m) => Text -> Text -> Text -> m (MeshResult ())
updateResponse requestId status resp = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamIV.status status,
          Se.Set BeamIV.idfyResponse $ Just resp,
          Se.Set BeamIV.updatedAt now
        ]
        [Se.Is BeamIV.requestId (Se.Eq requestId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateExtractValidationStatus :: (L.MonadFlow m, MonadTime m) => Text -> ImageExtractionValidation -> m (MeshResult ())
updateExtractValidationStatus requestId status = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamIV.imageExtractionValidation status,
          Se.Set BeamIV.updatedAt now
        ]
        [Se.Is BeamIV.requestId (Se.Eq requestId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

deleteByPersonId :: L.MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamIV.driverId (Se.Eq personId)]
    Nothing -> pure ()

transformBeamIdfyVerificationToDomain :: BeamIV.IdfyVerification -> IdfyVerification
transformBeamIdfyVerificationToDomain BeamIV.IdfyVerificationT {..} = do
  IdfyVerification
    { id = Id id,
      documentImageId1 = Id documentImageId1,
      documentImageId2 = Id <$> documentImageId2,
      driverId = Id driverId,
      requestId = requestId,
      docType = docType,
      status = status,
      issueDateOnDoc = issueDateOnDoc,
      driverDateOfBirth = driverDateOfBirth,
      documentNumber = EncryptedHashed (Encrypted documentNumberEncrypted) documentNumberHash,
      imageExtractionValidation = imageExtractionValidation,
      idfyResponse = idfyResponse,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainIdfyVerificationToBeam :: IdfyVerification -> BeamIV.IdfyVerification
transformDomainIdfyVerificationToBeam IdfyVerification {..} =
  BeamIV.IdfyVerificationT
    { BeamIV.id = getId id,
      BeamIV.documentImageId1 = getId documentImageId1,
      BeamIV.documentImageId2 = getId <$> documentImageId2,
      BeamIV.driverId = getId driverId,
      BeamIV.requestId = requestId,
      BeamIV.docType = docType,
      BeamIV.status = status,
      BeamIV.issueDateOnDoc = issueDateOnDoc,
      BeamIV.driverDateOfBirth = driverDateOfBirth,
      BeamIV.documentNumberEncrypted = documentNumber & unEncrypted . (.encrypted),
      BeamIV.documentNumberHash = documentNumber & (.hash),
      BeamIV.imageExtractionValidation = imageExtractionValidation,
      BeamIV.idfyResponse = idfyResponse,
      BeamIV.createdAt = createdAt,
      BeamIV.updatedAt = updatedAt
    }
