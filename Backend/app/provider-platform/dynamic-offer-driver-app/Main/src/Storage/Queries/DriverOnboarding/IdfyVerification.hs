{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.IdfyVerification where

import Domain.Types.DriverOnboarding.IdfyVerification
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person (Person)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.IdfyVerification as BeamIV
import Storage.Tabular.DriverOnboarding.IdfyVerification

create :: IdfyVerification -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id IdfyVerification ->
  m (Maybe IdfyVerification)
findById = Esq.findById

findAllByDriverId ::
  Transactionable m =>
  Id Person ->
  m [IdfyVerification]
findAllByDriverId driverId = do
  findAll $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $ verifications ^. IdfyVerificationDriverId ==. val (toKey driverId)
    return verifications

findLatestByDriverIdAndDocType ::
  Transactionable m =>
  Id Person ->
  ImageType ->
  m (Maybe IdfyVerification)
findLatestByDriverIdAndDocType driverId imgType = do
  verifications_ <- findAll $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $
      verifications ^. IdfyVerificationDriverId ==. val (toKey driverId)
        &&. verifications ^. IdfyVerificationDocType ==. val imgType
    orderBy [desc $ verifications ^. IdfyVerificationCreatedAt]
    return verifications
  pure $ headMaybe verifications_
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

findByRequestId ::
  Transactionable m =>
  Text ->
  m (Maybe IdfyVerification)
findByRequestId requestId = do
  findOne $ do
    verification <- from $ table @IdfyVerificationT
    where_ $ verification ^. IdfyVerificationRequestId ==. val requestId
    return verification

updateResponse ::
  Text ->
  Text ->
  Text ->
  SqlDB ()
updateResponse requestId status resp = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IdfyVerificationStatus =. val status,
        IdfyVerificationIdfyResponse =. val (Just resp),
        IdfyVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. IdfyVerificationRequestId ==. val requestId

updateExtractValidationStatus :: Text -> ImageExtractionValidation -> SqlDB ()
updateExtractValidationStatus requestId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IdfyVerificationImageExtractionValidation =. val status,
        IdfyVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. IdfyVerificationRequestId ==. val requestId

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $ verifications ^. IdfyVerificationDriverId ==. val (toKey personId)

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
      BeamIV.documentNumberEncrypted = documentNumber & unEncrypted . (.encrypted),
      BeamIV.documentNumberHash = documentNumber & (.hash),
      BeamIV.imageExtractionValidation = imageExtractionValidation,
      BeamIV.idfyResponse = idfyResponse,
      BeamIV.createdAt = createdAt,
      BeamIV.updatedAt = updatedAt
    }
