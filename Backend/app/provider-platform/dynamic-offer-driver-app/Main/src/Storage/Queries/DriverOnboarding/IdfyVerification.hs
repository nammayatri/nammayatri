{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.IdfyVerification where

import Domain.Types.DriverOnboarding.IdfyVerification as DDIV
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.IdfyVerification as BeamIV

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => IdfyVerification -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IdfyVerification -> m (Maybe IdfyVerification)
findById (Id idfvId) = findOneWithKV [Se.Is BeamIV.id $ Se.Eq idfvId]

findAllByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [IdfyVerification]
findAllByDriverId (Id driverId) = findAllWithKV [Se.Is BeamIV.driverId $ Se.Eq driverId]

findAllByDriverIdAndDocType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> ImageType -> m [IdfyVerification]
findAllByDriverIdAndDocType (Id driverId) imgType = findAllWithKV [Se.Is BeamIV.driverId $ Se.Eq driverId, Se.Is BeamIV.docType $ Se.Eq imgType]

findLatestByDriverIdAndDocType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> ImageType -> m (Maybe IdfyVerification)
findLatestByDriverIdAndDocType (Id driverId) imgType = findAllWithOptionsKV [Se.And [Se.Is BeamIV.driverId $ Se.Eq driverId, Se.Is BeamIV.docType $ Se.Eq imgType]] (Se.Desc BeamIV.createdAt) Nothing Nothing <&> listToMaybe

findByRequestId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe IdfyVerification)
findByRequestId requestId = findOneWithKV [Se.Is BeamIV.requestId $ Se.Eq requestId]

updateResponse :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> Text -> m ()
updateResponse requestId status resp = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamIV.status status, Se.Set BeamIV.idfyResponse $ Just resp, Se.Set BeamIV.updatedAt now]
    [Se.Is BeamIV.requestId (Se.Eq requestId)]

updateStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> m ()
updateStatus requestId status = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamIV.status status, Se.Set BeamIV.updatedAt now]
    [Se.Is BeamIV.requestId (Se.Eq requestId)]

updateExtractValidationStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> ImageExtractionValidation -> m ()
updateExtractValidationStatus requestId status = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamIV.imageExtractionValidation status, Se.Set BeamIV.updatedAt now]
    [Se.Is BeamIV.requestId (Se.Eq requestId)]

deleteByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamIV.driverId (Se.Eq personId)]

instance FromTType' BeamIV.IdfyVerification IdfyVerification where
  fromTType' BeamIV.IdfyVerificationT {..} = do
    pure $
      Just
        IdfyVerification
          { id = Id id,
            documentImageId1 = Id documentImageId1,
            documentImageId2 = Id <$> documentImageId2,
            dashboardPassedVehicleVariant = dashboardPassedVehicleVariant,
            retryCount = fromMaybe 0 retryCount,
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
            updatedAt = updatedAt,
            multipleRC = multipleRC
          }

instance ToTType' BeamIV.IdfyVerification IdfyVerification where
  toTType' IdfyVerification {..} = do
    BeamIV.IdfyVerificationT
      { BeamIV.id = getId id,
        BeamIV.documentImageId1 = getId documentImageId1,
        BeamIV.documentImageId2 = getId <$> documentImageId2,
        BeamIV.dashboardPassedVehicleVariant = dashboardPassedVehicleVariant,
        BeamIV.retryCount = Just retryCount,
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
        BeamIV.updatedAt = updatedAt,
        BeamIV.multipleRC = multipleRC
      }
