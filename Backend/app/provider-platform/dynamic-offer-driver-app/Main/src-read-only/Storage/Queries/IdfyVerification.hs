{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IdfyVerification where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IdfyVerification as Beam

create :: KvDbFlow m r => (Domain.Types.IdfyVerification.IdfyVerification -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.IdfyVerification.IdfyVerification] -> m ())
createMany = traverse_ create

deleteByPersonId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findAllByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.IdfyVerification.IdfyVerification])
findAllByDriverId (Kernel.Types.Id.Id driverId) = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findAllByDriverIdAndDocType ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.IdfyVerification.IdfyVerification])
findAllByDriverIdAndDocType (Kernel.Types.Id.Id driverId) docType = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.docType $ Se.Eq docType]]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.IdfyVerification.IdfyVerification -> m (Maybe Domain.Types.IdfyVerification.IdfyVerification))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByRequestId :: KvDbFlow m r => (Kernel.Prelude.Text -> m (Maybe Domain.Types.IdfyVerification.IdfyVerification))
findByRequestId requestId = do findOneWithKV [Se.Is Beam.requestId $ Se.Eq requestId]

findLatestByDriverIdAndDocType ::
  KvDbFlow m r =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.IdfyVerification.IdfyVerification])
findLatestByDriverIdAndDocType limit offset (Kernel.Types.Id.Id driverId) docType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.docType $ Se.Eq docType
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateExtractValidationStatus :: KvDbFlow m r => (Domain.Types.IdfyVerification.ImageExtractionValidation -> Kernel.Prelude.Text -> m ())
updateExtractValidationStatus imageExtractionValidation requestId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.imageExtractionValidation imageExtractionValidation, Se.Set Beam.updatedAt _now] [Se.Is Beam.requestId $ Se.Eq requestId]

updateResponse :: KvDbFlow m r => (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateResponse status idfyResponse requestId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.idfyResponse idfyResponse, Se.Set Beam.updatedAt _now] [Se.Is Beam.requestId $ Se.Eq requestId]

updateStatus :: KvDbFlow m r => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateStatus status requestId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.requestId $ Se.Eq requestId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.IdfyVerification.IdfyVerification -> m (Maybe Domain.Types.IdfyVerification.IdfyVerification))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.IdfyVerification.IdfyVerification -> m ())
updateByPrimaryKey (Domain.Types.IdfyVerification.IdfyVerification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.airConditioned airConditioned,
      Se.Set Beam.docType docType,
      Se.Set Beam.documentImageId1 (Kernel.Types.Id.getId documentImageId1),
      Se.Set Beam.documentImageId2 (Kernel.Types.Id.getId <$> documentImageId2),
      Se.Set Beam.documentNumberEncrypted (documentNumber & unEncrypted . encrypted),
      Se.Set Beam.documentNumberHash (documentNumber & hash),
      Se.Set Beam.driverDateOfBirth driverDateOfBirth,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.idfyResponse idfyResponse,
      Se.Set Beam.imageExtractionValidation imageExtractionValidation,
      Se.Set Beam.issueDateOnDoc issueDateOnDoc,
      Se.Set Beam.multipleRC multipleRC,
      Se.Set Beam.nameOnCard nameOnCard,
      Se.Set Beam.requestId requestId,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.status status,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IdfyVerification Domain.Types.IdfyVerification.IdfyVerification where
  fromTType' (Beam.IdfyVerificationT {..}) = do
    pure $
      Just
        Domain.Types.IdfyVerification.IdfyVerification
          { airConditioned = airConditioned,
            docType = docType,
            documentImageId1 = Kernel.Types.Id.Id documentImageId1,
            documentImageId2 = Kernel.Types.Id.Id <$> documentImageId2,
            documentNumber = EncryptedHashed (Encrypted documentNumberEncrypted) documentNumberHash,
            driverDateOfBirth = driverDateOfBirth,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            idfyResponse = idfyResponse,
            imageExtractionValidation = imageExtractionValidation,
            issueDateOnDoc = issueDateOnDoc,
            multipleRC = multipleRC,
            nameOnCard = nameOnCard,
            requestId = requestId,
            retryCount = retryCount,
            status = status,
            vehicleCategory = vehicleCategory,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IdfyVerification Domain.Types.IdfyVerification.IdfyVerification where
  toTType' (Domain.Types.IdfyVerification.IdfyVerification {..}) = do
    Beam.IdfyVerificationT
      { Beam.airConditioned = airConditioned,
        Beam.docType = docType,
        Beam.documentImageId1 = Kernel.Types.Id.getId documentImageId1,
        Beam.documentImageId2 = Kernel.Types.Id.getId <$> documentImageId2,
        Beam.documentNumberEncrypted = documentNumber & unEncrypted . encrypted,
        Beam.documentNumberHash = documentNumber & hash,
        Beam.driverDateOfBirth = driverDateOfBirth,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idfyResponse = idfyResponse,
        Beam.imageExtractionValidation = imageExtractionValidation,
        Beam.issueDateOnDoc = issueDateOnDoc,
        Beam.multipleRC = multipleRC,
        Beam.nameOnCard = nameOnCard,
        Beam.requestId = requestId,
        Beam.retryCount = retryCount,
        Beam.status = status,
        Beam.vehicleCategory = vehicleCategory,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
