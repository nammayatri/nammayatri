{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.HyperVergeVerification where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.HyperVergeVerification
import qualified Domain.Types.IdfyVerification
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.HyperVergeVerification as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.HyperVergeVerification.HyperVergeVerification -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.HyperVergeVerification.HyperVergeVerification] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.HyperVergeVerification.HyperVergeVerification])
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findAllByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.HyperVergeVerification.HyperVergeVerification])
findAllByDriverIdAndDocType driverId docType = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.docType $ Se.Eq docType]]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.HyperVergeVerification.HyperVergeVerification -> m (Maybe Domain.Types.HyperVergeVerification.HyperVergeVerification))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.HyperVergeVerification.HyperVergeVerification))
findByRequestId requestId = do findOneWithKV [Se.Is Beam.requestId $ Se.Eq requestId]

findLatestByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.HyperVergeVerification.HyperVergeVerification])
findLatestByDriverIdAndDocType limit offset driverId docType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.docType $ Se.Eq docType
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateExtractValidationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IdfyVerification.ImageExtractionValidation -> Kernel.Prelude.Text -> m ())
updateExtractValidationStatus imageExtractionValidation requestId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.imageExtractionValidation imageExtractionValidation, Se.Set Beam.updatedAt _now] [Se.Is Beam.requestId $ Se.Eq requestId]

updateResponse :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateResponse status hypervergeResponse requestId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.hypervergeResponse hypervergeResponse, Se.Set Beam.updatedAt _now] [Se.Is Beam.requestId $ Se.Eq requestId]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateStatus status requestId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.requestId $ Se.Eq requestId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.HyperVergeVerification.HyperVergeVerification -> m (Maybe Domain.Types.HyperVergeVerification.HyperVergeVerification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.HyperVergeVerification.HyperVergeVerification -> m ())
updateByPrimaryKey (Domain.Types.HyperVergeVerification.HyperVergeVerification {..}) = do
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
      Se.Set Beam.hypervergeResponse hypervergeResponse,
      Se.Set Beam.imageExtractionValidation imageExtractionValidation,
      Se.Set Beam.issueDateOnDoc issueDateOnDoc,
      Se.Set Beam.multipleRC multipleRC,
      Se.Set Beam.nameOnCard nameOnCard,
      Se.Set Beam.oxygen oxygen,
      Se.Set Beam.requestId requestId,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.status status,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.ventilator ventilator,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.HyperVergeVerification Domain.Types.HyperVergeVerification.HyperVergeVerification where
  fromTType' (Beam.HyperVergeVerificationT {..}) = do
    pure $
      Just
        Domain.Types.HyperVergeVerification.HyperVergeVerification
          { airConditioned = airConditioned,
            docType = docType,
            documentImageId1 = Kernel.Types.Id.Id documentImageId1,
            documentImageId2 = Kernel.Types.Id.Id <$> documentImageId2,
            documentNumber = EncryptedHashed (Encrypted documentNumberEncrypted) documentNumberHash,
            driverDateOfBirth = driverDateOfBirth,
            driverId = Kernel.Types.Id.Id driverId,
            hypervergeResponse = hypervergeResponse,
            id = Kernel.Types.Id.Id id,
            imageExtractionValidation = imageExtractionValidation,
            issueDateOnDoc = issueDateOnDoc,
            multipleRC = multipleRC,
            nameOnCard = nameOnCard,
            oxygen = oxygen,
            requestId = requestId,
            retryCount = retryCount,
            status = status,
            transactionId = transactionId,
            vehicleCategory = vehicleCategory,
            ventilator = ventilator,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.HyperVergeVerification Domain.Types.HyperVergeVerification.HyperVergeVerification where
  toTType' (Domain.Types.HyperVergeVerification.HyperVergeVerification {..}) = do
    Beam.HyperVergeVerificationT
      { Beam.airConditioned = airConditioned,
        Beam.docType = docType,
        Beam.documentImageId1 = Kernel.Types.Id.getId documentImageId1,
        Beam.documentImageId2 = Kernel.Types.Id.getId <$> documentImageId2,
        Beam.documentNumberEncrypted = documentNumber & unEncrypted . encrypted,
        Beam.documentNumberHash = documentNumber & hash,
        Beam.driverDateOfBirth = driverDateOfBirth,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.hypervergeResponse = hypervergeResponse,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.imageExtractionValidation = imageExtractionValidation,
        Beam.issueDateOnDoc = issueDateOnDoc,
        Beam.multipleRC = multipleRC,
        Beam.nameOnCard = nameOnCard,
        Beam.oxygen = oxygen,
        Beam.requestId = requestId,
        Beam.retryCount = retryCount,
        Beam.status = status,
        Beam.transactionId = transactionId,
        Beam.vehicleCategory = vehicleCategory,
        Beam.ventilator = ventilator,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
