{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CommonDriverOnboardingDocuments where

import qualified Domain.Types.CommonDriverOnboardingDocuments
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Image
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CommonDriverOnboardingDocuments as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments] -> m ())
createMany = traverse_ create

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m ([Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments]))
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId)]

findByDriverIdAndDocumentType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Domain.Types.DocumentVerificationConfig.DocumentType -> m ([Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments]))
findByDriverIdAndDocumentType driverId documentType = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId), Se.Is Beam.documentType $ Se.Eq documentType]]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments -> m (Maybe Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByImageId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Image.Image) -> m (Maybe Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments))
findByImageId documentImageId = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId <$> documentImageId)]

updateRejectReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments -> m ())
updateRejectReason rejectReason verificationStatus id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.rejectReason rejectReason, Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments -> m ())
updateVerificationStatus verificationStatus id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments -> m (Maybe Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments -> m ())
updateByPrimaryKey (Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentData documentData,
      Se.Set Beam.documentImageId (Kernel.Types.Id.getId <$> documentImageId),
      Se.Set Beam.documentType documentType,
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rejectReason rejectReason,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CommonDriverOnboardingDocuments Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments where
  fromTType' (Beam.CommonDriverOnboardingDocumentsT {..}) = do
    pure $
      Just
        Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments
          { documentData = documentData,
            documentImageId = Kernel.Types.Id.Id <$> documentImageId,
            documentType = documentType,
            driverId = Kernel.Types.Id.Id <$> driverId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rejectReason = rejectReason,
            verificationStatus = verificationStatus,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CommonDriverOnboardingDocuments Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments where
  toTType' (Domain.Types.CommonDriverOnboardingDocuments.CommonDriverOnboardingDocuments {..}) = do
    Beam.CommonDriverOnboardingDocumentsT
      { Beam.documentData = documentData,
        Beam.documentImageId = Kernel.Types.Id.getId <$> documentImageId,
        Beam.documentType = documentType,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rejectReason = rejectReason,
        Beam.verificationStatus = verificationStatus,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
