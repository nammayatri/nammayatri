{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Image (module Storage.Queries.Image, module ReExport) where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
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
import qualified Storage.Beam.Image as Beam
import Storage.Queries.ImageExtra as ReExport
import qualified Tools.Error

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Image.Image -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Image.Image] -> m ())
createMany = traverse_ create

addFailureReason :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
addFailureReason failureReason id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.failureReason failureReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findAllByRcId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.Image.Image])
findAllByRcId rcId = do findAllWithKV [Se.Is Beam.rcId $ Se.Eq rcId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.Image.Image))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m [Domain.Types.Image.Image])
findByMerchantId merchantId = do findAllWithDb [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findByWrokflowTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.Image.Image])
findByWrokflowTransactionId workflowTransactionId = do findAllWithKV [Se.Is Beam.workflowTransactionId $ Se.Eq workflowTransactionId]

findImagesByPersonAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.Image.Image])
findImagesByPersonAndType limit offset merchantId personId imageType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.imageType $ Se.Eq imageType
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateDocumentExpiry :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateDocumentExpiry documentExpiry id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.documentExpiry documentExpiry, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVerificationStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Documents.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateVerificationStatus verificationStatus reviewerEmail workflowTransactionId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.reviewerEmail reviewerEmail, Se.Set Beam.updatedAt _now] [Se.Is Beam.workflowTransactionId $ Se.Eq workflowTransactionId]

updateVerificationStatusAndExpiry ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Documents.VerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusAndExpiry verificationStatus documentExpiry imageType id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.documentExpiry documentExpiry,
      Se.Set Beam.imageType imageType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.Image.Image))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Image.Image -> m ())
updateByPrimaryKey (Domain.Types.Image.Image {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentExpiry documentExpiry,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.imageType imageType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.rcId rcId,
      Se.Set Beam.reviewerEmail reviewerEmail,
      Se.Set Beam.s3Path s3Path,
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.workflowTransactionId workflowTransactionId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
