{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MediaFileDocument (module Storage.Queries.MediaFileDocument, module ReExport) where

import qualified Domain.Types.Common
import qualified Domain.Types.MediaFileDocument
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFileDocument as Beam
import Storage.Queries.MediaFileDocumentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MediaFileDocument.MediaFileDocument -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MediaFileDocument.MediaFileDocument] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantOpCityIdAndRcIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Domain.Types.Common.MediaFileDocumentType -> m [Domain.Types.MediaFileDocument.MediaFileDocument])
findAllByMerchantOpCityIdAndRcIdAndType merchantOperatingCityId rcId mediaFileDocumentType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId),
          Se.Is Beam.mediaFileDocumentType $ Se.Eq mediaFileDocumentType
        ]
    ]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.MediaFileDocument.MediaFileDocumentStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument -> m ())
updateStatus status fileHash id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.fileHash fileHash, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument -> m (Maybe Domain.Types.MediaFileDocument.MediaFileDocument))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MediaFileDocument.MediaFileDocument -> m ())
updateByPrimaryKey (Domain.Types.MediaFileDocument.MediaFileDocument {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.creatorId (Kernel.Types.Id.getId creatorId),
      Se.Set Beam.fileHash fileHash,
      Se.Set Beam.mediaFileDocumentType mediaFileDocumentType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.s3Path s3Path,
      Se.Set Beam.status status,
      Se.Set Beam.uploadLink uploadLink,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
