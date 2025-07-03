{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MediaFileDocument where

import qualified Domain.Types.Common
import qualified Domain.Types.MediaFileDocument
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFileDocument as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MediaFileDocument.MediaFileDocument -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MediaFileDocument.MediaFileDocument] -> m ())
createMany = traverse_ create

findOneByMerchantOpCityIdAndRcIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Domain.Types.Common.MediaFileDocumentType -> m (Maybe Domain.Types.MediaFileDocument.MediaFileDocument))
findOneByMerchantOpCityIdAndRcIdAndType merchantOperatingCityId rcId mediaFileDocumentType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId),
          Se.Is Beam.mediaFileDocumentType $ Se.Eq mediaFileDocumentType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument -> m (Maybe Domain.Types.MediaFileDocument.MediaFileDocument))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MediaFileDocument.MediaFileDocument -> m ())
updateByPrimaryKey (Domain.Types.MediaFileDocument.MediaFileDocument {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.creatorId (Kernel.Types.Id.getId creatorId),
      Se.Set Beam.mediaFileDocumentType mediaFileDocumentType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.s3Path s3Path,
      Se.Set Beam.status status,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MediaFileDocument Domain.Types.MediaFileDocument.MediaFileDocument where
  fromTType' (Beam.MediaFileDocumentT {..}) = do
    pure $
      Just
        Domain.Types.MediaFileDocument.MediaFileDocument
          { creatorId = Kernel.Types.Id.Id creatorId,
            id = Kernel.Types.Id.Id id,
            mediaFileDocumentType = mediaFileDocumentType,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rcId = Kernel.Types.Id.Id rcId,
            s3Path = s3Path,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MediaFileDocument Domain.Types.MediaFileDocument.MediaFileDocument where
  toTType' (Domain.Types.MediaFileDocument.MediaFileDocument {..}) = do
    Beam.MediaFileDocumentT
      { Beam.creatorId = Kernel.Types.Id.getId creatorId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mediaFileDocumentType = mediaFileDocumentType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.s3Path = s3Path,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
