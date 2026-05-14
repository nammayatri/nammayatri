{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MerchantDocuments.Storage.Queries.MerchantDocument where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified MerchantDocuments.Domain.Types.Common
import qualified MerchantDocuments.Domain.Types.MerchantDocument
import qualified MerchantDocuments.Storage.Beam.MerchantDocument as Beam
import qualified MerchantDocuments.Storage.BeamFlow
import qualified Sequelize as Se

create :: (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) => (MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument -> m ())
create = createWithKV

createMany :: (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) => ([MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument] -> m ())
createMany = traverse_ create

deleteById :: (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantId ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant -> m [MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument])
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findAllByMerchantIdAndRole ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant -> MerchantDocuments.Domain.Types.MerchantDocument.Role -> m [MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument])
findAllByMerchantIdAndRole merchantId role = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.role $ Se.Eq role]]

findAllByMerchantIdCityAndRole ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.MerchantOperatingCity) -> MerchantDocuments.Domain.Types.MerchantDocument.Role -> m [MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument])
findAllByMerchantIdCityAndRole merchantId merchantOperatingCityId role = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

findById ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument -> m (Maybe MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantIdCityRoleType ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.MerchantOperatingCity) -> Kernel.Prelude.Text -> MerchantDocuments.Domain.Types.MerchantDocument.Role -> m [MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument])
findByMerchantIdCityRoleType merchantId merchantOperatingCityId documentType role = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

findByMerchantIdRoleType ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant -> Kernel.Prelude.Text -> MerchantDocuments.Domain.Types.MerchantDocument.Role -> m [MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument])
findByMerchantIdRoleType merchantId documentType role = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

findByMerchantIdRoleTypeCityLang ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.MerchantOperatingCity) -> Kernel.Prelude.Text -> MerchantDocuments.Domain.Types.MerchantDocument.Role -> Kernel.External.Types.Language -> m (Maybe MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument))
findByMerchantIdRoleTypeCityLang merchantId merchantOperatingCityId documentType role language = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.role $ Se.Eq role,
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

updateUrlAndTitle ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Id.Id MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument -> m ())
updateUrlAndTitle url title id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.url url, Se.Set Beam.title title, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument -> m (Maybe MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (MerchantDocuments.Storage.BeamFlow.BeamFlow m r) => (MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument -> m ())
updateByPrimaryKey (MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentType documentType,
      Se.Set Beam.language language,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.platformType platformType,
      Se.Set Beam.role role,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.url url
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantDocument MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument where
  fromTType' (Beam.MerchantDocumentT {..}) = do
    pure $
      Just
        MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument
          { createdAt = createdAt,
            documentType = documentType,
            id = Kernel.Types.Id.Id id,
            language = language,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            platformType = platformType,
            role = role,
            title = title,
            updatedAt = updatedAt,
            url = url
          }

instance ToTType' Beam.MerchantDocument MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument where
  toTType' (MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument {..}) = do
    Beam.MerchantDocumentT
      { Beam.createdAt = createdAt,
        Beam.documentType = documentType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.platformType = platformType,
        Beam.role = role,
        Beam.title = title,
        Beam.updatedAt = updatedAt,
        Beam.url = url
      }
