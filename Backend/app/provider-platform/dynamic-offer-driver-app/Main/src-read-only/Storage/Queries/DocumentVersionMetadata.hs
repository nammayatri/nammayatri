{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentVersionMetadata where

import qualified Data.Aeson
import qualified Domain.Types.DocumentVersionMetadata
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.DocumentVersionMetadata as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata))
findByPrimaryKey documentVersionId = do findOneWithKV [Se.And [Se.Is Beam.documentVersionId $ Se.Eq documentVersionId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata -> m ())
updateByPrimaryKey (Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentNumberEncrypted (((documentNumber & unEncrypted . encrypted))),
      Se.Set Beam.documentNumberHash ((documentNumber & hash)),
      Se.Set Beam.documentType documentType,
      Se.Set Beam.metadata (Data.Aeson.toJSON <$> metadata),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.documentVersionId $ Se.Eq documentVersionId]]

instance FromTType' Beam.DocumentVersionMetadata Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata where
  fromTType' (Beam.DocumentVersionMetadataT {..}) = do
    pure $
      Just
        Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata
          { documentNumber = EncryptedHashed (Encrypted documentNumberEncrypted) documentNumberHash,
            documentType = documentType,
            documentVersionId = documentVersionId,
            metadata = Kernel.Utils.JSON.valueToMaybe =<< metadata,
            personId = Kernel.Types.Id.Id personId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DocumentVersionMetadata Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata where
  toTType' (Domain.Types.DocumentVersionMetadata.DocumentVersionMetadata {..}) = do
    Beam.DocumentVersionMetadataT
      { Beam.documentNumberEncrypted = ((documentNumber & unEncrypted . encrypted)),
        Beam.documentNumberHash = (documentNumber & hash),
        Beam.documentType = documentType,
        Beam.documentVersionId = documentVersionId,
        Beam.metadata = Data.Aeson.toJSON <$> metadata,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
