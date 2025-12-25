{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BapMetadata where

import qualified Data.Text
import qualified Domain.Types.BapMetadata
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BapMetadata as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BapMetadata.BapMetadata -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BapMetadata.BapMetadata] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findBySubscriberIdAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> Kernel.Prelude.Maybe Data.Text.Text -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findBySubscriberIdAndDomain id domain = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.domain $ Se.Eq domain]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BapMetadata.BapMetadata -> m ())
updateByPrimaryKey (Domain.Types.BapMetadata.BapMetadata {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.domain domain,
      Se.Set Beam.logoUrl (Kernel.Prelude.fmap showBaseUrl logoUrl),
      Se.Set Beam.name name,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BapMetadata Domain.Types.BapMetadata.BapMetadata where
  fromTType' (Beam.BapMetadataT {..}) = do
    logoUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) logoUrl
    pure $ Just Domain.Types.BapMetadata.BapMetadata {domain = domain, id = Kernel.Types.Id.Id id, logoUrl = logoUrl', name = name, createdAt = createdAt, updatedAt = updatedAt}

instance ToTType' Beam.BapMetadata Domain.Types.BapMetadata.BapMetadata where
  toTType' (Domain.Types.BapMetadata.BapMetadata {..}) = do
    Beam.BapMetadataT
      { Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.logoUrl = Kernel.Prelude.fmap showBaseUrl logoUrl,
        Beam.name = name,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
