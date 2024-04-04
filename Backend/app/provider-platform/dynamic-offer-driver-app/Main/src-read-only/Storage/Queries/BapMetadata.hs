{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BapMetadata where

import qualified Domain.Types.BapMetadata
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BapMetadata as Beam

create :: KvDbFlow m r => (Domain.Types.BapMetadata.BapMetadata -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.BapMetadata.BapMetadata] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata -> m (Maybe Domain.Types.BapMetadata.BapMetadata))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.BapMetadata.BapMetadata -> m ())
updateByPrimaryKey (Domain.Types.BapMetadata.BapMetadata {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.logoUrl (Kernel.Prelude.showBaseUrl logoUrl),
      Se.Set Beam.name name,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BapMetadata Domain.Types.BapMetadata.BapMetadata where
  fromTType' (Beam.BapMetadataT {..}) = do
    logoUrl' <- Kernel.Prelude.parseBaseUrl logoUrl
    pure $ Just Domain.Types.BapMetadata.BapMetadata {id = Kernel.Types.Id.Id id, logoUrl = logoUrl', name = name, createdAt = createdAt, updatedAt = updatedAt}

instance ToTType' Beam.BapMetadata Domain.Types.BapMetadata.BapMetadata where
  toTType' (Domain.Types.BapMetadata.BapMetadata {..}) = do
    Beam.BapMetadataT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.logoUrl = Kernel.Prelude.showBaseUrl logoUrl,
        Beam.name = name,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
