{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Client where

import qualified Domain.Types.Client
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Client as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Client.Client -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Client.Client] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Client.Client -> m (Maybe Domain.Types.Client.Client))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Client.Client -> m ())
updateByPrimaryKey (Domain.Types.Client.Client {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Client Domain.Types.Client.Client where
  fromTType' (Beam.ClientT {..}) = do
    pure $
      Just
        Domain.Types.Client.Client
          { id = Kernel.Types.Id.Id id,
            shortId = Kernel.Types.Id.ShortId shortId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Client Domain.Types.Client.Client where
  toTType' (Domain.Types.Client.Client {..}) = do
    Beam.ClientT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
