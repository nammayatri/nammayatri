{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OnSearchEvent where

import qualified Domain.Types.OnSearchEvent
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OnSearchEvent as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OnSearchEvent.OnSearchEvent -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OnSearchEvent.OnSearchEvent] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OnSearchEvent.OnSearchEvent -> m (Maybe Domain.Types.OnSearchEvent.OnSearchEvent))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OnSearchEvent.OnSearchEvent -> m ())
updateByPrimaryKey (Domain.Types.OnSearchEvent.OnSearchEvent {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppId bppId,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.errorType errorType,
      Se.Set Beam.messageId messageId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.OnSearchEvent Domain.Types.OnSearchEvent.OnSearchEvent where
  fromTType' (Beam.OnSearchEventT {..}) = do
    pure $
      Just
        Domain.Types.OnSearchEvent.OnSearchEvent
          { bppId = bppId,
            createdAt = createdAt,
            errorCode = errorCode,
            errorMessage = errorMessage,
            errorType = errorType,
            id = Kernel.Types.Id.Id id,
            messageId = messageId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OnSearchEvent Domain.Types.OnSearchEvent.OnSearchEvent where
  toTType' (Domain.Types.OnSearchEvent.OnSearchEvent {..}) = do
    Beam.OnSearchEventT
      { Beam.bppId = bppId,
        Beam.createdAt = createdAt,
        Beam.errorCode = errorCode,
        Beam.errorMessage = errorMessage,
        Beam.errorType = errorType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.messageId = messageId,
        Beam.updatedAt = updatedAt
      }
