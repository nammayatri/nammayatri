{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OneWayScheduledQuote where

import qualified Domain.Types.OneWayScheduledQuote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OneWayScheduledQuote as Beam
import Storage.Queries.Transformers.OneWayScheduledQuote

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote -> m (Maybe Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote -> m (Maybe Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote -> m ())
updateByPrimaryKey (Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt), Se.Set Beam.quoteId quoteId, Se.Set Beam.updatedAt (Just _now)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.OneWayScheduledQuote Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote where
  fromTType' (Beam.OneWayScheduledQuoteT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $ Just Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote {createdAt = createdAt', id = Kernel.Types.Id.Id id, quoteId = quoteId, updatedAt = updatedAt'}

instance ToTType' Beam.OneWayScheduledQuote Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote where
  toTType' (Domain.Types.OneWayScheduledQuote.OneWayScheduledQuote {..}) = do
    Beam.OneWayScheduledQuoteT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quoteId = quoteId,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
