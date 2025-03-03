{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MeterRideQuote where

import qualified Domain.Types.MeterRideQuote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MeterRideQuote as Beam
import Storage.Queries.Transformers.MeterRideQuote

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MeterRideQuote.MeterRideQuote -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MeterRideQuote.MeterRideQuote] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MeterRideQuote.MeterRideQuote -> m (Maybe Domain.Types.MeterRideQuote.MeterRideQuote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MeterRideQuote.MeterRideQuote -> m (Maybe Domain.Types.MeterRideQuote.MeterRideQuote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MeterRideQuote.MeterRideQuote -> m ())
updateByPrimaryKey (Domain.Types.MeterRideQuote.MeterRideQuote {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt), Se.Set Beam.quoteId quoteId, Se.Set Beam.updatedAt (Just _now)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MeterRideQuote Domain.Types.MeterRideQuote.MeterRideQuote where
  fromTType' (Beam.MeterRideQuoteT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $ Just Domain.Types.MeterRideQuote.MeterRideQuote {createdAt = createdAt', id = Kernel.Types.Id.Id id, quoteId = quoteId, updatedAt = updatedAt'}

instance ToTType' Beam.MeterRideQuote Domain.Types.MeterRideQuote.MeterRideQuote where
  toTType' (Domain.Types.MeterRideQuote.MeterRideQuote {..}) = do
    Beam.MeterRideQuoteT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quoteId = quoteId,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
