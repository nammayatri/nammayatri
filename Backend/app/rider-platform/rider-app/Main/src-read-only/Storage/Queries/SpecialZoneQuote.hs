{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SpecialZoneQuote where

import qualified Domain.Types.SpecialZoneQuote
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialZoneQuote as Beam
import Storage.Queries.Transformers.SpecialZoneQuote

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SpecialZoneQuote.SpecialZoneQuote -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SpecialZoneQuote.SpecialZoneQuote] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SpecialZoneQuote.SpecialZoneQuote -> m (Maybe Domain.Types.SpecialZoneQuote.SpecialZoneQuote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SpecialZoneQuote.SpecialZoneQuote -> m (Maybe Domain.Types.SpecialZoneQuote.SpecialZoneQuote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SpecialZoneQuote.SpecialZoneQuote -> m ())
updateByPrimaryKey (Domain.Types.SpecialZoneQuote.SpecialZoneQuote {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.quoteId quoteId, Se.Set Beam.updatedAt (Just _now)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SpecialZoneQuote Domain.Types.SpecialZoneQuote.SpecialZoneQuote where
  fromTType' (Beam.SpecialZoneQuoteT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $ Just Domain.Types.SpecialZoneQuote.SpecialZoneQuote {createdAt = createdAt', id = Kernel.Types.Id.Id id, quoteId = quoteId, updatedAt = updatedAt'}

instance ToTType' Beam.SpecialZoneQuote Domain.Types.SpecialZoneQuote.SpecialZoneQuote where
  toTType' (Domain.Types.SpecialZoneQuote.SpecialZoneQuote {..}) = do
    Beam.SpecialZoneQuoteT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quoteId = quoteId,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
