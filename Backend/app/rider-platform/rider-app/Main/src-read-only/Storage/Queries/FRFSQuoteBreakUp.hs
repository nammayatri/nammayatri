{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuoteBreakUp where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteBreakUp
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuoteBreakUp as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp] -> m ())
createMany = traverse_ create

deleteByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ())
deleteByQuoteId quoteId = do deleteWithKV [Se.And [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]]

findAllByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ([Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp]))
findAllByQuoteId quoteId = do findAllWithKVAndConditionalDB [Se.And [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]] Nothing

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp -> m (Maybe Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp -> m (Maybe Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp -> m ())
updateByPrimaryKey (Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount (((.amount) amount)),
      Se.Set Beam.currency ((Just $ (.currency) amount)),
      Se.Set Beam.description description,
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSQuoteBreakUp Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp where
  fromTType' (Beam.FRFSQuoteBreakUpT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp
          { amount = Kernel.Types.Common.mkPrice currency amount,
            description = description,
            id = Kernel.Types.Id.Id id,
            quoteId = Kernel.Types.Id.Id quoteId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuoteBreakUp Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp where
  toTType' (Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp {..}) = do
    Beam.FRFSQuoteBreakUpT
      { Beam.amount = ((.amount) amount),
        Beam.currency = (Just $ (.currency) amount),
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
