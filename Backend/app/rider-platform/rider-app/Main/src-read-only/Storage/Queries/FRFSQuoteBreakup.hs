{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuoteBreakup where

import qualified Domain.Types.FRFSQuoteBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuoteBreakup as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup -> m (Maybe Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup -> m ())
updateByPrimaryKey (Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.quoteCategoryId (Kernel.Types.Id.getId quoteCategoryId),
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.tag tag,
      Se.Set Beam.value value,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSQuoteBreakup Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup where
  fromTType' (Beam.FRFSQuoteBreakupT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            quoteCategoryId = Kernel.Types.Id.Id quoteCategoryId,
            quoteId = Kernel.Types.Id.Id quoteId,
            tag = tag,
            value = value,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuoteBreakup Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup where
  toTType' (Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup {..}) = do
    Beam.FRFSQuoteBreakupT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.quoteCategoryId = Kernel.Types.Id.getId quoteCategoryId,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.tag = tag,
        Beam.value = value,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
