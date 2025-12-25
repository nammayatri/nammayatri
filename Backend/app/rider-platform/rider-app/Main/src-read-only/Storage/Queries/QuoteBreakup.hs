{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.QuoteBreakup where

import qualified Domain.Types.QuoteBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.QuoteBreakup as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.QuoteBreakup.QuoteBreakup -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.QuoteBreakup.QuoteBreakup] -> m ())
createMany = traverse_ create

findAllByQuoteIdT :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.QuoteBreakup.QuoteBreakup])
findAllByQuoteIdT quoteId = do findAllWithKVAndConditionalDB [Se.Is Beam.quoteId $ Se.Eq quoteId] Nothing

instance FromTType' Beam.QuoteBreakup Domain.Types.QuoteBreakup.QuoteBreakup where
  fromTType' (Beam.QuoteBreakupT {..}) = do
    pure $
      Just
        Domain.Types.QuoteBreakup.QuoteBreakup
          { id = Kernel.Types.Id.Id id,
            price = Kernel.Types.Common.mkPrice priceCurrency priceValue,
            quoteId = quoteId,
            title = title,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.QuoteBreakup Domain.Types.QuoteBreakup.QuoteBreakup where
  toTType' (Domain.Types.QuoteBreakup.QuoteBreakup {..}) = do
    Beam.QuoteBreakupT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.priceCurrency = Just $ (.currency) price,
        Beam.priceValue = (.amount) price,
        Beam.quoteId = quoteId,
        Beam.title = title,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
