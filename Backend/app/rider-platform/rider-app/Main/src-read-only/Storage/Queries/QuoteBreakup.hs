{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.QuoteBreakup where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.QuoteBreakup
import qualified Storage.Beam.QuoteBreakup as Beam
import qualified Kernel.Types.Id
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.QuoteBreakup.QuoteBreakup -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.QuoteBreakup.QuoteBreakup] -> m ())
createMany = traverse_ create
findAllByQuoteIdT :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.QuoteBreakup.QuoteBreakup]))
findAllByQuoteIdT quoteId = do findAllWithKVAndConditionalDB [Se.Is Beam.quoteId $ Se.Eq quoteId] Nothing



instance FromTType' Beam.QuoteBreakup Domain.Types.QuoteBreakup.QuoteBreakup
    where fromTType' (Beam.QuoteBreakupT {..}) = do pure $ Just Domain.Types.QuoteBreakup.QuoteBreakup{id = Kernel.Types.Id.Id id,
                                                                                                       price = Kernel.Types.Common.mkPrice priceCurrency priceValue,
                                                                                                       quoteId = quoteId,
                                                                                                       title = title,
                                                                                                       merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                       merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                       createdAt = createdAt,
                                                                                                       updatedAt = updatedAt}
instance ToTType' Beam.QuoteBreakup Domain.Types.QuoteBreakup.QuoteBreakup
    where toTType' (Domain.Types.QuoteBreakup.QuoteBreakup {..}) = do Beam.QuoteBreakupT{Beam.id = Kernel.Types.Id.getId id,
                                                                                         Beam.priceCurrency = (Just $ (.currency) price),
                                                                                         Beam.priceValue = ((.amount) price),
                                                                                         Beam.quoteId = quoteId,
                                                                                         Beam.title = title,
                                                                                         Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                         Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                         Beam.createdAt = createdAt,
                                                                                         Beam.updatedAt = updatedAt}



