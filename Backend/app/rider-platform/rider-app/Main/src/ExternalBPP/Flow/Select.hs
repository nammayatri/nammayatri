module ExternalBPP.Flow.Select where

import Domain.Action.Beckn.FRFS.Common
import Domain.Types.BecknConfig
import Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common

select :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> BecknConfig -> DFRFSQuote.FRFSQuote -> [DFRFSQuoteCategory.FRFSQuoteCategory] -> m DOnSelect
select _merchant _merchantOperatingCity _integratedBPPConfig _bapConfig quote quoteCategories = do
  return $
    DOnSelect
      { providerId = quote.providerId,
        validTill = Just quote.validTill,
        fareBreakUp = [],
        transactionId = quote.searchId.getId,
        messageId = quote.id.getId,
        categories =
          map
            ( \category ->
                DCategorySelect
                  { bppItemId = category.bppItemId,
                    quantity = category.selectedQuantity,
                    category = category.category,
                    price = category.offeredPrice
                  }
            )
            quoteCategories
      }
