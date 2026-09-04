-- Re-derives the fulfillment id's kind from the search's own isScheduled flag instead of the untrustworthy echo, since the on_select fulfillment.type override collapses the trip category info Layer 1's /init parse relies on, causing scheduled Quote ids to misparse as DriverQuote ids.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Init
  ( correctFulfillmentId,
  )
where

import qualified Domain.Action.Beckn.Init as DInit
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Queries.SearchRequest as QSR

-- | If the search was scheduled, treats the fulfillment id as a QuoteId regardless of Layer 1's parse.
correctFulfillmentId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> DInit.FulfillmentId -> m DInit.FulfillmentId
correctFulfillmentId transactionId fulfillmentId = do
  mbSearchRequest <- QSR.findByTransactionId transactionId
  pure $ case mbSearchRequest of
    Just searchRequest | searchRequest.isScheduled -> DInit.QuoteId (Id (fulfillmentIdText fulfillmentId))
    _ -> fulfillmentId
  where
    fulfillmentIdText (DInit.QuoteId quoteId) = quoteId.getId
    fulfillmentIdText (DInit.DriverQuoteId driverQuoteId) = driverQuoteId.getId
