-- | MSIL pilot: Layer 2 correction for /init's fulfillment-id parse.
-- Beckn.OnDemand.Transformer.Init.buildDInitReq (Layer 1, untouched)
-- re-derives tripCategory -- and from it, whether the incoming /init's
-- fulfillment id should be parsed as a QuoteId or a DriverQuoteId -- from the
-- fulfillment.type the BAP echoes back, for any "value-add" BAP. That echo is
-- only trustworthy if fulfillment.type round-trips unchanged; MSIL's own
-- override on /on_select (Beckn.OnDemand.Utils.MSIL.Common.overrideFulfillmentType)
-- collapses it to just SELF_PICKUP/DELIVERY, so a real Quote-based scheduled trip's
-- echo can no longer be trusted to recover its original trip category: Layer
-- 1 misreads DELIVERY as OneWay OneWayOnDemandDynamicOffer
-- (EstimateBased/DriverQuoteId) and fails the lookup with
-- DRIVER_QUOTE_NOT_FOUND, because the id is actually a Quote's.
-- 'correctFulfillmentId' undoes exactly that misparse, for MSIL pilot
-- merchants only, by re-deriving the fulfillment id's kind from the search's
-- own isScheduled flag (SearchRequest.isScheduled, stored independently of
-- the fulfillment.type round-trip) instead of trusting Layer 1's echo-based
-- parse.
--
-- Kept separate from Beckn.OnDemand.Utils.MSIL.Common (which builds
-- the override this corrects for) to avoid an import cycle: this needs
-- Domain.Action.Beckn.Init, which SharedLogic.CallBAP (a caller of the
-- Utils.MSIL.Common override) transitively imports.
module Beckn.OnDemand.Transformer.MSIL.Init
  ( correctFulfillmentId,
  )
where

import qualified Domain.Action.Beckn.Init as DInit
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Queries.SearchRequest as QSR

-- | Verifying: if this transaction's search was scheduled, the /init
-- fulfillment id is a QuoteId regardless of what Layer 1's echo-based parse
-- concluded; otherwise Layer 1's own parse is left untouched. Same underlying
-- id text either way -- only which constructor wraps it changes.
correctFulfillmentId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> DInit.FulfillmentId -> m DInit.FulfillmentId
correctFulfillmentId transactionId fulfillmentId = do
  mbSearchRequest <- QSR.findByTransactionId transactionId
  pure $ case mbSearchRequest of
    Just searchRequest | searchRequest.isScheduled -> DInit.QuoteId (Id (fulfillmentIdText fulfillmentId))
    _ -> fulfillmentId
  where
    fulfillmentIdText (DInit.QuoteId quoteId) = quoteId.getId
    fulfillmentIdText (DInit.DriverQuoteId driverQuoteId) = driverQuoteId.getId
