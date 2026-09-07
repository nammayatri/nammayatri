-- Re-derives the fulfillment id's kind from the search's own isScheduled flag instead of the untrustworthy echo, since the on_select fulfillment.type override collapses the trip category info Layer 1's /init parse relies on, causing scheduled Quote ids to misparse as DriverQuote ids. Also extracts the wire item's add-ons, since Layer 1 always leaves InitReq.addOns empty.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Init
  ( correctFulfillmentId,
    buildOndcScheduledRideInitReq,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
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

-- | The single patch operation for pilot merchants: corrects the fulfillment id and patches in the wire item's add-ons (Layer 1 always leaves this empty).
buildOndcScheduledRideInitReq :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Spec.InitReq -> DInit.InitReq -> m DInit.InitReq
buildOndcScheduledRideInitReq transactionId req dInitReq = do
  correctedFulfillmentId <- correctFulfillmentId transactionId dInitReq.fulfillmentId
  pure dInitReq {DInit.fulfillmentId = correctedFulfillmentId, DInit.addOns = extractAddOns req}

-- | The add-ons echoed on the wire item (item.add_ons) -- a BAP can select more than one add-on on the same item.
extractAddOns :: Spec.InitReq -> [Spec.AddOn]
extractAddOns req = fromMaybe [] $ do
  items <- req.initReqMessage.confirmReqMessageOrder.orderItems
  item <- case items of
    [i] -> Just i
    _ -> Nothing
  item.itemAddOns
