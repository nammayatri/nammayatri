-- | MSIL pilot: on_confirm announces the order's fulfillment.id as
-- booking.quoteId (Beckn.ACL.OnConfirm.hs's `Spec.fulfillmentId = Just
-- res.booking.quoteId`), but every post-confirm order-update callback
-- (on_update/on_status, built via Beckn.OnDemand.Utils.Common.mkFulfillmentV2)
-- uses ride.id instead. For most BAPs this drift goes unnoticed, but ONDC
-- Workbench validates that a booking's fulfillment.id stays the same value
-- it first saw at on_confirm ("selection"), and NACKs on_update with
-- "Fulfillment ID <ride.id> in on_update does not match selection" the moment
-- it differs.
--
-- This patches the fulfillment id (and the item's fulfillment_ids that
-- reference it) back to the quote id on every outgoing MSIL order payload
-- built from a ride, so ride.id is never leaked onto the wire for MSIL.
module Beckn.OnDemand.Utils.MSIL.FulfillmentId
  ( overrideOrderFulfillmentId,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import Kernel.Prelude

-- | Overrides order.fulfillments[*].id and order.items[*].fulfillment_ids
-- to the given quote id -- there is always exactly one fulfillment per
-- order in this app's model, so an unconditional overwrite is safe.
overrideOrderFulfillmentId :: Text -> Spec.Order -> Spec.Order
overrideOrderFulfillmentId quoteId order =
  order
    { Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments,
      Spec.orderItems = map patchItem <$> order.orderItems
    }
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentId = Just quoteId}
    patchItem item = item {Spec.itemFulfillmentIds = Just [quoteId]}
