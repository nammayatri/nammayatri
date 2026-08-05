-- | MSIL pilot: common override for the wire fulfillment.type code
-- (BecknV2.OnDemand.Utils.Common.tripCategoryToFulfillmentType, untouched --
-- Layer 1's own mapping is left exactly as it is for every other merchant),
-- shared across every MSIL transformer that sends order.fulfillments to the
-- BAP (Search/OnSelect/OnInit/OnConfirm/OnCancel/OnStatus, ...).
--
-- For MSIL, whatever fulfillment type code Layer 1 (or MSIL's own Quote-based
-- builder) computed is collapsed to just two possible values: RIDE_OTP stays
-- RIDE_OTP-equivalent (SELF_PICKUP, the v2.1.0 off-us encoding of ride-OTP);
-- every other trip category becomes DELIVERY.
--
-- This collapse is lossy for /init parsing -- see
-- Beckn.OnDemand.Transformer.MSIL.Init.correctFulfillmentId for the
-- corresponding fix (kept in a separate module to avoid an import cycle:
-- that fix needs Domain.Action.Beckn.Init, which this module's callers
-- -- e.g. SharedLogic.CallBAP -- transitively import).
module Beckn.OnDemand.Utils.MSIL.FulfillmentType
  ( overrideFulfillmentType,
    patchOrderFulfillmentTypes,
    patchProviderFulfillmentTypes,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import Kernel.Prelude

-- | RIDE_OTP -> SELF_PICKUP; every other fulfillment-type code -> DELIVERY.
overrideFulfillmentType :: Text -> Text
overrideFulfillmentType fulfillmentType
  | fulfillmentType == show Enums.RIDE_OTP = show Enums.SELF_PICKUP
  | otherwise = show Enums.DELIVERY

-- | The single patch operation for an order: overrides every one of the
-- order's fulfillments' type code per the rule above. Every other field is
-- passed through untouched.
patchOrderFulfillmentTypes :: Spec.Order -> Spec.Order
patchOrderFulfillmentTypes order =
  order {Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentType = overrideFulfillmentType <$> fulfillment.fulfillmentType}

-- | Same patch, for on_search's catalog.providers[*].fulfillments -- a
-- structurally different field (Provider.providerFulfillments) from
-- order.fulfillments, so it needs its own top-level patch function even
-- though it shares the same per-fulfillment rule above.
patchProviderFulfillmentTypes :: Spec.Provider -> Spec.Provider
patchProviderFulfillmentTypes provider =
  provider {Spec.providerFulfillments = map patchFulfillment <$> provider.providerFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentType = overrideFulfillmentType <$> fulfillment.fulfillmentType}
