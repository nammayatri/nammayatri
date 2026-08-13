-- | MSIL pilot: Layer 2 patch for /on_confirm's fulfillment state. Layer 1
-- (Beckn.ACL.OnConfirm.bookingStatusCode, buildOnConfirmMessageV2 -- both
-- untouched) returns Enums.NEW for the no-driver-yet static/scheduled path.
-- NEW does not appear anywhere in the ONDC v2.1.0 fulfillmentState vocabulary --
-- it's explicitly documented as "Custom type only used for on-us transaction"
-- (BecknV2.OnDemand.Enums.FulfillmentState) -- while the reference on_confirm
-- example uses RIDE_CONFIRMED for exactly this case (order confirmed, no driver
-- yet). Gated behind scheduledCategorySignalMerchantIds initially rather than a
-- direct Layer 1 fix, because unlike the other MSIL modules this touches a value
-- on a call already live in production for every merchant today -- see doc 25 s8
-- for the promotion plan once this is confirmed safe against real traffic.
module Beckn.OnDemand.Transformer.MSIL.OnConfirm
  ( fixFulfillmentState,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude

-- | Layer 2: takes the already-built on_confirm message from Layer 1 and rewrites
-- every occurrence of the NEW state code -- both the top-level fulfillment state
-- and the cancellation_terms declarations that echo it -- to RIDE_CONFIRMED.
-- Every other field is passed through untouched.
fixFulfillmentState :: Spec.ConfirmReqMessage -> Spec.ConfirmReqMessage
fixFulfillmentState msg = msg {Spec.confirmReqMessageOrder = fixOrder (Spec.confirmReqMessageOrder msg)}
  where
    fixOrder order =
      order
        { Spec.orderFulfillments = map fixFulfillment <$> Spec.orderFulfillments order,
          Spec.orderCancellationTerms = map fixCancellationTerm <$> Spec.orderCancellationTerms order
        }
    fixFulfillment fulfillment = fulfillment {Spec.fulfillmentState = fixState <$> Spec.fulfillmentState fulfillment}
    fixCancellationTerm term = term {Spec.cancellationTermFulfillmentState = fixState <$> Spec.cancellationTermFulfillmentState term}
    fixState fulfillmentState = fulfillmentState {Spec.fulfillmentStateDescriptor = fixDescriptor <$> Spec.fulfillmentStateDescriptor fulfillmentState}
    fixDescriptor descriptor
      | Spec.descriptorCode descriptor == Just (show Enums.NEW) = descriptor {Spec.descriptorCode = Just (show Enums.RIDE_CONFIRMED)}
      | otherwise = descriptor
