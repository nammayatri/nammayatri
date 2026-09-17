-- | MSIL pilot: shared override for fulfillment-state descriptor codes that
-- Layer 1 produces but that aren't in ONDC v2.1.0's fulfillmentState
-- vocabulary (RIDE_CANCELLED/RIDE_ENDED/RIDE_STARTED/RIDE_ASSIGNED/
-- RIDE_ENROUTE_PICKUP/RIDE_ARRIVED_PICKUP/RIDE_CONFIRMED):
--
--   * NEW -- "Custom type only used for on-us transaction"
--     (BecknV2.OnDemand.Enums.FulfillmentState) -- the no-driver-yet
--     static/scheduled on_confirm path (Beckn.ACL.OnConfirm.bookingStatusCode)
--     uses this; the reference on_confirm example uses RIDE_CONFIRMED instead.
--   * SCHEDULED_RIDE_ASSIGNED -- a real Enums.FulfillmentState constructor,
--     but not one of the seven ONDC-valid wire values -- produced whenever a
--     driver is assigned to a scheduled ride (Domain.Types.Ride.RideStatus
--     UPCOMING), on both the on_confirm phased-assignment push
--     (SharedLogic.CallBAP.buildOnConfirmMessage) and the on_update
--     ride-assignment push (SharedLogic.CallBAP.sendRideAssignedUpdateToBAP),
--     which both build off the same Beckn.OnDemand.Transformer.OnUpdate
--     dispatch. ONDC has no separate "scheduled" fulfillment-state code --
--     RIDE_ASSIGNED covers both.
--
-- Shared in one place (rather than duplicated per call site, the way the
-- original on_confirm-only fix was written) so every producer stays
-- consistent as new ones are found. Kept as an Order->Order patch, not tied
-- to any one message type, since on_confirm and on_update both wrap their
-- order in the same Spec.ConfirmReqMessage shape
-- (BecknV2.OnDemand.Types.OnUpdateReq.onUpdateReqMessage is itself a
-- ConfirmReqMessage).
module Beckn.OnDemand.Utils.MSIL.FulfillmentState
  ( overrideOrderFulfillmentState,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude

overrideOrderFulfillmentState :: Spec.Order -> Spec.Order
overrideOrderFulfillmentState order =
  order
    { Spec.orderFulfillments = map fixFulfillment <$> Spec.orderFulfillments order,
      Spec.orderCancellationTerms = map fixCancellationTerm <$> Spec.orderCancellationTerms order
    }
  where
    fixFulfillment fulfillment = fulfillment {Spec.fulfillmentState = fixState <$> Spec.fulfillmentState fulfillment}
    fixCancellationTerm term = term {Spec.cancellationTermFulfillmentState = fixState <$> Spec.cancellationTermFulfillmentState term}
    fixState fulfillmentState = fulfillmentState {Spec.fulfillmentStateDescriptor = fixDescriptor <$> Spec.fulfillmentStateDescriptor fulfillmentState}
    fixDescriptor descriptor = descriptor {Spec.descriptorCode = overrideCode <$> Spec.descriptorCode descriptor}
    overrideCode code
      | code == show Enums.NEW = show Enums.RIDE_CONFIRMED
      | code == show Enums.SCHEDULED_RIDE_ASSIGNED = show Enums.RIDE_ASSIGNED
      | otherwise = code
