-- Patches in the add-ons echoed on the wire item, since Layer 1 (Beckn.ACL.Confirm.buildConfirmReqV2) always leaves DConfirmReq.addOns empty.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Confirm
  ( buildOndcScheduledRideConfirmReq,
  )
where

import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Confirm as DConfirm

-- | Layer 2: takes the DConfirmReq Layer 1 already built (addOns = []) plus the original wire request, and returns it with addOns decided.
buildOndcScheduledRideConfirmReq :: Spec.ConfirmReq -> DConfirm.DConfirmReq -> DConfirm.DConfirmReq
buildOndcScheduledRideConfirmReq req dConfirmReq =
  dConfirmReq {DConfirm.addOns = OSRCommon.extractAddOns req.confirmReqMessage.confirmReqMessageOrder.orderItems}
