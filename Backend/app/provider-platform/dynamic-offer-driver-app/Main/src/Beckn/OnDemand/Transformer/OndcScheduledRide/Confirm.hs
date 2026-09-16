-- Patches in the add-ons echoed on the wire item, since Layer 1 (Beckn.ACL.Confirm.buildConfirmReqV2) always leaves DConfirmReq.addOns empty.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Confirm
  ( buildOndcScheduledRideConfirmReq,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Kernel.Prelude

-- | Layer 2: takes the DConfirmReq Layer 1 already built (addOns = []) plus the original wire request, and returns it with addOns decided.
buildOndcScheduledRideConfirmReq :: Spec.ConfirmReq -> DConfirm.DConfirmReq -> DConfirm.DConfirmReq
buildOndcScheduledRideConfirmReq req dConfirmReq =
  dConfirmReq {DConfirm.addOns = extractAddOns req}

-- | The rider add-ons echoed on the wire item (item.add_ons) -- a BAP can select more than one add-on on the same item.
extractAddOns :: Spec.ConfirmReq -> [Spec.AddOn]
extractAddOns req = fromMaybe [] $ do
  items <- req.confirmReqMessage.confirmReqMessageOrder.orderItems
  item <- case items of
    [i] -> Just i
    _ -> Nothing
  item.itemAddOns
