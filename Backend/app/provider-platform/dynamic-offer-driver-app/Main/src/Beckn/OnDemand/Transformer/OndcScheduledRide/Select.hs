-- Reads the BAP's proposed fare off the wire item's own price and sets negotiatedFare from it, and patches in the wire item's add-ons, since Layer 1 always leaves both empty and doesn't know about the ONDC pilot.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Select
  ( ondcScheduledRideParser,
  )
where

import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Select as DSelect
import EulerHS.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Price (highPrecMoneyFromText)

-- | The BAP's proposed total fare, read from the wire item's own price object.
getNegotiatedFare :: Spec.ConfirmReqMessage -> Maybe HighPrecMoney
getNegotiatedFare message = do
  item <- OSRCommon.soleOrderItem message.confirmReqMessageOrder.orderItems
  price <- item.itemPrice
  priceValue <- price.priceValue
  highPrecMoneyFromText priceValue

-- | Sets negotiatedFare and addOns on Layer 1's DSelectReq.
ondcScheduledRideParser :: Spec.ConfirmReqMessage -> DSelect.DSelectReq -> DSelect.DSelectReq
ondcScheduledRideParser message dSelectReq =
  dSelectReq
    { DSelect.negotiatedFare = getNegotiatedFare message,
      DSelect.addOns = OSRCommon.extractAddOns message.confirmReqMessageOrder.orderItems
    }
