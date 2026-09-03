-- Reads the BAP's proposed fare off the wire item's own price and sets negotiatedFare from it, since Layer 1 always leaves it Nothing and doesn't know about the ONDC pilot.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Select
  ( ondcScheduledRideParser,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Select as DSelect
import EulerHS.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Price (highPrecMoneyFromText)

-- | The BAP's proposed total fare, read from the wire item's own price object.
getNegotiatedFare :: Spec.ConfirmReqMessage -> Maybe HighPrecMoney
getNegotiatedFare message = do
  items <- message.confirmReqMessageOrder.orderItems
  item <- case items of
    [i] -> Just i
    _ -> Nothing
  price <- item.itemPrice
  priceValue <- price.priceValue
  highPrecMoneyFromText priceValue

-- | Sets negotiatedFare on Layer 1's DSelectReq.
ondcScheduledRideParser :: Spec.ConfirmReqMessage -> DSelect.DSelectReq -> DSelect.DSelectReq
ondcScheduledRideParser message dSelectReq =
  dSelectReq {DSelect.negotiatedFare = getNegotiatedFare message}
