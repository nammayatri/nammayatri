module Beckn.ACL.Confirm where

import Beckn.Spec.Common
import Beckn.Spec.Confirm
import Domain.Action.UI.QuoteConfirm
import Kernel.Prelude
import Servant.Client

mkConfirmMessage :: ConfirmMessageD -> Order
mkConfirmMessage msg = do
  let dQuote = msg.quote
  let provider = ProviderId "" -- ?
      item =
        Item
          { route_code = dQuote.routeCode,
            start_stop = dQuote.departureStationId.getId,
            end_stop = dQuote.arrivalStationId.getId,
            start_time = dQuote.departureTime,
            end_time = dQuote.arrivalTime
          }
      quantity = msg.quantity
      items = replicate quantity item
      billing =
        Billing
          { name = msg.requestorName
          }
      onePrice = rupeePrice $ fromIntegral dQuote.fare
      totalAmount = dQuote.fare * fromIntegral quantity
      totalPrice = rupeePrice $ fromIntegral totalAmount
      breakupTitle = dQuote.description
      breakupItem = BreakupItem breakupTitle onePrice
      quote =
        Quotation
          { price = totalPrice,
            breakup = replicate quantity breakupItem,
            ttl = Nothing
          }
      payment =
        Payment
          { uri = BaseUrl Http "fake.org" 80 "fake",
            tl_method = HttpGet,
            params = rupeeParams $ realToFrac totalAmount,
            _type = PRE_FULFILLMENT,
            status = NOT_PAID
          }
  Order {..}
