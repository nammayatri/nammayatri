module Core.ACL.Confirm where

import Beckn.Prelude
import Core.Spec.Common.Billing
import Core.Spec.Common.Payment
import Core.Spec.Common.Price (rupeePrice)
import Core.Spec.Common.ProviderId (ProviderId (ProviderId))
import Core.Spec.Common.Quotation
import Core.Spec.Confirm
import Core.Spec.Confirm.Item
<<<<<<< HEAD
import Domain.Endpoints.UI.QuoteConfirm
=======
import Domain.Confirm
>>>>>>> Added confirm/on_confirm for public transport bap
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
      onePrice = rupeePrice dQuote.fare
      totalAmount = dQuote.fare * fromIntegral quantity
      totalPrice = rupeePrice totalAmount
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
            params = rupeeParams totalAmount,
            _type = PRE_FULFILLMENT,
            status = NOT_PAID
          }
  Order {..}
