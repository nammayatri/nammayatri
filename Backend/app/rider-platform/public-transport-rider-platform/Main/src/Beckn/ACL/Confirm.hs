{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
