{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit (mkOnInitMessage) where

import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import qualified Domain.Action.Beckn.Init as DInit
import Kernel.Prelude
import Kernel.Types.Common (Money)

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res =
  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = booking.id.getId,
            state = OnInit.NEW,
            items = Nothing,
            quote =
              OnInit.Quote
                { price = mkPrice booking.estimatedFare booking.estimatedTotalFare,
                  breakup = mkBreakup booking.estimatedFare booking.discount
                },
            payment = mkPayment booking.estimatedTotalFare
          }
    }
  where
    booking = res.booking

mkPrice :: Money -> Money -> OnInit.QuotePrice
mkPrice estimatedFare estimatedTotalFare =
  OnInit.QuotePrice
    { currency = "INR",
      value = realToFrac estimatedFare,
      offered_value = realToFrac estimatedTotalFare
    }

mkBreakup :: Money -> Maybe Money -> [OnInit.BreakupItem]
mkBreakup estimatedFare mbDiscount = [estimatedFareBreakupItem] <> maybeToList mbDiscountBreakupItem
  where
    estimatedFareBreakupItem =
      OnInit.BreakupItem
        { title = "Estimated trip fare",
          price =
            OnInit.BreakupItemPrice
              { currency = "INR",
                value = realToFrac estimatedFare
              }
        }

    mbDiscountBreakupItem =
      mbDiscount <&> \discount ->
        OnInit.BreakupItem
          { title = "Discount",
            price =
              OnInit.BreakupItemPrice
                { currency = "INR",
                  value = realToFrac discount
                }
          }

mkPayment :: Money -> OnInit.Payment
mkPayment estimatedTotalFare =
  OnInit.Payment
    { collected_by = "BAP",
      params =
        OnInit.PaymentParams
          { amount = realToFrac estimatedTotalFare,
            currency = "INR"
          },
      time = OnInit.TimeDuration "P2D",
      _type = OnInit.ON_FULFILLMENT
    }
