{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import Beckn.Types.Core.Taxi.OnInit as OnInit
import Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.ItemId as ItemId
import Kernel.Prelude
import SharedLogic.FareCalculator

mkOnInitRecurringBookingMessage :: DInit.InitRecurringBookingRes -> OnInit.OnInitMessage
mkOnInitRecurringBookingMessage res = do
  let rb = res.booking
      currency = "INR"

  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = rb.id.getId,
            state = OnInit.NEW,
            items =
              Just
                [ OnInit.OrderItem
                    { quantity = OnInit.Quantity {count = 1},
                      id = ItemId.toText $ ItemId.RecurringTrip res.farePolicy.vehicleVariant
                    }
                ],
            quote =
              OnInit.Quote
                { price =
                    OnInit.QuotePrice
                      { currency,
                        value = 0,
                        offered_value = 0
                      },
                  breakup = []
                },
            payment =
              OnInit.Payment
                { collected_by = "N/A",
                  params =
                    OnInit.PaymentParams
                      { currency = currency,
                        amount = 0
                      },
                  _type = OnInit.ON_FULFILLMENT,
                  time = OnInit.TimeDuration "FIXME"
                },
            fulfillment = Nothing
          }
    }

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res = do
  let rb = res.booking
      fareDecimalValue = fromIntegral rb.estimatedFare
      currency = "INR"
      breakup_ = mkBreakupList (OnInit.BreakupItemPrice currency . fromIntegral) OnInit.BreakupItem rb.fareParams

  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = res.booking.id.getId,
            state = OnInit.NEW,
            items = Nothing,
            quote =
              OnInit.Quote
                { price =
                    OnInit.QuotePrice
                      { currency,
                        value = fareDecimalValue,
                        offered_value = fareDecimalValue
                      },
                  breakup = breakup_
                },
            payment =
              OnInit.Payment
                { collected_by = "BPP",
                  params =
                    OnInit.PaymentParams
                      { currency = currency,
                        amount = fareDecimalValue
                      },
                  _type = OnInit.ON_FULFILLMENT,
                  time = OnInit.TimeDuration "FIXME"
                },
            fulfillment = Nothing
          }
    }
