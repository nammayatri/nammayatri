module Core.ACL.OnInit where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Core.Taxi.OnInit as OnInit
import Domain.Action.Beckn.Init as DInit

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res = do
  let rb = res.rideBooking
      fareDecimalValue = DecimalValue $ toRational $ amountToDouble rb.estimatedFare
      currency = "INR"
  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = res.rideBooking.id.getId,
            state = OnInit.NEW,
            quote =
              OnInit.Quote
                { price =
                    OnInit.QuotePrice
                      { currency = "INR",
                        value = fareDecimalValue,
                        offered_value = fareDecimalValue
                      },
                  breakup =
                    [ OnInit.BreakupItem
                        { title = "fare",
                          price =
                            OnInit.BreakupItemPrice {currency, value = fareDecimalValue}
                        }
                    ]
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
                }
          }
    }
