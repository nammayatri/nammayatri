module Beckn.ACL.OnInit where

import Beckn.Types.Core.Taxi.OnInit as OnInit
import Domain.Action.Beckn.Init as DInit
import Kernel.Prelude
import SharedLogic.FareCalculator

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
                }
          }
    }
