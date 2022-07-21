module Core.ACL.OnInit where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnInit as OnInit
import Domain.Action.Beckn.Init as DInit
import Product.FareCalculator.Calculator (fareSum, mkBreakupList)
import Utils.Common (amountToDecimalValue)

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res = do
  let rb = res.booking
      fareParams = rb.fareParams
      fareDecimalValue = amountToDecimalValue $ fareSum fareParams
      currency = "INR"
      breakup_ = mkBreakupList (OnInit.BreakupItemPrice currency . amountToDecimalValue) OnInit.BreakupItem fareParams

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
