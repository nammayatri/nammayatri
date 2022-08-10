module Core.ACL.OnInit (mkOnInitMessage) where

import Beckn.Prelude
import Beckn.Types.Amount
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import Core.ACL.Common
import qualified Domain.Action.Beckn.Init as DInit

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res =
  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = booking.id.getId,
            state = OnInit.NEW,
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

mkPrice :: Amount -> Amount -> OnInit.QuotePrice
mkPrice estimatedFare estimatedTotalFare =
  OnInit.QuotePrice
    { currency = "INR",
      value = amountToRoundedDecimal estimatedFare,
      offered_value = amountToRoundedDecimal estimatedTotalFare
    }

mkBreakup :: Amount -> Maybe Amount -> [OnInit.BreakupItem]
mkBreakup estimatedFare mbDiscount = [estimatedFareBreakupItem] <> maybeToList mbDiscountBreakupItem
  where
    estimatedFareBreakupItem =
      OnInit.BreakupItem
        { title = "Estimated trip fare",
          price =
            OnInit.BreakupItemPrice
              { currency = "INR",
                value = amountToRoundedDecimal estimatedFare
              }
        }

    mbDiscountBreakupItem =
      mbDiscount <&> \discount ->
        OnInit.BreakupItem
          { title = "Discount",
            price =
              OnInit.BreakupItemPrice
                { currency = "INR",
                  value = amountToRoundedDecimal discount
                }
          }

mkPayment :: Amount -> OnInit.Payment
mkPayment estimatedTotalFare =
  OnInit.Payment
    { collected_by = "BAP",
      params =
        OnInit.PaymentParams
          { amount = amountToRoundedDecimal estimatedTotalFare,
            currency = "INR"
          },
      time = OnInit.TimeDuration "P2D",
      _type = OnInit.ON_FULFILLMENT
    }
