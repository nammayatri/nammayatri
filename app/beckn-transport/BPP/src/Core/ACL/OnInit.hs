module Core.ACL.OnInit (mkOnInitMessage) where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.DecimalValue (DecimalValue)
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
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
                { price = mkPrice (fromIntegral booking.estimatedFare) (fromIntegral booking.estimatedTotalFare),
                  breakup = mkBreakup (fromIntegral booking.estimatedFare) (fromIntegral <$> booking.discount)
                },
            payment = mkPayment $ fromIntegral booking.estimatedTotalFare
          }
    }
  where
    booking = res.booking

mkPrice :: DecimalValue -> DecimalValue -> OnInit.QuotePrice
mkPrice estimatedFare estimatedTotalFare =
  OnInit.QuotePrice
    { currency = "INR",
      value = estimatedFare,
      offered_value = estimatedTotalFare
    }

mkBreakup :: DecimalValue -> Maybe DecimalValue -> [OnInit.BreakupItem]
mkBreakup estimatedFare mbDiscount = [estimatedFareBreakupItem] <> maybeToList mbDiscountBreakupItem
  where
    estimatedFareBreakupItem =
      OnInit.BreakupItem
        { title = "Estimated trip fare",
          price =
            OnInit.BreakupItemPrice
              { currency = "INR",
                value = estimatedFare
              }
        }

    mbDiscountBreakupItem =
      mbDiscount <&> \discount ->
        OnInit.BreakupItem
          { title = "Discount",
            price =
              OnInit.BreakupItemPrice
                { currency = "INR",
                  value = discount
                }
          }

mkPayment :: DecimalValue -> OnInit.Payment
mkPayment estimatedTotalFare =
  OnInit.Payment
    { collected_by = "BAP",
      params =
        OnInit.PaymentParams
          { amount = estimatedTotalFare,
            currency = "INR"
          },
      time = OnInit.TimeDuration "P2D",
      _type = OnInit.ON_FULFILLMENT
    }
