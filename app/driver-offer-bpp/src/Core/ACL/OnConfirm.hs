module Core.ACL.OnConfirm (mkOnConfirmMessage) where

import Beckn.Prelude
import Beckn.Types.Amount (Amount)
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import Beckn.Types.Id
import qualified Core.ACL.Common as Common
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.RideBooking.BookingLocation as DBL

mkOnConfirmMessage :: UTCTime -> DConfirm.DConfirmRes -> OnConfirm.OnConfirmMessage
mkOnConfirmMessage now res = do
  let booking = res.booking
  let vehicleVariant = Common.castVariant res.booking.vehicleVariant
  let itemCode = OnConfirm.ItemCode OnConfirm.ONE_WAY_TRIP vehicleVariant Nothing Nothing
  OnConfirm.OnConfirmMessage
    { order =
        OnConfirm.Order
          { id = getId booking.id,
            state = "ACTIVE",
            items = [mkOrderItem itemCode],
            fulfillment = mkFulfillmentInfo res.fromLocation res.toLocation now, -- booking.startTime, --FIXME
            quote =
              OnConfirm.Quote
                { price = mkPrice booking.estimatedFare booking.estimatedFare,
                  breakup = mkBreakup booking.estimatedFare
                },
            payment = mkPayment booking.estimatedFare
          }
    }

mkOrderItem :: OnConfirm.ItemCode -> OnConfirm.OrderItem
mkOrderItem code =
  OnConfirm.OrderItem
    { descriptor =
        OnConfirm.Descriptor
          { code = code
          }
    }

mkFulfillmentInfo :: DBL.BookingLocation -> DBL.BookingLocation -> UTCTime -> OnConfirm.FulfillmentInfo
mkFulfillmentInfo fromLoc toLoc startTime =
  OnConfirm.FulfillmentInfo
    { state = OnConfirm.FulfillmentState "SEARCHING_FOR_DRIVERS",
      start =
        OnConfirm.StartInfo
          { location =
              OnConfirm.Location
                { gps =
                    OnConfirm.Gps
                      { lat = fromLoc.lat,
                        lon = fromLoc.lon
                      },
                  address = castAddress fromLoc.address
                },
            time = OnConfirm.TimeTimestamp startTime
          },
      end =
        Just
          OnConfirm.StopInfo
            { location =
                OnConfirm.Location
                  { gps =
                      OnConfirm.Gps
                        { lat = toLoc.lat,
                          lon = toLoc.lon
                        },
                    address = castAddress toLoc.address
                  }
            }
    }
  where
    castAddress DBL.LocationAddress {..} = OnConfirm.Address {area_code = areaCode, ..}

mkPrice :: Amount -> Amount -> OnConfirm.QuotePrice
mkPrice estimatedFare estimatedTotalFare =
  OnConfirm.QuotePrice
    { currency = "INR",
      value = realToFrac estimatedFare,
      offered_value = realToFrac estimatedTotalFare
    }

mkBreakup :: Amount -> [OnConfirm.BreakupItem]
mkBreakup estimatedFare = [estimatedFareBreakupItem]
  where
    estimatedFareBreakupItem =
      OnConfirm.BreakupItem
        { title = "Estimated trip fare",
          price =
            OnConfirm.BreakupItemPrice
              { currency = "INR",
                value = realToFrac estimatedFare
              }
        }

mkPayment :: Amount -> OnConfirm.Payment
mkPayment estimatedTotalFare =
  OnConfirm.Payment
    { collected_by = "BAP",
      params =
        OnConfirm.PaymentParams
          { amount = realToFrac estimatedTotalFare,
            currency = "INR"
          },
      time = OnConfirm.TimeDuration "P2D",
      _type = OnConfirm.ON_FULFILLMENT
    }
