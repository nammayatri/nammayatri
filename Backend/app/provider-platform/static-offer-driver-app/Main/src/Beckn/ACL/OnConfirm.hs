{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (mkOnConfirmMessage) where

import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.Vehicle as Veh
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

mkOnConfirmMessage :: DConfirm.DConfirmRes -> OnConfirm.OnConfirmMessage
mkOnConfirmMessage res = do
  let booking = res.booking
  OnConfirm.OnConfirmMessage
    { order =
        OnConfirm.Order
          { id = getId booking.id,
            state = "ACTIVE",
            items = [mkOrderItem itemCode],
            fulfillment = mkFulfillmentInfo res.fromLocation res.toLocation booking.startTime,
            quote =
              OnConfirm.Quote
                { price = mkPrice booking.estimatedFare booking.estimatedTotalFare,
                  breakup = mkBreakup booking.estimatedFare booking.discount
                },
            payment = mkPayment booking.estimatedTotalFare
          }
    }
  where
    itemCode = do
      let (fpType, mbDistance, mbDuration) = case res.bDetails of
            DConfirm.OneWayDetails -> (OnConfirm.ONE_WAY_TRIP, Nothing, Nothing)
            DConfirm.RentalDetails {baseDistance, baseDuration} -> (OnConfirm.RENTAL_TRIP, Just baseDistance, Just baseDuration)
          vehicleVariant = case res.booking.vehicleVariant of
            Veh.SEDAN -> OnConfirm.SEDAN
            Veh.SUV -> OnConfirm.SUV
            Veh.HATCHBACK -> OnConfirm.HATCHBACK
      OnConfirm.ItemCode fpType vehicleVariant mbDistance mbDuration

mkOrderItem :: OnConfirm.ItemCode -> OnConfirm.OrderItem
mkOrderItem code =
  OnConfirm.OrderItem
    { descriptor =
        OnConfirm.Descriptor
          { code = code
          }
    }

mkFulfillmentInfo :: DBL.BookingLocation -> Maybe DBL.BookingLocation -> UTCTime -> OnConfirm.FulfillmentInfo
mkFulfillmentInfo fromLoc mbToLoc startTime =
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
            time = OnConfirm.TimeTimestamp startTime,
            authorization = Nothing
          },
      end =
        mbToLoc >>= \toLoc ->
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
    castAddress DBL.LocationAddress {..} = OnConfirm.Address {area_code = areaCode, ward = Nothing, locality = area, door = Nothing, ..}

mkPrice :: Money -> Money -> OnConfirm.QuotePrice
mkPrice estimatedFare estimatedTotalFare =
  OnConfirm.QuotePrice
    { currency = "INR",
      value = realToFrac estimatedFare,
      offered_value = realToFrac estimatedTotalFare
    }

mkBreakup :: Money -> Maybe Money -> [OnConfirm.BreakupItem]
mkBreakup estimatedFare mbDiscount = [estimatedFareBreakupItem] <> maybeToList mbDiscountBreakupItem
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

    mbDiscountBreakupItem =
      mbDiscount <&> \discount ->
        OnConfirm.BreakupItem
          { title = "Discount",
            price =
              OnConfirm.BreakupItemPrice
                { currency = "INR",
                  value = realToFrac discount
                }
          }

mkPayment :: Money -> OnConfirm.Payment
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
