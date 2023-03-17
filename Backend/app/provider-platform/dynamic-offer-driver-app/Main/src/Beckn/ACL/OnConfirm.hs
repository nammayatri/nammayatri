{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmMessage) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking as DConfirm
import qualified Domain.Types.Booking.BookingLocation as DBL
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator

buildOnConfirmMessage :: MonadFlow m => UTCTime -> DConfirm.DConfirmRes -> m OnConfirm.OnConfirmMessage
buildOnConfirmMessage now res = do
  let booking = res.booking
  let vehicleVariant = Common.castVariant res.booking.vehicleVariant
  let itemCode = OnConfirm.ItemCode OnConfirm.ONE_WAY_TRIP vehicleVariant Nothing Nothing
      fareParams = booking.fareParams
      totalFareDecimal = fromIntegral booking.estimatedFare
      currency = "INR"
  fulfillmentDetails <- case booking.bookingType of
    DConfirm.SpecialZoneBooking -> do
      otpCode <- booking.specialZoneOtpCode & fromMaybeM (RideWithBookingIdNotFound booking.id.getId) --replace me with this err OtpNotFoundForSpecialZoneBooking
      return $ mkSpecialZoneFulfillmentInfo res.fromLocation res.toLocation now otpCode
    DConfirm.NormalBooking -> return $ mkFulfillmentInfo res.fromLocation res.toLocation now
  return $
    OnConfirm.OnConfirmMessage
      { order =
          OnConfirm.Order
            { id = getId booking.id,
              state = "ACTIVE",
              items = [mkOrderItem itemCode],
              fulfillment = fulfillmentDetails,
              quote =
                OnConfirm.Quote
                  { price =
                      OnConfirm.QuotePrice
                        { currency,
                          value = totalFareDecimal,
                          offered_value = totalFareDecimal
                        },
                    breakup =
                      mkBreakupList
                        (OnConfirm.BreakupItemPrice currency . fromIntegral)
                        OnConfirm.BreakupItem
                        fareParams
                  },
              payment =
                OnConfirm.Payment
                  { collected_by = "BAP",
                    params =
                      OnConfirm.PaymentParams
                        { currency,
                          amount = totalFareDecimal
                        },
                    _type = OnConfirm.ON_FULFILLMENT,
                    time = OnConfirm.TimeDuration "P2D"
                  }
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

mklocation :: DBL.BookingLocation -> OnConfirm.Location
mklocation loc =
  OnConfirm.Location
    { gps =
        OnConfirm.Gps
          { lat = loc.lat,
            lon = loc.lon
          },
      address = castAddress loc.address
    }
  where
    castAddress DBL.LocationAddress {..} = OnConfirm.Address {area_code = areaCode, locality = area, ward = Nothing, door = Nothing, ..}

mkFulfillmentInfo :: DBL.BookingLocation -> DBL.BookingLocation -> UTCTime -> OnConfirm.FulfillmentInfo
mkFulfillmentInfo fromLoc toLoc startTime =
  OnConfirm.FulfillmentInfo
    { state = OnConfirm.FulfillmentState "TRIP_ASSIGNED",
      start =
        OnConfirm.StartInfo
          { location = mklocation fromLoc,
            time = OnConfirm.TimeTimestamp startTime,
            authorization = Nothing
          },
      end =
        Just
          OnConfirm.StopInfo
            { location = mklocation toLoc
            }
    }

mkSpecialZoneFulfillmentInfo :: DBL.BookingLocation -> DBL.BookingLocation -> UTCTime -> Text -> OnConfirm.FulfillmentInfo
mkSpecialZoneFulfillmentInfo fromLoc toLoc startTime otp = do
  let authorization =
        Just $
          OnConfirm.Authorization
            { _type = "OTP",
              token = otp
            }
  OnConfirm.FulfillmentInfo
    { state = OnConfirm.FulfillmentState "NEW",
      start =
        OnConfirm.StartInfo
          { location = mklocation fromLoc,
            time = OnConfirm.TimeTimestamp startTime,
            authorization = authorization
          },
      end =
        Just
          OnConfirm.StopInfo
            { location = mklocation toLoc
            }
    }
