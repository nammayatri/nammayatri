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

buildOnConfirmMessage :: MonadFlow m => DConfirm.DConfirmRes -> m OnConfirm.OnConfirmMessage
buildOnConfirmMessage res = do
  let booking = res.booking
  let vehicleVariant = Common.castVariant res.booking.vehicleVariant
  let itemId =
        Init.ItemId
          { providerName = res.transporter.shortId.getShortId,
            vehicleVariant
          }
      fareParams = booking.fareParams
      totalFareDecimal = fromIntegral booking.estimatedFare
      currency = "INR"
  fulfillmentDetails <- case booking.bookingType of
    DConfirm.SpecialZoneBooking -> do
      otpCode <- booking.specialZoneOtpCode & fromMaybeM (OtpNotFoundForSpecialZoneBooking booking.id.getId)
      return $ mkSpecialZoneFulfillmentInfo res.fromLocation res.toLocation otpCode booking.quoteId OnConfirm.RIDE
    DConfirm.NormalBooking -> return $ mkFulfillmentInfo res.fromLocation res.toLocation booking.quoteId OnConfirm.RIDE_OTP res.driverName
  return $
    OnConfirm.OnConfirmMessage
      { order =
          OnConfirm.Order
            { id = getId booking.id,
              state = "ACTIVE",
              items = [mkOrderItem itemId booking.quoteId currency totalFareDecimal],
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
              vehicle =
                OnConfirm.Vehicle
                  { category = vehicleVariant
                  },
              provider =
                res.driverId >>= \dId ->
                  Just
                    OnConfirm.Provider
                      { id = dId
                      },
              payment =
                OnConfirm.Payment
                  { params =
                      OnConfirm.PaymentParams
                        { collected_by = "BAP",
                          instrument = Nothing,
                          currency = Just currency,
                          amount = Just totalFareDecimal
                        },
                    _type = OnConfirm.ON_FULFILLMENT,
                    time = OnConfirm.TimeDuration "P2D",
                    uri = res.paymentUrl
                  }
            }
      }

mkOrderItem :: OnConfirm.ItemId -> Text -> Text -> Decimal -> OnConfirm.OrderItem
mkOrderItem itemId fulfillmentId currency totalFareDecimal =
  OnConfirm.OrderItem
    { id = itemId,
      fulfillment_id = fulfillmentId,
      price =
        OnConfirm.Price
          { currency,
            value = totalFareDecimal
          },
      descriptor =
        OnConfirm.Descriptor
          { short_desc = itemId
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
    castAddress DBL.LocationAddress {..} = OnConfirm.Address {area_code = areaCode, locality = area, ward = Nothing, ..}

mkFulfillmentInfo :: DBL.BookingLocation -> DBL.BookingLocation -> Text -> OnConfirm.FulfillmentType -> Maybe Text -> OnConfirm.FulfillmentInfo
mkFulfillmentInfo fromLoc toLoc fulfillmentId fulfillmentType driverName =
  OnConfirm.FulfillmentInfo
    { id = fulfillmentId,
      _type = fulfillmentType,
      state = OnConfirm.FulfillmentState "TRIP_ASSIGNED",
      start =
        OnConfirm.StartInfo
          { location = mklocation fromLoc,
            authorization = Nothing
          },
      end =
        Just
          OnConfirm.StopInfo
            { location = mklocation toLoc
            },
      agent =
        driverName >>= \dName ->
          Just
            OnConfirm.Agent
              { name = dName,
                rateable = True
              }
    }

mkSpecialZoneFulfillmentInfo :: DBL.BookingLocation -> DBL.BookingLocation -> Text -> Text -> OnConfirm.FulfillmentType -> OnConfirm.FulfillmentInfo
mkSpecialZoneFulfillmentInfo fromLoc toLoc otp fulfillmentId fulfillmentType = do
  let authorization =
        Just $
          OnConfirm.Authorization
            { _type = "OTP",
              token = otp
            }
  OnConfirm.FulfillmentInfo
    { id = fulfillmentId,
      _type = fulfillmentType,
      state = OnConfirm.FulfillmentState "NEW",
      start =
        OnConfirm.StartInfo
          { location = mklocation fromLoc,
            authorization = authorization
          },
      end =
        Just
          OnConfirm.StopInfo
            { location = mklocation toLoc
            },
      agent = Nothing
    }
