{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Confirm (buildConfirmReq) where

import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.LocationAddress as DBL
import qualified Domain.Types.Vehicle.Variant as VehVar
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

buildConfirmReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessage)
buildConfirmReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  context <- buildTaxiContext Context.CONFIRM messageId (Just res.transactionId) bapIDs.cabs bapURIs.cabs (Just res.bppId) (Just res.bppUrl) res.city
  pure $ BecknReq context $ mkConfirmMessage res

mkConfirmMessage :: DOnInit.OnInitRes -> Confirm.ConfirmMessage
mkConfirmMessage res = do
  let vehicleVariant = castVehicleVariant res.vehicleVariant
  let itemId =
        Confirm.ItemId
          { providerName = res.providerShortId,
            vehicleVariant
          }
  pure
    Confirm.ConfirmMessage
      { order =
          Confirm.Order
            { id = getId res.bppBookingId,
              items =
                [ Confirm.OrderItem
                    { id = itemId,
                      price = Nothing
                    }
                ],
              fulfillment = mkFulfillment res.fulfillmentId fulfillmentType res.fromLocationAddress res.mbToLocationAddress res.riderPhoneCountryCode res.riderPhoneNumber res.mbRiderName,
              payment = mkPayment res.estimatedTotalFare res.paymentUrl,
              quote = Nothing,
              provider =
                res.driverId >>= \dId ->
                  Just
                    Confirm.Provider
                      { id = dId
                      }
            }
      }
  where
    castVehicleVariant = \case
      VehVar.SEDAN -> Confirm.SEDAN
      VehVar.SUV -> Confirm.SUV
      VehVar.HATCHBACK -> Confirm.HATCHBACK
      VehVar.AUTO_RICKSHAW -> Confirm.AUTO_RICKSHAW
      VehVar.TAXI -> Confirm.TAXI
      VehVar.TAXI_PLUS -> Confirm.TAXI_PLUS
    fulfillmentType = case res.bookingDetails of
      DRB.OneWaySpecialZoneDetails _ -> Confirm.RIDE_OTP
      _ -> Confirm.RIDE

mkFulfillment :: Maybe Text -> Confirm.FulfillmentType -> DBL.BookingLocation -> Maybe DBL.BookingLocation -> Text -> Text -> Maybe Text -> VehVar.Variant -> Confirm.FulfillmentInfo
mkFulfillment fulfillmentId fulfillmentType startLoc mbStopLoc riderPhoneCountryCode riderPhoneNumber mbRiderName vehicleVariant =
  Confirm.FulfillmentInfo
    { id = fulfillmentId,
      _type = fulfillmentType,
      start =
        Confirm.StartInfo
          { location =
              Confirm.Location
                { gps =
                    Confirm.Gps
                      { lat = startLoc.lat,
                        lon = startLoc.lon
                      },
                  address = Just $ mkLocation startLoc
                },
            authorization = Nothing
          },
      end =
        mbStopLoc <&> \stopLoc ->
          Confirm.StopInfo
            { location =
                Confirm.Location
                  { gps =
                      Confirm.Gps
                        { lat = stopLoc.lat,
                          lon = stopLoc.lon
                        },
                    address = Just $ mkLocation stopLoc
                  }
            },
      customer =
        Confirm.OrderCustomer
          { contact =
              Confirm.Contact
                { phone =
                    Confirm.Phone
                      { country_code = riderPhoneCountryCode,
                        number = riderPhoneNumber
                      }
                },
            person =
              mbRiderName <&> \riderName ->
                Confirm.OrderPerson
                  { name = riderName
                  }
          },
      vehicle =
        Init.Vehicle
          { category = vehicleVariant
          }
    }

mkLocation :: DBL.LocationAddress -> Confirm.LocationAddress
mkLocation DBL.LocationAddress {..} =
  Confirm.Address
    { area_code = areaCode,
      locality = area,
      ward = ward,
      door = door,
      ..
    }

mkPayment :: Money -> Maybe Text -> Confirm.Payment
mkPayment estimatedTotalFare uri =
  Confirm.Payment
    { _type = Confirm.ON_FULFILLMENT,
      time = Confirm.TimeDuration "P2D",
      params =
        Init.PaymentParams
          { collected_by = "BAP",
            instrument = Nothing,
            currency = "INR",
            amount = Just realToFrac estimatedTotalFare
          },
      uri
    }
