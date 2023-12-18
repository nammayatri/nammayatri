{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Confirm (buildConfirmReq, buildConfirmReqV2) where

-- import qualified Beckn.Types.Core.Taxi.Common.Image as Image
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

buildConfirmReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessage)
buildConfirmReq res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.CONFIRM messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country False
  message <- mkConfirmMessage res
  pure $ BecknReq context message

buildConfirmReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessageV2)
buildConfirmReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.CONFIRM messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country False
  message <- mkConfirmMessageV2 res
  pure $ BecknReq context message

mkConfirmMessage :: (MonadFlow m) => DOnInit.OnInitRes -> m Confirm.ConfirmMessage
mkConfirmMessage res = do
  let vehicleVariant = castVehicleVariant res.vehicleVariant
      nightSafetyCheck = res.nightSafetyCheck
  pure
    Confirm.ConfirmMessage
      { order =
          Confirm.Order
            { id = getId res.bppBookingId,
              items =
                [ Confirm.OrderItem
                    { id = res.itemId,
                      price = Nothing
                    }
                ],
              fulfillment = mkFulfillment res.fulfillmentId fulfillmentType res.fromLocation res.mbToLocation res.riderPhoneCountryCode res.riderPhoneNumber res.mbRiderName vehicleVariant nightSafetyCheck,
              payment = mkPayment res.estimatedTotalFare res.paymentUrl,
              quote =
                Confirm.Quote
                  { price =
                      Confirm.QuotePrice
                        { value = fromIntegral res.estimatedFare,
                          offered_value = fromIntegral res.estimatedTotalFare,
                          currency = "INR"
                        },
                    breakup = Nothing
                  },
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

mkConfirmMessageV2 :: (MonadFlow m) => DOnInit.OnInitRes -> m Confirm.ConfirmMessageV2
mkConfirmMessageV2 res = do
  let vehicleVariant = castVehicleVariant res.vehicleVariant
      nightSafetyCheck = res.nightSafetyCheck
  pure
    Confirm.ConfirmMessageV2
      { order =
          Confirm.OrderV2
            { id = getId res.bppBookingId,
              items =
                [ Confirm.OrderItem
                    { id = res.itemId,
                      price = Nothing
                    }
                ],
              fulfillments = [mkFulfillmentV2 res.fulfillmentId fulfillmentType res.fromLocation res.mbToLocation res.riderPhoneCountryCode res.riderPhoneNumber res.mbRiderName vehicleVariant nightSafetyCheck],
              payments = [mkPaymentV2 res.estimatedTotalFare res.paymentUrl],
              quote =
                Confirm.Quote
                  { price =
                      Confirm.QuotePrice
                        { value = fromIntegral res.estimatedFare,
                          offered_value = fromIntegral res.estimatedTotalFare,
                          currency = "INR"
                        },
                    breakup = Nothing
                  },
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

mkFulfillment :: Maybe Text -> Confirm.FulfillmentType -> DL.Location -> Maybe DL.Location -> Text -> Text -> Maybe Text -> Confirm.VehicleVariant -> Bool -> Confirm.FulfillmentInfo
mkFulfillment fulfillmentId fulfillmentType startLoc mbStopLoc riderPhoneCountryCode riderPhoneNumber mbRiderName vehicleVariant nightSafetyCheck =
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
                  address = mkAddress startLoc.address
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
                    address = mkAddress stopLoc.address
                  }
            },
      customer =
        Confirm.Customer
          { contact =
              Confirm.Contact
                { phone =
                    Confirm.Phone
                      { phoneNumber = riderPhoneNumber,
                        phoneCountryCode = riderPhoneCountryCode
                      }
                },
            person =
              mbRiderName <&> \riderName ->
                Confirm.OrderPerson
                  { name = riderName,
                    tags = Just $ Confirm.TG [mkCustomerInfoTags]
                  }
          },
      vehicle =
        Confirm.Vehicle
          { category = vehicleVariant
          }
    }
  where
    mkCustomerInfoTags =
      Tags.TagGroup
        { display = False,
          code = "customer_info",
          name = "Customer Information",
          list =
            [ Tags.Tag
                { display = (\_ -> Just False) =<< Just nightSafetyCheck,
                  code = (\_ -> Just "night_safety_check") =<< Just nightSafetyCheck,
                  name = (\_ -> Just "Night Safety Check") =<< Just nightSafetyCheck,
                  value = (Just . show) =<< Just nightSafetyCheck
                }
            ]
        }

mkFulfillmentV2 :: Maybe Text -> Confirm.FulfillmentType -> DL.Location -> Maybe DL.Location -> Text -> Text -> Maybe Text -> Confirm.VehicleVariant -> Bool -> Confirm.FulfillmentInfoV2
mkFulfillmentV2 fulfillmentId fulfillmentType startLoc mbStopLoc riderPhoneCountryCode riderPhoneNumber mbRiderName vehicleVariant nightSafetyCheck =
  let start =
        Confirm.Stop
          { location =
              Confirm.Location
                { gps =
                    Confirm.Gps
                      { lat = startLoc.lat,
                        lon = startLoc.lon
                      },
                  address = mkAddress startLoc.address
                },
            stopType = Confirm.START,
            authorization = Nothing
          }
      end =
        mbStopLoc <&> \stopLoc ->
          Confirm.Stop
            { location =
                Confirm.Location
                  { gps =
                      Confirm.Gps
                        { lat = stopLoc.lat,
                          lon = stopLoc.lon
                        },
                    address = mkAddress stopLoc.address
                  },
              stopType = Confirm.END,
              authorization = Nothing
            }
      end' = maybeToList end
   in Confirm.FulfillmentInfoV2
        { id = fulfillmentId,
          _type = fulfillmentType,
          stops = [start] <> end',
          customer =
            Confirm.CustomerV2
              { contact =
                  Confirm.Contact
                    { phone =
                        Confirm.Phone
                          { phoneNumber = riderPhoneNumber,
                            phoneCountryCode = riderPhoneCountryCode
                          }
                    },
                person =
                  mbRiderName <&> \riderName ->
                    Confirm.OrderPersonV2
                      { name = riderName,
                        id = Nothing,
                        image = Nothing,
                        tags = Just [mkCustomerInfoTags]
                      }
              },
          vehicle =
            Confirm.Vehicle
              { category = vehicleVariant
              }
        }
  where
    mkCustomerInfoTags =
      Confirm.TagGroupV2
        { display = False,
          descriptor =
            Confirm.DescriptorV2
              { code = Just "customer_info",
                name = Just "Customer Information",
                short_desc = Nothing
              },
          list =
            [ Confirm.TagV2
                { display = (\_ -> Just False) =<< Just nightSafetyCheck,
                  descriptor =
                    Just
                      Confirm.DescriptorV2
                        { code = (\_ -> Just "night_safety_check") =<< Just nightSafetyCheck,
                          name = (\_ -> Just "Night Safety Check") =<< Just nightSafetyCheck,
                          short_desc = Nothing
                        },
                  value = (Just . show) =<< Just nightSafetyCheck
                }
            ]
        }

mkAddress :: DLA.LocationAddress -> Confirm.Address
mkAddress DLA.LocationAddress {..} =
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
      params =
        Confirm.PaymentParams
          { collected_by = Confirm.BPP,
            instrument = Nothing,
            currency = "INR",
            amount = Just $ realToFrac estimatedTotalFare
          },
      uri
    }

mkPaymentV2 :: Money -> Maybe Text -> Confirm.PaymentV2
mkPaymentV2 estimatedTotalFare uri =
  Confirm.PaymentV2
    { --id = Nothing,
      collectedBy = Confirm.BPP,
      _type = Confirm.ON_FULFILLMENT,
      params =
        Confirm.PaymentParamsV2
          { -- collected_by = Confirm.BPP,
            instrument = Nothing,
            currency = "INR",
            amount = Just $ realToFrac estimatedTotalFare
          },
      uri,
      status = Nothing,
      buyerAppFindeFeeType = Nothing,
      buyerAppFinderFeeAmount = Nothing,
      settlementDetails = Nothing
    }
