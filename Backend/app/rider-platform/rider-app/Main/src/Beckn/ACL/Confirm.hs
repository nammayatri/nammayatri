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

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
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

buildConfirmReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOnInit.OnInitRes ->
  m Spec.ConfirmReq
buildConfirmReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchant.defaultCity res.merchant.country

  let message = mkConfirmMessageV2 res
  pure $ Spec.ConfirmReq context message

mkConfirmMessageV2 :: DOnInit.OnInitRes -> Spec.ConfirmReqMessage
mkConfirmMessageV2 res = do
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res
    }

tfOrder :: DOnInit.OnInitRes -> Spec.Order
tfOrder res =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.bppBookingId.getId,
      orderItems = tfItems res,
      orderPayments = tfPayments res,
      orderProvider = tfProvider res,
      orderQuote = tfQuotation res,
      orderStatus = Nothing
    }

tfFulfillments :: DOnInit.OnInitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just $
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = res.fulfillmentId,
          fulfillmentState = Nothing,
          fulfillmentStops = Utils.mkStops' res.fromLocation res.mbToLocation,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ mkFulfillmentType res.bookingDetails,
          fulfillmentVehicle = tfVehicle res
        }
    ]
  where
    mkFulfillmentType = \case
      DRB.OneWaySpecialZoneDetails _ -> "RIDE_OTP"
      _ -> "RIDE"

tfItems :: DOnInit.OnInitRes -> Maybe [Spec.Item]
tfItems res =
  Just $
    [ Spec.Item
        { itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just res.itemId,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = Nothing,
          itemTags = Nothing
        }
    ]

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DOnInit.OnInitRes -> Maybe [Spec.Payment]
tfPayments res =
  Just $
    [ Spec.Payment
        { paymentCollectedBy = Just "BPP",
          paymentId = Nothing,
          paymentParams = mkParams,
          paymentStatus = Nothing,
          paymentTags = Nothing,
          paymentType = Just "ON_FULFILLMENT"
        }
    ]
  where
    mkParams =
      Just $
        Spec.PaymentParams
          { paymentParamsAmount = Just $ encodeToText res.estimatedTotalFare,
            paymentParamsBankAccountNumber = Nothing,
            paymentParamsBankCode = Nothing,
            paymentParamsCurrency = Just "INR",
            paymentParamsVirtualPaymentAddress = Nothing
          }

tfProvider :: DOnInit.OnInitRes -> Maybe Spec.Provider
tfProvider res = do
  driverId <- res.driverId
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just driverId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }

tfQuotation :: DOnInit.OnInitRes -> Maybe Spec.Quotation
tfQuotation res =
  Just $
    Spec.Quotation
      { quotationBreakup = Nothing,
        quotationPrice = tfQuotationPrice res,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DOnInit.OnInitRes -> Maybe Spec.Price
tfQuotationPrice res =
  Just $
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.estimatedTotalFare,
        priceValue = Just $ encodeToText res.estimatedFare
      }

tfCustomer :: DOnInit.OnInitRes -> Maybe Spec.Customer
tfCustomer res =
  Just $
    Spec.Customer
      { customerContact = mkContact,
        customerPerson = mkPerson
      }
  where
    mkContact =
      Just $
        Spec.Contact
          { contactPhone = Just res.riderPhoneNumber
          }

    mkPerson = do
      riderName <- res.mbRiderName
      return $
        Spec.Person
          { personId = Nothing,
            personImage = Nothing,
            personName = Just riderName,
            personTags = mkPersonTags
          }

    mkPersonTags =
      Just $
        [ Spec.TagGroup
            { tagGroupDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "customer_info",
                      descriptorName = Just "Customer Information",
                      descriptorShortDesc = Nothing
                    },
              tagGroupDisplay = Just False,
              tagGroupList = mkPersonTag
            }
        ]

    mkPersonTag =
      Just $
        [ Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "night_safety_check",
                      descriptorName = Just "Night Safety Check",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.nightSafetyCheck
            }
        ]

tfVehicle :: DOnInit.OnInitRes -> Maybe Spec.Vehicle
tfVehicle res =
  Just $
    Spec.Vehicle
      { vehicleCategory = Just $ castVehicleVariant res.vehicleVariant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing,
        vehicleVariant = Nothing
      }
  where
    castVehicleVariant = \case
      VehVar.SEDAN -> "SEDAN"
      VehVar.SUV -> "SUV"
      VehVar.HATCHBACK -> "HATCHBACK"
      VehVar.AUTO_RICKSHAW -> "AUTO_RICKSHAW"
      VehVar.TAXI -> "TAXI"
      VehVar.TAXI_PLUS -> "TAXI_PLUS"
