{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmMessage, buildOnConfirmMessageV2) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking as DConfirm
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.Prelude
import Kernel.Types.Beckn.DecimalValue
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator

buildOnConfirmMessage :: MonadFlow m => DConfirm.DConfirmRes -> m OnConfirm.OnConfirmMessage
buildOnConfirmMessage res = do
  let booking = res.booking
  let vehicleVariant = Common.castVariant res.booking.vehicleVariant
  let itemId = Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant
      fareParams = booking.fareParams
      totalFareDecimal = fromIntegral booking.estimatedFare
      currency = "INR"
  fulfillmentDetails <- case booking.bookingType of
    DConfirm.SpecialZoneBooking -> do
      otpCode <- booking.specialZoneOtpCode & fromMaybeM (OtpNotFoundForSpecialZoneBooking booking.id.getId)
      return $ mkSpecialZoneFulfillmentInfo res.fromLocation res.toLocation otpCode booking.quoteId OnConfirm.RIDE_OTP res.riderPhoneNumber res.riderMobileCountryCode res.riderName vehicleVariant
    DConfirm.NormalBooking -> return $ mkFulfillmentInfo res.fromLocation res.toLocation booking.quoteId OnConfirm.RIDE res.driverName res.riderPhoneNumber res.riderMobileCountryCode res.riderName vehicleVariant
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
                      Just $
                        mkBreakupList
                          (OnConfirm.BreakupItemPrice currency . fromIntegral)
                          OnConfirm.BreakupItem
                          fareParams
                  },
              provider =
                res.driverId >>= \dId ->
                  Just $
                    OnConfirm.Provider
                      { id = dId
                      },
              payment =
                OnConfirm.Payment
                  { params =
                      OnConfirm.PaymentParams
                        { collected_by = OnConfirm.BPP,
                          instrument = Nothing,
                          currency = currency,
                          amount = Just totalFareDecimal
                        },
                    _type = OnConfirm.ON_FULFILLMENT,
                    uri = booking.paymentUrl
                  }
            }
      }

mkOrderItem :: Text -> Text -> Text -> DecimalValue -> OnConfirm.OrderItem
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
          { short_desc = Just itemId,
            code = Nothing
          }
    }

mklocation :: DL.Location -> OnConfirm.Location
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
    castAddress DL.LocationAddress {..} = OnConfirm.Address {area_code = areaCode, locality = area, ward = Nothing, ..}

mkFulfillmentInfo :: DL.Location -> DL.Location -> Text -> OnConfirm.FulfillmentType -> Maybe Text -> Text -> Text -> Maybe Text -> OnConfirm.VehicleVariant -> OnConfirm.FulfillmentInfo
mkFulfillmentInfo fromLoc toLoc fulfillmentId fulfillmentType driverName riderPhoneNumber riderMobileCountryCode mbRiderName vehicleVariant =
  OnConfirm.FulfillmentInfo
    { id = fulfillmentId,
      _type = fulfillmentType,
      state =
        OnConfirm.FulfillmentState
          { descriptor =
              OnConfirm.Descriptor
                { short_desc = Nothing,
                  code = Just "TRIP_ASSIGNED"
                }
          },
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
      vehicle =
        OnConfirm.Vehicle
          { category = vehicleVariant
          },
      customer =
        OnConfirm.Customer
          { contact =
              OnConfirm.Contact
                { phone =
                    OnConfirm.Phone
                      { phoneNumber = riderPhoneNumber,
                        phoneCountryCode = riderMobileCountryCode
                      }
                },
            person =
              mbRiderName <&> \riderName ->
                OnConfirm.OrderPerson
                  { name = riderName,
                    tags = Nothing
                  }
          },
      agent =
        driverName >>= \dName ->
          Just
            OnConfirm.Agent
              { name = dName,
                rateable = True,
                tags = Nothing,
                phone = Nothing,
                image = Nothing
              }
    }

mkSpecialZoneFulfillmentInfo :: DL.Location -> DL.Location -> Text -> Text -> OnConfirm.FulfillmentType -> Text -> Text -> Maybe Text -> OnConfirm.VehicleVariant -> OnConfirm.FulfillmentInfo
mkSpecialZoneFulfillmentInfo fromLoc toLoc otp fulfillmentId fulfillmentType riderPhoneNumber riderMobileCountryCode mbRiderName vehicleVariant = do
  let authorization =
        Just $
          OnConfirm.Authorization
            { _type = "OTP",
              token = otp
            }
  OnConfirm.FulfillmentInfo
    { id = fulfillmentId,
      _type = fulfillmentType,
      state =
        OnConfirm.FulfillmentState
          { descriptor =
              OnConfirm.Descriptor
                { code = Just "NEW",
                  short_desc = Nothing
                }
          },
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
      vehicle =
        OnConfirm.Vehicle
          { category = vehicleVariant
          },
      customer =
        OnConfirm.Customer
          { contact =
              OnConfirm.Contact
                { phone =
                    OnConfirm.Phone
                      { phoneNumber = riderPhoneNumber,
                        phoneCountryCode = riderMobileCountryCode
                      }
                },
            person =
              mbRiderName <&> \riderName ->
                OnConfirm.OrderPerson
                  { name = riderName,
                    tags = Nothing
                  }
          },
      agent = Nothing
    }

buildOnConfirmMessageV2 :: MonadFlow m => DConfirm.DConfirmRes -> m Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res = do
  order <- tfOrder res
  return $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: MonadFlow m => DConfirm.DConfirmRes -> m Spec.Order
tfOrder res = do
  fulfillments <- tfFulfillments res
  return $
    Spec.Order
      { orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderFulfillments = fulfillments,
        orderId = Just res.booking.id.getId,
        orderItems = tfItems res,
        orderPayments = tfPayments res,
        orderProvider = tfProvider res,
        orderQuote = tfQuotation res,
        orderStatus = Just "ACTIVE"
      }

tfFulfillments :: MonadFlow m => DConfirm.DConfirmRes -> m (Maybe [Spec.Fulfillment])
tfFulfillments res = do
  stops <- mkStops res.booking.bookingType
  return $
    Just $
      [ Spec.Fulfillment
          { fulfillmentAgent = tfAgent res,
            fulfillmentCustomer = tfCustomer res,
            fulfillmentId = Just res.booking.quoteId,
            fulfillmentState = mkFulfillmentState res.booking.bookingType,
            fulfillmentStops = stops,
            fulfillmentTags = Nothing,
            fulfillmentType = Just $ mkFulfillmentType res.booking.bookingType,
            fulfillmentVehicle = tfVehicle res
          }
      ]
  where
    mkFulfillmentType = \case
      DConfirm.NormalBooking -> "RIDE"
      DConfirm.SpecialZoneBooking -> "RIDE_OTP"

    mkFulfillmentState = \case
      DConfirm.NormalBooking ->
        Just $
          Spec.FulfillmentState
            { fulfillmentStateDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "TRIP_ASSIGNED",
                      descriptorShortDesc = Nothing,
                      descriptorName = Nothing
                    }
            }
      DConfirm.SpecialZoneBooking ->
        Just $
          Spec.FulfillmentState
            { fulfillmentStateDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just "NEW",
                      descriptorShortDesc = Nothing,
                      descriptorName = Nothing
                    }
            }

    mkStops = \case
      DConfirm.NormalBooking -> return $ Utils.mkStops' res.booking.fromLocation res.booking.toLocation Nothing
      DConfirm.SpecialZoneBooking -> do
        otpCode <- res.booking.specialZoneOtpCode & fromMaybeM (OtpNotFoundForSpecialZoneBooking res.booking.id.getId)
        return $ Utils.mkStops' res.booking.fromLocation res.booking.toLocation (Just otpCode)

tfItems :: DConfirm.DConfirmRes -> Maybe [Spec.Item]
tfItems res =
  Just $
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor res,
          itemFulfillmentIds = Just [res.booking.quoteId],
          itemId = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = tfItemPrice res,
          itemTags = Nothing
        }
    ]

tfItemDescriptor :: DConfirm.DConfirmRes -> Maybe Spec.Descriptor
tfItemDescriptor res =
  Just $
    Spec.Descriptor
      { descriptorCode = Nothing,
        descriptorShortDesc = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
        descriptorName = Nothing
      }

tfItemPrice :: DConfirm.DConfirmRes -> Maybe Spec.Price
tfItemPrice res =
  Just $
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Nothing,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DConfirm.DConfirmRes -> Maybe [Spec.Payment]
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
          { paymentParamsAmount = Just $ encodeToText res.booking.estimatedFare,
            paymentParamsBankAccountNumber = Nothing,
            paymentParamsBankCode = Nothing,
            paymentParamsCurrency = Just "INR",
            paymentParamsVirtualPaymentAddress = Nothing
          }

tfProvider :: DConfirm.DConfirmRes -> Maybe Spec.Provider
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

tfQuotation :: DConfirm.DConfirmRes -> Maybe Spec.Quotation
tfQuotation res =
  Just $
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup res,
        quotationPrice = tfQuotationPrice res,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DConfirm.DConfirmRes -> Maybe Spec.Price
tfQuotationPrice res =
  Just $
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.booking.estimatedFare,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

mkQuotationBreakup :: DConfirm.DConfirmRes -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup res =
  Just $
    mkBreakupList mkPrice mkQuotationBreakupInner res.booking.fareParams
  where
    mkPrice money =
      Just $
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText money
          }

    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }

tfVehicle :: DConfirm.DConfirmRes -> Maybe Spec.Vehicle
tfVehicle res =
  Just $
    Spec.Vehicle
      { vehicleCategory = Just $ castVehicleVariant res.booking.vehicleVariant,
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

tfAgent :: DConfirm.DConfirmRes -> Maybe Spec.Agent
tfAgent res = do
  driverName <- res.driverName
  return $
    Spec.Agent
      { agentContact = Nothing,
        agentPerson =
          Just $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just driverName,
                personTags = Nothing
              }
      }

tfCustomer :: DConfirm.DConfirmRes -> Maybe Spec.Customer
tfCustomer res =
  return $
    Spec.Customer
      { customerContact =
          Just $
            Spec.Contact
              { contactPhone = Just res.riderPhoneNumber -- TODO: Check with ONDC how to pass country code
              },
        customerPerson = do
          riderName <- res.riderName
          return $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just riderName,
                personTags = Nothing
              }
      }
