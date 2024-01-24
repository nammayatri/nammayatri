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
import Kernel.Prelude
import Kernel.Types.Beckn.DecimalValue
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator

buildOnConfirmMessage :: MonadFlow m => DConfirm.DConfirmResp -> m OnConfirm.OnConfirmMessage
buildOnConfirmMessage res = do
  let booking = res.booking
  let vehicleVariant = Common.castVariant res.booking.vehicleVariant
  let itemId = Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant
      fareParams = booking.fareParams
      totalFareDecimal = fromIntegral booking.estimatedFare
      currency = "INR"
  fulfillmentDetails <- mkFulfillmentInfo booking res.quoteType res.fromLocation res.toLocation booking.quoteId booking.specialZoneOtpCode res.rideInfo res.riderPhoneNumber res.riderMobileCountryCode res.riderName vehicleVariant
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
                        mkFareParamsBreakups
                          (OnConfirm.BreakupItemPrice currency . fromIntegral)
                          OnConfirm.BreakupItem
                          fareParams
                  },
              provider =
                res.rideInfo >>= \rideInfo ->
                  Just $
                    OnConfirm.Provider
                      { id = rideInfo.driver.id.getId
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

mkFulfillmentInfo :: MonadFlow m => DConfirm.Booking -> DConfirm.ValidatedQuote -> DL.Location -> Maybe DL.Location -> Text -> Maybe Text -> Maybe DConfirm.RideInfo -> Text -> Text -> Maybe Text -> OnConfirm.VehicleVariant -> m OnConfirm.FulfillmentInfo
mkFulfillmentInfo booking quoteType fromLoc mbToLoc fulfillmentId mbOtp mbRideInfo riderPhoneNumber riderMobileCountryCode mbRiderName vehicleVariant = do
  authorization <-
    case quoteType of
      DConfirm.DriverQuote _ _ -> pure Nothing
      DConfirm.StaticQuote _ -> pure Nothing
      DConfirm.RideOtpQuote _ -> do
        otp <- mbOtp & fromMaybeM (OtpNotFoundForSpecialZoneBooking booking.id.getId)
        pure $
          Just $
            OnConfirm.Authorization
              { _type = "OTP",
                token = otp
              }

  let agent =
        mbRideInfo >>= \rideInfo ->
          Just
            OnConfirm.Agent
              { name = rideInfo.driver.firstName,
                rateable = True,
                tags = Nothing,
                phone = Nothing,
                image = Nothing
              }
  return $
    OnConfirm.FulfillmentInfo
      { id = fulfillmentId,
        _type = Common.mkFulfillmentType booking.tripCategory,
        state =
          OnConfirm.FulfillmentState
            { descriptor =
                OnConfirm.Descriptor
                  { short_desc = Nothing,
                    code = Just (bookingStatusCode quoteType)
                  }
            },
        start =
          OnConfirm.StartInfo
            { location = mklocation fromLoc,
              authorization = authorization
            },
        end =
          ( \toLoc ->
              OnConfirm.StopInfo
                { location = mklocation toLoc
                }
          )
            <$> mbToLoc,
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
        agent = agent
      }

bookingStatusCode :: DConfirm.ValidatedQuote -> Text
bookingStatusCode (DConfirm.DriverQuote _ _) = "TRIP_ASSIGNED"
bookingStatusCode (DConfirm.StaticQuote _) = "NEW"
bookingStatusCode (DConfirm.RideOtpQuote _) = "NEW"

buildOnConfirmMessageV2 :: MonadFlow m => DConfirm.DConfirmResp -> m Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res = do
  order <- tfOrder res
  return $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: MonadFlow m => DConfirm.DConfirmResp -> m Spec.Order
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

tfFulfillments :: MonadFlow m => DConfirm.DConfirmResp -> m (Maybe [Spec.Fulfillment])
tfFulfillments res = do
  let stops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.specialZoneOtpCode
  return $
    Just
      [ Spec.Fulfillment
          { fulfillmentAgent = tfAgent res,
            fulfillmentCustomer = tfCustomer res,
            fulfillmentId = Just res.booking.quoteId,
            fulfillmentState = mkFulfillmentState res.quoteType,
            fulfillmentStops = stops,
            fulfillmentTags = Nothing,
            fulfillmentType = Just $ Common.mkFulfillmentType res.booking.tripCategory,
            fulfillmentVehicle = tfVehicle res
          }
      ]
  where
    mkFulfillmentState quoteType =
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just (bookingStatusCode quoteType),
                    descriptorShortDesc = Nothing,
                    descriptorName = Nothing
                  }
          }

tfItems :: DConfirm.DConfirmResp -> Maybe [Spec.Item]
tfItems res =
  Just
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

tfItemDescriptor :: DConfirm.DConfirmResp -> Maybe Spec.Descriptor
tfItemDescriptor res =
  Just
    Spec.Descriptor
      { descriptorCode = Nothing,
        descriptorShortDesc = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
        descriptorName = Nothing
      }

tfItemPrice :: DConfirm.DConfirmResp -> Maybe Spec.Price
tfItemPrice res =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Nothing,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

-- TODO: Discuss payment info transmission with ONDC
tfPayments :: DConfirm.DConfirmResp -> Maybe [Spec.Payment]
tfPayments res =
  Just
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
      Just
        Spec.PaymentParams
          { paymentParamsAmount = Just $ encodeToText res.booking.estimatedFare,
            paymentParamsBankAccountNumber = Nothing,
            paymentParamsBankCode = Nothing,
            paymentParamsCurrency = Just "INR",
            paymentParamsVirtualPaymentAddress = Nothing
          }

tfProvider :: DConfirm.DConfirmResp -> Maybe Spec.Provider
tfProvider res = do
  rideInfo <- res.rideInfo
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just rideInfo.driver.id.getId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }

tfQuotation :: DConfirm.DConfirmResp -> Maybe Spec.Quotation
tfQuotation res =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup res,
        quotationPrice = tfQuotationPrice res,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DConfirm.DConfirmResp -> Maybe Spec.Price
tfQuotationPrice res =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText res.booking.estimatedFare,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

mkQuotationBreakup :: DConfirm.DConfirmResp -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup res =
  -- TODO::Beckn, `quotationBreakupInnerTitle` may not be according to spec.
  Just $
    mkFareParamsBreakups mkPrice mkQuotationBreakupInner res.booking.fareParams
  where
    mkPrice money =
      Just
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

tfVehicle :: DConfirm.DConfirmResp -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVariant res.booking.vehicleVariant
  Just
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing
      }

tfAgent :: DConfirm.DConfirmResp -> Maybe Spec.Agent
tfAgent res = do
  rideInfo <- res.rideInfo
  return $
    Spec.Agent
      { agentContact = Nothing,
        agentPerson =
          Just
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just rideInfo.driver.firstName,
                personTags = Nothing
              }
      }

tfCustomer :: DConfirm.DConfirmResp -> Maybe Spec.Customer
tfCustomer res =
  return $
    Spec.Customer
      { customerContact =
          Just
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
