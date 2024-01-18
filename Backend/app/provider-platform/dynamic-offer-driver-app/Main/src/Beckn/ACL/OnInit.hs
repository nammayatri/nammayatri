{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils hiding (mkStops)
import qualified Beckn.OnDemand.Utils.OnInit as Utils
import Beckn.Types.Core.Taxi.OnInit as OnInit
import qualified BecknV2.OnDemand.Types as Spec
import Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.Prelude
import Kernel.Utils.Common (encodeToText)
import SharedLogic.FareCalculator

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res = do
  let rb = res.booking
      vehicleVariant = castVehicleVariant res.booking.vehicleVariant
      itemId = Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant
      fareDecimalValue = fromIntegral rb.estimatedFare
      currency = "INR"
      breakup_ =
        mkBreakupList (OnInit.BreakupItemPrice currency . fromIntegral) OnInit.BreakupItem rb.fareParams
          & filter (filterRequiredBreakups $ DFParams.getFareParametersType rb.fareParams) -- TODO: Remove after roll out
  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = res.booking.id.getId,
            items =
              [ OnInit.OrderItem
                  { id = itemId,
                    fulfillment_id = res.booking.quoteId,
                    price =
                      OnInit.Price
                        { currency,
                          value = fareDecimalValue
                        },
                    descriptor =
                      OnInit.Descriptor
                        { short_desc = Just itemId,
                          code = Nothing
                        }
                  }
              ],
            fulfillment =
              OnInit.FulfillmentInfo
                { id = res.booking.quoteId,
                  _type = buildFulfillmentType res.booking.bookingType,
                  start =
                    OnInit.StartInfo
                      { location =
                          OnInit.Location
                            { gps =
                                OnInit.Gps
                                  { lat = res.booking.fromLocation.lat,
                                    lon = res.booking.fromLocation.lon
                                  },
                              address = castAddress res.booking.fromLocation.address
                            },
                        authorization = Nothing
                      },
                  end =
                    Just
                      OnInit.StopInfo
                        { location =
                            OnInit.Location
                              { gps =
                                  OnInit.Gps
                                    { lat = res.booking.toLocation.lat,
                                      lon = res.booking.toLocation.lon
                                    },
                                address = castAddress res.booking.toLocation.address
                              }
                        },
                  vehicle =
                    OnInit.Vehicle
                      { category = vehicleVariant
                      },
                  agent =
                    res.driverName >>= \driverName ->
                      Just
                        OnInit.Agent
                          { name = driverName,
                            rateable = True,
                            tags = Nothing,
                            phone = Nothing,
                            image = Nothing
                          }
                },
            state = OnInit.NEW,
            quote =
              OnInit.Quote
                { price =
                    OnInit.QuotePrice
                      { currency,
                        value = fareDecimalValue,
                        offered_value = fareDecimalValue
                      },
                  breakup = Just breakup_
                },
            provider =
              res.driverId >>= \dId ->
                Just
                  OnInit.Provider
                    { id = dId
                    },
            payment =
              OnInit.Payment
                { params =
                    OnInit.PaymentParams
                      { collected_by = OnInit.BPP, --maybe OnInit.BPP (Common.castDPaymentCollector . (.collectedBy)) res.paymentMethodInfo,
                        instrument = Common.castDPaymentInstrument . (.paymentInstrument) <$> res.paymentMethodInfo,
                        currency = currency,
                        amount = Just fareDecimalValue
                      },
                  _type = maybe OnInit.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) res.paymentMethodInfo,
                  uri = res.booking.paymentUrl
                }
          }
    }
  where
    castAddress DL.LocationAddress {..} = OnInit.Address {area_code = areaCode, locality = area, ward = Nothing, ..}
    castVehicleVariant = \case
      VehVar.SEDAN -> OnInit.SEDAN
      VehVar.SUV -> OnInit.SUV
      VehVar.HATCHBACK -> OnInit.HATCHBACK
      VehVar.AUTO_RICKSHAW -> OnInit.AUTO_RICKSHAW
      VehVar.TAXI -> OnInit.TAXI
      VehVar.TAXI_PLUS -> OnInit.TAXI_PLUS
    buildFulfillmentType = \case
      DRB.NormalBooking -> OnInit.RIDE
      DRB.SpecialZoneBooking -> OnInit.RIDE_OTP
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.title == "BASE_FARE"
            || breakup.title == "SERVICE_CHARGE"
            || breakup.title == "DEAD_KILOMETER_FARE"
            || breakup.title == "EXTRA_DISTANCE_FARE"
            || breakup.title == "DRIVER_SELECTED_FARE"
            || breakup.title == "CUSTOMER_SELECTED_FARE"
            || breakup.title == "TOTAL_FARE"
            || breakup.title == "WAITING_OR_PICKUP_CHARGES"
            || breakup.title == "EXTRA_TIME_FARE"
        DFParams.Slab ->
          breakup.title == "BASE_FARE"
            || breakup.title == "SERVICE_CHARGE"
            || breakup.title == "WAITING_OR_PICKUP_CHARGES"
            || breakup.title == "PLATFORM_FEE"
            || breakup.title == "SGST"
            || breakup.title == "CGST"
            || breakup.title == "FIXED_GOVERNMENT_RATE"
            || breakup.title == "CUSTOMER_SELECTED_FARE"
            || breakup.title == "TOTAL_FARE"
            || breakup.title == "NIGHT_SHIFT_CHARGE"
            || breakup.title == "EXTRA_TIME_FARE"

mkOnInitMessageV2 :: DInit.InitRes -> Spec.ConfirmReqMessage
mkOnInitMessageV2 res =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res
    }

tfOrder :: DInit.InitRes -> Spec.Order
tfOrder res =
  Spec.Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderFulfillments = tfFulfillments res,
      orderId = Just res.booking.id.getId,
      orderItems = tfItems res,
      orderPayments = tfPayments res,
      orderProvider = tfProvider res,
      orderQuote = tfQuotation res,
      orderStatus = Just "NEW"
    }

tfFulfillments :: DInit.InitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just $
    [ Spec.Fulfillment
        { fulfillmentAgent = tfAgent res,
          fulfillmentCustomer = Nothing,
          fulfillmentId = Just res.booking.quoteId,
          fulfillmentState = Nothing,
          fulfillmentStops = Utils.mkStops res.booking.fromLocation res.booking.toLocation,
          fulfillmentTags = Nothing,
          fulfillmentType = Just $ mkFulfillmentType res.booking.bookingType,
          fulfillmentVehicle = tfVehicle res
        }
    ]
  where
    mkFulfillmentType = \case
      DRB.NormalBooking -> "RIDE"
      DRB.SpecialZoneBooking -> "RIDE_OTP"

tfItems :: DInit.InitRes -> Maybe [Spec.Item]
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

tfItemDescriptor :: DInit.InitRes -> Maybe Spec.Descriptor
tfItemDescriptor res =
  Just $
    Spec.Descriptor
      { descriptorCode = Nothing,
        descriptorShortDesc = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
        descriptorName = Nothing
      }

tfItemPrice :: DInit.InitRes -> Maybe Spec.Price
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
tfPayments :: DInit.InitRes -> Maybe [Spec.Payment]
tfPayments res = do
  let paymentMethodInfo = res.paymentMethodInfo
  Just $
    [ Spec.Payment
        { paymentCollectedBy = Just "BPP",
          paymentId = Nothing,
          paymentParams = mkParams paymentMethodInfo,
          paymentStatus = Nothing,
          paymentTags = Nothing,
          paymentType = Just $ maybe "ON_FULFILLMENT" (Utils.castDPaymentType . (.paymentType)) paymentMethodInfo
        }
    ]
  where
    mkParams _paymentMethodInfo =
      Just $
        Spec.PaymentParams
          { paymentParamsAmount = Just $ encodeToText res.booking.estimatedFare,
            paymentParamsBankAccountNumber = Nothing,
            paymentParamsBankCode = Nothing,
            paymentParamsCurrency = Just "INR",
            paymentParamsVirtualPaymentAddress = Nothing
          }

tfProvider :: DInit.InitRes -> Maybe Spec.Provider
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

tfQuotation :: DInit.InitRes -> Maybe Spec.Quotation
tfQuotation res =
  Just $
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup res,
        quotationPrice = tfQuotationPrice res,
        quotationTtl = Nothing
      }

tfQuotationPrice :: DInit.InitRes -> Maybe Spec.Price
tfQuotationPrice res =
  Just $
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Nothing,
        priceValue = Just $ encodeToText res.booking.estimatedFare
      }

mkQuotationBreakup :: DInit.InitRes -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup res =
  let rb = res.booking
      fareParams = mkBreakupList mkPrice mkQuotationBreakupInner rb.fareParams
   in Just $ filter (filterRequiredBreakups $ DFParams.getFareParametersType rb.fareParams) fareParams -- TODO: Remove after roll out
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

    filterRequiredBreakups :: DFParams.FareParametersType -> Spec.QuotationBreakupInner -> Bool
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.quotationBreakupInnerTitle == Just "BASE_FARE"
            || breakup.quotationBreakupInnerTitle == Just "SERVICE_CHARGE"
            || breakup.quotationBreakupInnerTitle == Just "DEAD_KILOMETER_FARE"
            || breakup.quotationBreakupInnerTitle == Just "EXTRA_DISTANCE_FARE"
            || breakup.quotationBreakupInnerTitle == Just "DRIVER_SELECTED_FARE"
            || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_SELECTED_FARE"
            || breakup.quotationBreakupInnerTitle == Just "TOTAL_FARE"
            || breakup.quotationBreakupInnerTitle == Just "WAITING_OR_PICKUP_CHARGES"
            || breakup.quotationBreakupInnerTitle == Just "EXTRA_TIME_FARE"
        DFParams.Slab ->
          breakup.quotationBreakupInnerTitle == Just "BASE_FARE"
            || breakup.quotationBreakupInnerTitle == Just "SERVICE_CHARGE"
            || breakup.quotationBreakupInnerTitle == Just "WAITING_OR_PICKUP_CHARGES"
            || breakup.quotationBreakupInnerTitle == Just "PLATFORM_FEE"
            || breakup.quotationBreakupInnerTitle == Just "SGST"
            || breakup.quotationBreakupInnerTitle == Just "CGST"
            || breakup.quotationBreakupInnerTitle == Just "FIXED_GOVERNMENT_RATE"
            || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_SELECTED_FARE"
            || breakup.quotationBreakupInnerTitle == Just "TOTAL_FARE"
            || breakup.quotationBreakupInnerTitle == Just "NIGHT_SHIFT_CHARGE"
            || breakup.quotationBreakupInnerTitle == Just "EXTRA_TIME_FARE"

tfVehicle :: DInit.InitRes -> Maybe Spec.Vehicle
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

tfAgent :: DInit.InitRes -> Maybe Spec.Agent
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
