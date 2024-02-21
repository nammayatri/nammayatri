{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm (buildOnConfirmMessageV2) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.List as L
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Utils.Common
import SharedLogic.FareCalculator

bookingStatusCode :: DConfirm.ValidatedQuote -> Maybe Text
bookingStatusCode (DConfirm.DriverQuote _ _) = Nothing -- TODO: refactor it like so case match is not needed
bookingStatusCode (DConfirm.StaticQuote _) = Just "NEW"
bookingStatusCode (DConfirm.RideOtpQuote _) = Just "NEW"

buildOnConfirmMessageV2 :: MonadFlow m => DConfirm.DConfirmResp -> DBC.BecknConfig -> m Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res becknConfig = do
  order <- tfOrder res becknConfig
  return $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: MonadFlow m => DConfirm.DConfirmResp -> DBC.BecknConfig -> m Spec.Order
tfOrder res becknConfig = do
  fulfillments <- tfFulfillments res
  cancellationTerms <- tfCancellationTerms becknConfig
  return $
    Spec.Order
      { orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Just cancellationTerms,
        orderFulfillments = fulfillments,
        orderId = Just res.booking.id.getId,
        orderItems = tfItems res,
        orderPayments = tfPayments res,
        orderProvider = Nothing,
        orderQuote = tfQuotation res,
        orderStatus = Just "ACTIVE"
      }

tfFulfillments :: MonadFlow m => DConfirm.DConfirmResp -> m (Maybe [Spec.Fulfillment])
tfFulfillments res = do
  let stops = Utils.mkStops' res.booking.fromLocation res.booking.toLocation res.booking.specialZoneOtpCode
  return $
    Just
      [ Spec.Fulfillment
          { fulfillmentAgent = Nothing,
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
    mkFulfillmentState quoteType = do
      fulfilState <- bookingStatusCode quoteType
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just fulfilState,
                    descriptorShortDesc = Nothing,
                    descriptorName = Nothing
                  }
          }

tfItems :: DConfirm.DConfirmResp -> Maybe [Spec.Item]
tfItems res =
  Just
    [ Spec.Item
        { itemDescriptor = Nothing,
          itemFulfillmentIds = Just [res.booking.quoteId],
          itemId = Just $ Common.mkItemId res.transporter.shortId.getShortId res.booking.vehicleVariant,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = tfItemPrice res,
          itemTags = Nothing
        }
    ]

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
        { paymentCollectedBy = Just $ show Enums.BPP,
          paymentId = Nothing,
          paymentParams = mkParams,
          paymentStatus = Nothing,
          paymentTags = Nothing,
          paymentType = Just $ show Enums.ON_FULFILLMENT
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
          Just $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just riderName,
                personTags = Nothing
              }
      }

tfCancellationTerms :: MonadFlow m => DBC.BecknConfig -> m [Spec.CancellationTerm]
tfCancellationTerms becknConfig =
  pure $
    L.singleton
      Spec.CancellationTerm
        { cancellationTermCancellationFee = Utils.tfCancellationFee becknConfig.cancellationFeeAmount becknConfig.cancellationFeePercentage,
          cancellationTermFulfillmentState = Nothing,
          cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
        }
