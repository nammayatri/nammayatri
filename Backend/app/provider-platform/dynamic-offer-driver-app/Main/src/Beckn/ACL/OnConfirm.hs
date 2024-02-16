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
import BecknV2.OnDemand.Enums
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQMerch

bookingStatusCode :: DConfirm.ValidatedQuote -> Maybe Text
bookingStatusCode (DConfirm.DriverQuote _ _) = Nothing
bookingStatusCode (DConfirm.StaticQuote _) = Just "NEW"
bookingStatusCode (DConfirm.RideOtpQuote _) = Just "NEW"

buildOnConfirmMessageV2 :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DConfirm.DConfirmResp -> m Spec.ConfirmReqMessage
buildOnConfirmMessageV2 res = do
  order <- tfOrder res
  return $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DConfirm.DConfirmResp -> m Spec.Order
tfOrder res = do
  fulfillments <- tfFulfillments res
  payments <- tfPayments res
  return $
    Spec.Order
      { orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderFulfillments = fulfillments,
        orderId = Just res.booking.id.getId,
        orderItems = tfItems res,
        orderPayments = payments,
        orderProvider = Nothing,
        orderQuote = tfQuotation res,
        orderStatus = Just "ACTIVE"
      }

-- Note: res.rideInfo will have value only in case of normal ride flow.
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
tfPayments :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DConfirm.DConfirmResp -> m (Maybe [Spec.Payment])
tfPayments res = do
  let amount = fromIntegral (res.booking.estimatedFare.getMoney)
  merchant <- CQMerch.findById res.booking.providerId >>= fromMaybeM (MerchantNotFound res.booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdAndDomain merchant.id "MOBILITY" >>= fromMaybeM (InternalError "Beckn Config not found")
  let mkParams :: (Maybe BknPaymentParams) = maybe Nothing (readMaybe . T.unpack) bppConfig.paymentParamsJson
  return $ Just $ DL.singleton $ mkPayment (show merchant.city) (show bppConfig.collectedBy) NOT_PAID (Just amount) Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

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
          Just $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = Just riderName,
                personTags = Nothing
              }
      }
