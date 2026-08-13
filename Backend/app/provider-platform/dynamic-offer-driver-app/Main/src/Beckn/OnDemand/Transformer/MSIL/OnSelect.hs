-- | MSIL pilot: /on_select builder for the new Quote-based (static/scheduled)
-- /select capability. There is no Layer 1 equivalent for this call at all --
-- /select is structurally skipped today for static-offer trips (a firm Quote
-- already exists from /search, so nothing to "select") -- so unlike
-- Beckn.OnDemand.Transformer.MSIL.Search/OnSearch, this module isn't patching
-- an existing builder's output, it *is* the first implementation. It mirrors
-- Beckn.ACL.OnSelect's existing DriverQuote-sourced mkOnSelectMessageV2 as
-- closely as the field differences allow, deliberately kept as a separate
-- small builder rather than a forced shared abstraction (see doc 23 s4): no
-- estimateId (Quote has none), no agent/driver fields (no driver assigned yet
-- at /select time for this flow -- assignment happens later, at Confirm-time
-- batching), vehicleServiceTier instead of vehicleVariant.
module Beckn.OnDemand.Transformer.MSIL.OnSelect
  ( mkOnSelectMessageV2FromQuote,
  )
where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import BecknV2.OnDemand.Utils.Constructors
import BecknV2.OnDemand.Utils.Payment
import BecknV2.Utils
import qualified Data.Text as T
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as DQuote
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as Variant
import Kernel.Prelude
import qualified Kernel.Types.Common as Common (mkPrice)
import Kernel.Utils.Common
import SharedLogic.FareCalculator (mkFareParamsBreakups)

mkOnSelectMessageV2FromQuote ::
  DBC.BecknConfig ->
  DM.Merchant ->
  SearchRequest ->
  DQuote.Quote ->
  DVST.VehicleServiceTier ->
  Maybe FarePolicyD.FullFarePolicy ->
  UTCTime ->
  Spec.OnSelectReqMessage
mkOnSelectMessageV2FromQuote bppConfig merchant searchRequest quote vehicleServiceTierItem mbFarePolicy now = do
  let fulfillment = mkFulfillmentFromQuote searchRequest quote
      paymentV2 = mkPaymentFromQuote bppConfig merchant quote
  Spec.OnSelectReqMessage $
    Just
      emptyOrder
        { Spec.orderFulfillments = Just [fulfillment],
          Spec.orderItems = Just [mkItemFromQuote fulfillment vehicleServiceTierItem quote mbFarePolicy],
          Spec.orderQuote = Just $ mkQuoteFromQuote quote now,
          Spec.orderPayments = Just [paymentV2],
          Spec.orderProvider = mkProviderFromQuote bppConfig
        }

mkFulfillmentFromQuote :: SearchRequest -> DQuote.Quote -> Spec.Fulfillment
mkFulfillmentFromQuote searchRequest quote =
  emptyFulfillment
    { Spec.fulfillmentId = Just quote.id.getId,
      Spec.fulfillmentStops = Utils.mkStops' searchRequest.fromLocation searchRequest.toLocation searchRequest.stops Nothing Nothing (Just searchRequest.startTime) (Utils.mkScheduledPickupDuration searchRequest.isScheduled),
      Spec.fulfillmentVehicle = Just $ mkVehicleFromQuote quote,
      Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType quote.tripCategory
    }

mkVehicleFromQuote :: DQuote.Quote -> Spec.Vehicle
mkVehicleFromQuote quote =
  let (category, variant) = Utils.castVariant (Variant.castServiceTierToVariant quote.vehicleServiceTier)
   in emptyVehicle
        { Spec.vehicleCategory = Just category,
          Spec.vehicleVariant = Just variant
        }

mkPaymentFromQuote :: DBC.BecknConfig -> DM.Merchant -> DQuote.Quote -> Spec.Payment
mkPaymentFromQuote bppConfig merchant quote = do
  let mPrice = Just $ Common.mkPrice (Just quote.currency) quote.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

mkItemFromQuote :: Spec.Fulfillment -> DVST.VehicleServiceTier -> DQuote.Quote -> Maybe FarePolicyD.FullFarePolicy -> Spec.Item
mkItemFromQuote fulfillment vehicleServiceTierItem quote mbFarePolicy = do
  let fulfillmentId = fulfillment.fulfillmentId & fromMaybe (error $ "It should never happen as we have created fulfillment:-" <> show fulfillment)
  emptyItem
    { -- Quote has no separate estimateId the way DriverQuote does -- item.id and
      -- fulfillment.id both echo Quote.id (same value /on_search already sent).
      Spec.itemId = Just quote.id.getId,
      Spec.itemFulfillmentIds = Just [fulfillmentId],
      Spec.itemPrice = Just $ mkPriceFromQuote quote,
      Spec.itemTags = mkItemTagsFromQuote quote mbFarePolicy,
      Spec.itemDescriptor = mkItemDescriptorFromQuote vehicleServiceTierItem,
      Spec.itemCategoryIds = Just [Utils.tripCategoryToCategoryCode quote.tripCategory]
    }

mkItemDescriptorFromQuote :: DVST.VehicleServiceTier -> Maybe Spec.Descriptor
mkItemDescriptorFromQuote vehicleServiceTierItem =
  Just
    Spec.Descriptor
      { descriptorLongDesc = Nothing,
        descriptorCode = Just $ show vehicleServiceTierItem.serviceTierType,
        descriptorShortDesc = vehicleServiceTierItem.shortDescription,
        descriptorName = Just vehicleServiceTierItem.name
      }

mkPriceFromQuote :: DQuote.Quote -> Spec.Price
mkPriceFromQuote quote =
  emptyPrice
    { Spec.priceCurrency = Just $ show quote.currency,
      Spec.priceValue = Just $ show quote.estimatedFare
    }

mkItemTagsFromQuote :: DQuote.Quote -> Maybe FarePolicyD.FullFarePolicy -> Maybe [Spec.TagGroup]
mkItemTagsFromQuote quote mbFarePolicy =
  Utils.mkRateCardTag quote.distance quote.fareParams.customerCancellationDues Nothing quote.estimatedFare quote.fareParams.congestionChargeViaDp (Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy) Nothing Nothing Nothing

mkQuoteFromQuote :: DQuote.Quote -> UTCTime -> Spec.Quotation
mkQuoteFromQuote quote now = do
  let nominalDifferenceTime = diffUTCTime quote.validTill now
  Spec.Quotation
    { quotationBreakup = Just $ mkQuoteBreakupFromQuote quote,
      quotationPrice = Just $ mkPriceFromQuote quote,
      quotationTtl = Just $ formatTimeDifference nominalDifferenceTime
    }

mkQuoteBreakupFromQuote :: DQuote.Quote -> [Spec.QuotationBreakupInner]
mkQuoteBreakupFromQuote quote = do
  let fareParams = mkFareParamsBreakups mkBreakupPrice mkQuotationBreakupInner quote.fareParams
   in filter filterRequiredBreakups fareParams
  where
    mkBreakupPrice money =
      Just
        emptyPrice
          { Spec.priceCurrency = Just $ show quote.currency,
            Spec.priceValue = Just $ encodeToText money
          }
    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }
    filterRequiredBreakups breakup =
      breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.RIDE_STOP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.PER_STOP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_VAT)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_FARE_TAX_EXCLUSIVE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_FARE_TAX)

mkProviderFromQuote :: DBC.BecknConfig -> Maybe Spec.Provider
mkProviderFromQuote becknConfig =
  return $ emptyProvider {Spec.providerId = Just $ becknConfig.subscriberId}
