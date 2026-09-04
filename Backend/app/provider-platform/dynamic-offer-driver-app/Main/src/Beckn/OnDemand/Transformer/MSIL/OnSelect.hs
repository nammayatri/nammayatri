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
import qualified Beckn.OnDemand.Utils.MSIL.Breakup as MSILBreakup
import Beckn.OnDemand.Utils.MSIL.Category (scheduledCategoryCode)
import qualified Beckn.OnDemand.Utils.MSIL.FulfillmentType as MSILFulfillmentType
import qualified Beckn.OnDemand.Utils.MSIL.RouteInfo as MSILRouteInfo
import qualified Beckn.OnDemand.Utils.MSIL.VehicleEnergyType as MSILVehicleEnergyType
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

-- | Building: also adds ROUTE_INFO (WAYPOINTS + ENCODED_POLYLINE, from the
-- fallback route cached at search time) to the fulfillment's tags, overrides
-- fulfillment.type per the RIDE_OTP->SELF_PICKUP/otherwise->DELIVERY rule
-- (Beckn.OnDemand.Utils.MSIL.FulfillmentType), and overrides
-- vehicle.energy_type to a valid ONDC v2.1.0 code
-- (Beckn.OnDemand.Utils.MSIL.VehicleEnergyType). This whole function only
-- ever runs for pilot merchants (gated at the API/Beckn/Select.hs dispatch
-- level), so this applies unconditionally -- no separate gate needed.
mkOnSelectMessageV2FromQuote ::
  (CacheFlow m r, MonadFlow m) =>
  Bool ->
  DBC.BecknConfig ->
  DM.Merchant ->
  SearchRequest ->
  DQuote.Quote ->
  DVST.VehicleServiceTier ->
  Maybe FarePolicyD.FullFarePolicy ->
  UTCTime ->
  m Spec.OnSelectReqMessage
mkOnSelectMessageV2FromQuote isValueAddNP bppConfig merchant searchRequest quote vehicleServiceTierItem mbFarePolicy now = do
  let hasStops = not $ null searchRequest.stops
      fulfillment = mkFulfillmentFromQuote searchRequest quote
      paymentV2 = mkPaymentFromQuote bppConfig merchant quote
      order =
        MSILVehicleEnergyType.patchOrderVehicleEnergyType . MSILFulfillmentType.patchOrderFulfillmentTypes $
          emptyOrder
            { Spec.orderFulfillments = Just [fulfillment],
              Spec.orderItems = Just [mkItemFromQuote fulfillment vehicleServiceTierItem quote mbFarePolicy hasStops],
              Spec.orderQuote = Just $ mkQuoteFromQuote isValueAddNP quote now,
              Spec.orderPayments = Just [paymentV2],
              Spec.orderProvider = mkProviderFromQuote bppConfig
            }
  patchedOrder <- MSILRouteInfo.patchOrderRouteInfo searchRequest.transactionId order
  pure $ Spec.OnSelectReqMessage (Just patchedOrder)

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

mkItemFromQuote :: Spec.Fulfillment -> DVST.VehicleServiceTier -> DQuote.Quote -> Maybe FarePolicyD.FullFarePolicy -> Bool -> Spec.Item
mkItemFromQuote fulfillment vehicleServiceTierItem quote mbFarePolicy hasStops = do
  let fulfillmentId = fulfillment.fulfillmentId & fromMaybe (error $ "It should never happen as we have created fulfillment:-" <> show fulfillment)
  emptyItem
    { -- Quote has no separate estimateId the way DriverQuote does -- item.id and
      -- fulfillment.id both echo Quote.id (same value /on_search already sent).
      Spec.itemId = Just quote.id.getId,
      Spec.itemFulfillmentIds = Just [fulfillmentId],
      Spec.itemPrice = Just $ mkPriceFromQuote quote,
      Spec.itemTags = mkItemTagsFromQuote quote mbFarePolicy hasStops,
      Spec.itemDescriptor = mkItemDescriptorFromQuote quote vehicleServiceTierItem,
      -- Must agree with the provider-level category id on_search declared for
      -- this catalog (Beckn.OnDemand.Transformer.MSIL.OnSearch): scheduled
      -- quotes get SCHEDULED_TRIP/SCHEDULED_RENTAL, same as there; quotes from
      -- a plain (non-scheduled) MSIL search keep the ordinary ON_DEMAND_* code.
      Spec.itemCategoryIds = Just [mkQuoteCategoryId quote]
    }

mkQuoteCategoryId :: DQuote.Quote -> Text
mkQuoteCategoryId quote =
  let baseCode = Utils.tripCategoryToCategoryCode quote.tripCategory
   in if quote.isScheduled then scheduledCategoryCode baseCode else baseCode

-- | ONDC v2.1.0's TRV10 schema restricts items[*].descriptor.code to "RIDE" or
-- "RENTAL" -- the vehicle service tier (e.g. "COMFY") that used to be here
-- doesn't validate; the tier itself is still conveyed via descriptorName/
-- descriptorShortDesc, only the code changes. Same RIDE/RENTAL derivation as
-- on_search's Beckn.OnDemand.Transformer.MSIL.OnSearch.msilPatchCatalogCompliance.
mkItemDescriptorFromQuote :: DQuote.Quote -> DVST.VehicleServiceTier -> Maybe Spec.Descriptor
mkItemDescriptorFromQuote quote vehicleServiceTierItem =
  Just
    Spec.Descriptor
      { descriptorLongDesc = Nothing,
        descriptorCode = Just (if isRentalQuote quote then "RENTAL" else "RIDE"),
        descriptorShortDesc = vehicleServiceTierItem.shortDescription,
        descriptorName = Just vehicleServiceTierItem.name
      }

isRentalQuote :: DQuote.Quote -> Bool
isRentalQuote quote = "RENTAL" `T.isInfixOf` Utils.tripCategoryToCategoryCode quote.tripCategory

mkPriceFromQuote :: DQuote.Quote -> Spec.Price
mkPriceFromQuote quote =
  emptyPrice
    { Spec.priceCurrency = Just $ show quote.currency,
      Spec.priceValue = Just $ show quote.estimatedFare
    }

mkItemTagsFromQuote :: DQuote.Quote -> Maybe FarePolicyD.FullFarePolicy -> Bool -> Maybe [Spec.TagGroup]
mkItemTagsFromQuote quote mbFarePolicy hasStops =
  Utils.mkRateCardTag quote.distance quote.fareParams.customerCancellationDues Nothing quote.estimatedFare quote.fareParams.congestionChargeViaDp (Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy) Nothing Nothing Nothing hasStops

mkQuoteFromQuote :: Bool -> DQuote.Quote -> UTCTime -> Spec.Quotation
mkQuoteFromQuote isValueAddNP quote now = do
  let nominalDifferenceTime = diffUTCTime quote.validTill now
  Spec.Quotation
    { quotationBreakup = Just $ mkQuoteBreakupFromQuote isValueAddNP quote,
      quotationPrice = Just $ mkPriceFromQuote quote,
      quotationTtl = Just $ formatTimeDifference nominalDifferenceTime
    }

mkQuoteBreakupFromQuote :: Bool -> DQuote.Quote -> [Spec.QuotationBreakupInner]
mkQuoteBreakupFromQuote isValueAddNP quote = do
  let fareParams = mkFareParamsBreakups isValueAddNP mkBreakupPrice mkQuotationBreakupInner quote.fareParams
   in mapMaybe remapBreakup fareParams
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
    -- ONDC v2.1.0's TRV10 schema restricts quote.breakup[*].title to a fixed
    -- allowed vocabulary -- see Beckn.OnDemand.Utils.MSIL.Breakup for the
    -- actual remap table and why.
    remapBreakup breakup = case breakup.quotationBreakupInnerTitle >>= MSILBreakup.remapBreakupTitle of
      Nothing -> Nothing
      Just newTitle -> Just breakup {Spec.quotationBreakupInnerTitle = Just newTitle}

mkProviderFromQuote :: DBC.BecknConfig -> Maybe Spec.Provider
mkProviderFromQuote becknConfig =
  return $ emptyProvider {Spec.providerId = Just $ becknConfig.subscriberId}
