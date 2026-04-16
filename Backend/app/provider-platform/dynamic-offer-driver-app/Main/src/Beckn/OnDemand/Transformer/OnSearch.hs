module Beckn.OnDemand.Transformer.OnSearch where

import qualified Beckn.OnDemand.Utils.Common
import Beckn.OnDemand.Utils.OnSearch
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Context
import qualified Data.List
import qualified Data.Text
import qualified Domain.Action.Beckn.Search
import qualified Domain.Types
import Domain.Types.BecknConfig as DBC
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Price

buildOnSearchMessage :: Domain.Action.Beckn.Search.DSearchRes -> DBC.BecknConfig -> Bool -> Maybe BecknV2.OnDemand.Types.OnSearchReqMessage
buildOnSearchMessage res bppConfig isValueAddNP = do
  let onSearchReqMessageCatalog_ = tfCatalog res bppConfig isValueAddNP
      returnData = BecknV2.OnDemand.Types.OnSearchReqMessage {onSearchReqMessageCatalog = onSearchReqMessageCatalog_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

buildOnSearchRideReq :: (Monad m, Kernel.Types.App.MonadFlow m) => Kernel.Prelude.Text -> DBC.BecknConfig -> Domain.Action.Beckn.Search.DSearchRes -> Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Data.Text.Text -> Maybe Data.Text.Text -> Data.Text.Text -> Kernel.Prelude.BaseUrl -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Beckn.Context.Country -> Bool -> m BecknV2.OnDemand.Types.OnSearchReq
buildOnSearchRideReq onSearchTtl bppConfig res action domain messageId transactionId bapId bapUri bppId bppUri city country isValueAddNP = do
  let onSearchReqError_ = Nothing
  onSearchReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2_1 action domain messageId transactionId bapId bapUri bppId bppUri city country (Just onSearchTtl)
  let onSearchReqMessage_ = buildOnSearchMessage res bppConfig isValueAddNP
  pure $ BecknV2.OnDemand.Types.OnSearchReq {onSearchReqContext = onSearchReqContext_, onSearchReqError = onSearchReqError_, onSearchReqMessage = onSearchReqMessage_}

tfCatalog :: Domain.Action.Beckn.Search.DSearchRes -> DBC.BecknConfig -> Bool -> BecknV2.OnDemand.Types.Catalog
tfCatalog res bppConfig isValueAddNP = do
  let catalogDescriptor_ = tfCatalogDescriptor res
      catalogProviders_ = tfCatalogProviders res bppConfig isValueAddNP & Just . Data.List.singleton
      catalogTags_ = Beckn.OnDemand.Utils.Common.mkBppTermsTags bppConfig
  BecknV2.OnDemand.Types.Catalog {catalogDescriptor = catalogDescriptor_, catalogProviders = catalogProviders_, catalogTags = catalogTags_}

tfCatalogDescriptor :: Domain.Action.Beckn.Search.DSearchRes -> Maybe BecknV2.OnDemand.Types.Descriptor
tfCatalogDescriptor res = do
  let descriptorCode_ = Nothing
      descriptorName_ = Just res.provider.name
      descriptorShortDesc_ = Nothing
      returnData = BecknV2.OnDemand.Types.Descriptor {descriptorCode = descriptorCode_, descriptorName = descriptorName_, descriptorShortDesc = descriptorShortDesc_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfCatalogProviders :: Domain.Action.Beckn.Search.DSearchRes -> DBC.BecknConfig -> Bool -> BecknV2.OnDemand.Types.Provider
tfCatalogProviders res bppConfig isValueAddNP = do
  let providerId_ = Just bppConfig.subscriberId
      providerLocations_ = Just $ Beckn.OnDemand.Utils.OnSearch.mkProviderLocationsWithDefault res.fromLocation ((map (\(_, _, c, _) -> c) res.estimates) <> (map (\(_, _, c, _) -> c) res.quotes))
      providerPayments_ = Just $ mkPayment res.provider bppConfig Nothing
      providerDescriptor_ = tfCatalogDescriptor res
      pricings = (map (Beckn.OnDemand.Utils.Common.convertEstimateToPricing res.specialLocationName) res.estimates) <> (map (Beckn.OnDemand.Utils.Common.convertQuoteToPricing res.specialLocationName) res.quotes)
      providerFulfillments_ = map (tfProviderFulfillments res) pricings & Just
      providerItems_ = Just $ map (tfProviderItems res isValueAddNP) pricings
      providerCategories_ = Just $ Beckn.OnDemand.Utils.OnSearch.mkTripCategories pricings
      providerTags_ = Nothing
  BecknV2.OnDemand.Types.Provider {providerCategories = providerCategories_, providerDescriptor = providerDescriptor_, providerFulfillments = providerFulfillments_, providerId = providerId_, providerItems = providerItems_, providerLocations = providerLocations_, providerPayments = providerPayments_, providerTags = providerTags_}

tfItemPrice :: Beckn.OnDemand.Utils.Common.Pricing -> Maybe BecknV2.OnDemand.Types.Price
tfItemPrice pricing = do
  let priceComputedValue_ = Nothing
      priceCurrency_ = Just $ show pricing.currency
      priceMaximumValue_ = Just $ Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just pricing.currency) pricing.pricingMaxFare
      priceMinimumValue_ = Just $ Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just pricing.currency) pricing.pricingMinFare
      priceOfferedValue_ = Nothing
      priceValue_ = Just $ Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just pricing.currency) pricing.pricingMinFare
      returnData = BecknV2.OnDemand.Types.Price {priceComputedValue = priceComputedValue_, priceCurrency = priceCurrency_, priceMaximumValue = priceMaximumValue_, priceMinimumValue = priceMinimumValue_, priceOfferedValue = priceOfferedValue_, priceValue = priceValue_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfProviderFulfillments :: Domain.Action.Beckn.Search.DSearchRes -> Beckn.OnDemand.Utils.Common.Pricing -> BecknV2.OnDemand.Types.Fulfillment
tfProviderFulfillments res pricing = do
  let fulfillmentAgent_ = Nothing
      fulfillmentCustomer_ = Nothing
      fulfillmentId_ = Just pricing.pricingId
      fulfillmentState_ = Nothing
      fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops res.fromLocation res.toLocation res.stops
      fulfillmentTags_ = Nothing
      fulfillmentType_ = Just pricing.fulfillmentType
      fulfillmentVehicle_ = tfVehicle pricing
  BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}

tfProviderItems :: Domain.Action.Beckn.Search.DSearchRes -> Bool -> Beckn.OnDemand.Utils.Common.Pricing -> BecknV2.OnDemand.Types.Item
tfProviderItems res _isValueAddNP pricing = do
  let itemDescriptor_ = tfItemDescriptor pricing
      itemFulfillmentIds_ = Just [pricing.pricingId]
      itemId_ = Just $ "I-" <> pricing.pricingId
      itemLocationIds_ = Beckn.OnDemand.Utils.OnSearch.mkItemLocationIds ((map (\(_, _, c, _) -> c) res.estimates) <> (map (\(_, _, c, _) -> c) res.quotes))
      itemPaymentIds_ = Nothing
      itemTags_ = Just [Beckn.OnDemand.Utils.Common.mkFeatureListTagsForVariant pricing.vehicleVariant]
      itemPrice_ = tfItemPrice pricing
      itemCategoryIds_ = Just [Beckn.OnDemand.Utils.Common.tripCategoryToCategoryCodeWithSchedule pricing.isScheduled pricing.tripCategory]
  BecknV2.OnDemand.Types.Item {itemCancellationTerms = Just mkItemCancellationTerms, itemCategoryIds = itemCategoryIds_, itemDescriptor = itemDescriptor_, itemFulfillmentIds = itemFulfillmentIds_, itemId = itemId_, itemLocationIds = itemLocationIds_, itemPaymentIds = itemPaymentIds_, itemPrice = itemPrice_, itemTags = itemTags_}

-- | Build cancellation terms for on_search items per ONDC spec.
-- State-based cancellation fees:
--   RIDE_ASSIGNED: 0% fee
--   RIDE_ENROUTE_PICKUP: cancellation fee amount
--   RIDE_ARRIVED_PICKUP: cancellation fee amount
--   RIDE_STARTED: 100% fee
mkItemCancellationTerms :: [BecknV2.OnDemand.Types.CancellationTerm]
mkItemCancellationTerms =
  let mkAmountFee val = Just $ BecknV2.OnDemand.Types.Fee {feeAmount = Just BecknV2.OnDemand.Utils.Constructors.emptyPrice {BecknV2.OnDemand.Types.priceCurrency = Just "INR", BecknV2.OnDemand.Types.priceValue = Just val}, feePercentage = Nothing}
   in [ BecknV2.OnDemand.Types.CancellationTerm
          { cancellationTermCancellationFee = Just $ BecknV2.OnDemand.Types.Fee {feeAmount = Nothing, feePercentage = Just "0"},
            cancellationTermFulfillmentState = Just $ mkCancellationFulfillmentState "RIDE_ASSIGNED",
            cancellationTermReasonRequired = Just True
          },
        BecknV2.OnDemand.Types.CancellationTerm
          { cancellationTermCancellationFee = mkAmountFee "30",
            cancellationTermFulfillmentState = Just $ mkCancellationFulfillmentState "RIDE_ENROUTE_PICKUP",
            cancellationTermReasonRequired = Just True
          },
        BecknV2.OnDemand.Types.CancellationTerm
          { cancellationTermCancellationFee = mkAmountFee "50",
            cancellationTermFulfillmentState = Just $ mkCancellationFulfillmentState "RIDE_ARRIVED_PICKUP",
            cancellationTermReasonRequired = Just True
          },
        BecknV2.OnDemand.Types.CancellationTerm
          { cancellationTermCancellationFee = Just $ BecknV2.OnDemand.Types.Fee {feeAmount = Nothing, feePercentage = Just "100"},
            cancellationTermFulfillmentState = Just $ mkCancellationFulfillmentState "RIDE_STARTED",
            cancellationTermReasonRequired = Just True
          }
      ]
  where
    mkCancellationFulfillmentState :: Data.Text.Text -> BecknV2.OnDemand.Types.FulfillmentState
    mkCancellationFulfillmentState code =
      BecknV2.OnDemand.Types.FulfillmentState
        { fulfillmentStateDescriptor =
            Just $
              BecknV2.OnDemand.Types.Descriptor
                { descriptorCode = Just code,
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                }
        }

-- | Normalize vehicle category to spec-compliant TRV:10 values (AUTO_RICKSHAW, CAB, TWO_WHEELER)
normalizeVehicleCategory :: Text -> Text
normalizeVehicleCategory cat = case cat of
  "AUTO_RICKSHAW" -> "AUTO_RICKSHAW"
  "CAB" -> "CAB"
  "TWO_WHEELER" -> "TWO_WHEELER"
  "MOTORCYCLE" -> "TWO_WHEELER"
  "TOTO" -> "AUTO_RICKSHAW"
  "AMBULANCE" -> "CAB"
  "TRUCK" -> "CAB"
  "BUS" -> "CAB"
  "BOAT" -> "CAB"
  _ -> "CAB"

-- | Normalize vehicle variant to spec-compliant TRV:10 values (SEDAN, SUV, HATCHBACK, TWO_WHEELER, AUTO_RICKSHAW)
normalizeVehicleVariant :: Text -> Text -> Text
normalizeVehicleVariant normalizedCategory variant = case variant of
  -- Already spec-compliant
  "SEDAN" -> "SEDAN"
  "SUV" -> "SUV"
  "SUV_PLUS" -> "SUV"
  "HATCHBACK" -> "HATCHBACK"
  "TWO_WHEELER" -> "TWO_WHEELER"
  "AUTO_RICKSHAW" -> "AUTO_RICKSHAW"
  -- Map to closest spec-compliant variant based on category
  _ -> case normalizedCategory of
    "AUTO_RICKSHAW" -> "AUTO_RICKSHAW"
    "TWO_WHEELER" -> "TWO_WHEELER"
    "CAB" -> "SEDAN" -- Default CAB variants (TAXI, TAXI_PLUS, PREMIUM_SEDAN, BLACK, etc.) to SEDAN
    _ -> "SEDAN"

tfVehicle :: Beckn.OnDemand.Utils.Common.Pricing -> Maybe BecknV2.OnDemand.Types.Vehicle
tfVehicle pricing = do
  let (category, variant) = Beckn.OnDemand.Utils.Common.castVariant pricing.vehicleVariant
      normalizedCategory = normalizeVehicleCategory category
      vehicleCategory_ = Just normalizedCategory
      vehicleColor_ = Nothing
      vehicleMake_ = Nothing
      vehicleModel_ = Nothing
      vehicleRegistration_ = Nothing
      vehicleVariant_ = Just $ normalizeVehicleVariant normalizedCategory variant
      vehicleCapacity_ = pricing.vehicleServiceTierSeatingCapacity
      returnData = BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_, vehicleCapacity = vehicleCapacity_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfItemDescriptor :: Beckn.OnDemand.Utils.Common.Pricing -> Maybe BecknV2.OnDemand.Types.Descriptor
tfItemDescriptor pricing =
  Just
    BecknV2.OnDemand.Types.Descriptor
      { descriptorCode = Just $ tripCategoryToItemCode pricing.tripCategory,
        descriptorShortDesc = pricing.serviceTierDescription,
        descriptorName = Just pricing.serviceTierName
      }

-- | Map trip category to spec-compliant item descriptor code (RIDE or RENTAL)
tripCategoryToItemCode :: Domain.Types.TripCategory -> Data.Text.Text
tripCategoryToItemCode (Domain.Types.Rental _) = "RENTAL"
tripCategoryToItemCode _ = "RIDE"
