-- | MSIL-only pilot: Layer 2 of the on_search-building pipeline. Layer 1
-- (SharedLogic.SearchRequestProcessing.processSearchRequest, which internally calls the
-- whole Beckn.OnDemand.Transformer.OnSearch chain) builds the on_search reply exactly as
-- it does for every merchant today -- plain ON_DEMAND_TRIP/ON_DEMAND_RENTAL categories,
-- no awareness this module exists. This module runs only for merchants on the
-- TransporterConfig.enableScheduledCategorySignal city-level gate (dispatched from
-- API.Beckn.Search.search) and rewrites the already-built response's provider-level
-- categories to SCHEDULED_TRIP/SCHEDULED_RENTAL when the search was scheduled.
--
-- Both the provider-level "categories" block and each item's own
-- "category_ids" must agree on the same scheduled code -- a BAP correlating
-- item.category_ids against the declared catalog.providers[*].categories[*].id
-- would otherwise find no match for a Layer-1-built ON_DEMAND_TRIP item id
-- once the provider-level category itself has been rewritten to SCHEDULED_TRIP.
module Beckn.OnDemand.Transformer.MSIL.OnSearch
  ( msilOnSearchConverter,
    msilAddBppTerms,
    msilPatchProviderFulfillmentTypes,
    msilPatchScheduledLocations,
    msilPatchCatalogCompliance,
  )
where

import Beckn.OnDemand.Utils.MSIL.Category (scheduledCategoryCode)
import qualified Beckn.OnDemand.Utils.MSIL.FulfillmentType as MSILFulfillmentType
import qualified Beckn.OnDemand.Utils.MSIL.Terms as MSILTerms
import qualified Beckn.OnDemand.Utils.OnSearch as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import Data.Text (isInfixOf)
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.BecknConfig as DBC
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Gps as Gps

-- | Layer 2: takes the already-built on_search reply from Layer 1, plus the domain
-- result Layer 1 returns alongside it, and returns the reply with provider.categories
-- corrected. A single isScheduled check is enough for the whole search -- Layer 1
-- decides isScheduled once per search (Domain.Action.Beckn.Search.getPossibleTripOption)
-- and applies that same value to every estimate/quote the search produces, so there's
-- no per-item variance to account for.
msilOnSearchConverter :: DSearch.DSearchRes -> Spec.OnSearchReq -> Spec.OnSearchReq
msilOnSearchConverter dSearchRes onSearchReq
  | not isScheduled = onSearchReq
  | otherwise = onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    isScheduled =
      any (\(estimate, _, _, _) -> estimate.isScheduled) dSearchRes.estimates
        || any (\(quote, _, _, _) -> quote.isScheduled) dSearchRes.quotes

    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog (Spec.onSearchReqMessageCatalog msg)}
    fixCatalog cat = cat {Spec.catalogProviders = map fixProvider <$> Spec.catalogProviders cat}
    fixProvider provider =
      provider
        { Spec.providerCategories = map fixCategory <$> Spec.providerCategories provider,
          Spec.providerItems = map fixItem <$> Spec.providerItems provider
        }
    fixCategory category =
      category
        { Spec.categoryId = scheduledCategoryCode <$> Spec.categoryId category,
          Spec.categoryDescriptor = fixDescriptor <$> Spec.categoryDescriptor category
        }
    fixDescriptor descriptor = case Spec.descriptorCode descriptor of
      Just code ->
        descriptor
          { Spec.descriptorCode = Just (scheduledCategoryCode code),
            Spec.descriptorName = Just (Utils.categoryCodeToName (scheduledCategoryCode code))
          }
      Nothing -> descriptor
    fixItem item = item {Spec.itemCategoryIds = map scheduledCategoryCode <$> Spec.itemCategoryIds item}

-- | Building: adds BPP_TERMS (STATIC_TERMS + OFFLINE_CONTRACT) to
-- message.catalog.tags on the already-built on_search reply, additive
-- alongside whatever's already there.
msilAddBppTerms :: DBC.BecknConfig -> Spec.OnSearchReq -> Spec.OnSearchReq
msilAddBppTerms bppConfig onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = MSILTerms.patchCatalogTags bppConfig (Spec.onSearchReqMessageCatalog msg)}

-- | Building: overrides catalog.providers[*].fulfillments[*].type the same
-- way order.fulfillments is overridden for on_select/on_init/on_confirm --
-- see Beckn.OnDemand.Utils.MSIL.FulfillmentType.patchProviderFulfillmentTypes.
-- Unconditional (not isScheduled-gated), matching those other call sites:
-- the DELIVERY/SELF_PICKUP restriction applies to every MSIL catalog, not
-- just scheduled ones.
msilPatchProviderFulfillmentTypes :: Spec.OnSearchReq -> Spec.OnSearchReq
msilPatchProviderFulfillmentTypes onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog (Spec.onSearchReqMessageCatalog msg)}
    fixCatalog cat = cat {Spec.catalogProviders = map MSILFulfillmentType.patchProviderFulfillmentTypes <$> Spec.catalogProviders cat}

-- | Building, MSIL pilot + scheduled searches only: catalog.providers[*].locations
-- and items[*].location_ids are normally derived from currently-online nearby
-- drivers (Beckn.OnDemand.Utils.OnSearch.mkProviderLocations/mkItemLocationIds,
-- via DSearchRes's NearestDriverInfo) -- for a scheduled ride there's no driver
-- assigned or expected to be found near the pickup point at search time, so
-- that machinery legitimately has nothing to report and item.location_ids ends
-- up omitted, which ONDC's schema requires present. This adds a location built
-- from the search's own pickup coordinates (DSearchRes.fromLocation) instead,
-- and points every item lacking location_ids at it -- but only as a fallback:
-- if real driver-derived locations already exist, they're left untouched.
msilPatchScheduledLocations :: DSearch.DSearchRes -> Spec.OnSearchReq -> Spec.OnSearchReq
msilPatchScheduledLocations dSearchRes onSearchReq
  | not isScheduled = onSearchReq
  | otherwise = onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    isScheduled =
      any (\(estimate, _, _, _) -> estimate.isScheduled) dSearchRes.estimates
        || any (\(quote, _, _, _) -> quote.isScheduled) dSearchRes.quotes

    pickupGps :: Maybe Text
    pickupGps = A.decode $ A.encode Gps.Gps {lat = dSearchRes.fromLocation.lat, lon = dSearchRes.fromLocation.lon}

    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog (Spec.onSearchReqMessageCatalog msg)}
    fixCatalog cat = cat {Spec.catalogProviders = map fixProvider <$> Spec.catalogProviders cat}

    fixProvider provider =
      provider
        { Spec.providerLocations = providerLocations',
          Spec.providerItems = map patchItem <$> Spec.providerItems provider
        }
      where
        pickupLocationId = maybe "scheduled-pickup" (<> "-scheduled-pickup") (Spec.providerId provider)
        pickupLocation =
          Spec.Location
            { locationAddress = Nothing,
              locationAreaCode = Nothing,
              locationCity = Nothing,
              locationCountry = Nothing,
              locationGps = pickupGps,
              locationId = Just pickupLocationId,
              locationUpdatedAt = Nothing,
              locationState = Nothing
            }
        hasRealLocations = maybe False (not . null) (Spec.providerLocations provider)
        providerLocations'
          | hasRealLocations = Spec.providerLocations provider
          | otherwise = Just [pickupLocation]
        patchItem item
          | maybe True null (Spec.itemLocationIds item) = item {Spec.itemLocationIds = Just [pickupLocationId]}
          | otherwise = item

-- | ONDC v2.1.0's TRV10 schema restricts three catalog fields to small fixed
-- enums that Layer 1's builder (Beckn.OnDemand.Transformer.OnSearch) was
-- never scoped to -- it emits NammaYatri's own internal codes instead, valid
-- for every merchant today but not ONDC-compliant. Unconditional (not
-- isScheduled-gated), same as msilPatchProviderFulfillmentTypes.
--
--   * items[*].descriptor.code must be "RIDE" or "RENTAL" -- Layer 1 puts the
--     vehicle service tier there (e.g. "SEDAN"); derived here instead from
--     the item's own (untouched) category id.
--   * fulfillments[*].vehicle.variant must be one of five fixed values --
--     Layer 1 emits NammaYatri's much larger internal variant set (TAXI,
--     PREMIUM_SEDAN, BLACK_XL, ...); collapsed here into the nearest of the
--     five, defaulting to SEDAN.
--   * items[*].tags[*].descriptor.code must be one of a fixed allow-list --
--     Layer 1's item tags (VEHICLE_INFO, the SPECIAL_LOCATION_TAG/PICKUP_AREA/...
--     group from mkGeneralInfoTagGroup, etc.) mostly aren't on it; only
--     FARE_POLICY and FEATURE_LIST already are. Non-conforming tag groups are
--     dropped rather than remapped -- there's no lossless 1:1 target for them
--     in the allow-list.
msilPatchCatalogCompliance :: Spec.OnSearchReq -> Spec.OnSearchReq
msilPatchCatalogCompliance onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog (Spec.onSearchReqMessageCatalog msg)}
    fixCatalog cat = cat {Spec.catalogProviders = map fixProvider <$> Spec.catalogProviders cat}
    fixProvider provider =
      provider
        { Spec.providerItems = map fixItem <$> Spec.providerItems provider,
          Spec.providerFulfillments = map fixFulfillment <$> Spec.providerFulfillments provider
        }

    isRental item = maybe False (any ("RENTAL" `isInfixOf`)) (Spec.itemCategoryIds item)
    fixItem item =
      item
        { Spec.itemDescriptor = fixItemDescriptor <$> Spec.itemDescriptor item,
          Spec.itemTags = filter isAllowedTagGroup <$> Spec.itemTags item
        }
      where
        fixItemDescriptor descriptor = descriptor {Spec.descriptorCode = Just (if isRental item then "RENTAL" else "RIDE")}

    allowedTagGroupCodes :: [Text]
    allowedTagGroupCodes =
      [ "DISABILITY_VIS",
        "DISABILITY_HEA",
        "DISABILITY_MOB",
        "DISABILITY_LEP",
        "DISABILITY_SPE",
        "DISABILITY_INTEL",
        "MENTAL",
        "DISABILITY_BLOOD",
        "DISABILITY_DWARFISM",
        "DISABILITY_ACID_ATTACK_SURVIVOR",
        "DISABILITY_MULTIPLE_DIS",
        "FARE_POLICY",
        "INFO",
        "FEATURE_LIST"
      ]
    isAllowedTagGroup tagGroup =
      maybe False (`elem` allowedTagGroupCodes) (Spec.tagGroupDescriptor tagGroup >>= Spec.descriptorCode)

    allowedVehicleVariants :: [Text]
    allowedVehicleVariants = ["SEDAN", "SUV", "HATCHBACK", "TWO_WHEELER", "AUTO_RICKSHAW"]
    overrideVehicleVariant variant
      | variant `elem` allowedVehicleVariants = variant
      | variant `elem` ["BIKE", "DELIVERY_BIKE", "BIKE_PLUS"] = "TWO_WHEELER"
      | variant `elem` ["EV_AUTO_RICKSHAW", "AUTO_PLUS", "AUTO_LITE", "PINK_AUTO", "E_RICKSHAW"] = "AUTO_RICKSHAW"
      | variant == "EV_HATCHBACK" = "HATCHBACK"
      | variant `elem` ["SUV_PLUS", "EV_SUV"] = "SUV"
      | otherwise = "SEDAN"
    fixFulfillment fulfillment = fulfillment {Spec.fulfillmentVehicle = fixVehicle <$> Spec.fulfillmentVehicle fulfillment}
    fixVehicle vehicle = vehicle {Spec.vehicleVariant = overrideVehicleVariant <$> Spec.vehicleVariant vehicle}
