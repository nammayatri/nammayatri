-- Patches Layer 1's on_search reply to ONDC v2.1.0 compliance for pilot merchants (scheduled categories, terms, fulfillment types, locations, catalog fixes), since Layer 1 builds it identically for every merchant.
module Beckn.OnDemand.Transformer.OndcScheduledRide.OnSearch
  ( ondcScheduledRideOnSearchMessageBuild,
    ondcScheduledRideOnSearchConverter,
    ondcScheduledRideAddBppTerms,
    ondcScheduledRidePatchProviderFulfillmentTypes,
    ondcScheduledRidePatchScheduledLocations,
    ondcScheduledRidePatchCatalogCompliance,
  )
where

import qualified Beckn.OnDemand.Utils.OnSearch as Utils
import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.BapMetadata as DBapMetadata
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import qualified Storage.CachedQueries.BecknConfig as QBC

-- | Single entry point for API.Beckn.Search.search: fetches beckn_config and
-- the BAP's BapMetadata, then applies every ONDC-scheduled-ride patch to the
-- already-built on_search reply, in order.
ondcScheduledRideOnSearchMessageBuild :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => Id DM.Merchant -> Text -> DSearch.DSearchRes -> Spec.OnSearchReq -> m Spec.OnSearchReq
ondcScheduledRideOnSearchMessageBuild merchantId bapId dSearchRes onSearchReq = do
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchantId "MOBILITY" Enums.CAB >>= fromMaybeM (InternalError "Beckn Config not found")
  mbBapMetadata <- CQBapMetaData.findBySubscriberIdAndDomain (Id bapId) Domain.MOBILITY
  pure $
    ( ondcScheduledRidePatchCatalogCompliance
        . ondcScheduledRidePatchScheduledLocations dSearchRes
        . ondcScheduledRidePatchProviderFulfillmentTypes
        . ondcScheduledRideAddBppTerms mbBapMetadata bppConfig
        . ondcScheduledRideOnSearchConverter dSearchRes
    )
      onSearchReq

-- Rewrites provider-level categories and item category_ids to their scheduled equivalents, since on_search must announce SCHEDULED_TRIP/SCHEDULED_RENTAL categories for a scheduled search.
ondcScheduledRideOnSearchConverter :: DSearch.DSearchRes -> Spec.OnSearchReq -> Spec.OnSearchReq
ondcScheduledRideOnSearchConverter dSearchRes onSearchReq
  | not (OSRCommon.isSearchResultScheduled dSearchRes) = onSearchReq
  | otherwise = onSearchReq {Spec.onSearchReqMessage = fixMessage <$> onSearchReq.onSearchReqMessage}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog msg.onSearchReqMessageCatalog}
    fixCatalog cat = cat {Spec.catalogProviders = map fixProvider <$> cat.catalogProviders}
    fixProvider provider =
      provider
        { Spec.providerCategories = map fixCategory <$> provider.providerCategories,
          Spec.providerItems = map fixItem <$> provider.providerItems
        }
    fixCategory category =
      category
        { Spec.categoryId = OSRCommon.scheduledCategoryCode <$> category.categoryId,
          Spec.categoryDescriptor = fixDescriptor <$> category.categoryDescriptor
        }
    fixDescriptor descriptor = case descriptor.descriptorCode of
      Just code ->
        descriptor
          { Spec.descriptorCode = Just (OSRCommon.scheduledCategoryCode code),
            Spec.descriptorName = Just (Utils.categoryCodeToName (OSRCommon.scheduledCategoryCode code))
          }
      Nothing -> descriptor
    fixItem item = item {Spec.itemCategoryIds = map OSRCommon.scheduledCategoryCode <$> item.itemCategoryIds}

-- | Adds BPP_TERMS (STATIC_TERMS + OFFLINE_CONTRACT) to message.catalog.tags.
ondcScheduledRideAddBppTerms :: Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Spec.OnSearchReq -> Spec.OnSearchReq
ondcScheduledRideAddBppTerms mbBapMetadata bppConfig onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = OSRCommon.patchCatalogTags mbBapMetadata bppConfig (Spec.onSearchReqMessageCatalog msg)}

-- | Overrides catalog.providers[*].fulfillments[*].type, same rule as order.fulfillments.
ondcScheduledRidePatchProviderFulfillmentTypes :: Spec.OnSearchReq -> Spec.OnSearchReq
ondcScheduledRidePatchProviderFulfillmentTypes onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> onSearchReq.onSearchReqMessage}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog msg.onSearchReqMessageCatalog}
    fixCatalog cat = cat {Spec.catalogProviders = map OSRCommon.patchProviderFulfillmentTypes <$> cat.catalogProviders}

-- Falls back to a location built from the search's own pickup coordinates, only where a real one is missing, since a scheduled ride has no driver yet and ONDC requires the usual driver-derived location_ids to be present.
ondcScheduledRidePatchScheduledLocations :: DSearch.DSearchRes -> Spec.OnSearchReq -> Spec.OnSearchReq
ondcScheduledRidePatchScheduledLocations dSearchRes onSearchReq
  | not (OSRCommon.isSearchResultScheduled dSearchRes) = onSearchReq
  | otherwise = onSearchReq {Spec.onSearchReqMessage = fixMessage <$> onSearchReq.onSearchReqMessage}
  where
    pickupGps :: Maybe Text
    pickupGps = A.decode $ A.encode Gps.Gps {lat = dSearchRes.fromLocation.lat, lon = dSearchRes.fromLocation.lon}

    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog msg.onSearchReqMessageCatalog}
    fixCatalog cat = cat {Spec.catalogProviders = map fixProvider <$> cat.catalogProviders}

    fixProvider provider =
      provider
        { Spec.providerLocations = providerLocations',
          Spec.providerItems = map patchItem <$> provider.providerItems
        }
      where
        pickupLocationId = maybe "scheduled-pickup" (<> "-scheduled-pickup") provider.providerId
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
        hasRealLocations = maybe False (not . null) provider.providerLocations
        providerLocations'
          | hasRealLocations = provider.providerLocations
          | otherwise = Just [pickupLocation]
        patchItem item
          | maybe True null item.itemLocationIds = item {Spec.itemLocationIds = Just [pickupLocationId]}
          | otherwise = item

-- Remaps item.descriptor.code to RIDE/RENTAL, vehicle.variant to one of five values, and drops non-allow-listed item tags, since ONDC v2.1.0's TRV10 schema restricts these to small fixed enums.
ondcScheduledRidePatchCatalogCompliance :: Spec.OnSearchReq -> Spec.OnSearchReq
ondcScheduledRidePatchCatalogCompliance onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> onSearchReq.onSearchReqMessage}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = fixCatalog msg.onSearchReqMessageCatalog}
    fixCatalog cat = cat {Spec.catalogProviders = map fixProvider <$> cat.catalogProviders}
    fixProvider provider =
      provider
        { Spec.providerItems = map OSRCommon.fixItemCompliance <$> provider.providerItems,
          Spec.providerFulfillments = map fixFulfillment <$> provider.providerFulfillments
        }

    allowedVehicleVariants :: [Text]
    allowedVehicleVariants = ["SEDAN", "SUV", "HATCHBACK", "TWO_WHEELER", "AUTO_RICKSHAW"]
    overrideVehicleVariant variant
      | variant `elem` allowedVehicleVariants = variant
      | variant `elem` ["BIKE", "DELIVERY_BIKE", "BIKE_PLUS"] = "TWO_WHEELER"
      | variant `elem` ["EV_AUTO_RICKSHAW", "AUTO_PLUS", "AUTO_LITE", "PINK_AUTO", "E_RICKSHAW"] = "AUTO_RICKSHAW"
      | variant == "EV_HATCHBACK" = "HATCHBACK"
      | variant `elem` ["SUV_PLUS", "EV_SUV"] = "SUV"
      | otherwise = "SEDAN"
    fixFulfillment fulfillment = fulfillment {Spec.fulfillmentVehicle = fixVehicle <$> fulfillment.fulfillmentVehicle}
    fixVehicle vehicle = vehicle {Spec.vehicleVariant = overrideVehicleVariant <$> vehicle.vehicleVariant}
