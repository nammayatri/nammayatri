-- | MSIL-only pilot: Layer 2 of the on_search-building pipeline. Layer 1
-- (SharedLogic.SearchRequestProcessing.processSearchRequest, which internally calls the
-- whole Beckn.OnDemand.Transformer.OnSearch chain) builds the on_search reply exactly as
-- it does for every merchant today -- plain ON_DEMAND_TRIP/ON_DEMAND_RENTAL categories,
-- no awareness this module exists. This module runs only for merchants on the
-- AppEnv.scheduledCategorySignalMerchantIds allowlist (dispatched from
-- API.Beckn.Search.search) and rewrites the already-built response's provider-level
-- categories to SCHEDULED_TRIP/SCHEDULED_RENTAL when the search was scheduled.
--
-- Per the actual ONDC v2.1.0 schedule_trip on_search example, only the provider-level
-- "categories" block differs from the on-demand shape -- item.category_ids is untouched
-- either way, so this module never needs to look at (or correlate against) individual
-- items, and needs no changes to (or new dependency on internals of) the Layer 1 builder
-- chain in Beckn.OnDemand.Transformer.OnSearch.
module Beckn.OnDemand.Transformer.MSIL.OnSearch
  ( msilOnSearchConverter,
    msilAddBppTerms,
  )
where

import qualified Beckn.OnDemand.Utils.MSIL.Terms as MSILTerms
import qualified Beckn.OnDemand.Utils.OnSearch as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.BecknConfig as DBC
import EulerHS.Prelude

-- | ON_DEMAND_TRIP/ON_DEMAND_RENTAL -> SCHEDULED_TRIP/SCHEDULED_RENTAL; anything else
-- (INTERCITY_TRIP, ON_DEMAND_EASY_BOOKING, ...) passes through unchanged -- the ONDC
-- v2.1.0 schedule_trip spec only defines scheduled variants for trip and rental.
scheduledCategoryCode :: Text -> Text
scheduledCategoryCode = \case
  "ON_DEMAND_TRIP" -> "SCHEDULED_TRIP"
  "ON_DEMAND_RENTAL" -> "SCHEDULED_RENTAL"
  other -> other

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
    fixProvider provider = provider {Spec.providerCategories = map fixCategory <$> Spec.providerCategories provider}
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

-- | Building: adds BPP_TERMS (STATIC_TERMS + OFFLINE_CONTRACT) to
-- message.catalog.tags on the already-built on_search reply, additive
-- alongside whatever's already there.
msilAddBppTerms :: DBC.BecknConfig -> Spec.OnSearchReq -> Spec.OnSearchReq
msilAddBppTerms bppConfig onSearchReq =
  onSearchReq {Spec.onSearchReqMessage = fixMessage <$> Spec.onSearchReqMessage onSearchReq}
  where
    fixMessage msg = msg {Spec.onSearchReqMessageCatalog = MSILTerms.patchCatalogTags bppConfig (Spec.onSearchReqMessageCatalog msg)}
