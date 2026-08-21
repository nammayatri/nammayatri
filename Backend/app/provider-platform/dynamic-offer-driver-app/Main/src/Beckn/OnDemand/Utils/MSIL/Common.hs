-- | MSIL pilot: common overrides/helpers shared across every MSIL Layer 2
-- transformer (Search/OnSearch/OnSelect/OnInit/OnConfirm/OnStatus/...). One
-- module per concern proved more indirection than benefit for a set of
-- small, related patches that are all "take Layer 1's output, fix it up for
-- MSIL's ONDC v2.1.0 pilot"; kept together here instead.
module Beckn.OnDemand.Utils.MSIL.Common
  ( addTagGroup,
    scheduledCategoryCode,
    overrideOrderCategoryIds,
    overrideOrderFulfillmentState,
    overrideFulfillmentType,
    patchOrderFulfillmentTypes,
    patchProviderFulfillmentTypes,
    patchOrderRouteInfo,
    verifyIncomingStaticTerms,
    patchOrderTags,
    patchCatalogTags,
    dropNonConformingOrderTags,
    patchOrderVehicleEnergyType,
    applyOrderCategoryAndFulfillmentStateOverrides,
    applyOnConfirmOrderOverrides,
    applyOnInitOrderOverrides,
    applyOnSelectOrderOverrides,
    applyOnStatusOrderOverrides,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import qualified Control.Exception as E
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Domain.Types.BapMetadata as DBapMetadata
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.RideRoute as RI
import qualified Kernel.External.Maps.Google.PolyLinePoints as PolyLine
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import SharedLogic.Ride (searchRequestKey)
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData

-- | Append a tag group to an existing (possibly absent) tag-group list,
-- if there is one to add.
addTagGroup :: Maybe Spec.TagGroup -> Maybe [Spec.TagGroup] -> Maybe [Spec.TagGroup]
addTagGroup Nothing existingGroups = existingGroups
addTagGroup (Just newGroup) existingGroups = Just (fromMaybe [] existingGroups <> [newGroup])

-- Category --------------------------------------------------------------

-- | MSIL pilot: common override for the wire category id/descriptor code
-- (BecknV2.OnDemand.Utils.Common.tripCategoryToCategoryCode, untouched --
-- Layer 1's own mapping is left exactly as it is for every other merchant),
-- shared across every MSIL transformer that emits a scheduled-trip category
-- id -- on_search's provider-level categories and item.category_ids
-- (Beckn.OnDemand.Transformer.MSIL.OnSearch) and on_select's item.category_ids
-- (Beckn.OnDemand.Transformer.MSIL.OnSelect) must all agree on the same id,
-- or a BAP correlating item.category_ids against the declared
-- catalog.providers[*].categories[*].id will find no match.
--
-- ON_DEMAND_TRIP/ON_DEMAND_RENTAL -> SCHEDULED_TRIP/SCHEDULED_RENTAL; anything else
-- (INTERCITY_TRIP, ON_DEMAND_EASY_BOOKING, ...) passes through unchanged -- the ONDC
-- v2.1.0 schedule_trip spec only defines scheduled variants for trip and rental.
scheduledCategoryCode :: Text -> Text
scheduledCategoryCode = \case
  "ON_DEMAND_TRIP" -> "SCHEDULED_TRIP"
  "ON_DEMAND_RENTAL" -> "SCHEDULED_RENTAL"
  other -> other

-- | Same rewrite as scheduledCategoryCode, applied to every item's
-- category_ids on an order -- for on_confirm/on_update's Booking-based item
-- builders (Beckn.OnDemand.Utils.Common.tfItems,
-- Beckn.OnDemand.Utils.OnUpdate.tfItems), which unlike on_search/on_select
-- have no per-item isScheduled to check, only the booking's own. Explicitly
-- gated by the caller's isScheduled (not unconditional, unlike
-- overrideOrderFulfillmentState) -- MSIL pilot merchants also serve plain,
-- non-scheduled bookings, whose category_ids (ON_DEMAND_TRIP/ON_DEMAND_RENTAL)
-- are already correct and must not be touched.
overrideOrderCategoryIds :: Bool -> Spec.Order -> Spec.Order
overrideOrderCategoryIds isScheduled order
  | not isScheduled = order
  | otherwise = order {Spec.orderItems = map fixItem <$> order.orderItems}
  where
    fixItem item = item {Spec.itemCategoryIds = map scheduledCategoryCode <$> item.itemCategoryIds}

-- FulfillmentState --------------------------------------------------------

-- | MSIL pilot: shared override for fulfillment-state descriptor codes that
-- Layer 1 produces but that aren't in ONDC v2.1.0's fulfillmentState
-- vocabulary (RIDE_CANCELLED/RIDE_ENDED/RIDE_STARTED/RIDE_ASSIGNED/
-- RIDE_ENROUTE_PICKUP/RIDE_ARRIVED_PICKUP/RIDE_CONFIRMED):
--
--   * NEW -- "Custom type only used for on-us transaction"
--     (BecknV2.OnDemand.Enums.FulfillmentState) -- the no-driver-yet
--     static/scheduled on_confirm path (Beckn.ACL.OnConfirm.bookingStatusCode)
--     uses this; the reference on_confirm example uses RIDE_CONFIRMED instead.
--   * SCHEDULED_RIDE_ASSIGNED -- a real Enums.FulfillmentState constructor,
--     but not one of the seven ONDC-valid wire values -- produced whenever a
--     driver is assigned to a scheduled ride (Domain.Types.Ride.RideStatus
--     UPCOMING), on both the on_confirm phased-assignment push
--     (SharedLogic.CallBAP.buildOnConfirmMessage) and the on_update
--     ride-assignment push (SharedLogic.CallBAP.sendRideAssignedUpdateToBAP),
--     which both build off the same Beckn.OnDemand.Transformer.OnUpdate
--     dispatch. ONDC has no separate "scheduled" fulfillment-state code --
--     RIDE_ASSIGNED covers both.
--
-- Kept as an Order->Order patch, not tied to any one message type, since
-- on_confirm and on_update both wrap their order in the same
-- Spec.ConfirmReqMessage shape
-- (BecknV2.OnDemand.Types.OnUpdateReq.onUpdateReqMessage is itself a
-- ConfirmReqMessage).
overrideOrderFulfillmentState :: Spec.Order -> Spec.Order
overrideOrderFulfillmentState order =
  order
    { Spec.orderFulfillments = map fixFulfillment <$> order.orderFulfillments,
      Spec.orderCancellationTerms = map fixCancellationTerm <$> order.orderCancellationTerms
    }
  where
    fixFulfillment fulfillment = fulfillment {Spec.fulfillmentState = fixState <$> fulfillment.fulfillmentState}
    fixCancellationTerm term = term {Spec.cancellationTermFulfillmentState = fixState <$> term.cancellationTermFulfillmentState}
    fixState fulfillmentState = fulfillmentState {Spec.fulfillmentStateDescriptor = fixDescriptor <$> fulfillmentState.fulfillmentStateDescriptor}
    fixDescriptor descriptor = descriptor {Spec.descriptorCode = overrideCode <$> descriptor.descriptorCode}
    overrideCode code
      | code == show Enums.NEW = show Enums.RIDE_CONFIRMED
      | code == show Enums.SCHEDULED_RIDE_ASSIGNED = show Enums.RIDE_ASSIGNED
      | otherwise = code

-- FulfillmentType --------------------------------------------------------

-- | MSIL pilot: common override for the wire fulfillment.type code
-- (BecknV2.OnDemand.Utils.Common.tripCategoryToFulfillmentType, untouched --
-- Layer 1's own mapping is left exactly as it is for every other merchant),
-- shared across every MSIL transformer that sends order.fulfillments to the
-- BAP (Search/OnSelect/OnInit/OnConfirm/OnCancel/OnStatus, ...).
--
-- For MSIL, whatever fulfillment type code Layer 1 (or MSIL's own Quote-based
-- builder) computed is collapsed to just two possible values: RIDE_OTP stays
-- RIDE_OTP-equivalent (SELF_PICKUP, the v2.1.0 off-us encoding of ride-OTP);
-- every other trip category becomes DELIVERY.
--
-- This collapse is lossy for /init parsing -- see
-- Beckn.OnDemand.Transformer.MSIL.Init.correctFulfillmentId for the
-- corresponding fix (kept in a separate module to avoid an import cycle:
-- that fix needs Domain.Action.Beckn.Init, which this module's callers
-- -- e.g. SharedLogic.CallBAP -- transitively import).

-- | RIDE_OTP -> SELF_PICKUP; every other fulfillment-type code -> DELIVERY.
overrideFulfillmentType :: Text -> Text
overrideFulfillmentType fulfillmentType
  | fulfillmentType == show Enums.RIDE_OTP = show Enums.SELF_PICKUP
  | otherwise = show Enums.DELIVERY

-- | The single patch operation for an order: overrides every one of the
-- order's fulfillments' type code per the rule above. Every other field is
-- passed through untouched.
patchOrderFulfillmentTypes :: Spec.Order -> Spec.Order
patchOrderFulfillmentTypes order =
  order {Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentType = overrideFulfillmentType <$> fulfillment.fulfillmentType}

-- | Same patch, for on_search's catalog.providers[*].fulfillments -- a
-- structurally different field (Provider.providerFulfillments) from
-- order.fulfillments, so it needs its own top-level patch function even
-- though it shares the same per-fulfillment rule above.
patchProviderFulfillmentTypes :: Spec.Provider -> Spec.Provider
patchProviderFulfillmentTypes provider =
  provider {Spec.providerFulfillments = map patchFulfillment <$> provider.providerFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentType = overrideFulfillmentType <$> fulfillment.fulfillmentType}

-- RouteInfo --------------------------------------------------------

-- | MSIL pilot: the ROUTE_INFO tag group (WAYPOINTS, ENCODED_POLYLINE),
-- shared across every MSIL transformer that sends it to the BAP (OnSelect,
-- OnInit, OnConfirm, ...).
--
-- ONDC used to have the BAP send ROUTE_INFO (WAYPOINTS) to the BPP at
-- /search, which Beckn.OnDemand.Utils.Search.buildRoutePoints still parses.
-- Per the updated spec, the BPP now computes this itself and sends it back to
-- the BAP instead. Rather than compute a fresh route, this reuses the same
-- fallback route -- already computed once, at search time, whenever the BAP
-- didn't supply routePoints/routeDistance/routeDuration itself
-- (Domain.Action.Beckn.Search.getRouteServiceability) -- which is cached in
-- Redis under the search's transactionId (SharedLogic.Ride.searchRequestKey)
-- for the lifetime of the booking. WAYPOINTS is JSON-encoded (mirroring the
-- format Utils.Search.buildRoutePoints expects when parsing it off the BAP);
-- ENCODED_POLYLINE is the Google-polyline encoding of the same points
-- (Kernel.External.Maps.Google.PolyLinePoints.encode).
--
-- Not sent on on_search -- only on the later, order-bearing responses
-- (on_select/on_init/on_confirm/...), under order.fulfillments[].tags.

-- | Fetch the fallback route (computed and cached at search time, keyed by
-- transactionId) and build the ROUTE_INFO tag group from it. Nothing if no
-- route was ever cached for this transaction, or it has no points.
getRouteInfoTagGroup :: (CacheFlow m r, MonadFlow m) => Text -> m (Maybe Spec.TagGroup)
getRouteInfoTagGroup transactionId = do
  mbRouteInfo :: Maybe RI.RouteInfo <- Redis.runInMultiCloudRedisMaybeResult $ Redis.withMasterRedis $ Redis.get (searchRequestKey transactionId)
  pure $ mbRouteInfo >>= (.points) >>= mkTagGroup
  where
    mkTagGroup [] = Nothing
    mkTagGroup points =
      Just $
        Tag.getFullTagGroup
          Tag.ROUTE_INFO
          [ Tag.mkTag Tag.WAYPOINTS (Just . decodeUtf8 . BSL.toStrict $ Aeson.encode points),
            Tag.mkTag Tag.ENCODED_POLYLINE (Just $ PolyLine.encode points)
          ]

-- | The single patch operation for an order: adds the ROUTE_INFO tag group
-- (if a fallback route was cached for this transactionId) to every one of
-- the order's fulfillments' tags, additive alongside whatever Layer 1
-- already put there.
patchOrderRouteInfo :: (CacheFlow m r, MonadFlow m) => Text -> Spec.Order -> m Spec.Order
patchOrderRouteInfo transactionId order = do
  mbRouteInfoTagGroup <- getRouteInfoTagGroup transactionId
  pure $ order {Spec.orderFulfillments = map (patchFulfillment mbRouteInfoTagGroup) <$> order.orderFulfillments}
  where
    patchFulfillment mbRouteInfoTagGroup fulfillment =
      fulfillment {Spec.fulfillmentTags = addTagGroup mbRouteInfoTagGroup fulfillment.fulfillmentTags}

-- Terms --------------------------------------------------------

-- | MSIL pilot: the BAP_TERMS/BPP_TERMS tag groups (STATIC_TERMS,
-- OFFLINE_CONTRACT), shared across every MSIL transformer that touches them
-- (Search, OnSearch, OnConfirm, ...). Layer 1 never reads or builds either
-- group -- the only thing it does with STATIC_TERMS is emit it under the
-- legacy SETTLEMENT_TERMS group
-- (BecknV2.OnDemand.Utils.Payment.mkSettlementTagGroup, untouched here).
--
-- Verifying (parse-time, STATIC_TERMS only): extracts the BAP's declared
-- BAP_TERMS.STATIC_TERMS off an incoming wire message and stores it on that
-- BAP's BapMetadata row (Storage.CachedQueries.BapMetadata.updateStaticTermsUrlIfChanged),
-- so it's available to echo back later. OFFLINE_CONTRACT is never parsed --
-- only ever built, from config, per the task this module was written for.
--
-- Building: BPP_TERMS (STATIC_TERMS from beckn_config.staticTermsUrl,
-- OFFLINE_CONTRACT from beckn_config.offlineContract) is additive, alongside
-- whatever Layer 1 already put in order.tags -- order-level tags Layer 1 puts
-- there (if any) are left exactly as they are. BAP_TERMS (both tags, echoing
-- back what's on record for this BAP) is only built where the call site
-- explicitly asks for it (on_confirm).
--
-- Every patch/fix operation on an order's tag list goes through
-- 'patchOrderTags' (on_select/on_init/on_confirm); on_search has no Order at
-- all (Catalog -> Provider, no order wrapper), so its BPP_TERMS goes on
-- message.catalog.tags instead, via 'patchCatalogTags'.
--
-- Handler call sites never parse or store STATIC_TERMS themselves -- they
-- (or the MSIL transformer they delegate to, e.g.
-- Beckn.OnDemand.Transformer.MSIL.Search.msilParser) just extract the
-- relevant tag list off the wire message and hand it to
-- 'verifyIncomingStaticTerms', which does the whole parse-and-store pipeline
-- in one call.

-- | Extract BAP_TERMS.STATIC_TERMS off an incoming wire message's tag list,
-- parse it as a URL, and -- if it parsed and differs from what's on record --
-- store it on that BAP's BapMetadata row
-- (Storage.CachedQueries.BapMetadata.updateStaticTermsUrlIfChanged), so it's
-- available to echo back later (e.g. at on_confirm). Never throws -- a
-- malformed or absent value from the BAP just means we don't learn a
-- static-terms URL from this request, not a reason to fail the whole
-- search/init/confirm.
verifyIncomingStaticTerms :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DBapMetadata.BapMetadata -> Domain.Domain -> Maybe [Spec.TagGroup] -> m ()
verifyIncomingStaticTerms bapSubscriberId domain tagGroups =
  case Utils.getTagV2 Tag.BAP_TERMS Tag.STATIC_TERMS tagGroups of
    Nothing -> pure ()
    Just rawUrl -> do
      result <- liftIO $ E.try @E.SomeException $ parseBaseUrl rawUrl
      either (const (pure ())) (CQBapMetaData.updateStaticTermsUrlIfChanged bapSubscriberId (show domain)) result

boolTagValue :: Bool -> Text
boolTagValue True = "true"
boolTagValue False = "false"

mkBppTermsTagGroup :: DBC.BecknConfig -> Maybe Spec.TagGroup
mkBppTermsTagGroup bppConfig =
  case catMaybes [staticTermsTag, offlineContractTag] of
    [] -> Nothing
    tags -> Just $ Tag.getFullTagGroup Tag.BPP_TERMS tags
  where
    staticTermsTag = Tag.mkTag Tag.STATIC_TERMS . Just . showBaseUrl <$> bppConfig.staticTermsUrl
    offlineContractTag = Tag.mkTag Tag.OFFLINE_CONTRACT . Just . boolTagValue <$> bppConfig.offlineContract

mkBapTermsTagGroup :: Maybe BaseUrl -> DBC.BecknConfig -> Maybe Spec.TagGroup
mkBapTermsTagGroup mbBapStaticTermsUrl bppConfig =
  case catMaybes [staticTermsTag, offlineContractTag] of
    [] -> Nothing
    tags -> Just $ Tag.getFullTagGroup Tag.BAP_TERMS tags
  where
    staticTermsTag = Tag.mkTag Tag.STATIC_TERMS . Just . showBaseUrl <$> mbBapStaticTermsUrl
    -- OFFLINE_CONTRACT is one fact about the deal, not a separate claim per
    -- side -- BAP_TERMS and BPP_TERMS echo the same config-sourced value.
    offlineContractTag = Tag.mkTag Tag.OFFLINE_CONTRACT . Just . boolTagValue <$> bppConfig.offlineContract

-- | The single patch/fix operation for the order's top-level tag list
-- (message.order.tags -- a sibling of order.payments, per ONDC 2.1.0, NOT
-- nested inside any payment). Always adds BPP_TERMS (our own, from
-- beckn_config); when 'includeBapTerms' is True, also adds BAP_TERMS
-- (echoing back the BAP's own declared terms, if known) -- used only on
-- on_confirm, per this task's scope. on_search/on_select/on_init pass
-- 'includeBapTerms = False' and 'mbBapStaticTermsUrl = Nothing'.
--
-- Also drops anything Layer 1 already put in order.tags that isn't
-- BAP_TERMS/BPP_TERMS -- ONDC v2.1.0's TRV10 schema restricts
-- order.tags[*].descriptor.code to exactly those two. On_confirm's Layer 1
-- (Beckn.ACL.OnConfirm.tfOrder) puts a BPP_INVOICE_INFO group there for every
-- merchant today; that's valid for non-MSIL BAPs but fails this stricter
-- validation, so it's filtered out here rather than fixed in Layer 1.
patchOrderTags :: Bool -> Maybe BaseUrl -> DBC.BecknConfig -> Spec.Order -> Spec.Order
patchOrderTags includeBapTerms mbBapStaticTermsUrl bppConfig order =
  conformingOrder {Spec.orderTags = withBapTerms . withBppTerms $ conformingOrder.orderTags}
  where
    conformingOrder = dropNonConformingOrderTags order
    withBppTerms = addTagGroup (mkBppTermsTagGroup bppConfig)
    withBapTerms
      | includeBapTerms = addTagGroup (mkBapTermsTagGroup mbBapStaticTermsUrl bppConfig)
      | otherwise = \tagGroups -> tagGroups

-- | Drops anything already in order.tags that isn't BAP_TERMS/BPP_TERMS --
-- ONDC v2.1.0's TRV10 schema restricts order.tags[*].descriptor.code to
-- exactly those two. Several Layer 1 order builders put other tag groups
-- there for every merchant today (Beckn.ACL.OnConfirm.tfOrder's
-- BPP_INVOICE_INFO; Beckn.ACL.Common.Order.tfAssignedReqToOrder's
-- SETTLEMENT_TERMS, via mkOrderSettlementTags) -- valid for non-MSIL BAPs but
-- failing this stricter validation, so they're filtered out here rather than
-- fixed in Layer 1. Exported standalone (not just via 'patchOrderTags') for
-- callers like on_update's ride-assigned push that need the drop but not the
-- BAP_TERMS/BPP_TERMS addition.
dropNonConformingOrderTags :: Spec.Order -> Spec.Order
dropNonConformingOrderTags order = order {Spec.orderTags = fmap (filter isAllowedOrderTag) order.orderTags}
  where
    allowedOrderTagCodes = ["BAP_TERMS", "BPP_TERMS"]
    isAllowedOrderTag tagGroup = maybe False (`elem` allowedOrderTagCodes) (tagGroup.tagGroupDescriptor >>= (.descriptorCode))

-- | The single patch/fix operation for on_search's catalog-level tag list
-- (message.catalog.tags -- on_search has no Order to hang order.tags off of).
-- Only ever BPP_TERMS -- on_search has no notion of echoing the BAP's own
-- terms back.
patchCatalogTags :: DBC.BecknConfig -> Spec.Catalog -> Spec.Catalog
patchCatalogTags bppConfig catalog =
  catalog {Spec.catalogTags = addTagGroup (mkBppTermsTagGroup bppConfig) catalog.catalogTags}

-- VehicleEnergyType --------------------------------------------------------

-- | MSIL pilot: common override for the wire vehicle.energy_type code
-- (BecknV2.OnDemand.Types.vehicleEnergyType, sourced from vehicle.energyType
-- -- free text captured at driver onboarding, not validated against ONDC's
-- vocabulary), shared across every MSIL transformer that sends
-- order.fulfillments[].vehicle to the BAP (OnSelect/OnInit/OnConfirm/
-- OnStatus, ...).
--
-- ONDC v2.1.0 only accepts seven energy_type codes (ELECTRIC, PETROL, DIESEL,
-- HYDROGEN, BIOFUELS, CNG, LPG -- BecknV2.OnDemand.Enums.EnergyType). For
-- MSIL, whatever value Layer 1 put there is checked against that vocabulary;
-- anything outside it (including onboarding free text that doesn't match) is
-- overridden to PETROL. A missing value (Nothing -- we simply aren't sending
-- one) is left as Nothing.
validEnergyTypes :: [Text]
validEnergyTypes = show <$> [Enums.ELECTRIC, Enums.PETROL, Enums.DIESEL, Enums.HYDROGEN, Enums.BIOFUELS, Enums.CNG, Enums.LPG]

-- | Any of the seven ONDC v2.1.0 energy_type codes passes through unchanged;
-- anything else (including free-text onboarding values that don't match)
-- becomes PETROL.
overrideVehicleEnergyType :: Text -> Text
overrideVehicleEnergyType energyType
  | energyType `elem` validEnergyTypes = energyType
  | otherwise = show Enums.PETROL -- Need to discuss what should be passed if it is not c

-- | The single patch operation for an order: overrides every one of the
-- order's fulfillments' vehicle.energy_type per the rule above. Every other
-- field is passed through untouched.
patchOrderVehicleEnergyType :: Spec.Order -> Spec.Order
patchOrderVehicleEnergyType order =
  order {Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentVehicle = patchVehicle <$> fulfillment.fulfillmentVehicle}
    patchVehicle vehicle = vehicle {Spec.vehicleEnergyType = overrideVehicleEnergyType <$> vehicle.vehicleEnergyType}

-- Per-API composed overrides ----------------------------------------------

-- | overrideOrderCategoryIds + overrideOrderFulfillmentState, applied
-- together -- the two order-tags-only fixes SharedLogic.CallBAP applies to
-- an already-built on_confirm/on_update order for MSIL pilot merchants
-- (buildOnConfirmMessage, sendRideAssignedUpdateToBAP), where the message is
-- otherwise built and pushed exactly as it is for every other merchant.
applyOrderCategoryAndFulfillmentStateOverrides :: Bool -> Spec.Order -> Spec.Order
applyOrderCategoryAndFulfillmentStateOverrides isScheduled =
  overrideOrderCategoryIds isScheduled . overrideOrderFulfillmentState

-- | The full MSIL on_confirm order patch, in the order
-- Beckn.OnDemand.Transformer.MSIL.OnConfirm.msilOnConfirmMessageBuild applies
-- them: category ids and fulfillment-state codes, BAP_TERMS + BPP_TERMS
-- (includeBapTerms = True, since on_confirm is the one call site that echoes
-- the BAP's own terms back), fulfillment.type, vehicle.energy_type, and
-- finally ROUTE_INFO (the only step needing a monadic Redis read, so it has
-- to come last/be sequenced separately from the rest).
applyOnConfirmOrderOverrides :: (CacheFlow m r, MonadFlow m) => Bool -> Text -> Maybe BaseUrl -> DBC.BecknConfig -> Spec.Order -> m Spec.Order
applyOnConfirmOrderOverrides isScheduled transactionId mbBapStaticTermsUrl bppConfig =
  patchOrderRouteInfo transactionId
    . patchOrderVehicleEnergyType
    . patchOrderFulfillmentTypes
    . patchOrderTags True mbBapStaticTermsUrl bppConfig
    . applyOrderCategoryAndFulfillmentStateOverrides isScheduled

-- | The full MSIL on_init order patch, in the order
-- Beckn.OnDemand.Transformer.MSIL.OnInit.msilOnInitMessageBuild applies them:
-- BPP_TERMS only (includeBapTerms = False, unlike on_confirm -- on_init
-- doesn't echo the BAP's terms back), fulfillment.type, vehicle.energy_type,
-- then ROUTE_INFO.
applyOnInitOrderOverrides :: (CacheFlow m r, MonadFlow m) => Text -> DBC.BecknConfig -> Spec.Order -> m Spec.Order
applyOnInitOrderOverrides transactionId bppConfig =
  patchOrderRouteInfo transactionId
    . patchOrderVehicleEnergyType
    . patchOrderFulfillmentTypes
    . patchOrderTags False Nothing bppConfig

-- | The MSIL on_select order patch: fulfillment.type, vehicle.energy_type,
-- then ROUTE_INFO -- unlike on_confirm/on_init there's no BAP_TERMS/BPP_TERMS
-- or category-id step here, since /on_select's order is built fresh from a
-- Quote (Beckn.OnDemand.Transformer.MSIL.OnSelect.mkOnSelectMessageV2FromQuote)
-- rather than patched from an already-built Layer 1 message.
applyOnSelectOrderOverrides :: (CacheFlow m r, MonadFlow m) => Text -> Spec.Order -> m Spec.Order
applyOnSelectOrderOverrides transactionId =
  patchOrderRouteInfo transactionId
    . patchOrderVehicleEnergyType
    . patchOrderFulfillmentTypes

-- | The MSIL on_status order patch: fulfillment.type then vehicle.energy_type
-- only -- no ROUTE_INFO/terms/category-id step, since on_status is just Layer
-- 1's already-built on_update message re-wrapped
-- (Beckn.OnDemand.Transformer.MSIL.OnStatus.msilOnStatusMessageBuild), with
-- no new Redis lookup or tag work needed.
applyOnStatusOrderOverrides :: Spec.Order -> Spec.Order
applyOnStatusOrderOverrides =
  patchOrderVehicleEnergyType . patchOrderFulfillmentTypes
