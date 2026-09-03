-- Shared overrides/helpers used by every ONDC-scheduled-ride Layer 2 transformer, patching Layer 1's output to ONDC v2.1.0 compliance for the pilot.
module Beckn.OnDemand.Utils.OndcScheduledRide.Common
  ( addTagGroup,
    isSearchResultScheduled,
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
    remapBreakupTitle,
    overrideOrderBreakupTitles,
    overrideOrderFulfillmentId,
    fixItemCompliance,
    overrideOrderItemCompliance,
    overrideOrderStopAuthorizationStatus,
    applyOndcScheduledRideAssignedOrderOverrides,
    applyOndcScheduledRideOrderOverridesIfEnabled,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
import qualified Control.Exception as E
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (isInfixOf)
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.BapMetadata as DBapMetadata
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RideRoute as RI
import qualified Kernel.External.Maps.Google.PolyLinePoints as PolyLine
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Ride (searchRequestKey)
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Tools.Error

-- | Append a tag group to an existing (possibly absent) tag-group list,
-- if there is one to add.
addTagGroup :: Maybe Spec.TagGroup -> Maybe [Spec.TagGroup] -> Maybe [Spec.TagGroup]
addTagGroup Nothing existingGroups = existingGroups
addTagGroup (Just newGroup) existingGroups = Just (fromMaybe [] existingGroups <> [newGroup])

-- True if any estimate or quote in this search result is for a scheduled ride, shared by every on_search patch that needs to know.
isSearchResultScheduled :: DSearch.DSearchRes -> Bool
isSearchResultScheduled dSearchRes =
  any (\(estimate, _, _, _) -> estimate.isScheduled) dSearchRes.estimates
    || any (\(quote, _, _, _) -> quote.isScheduled) dSearchRes.quotes

-- Category --------------------------------------------------------------

-- Rewrites ON_DEMAND_TRIP/ON_DEMAND_RENTAL to their SCHEDULED_* equivalents, so on_search/on_select category ids agree and a BAP can correlate them.
scheduledCategoryCode :: Text -> Text
scheduledCategoryCode = \case
  "ON_DEMAND_TRIP" -> "SCHEDULED_TRIP"
  "ON_DEMAND_RENTAL" -> "SCHEDULED_RENTAL"
  other -> other

-- Applies scheduledCategoryCode to every item's category_ids, gated on the booking's isScheduled, since on_confirm/on_update item builders have no per-item isScheduled to check.
overrideOrderCategoryIds :: Bool -> Spec.Order -> Spec.Order
overrideOrderCategoryIds isScheduled order
  | not isScheduled = order
  | otherwise = order {Spec.orderItems = map fixItem <$> order.orderItems}
  where
    fixItem item = item {Spec.itemCategoryIds = map scheduledCategoryCode <$> item.itemCategoryIds}

-- FulfillmentState --------------------------------------------------------

-- Remaps NEW -> RIDE_CONFIRMED and SCHEDULED_RIDE_ASSIGNED -> RIDE_ASSIGNED on every fulfillment/cancellation-term, since Layer 1 emits codes outside ONDC v2.1.0's vocabulary.
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

-- Collapses Layer 1's fulfillment type to SELF_PICKUP (ride-OTP) or DELIVERY (everything else), since ONDC v2.1.0 only recognizes two fulfillment.type codes for this pilot.
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

-- | Same patch, for on_search's catalog.providers[*].fulfillments (a
-- structurally different field from order.fulfillments).
patchProviderFulfillmentTypes :: Spec.Provider -> Spec.Provider
patchProviderFulfillmentTypes provider =
  provider {Spec.providerFulfillments = map patchFulfillment <$> provider.providerFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentType = overrideFulfillmentType <$> fulfillment.fulfillmentType}

-- RouteInfo --------------------------------------------------------

-- Reuses the fallback route cached at search time to build the ROUTE_INFO tag group, since ONDC v2.1.0 expects the BPP (not the BAP) to compute and send route info.

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

-- Parses/stores the BAP's declared static terms and builds our own terms tag group to echo/attach, since Layer 1 has no notion of BAP_TERMS/BPP_TERMS (STATIC_TERMS, OFFLINE_CONTRACT).

-- | Extract BAP_TERMS.STATIC_TERMS off an incoming wire message's tag list,
-- parse it as a URL, and -- if it parsed and differs from what's on record --
-- store it on that BAP's BapMetadata row. Never throws.
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

mkBppTermsTagGroup :: Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Maybe Spec.TagGroup
mkBppTermsTagGroup mbBapMetadata bppConfig =
  case catMaybes [staticTermsTag, offlineContractTag] of
    [] -> Nothing
    tags -> Just $ Tag.getFullTagGroup Tag.BPP_TERMS tags
  where
    staticTermsTag = Tag.mkTag Tag.STATIC_TERMS . Just . showBaseUrl <$> bppConfig.staticTermsUrl
    offlineContractTag = Tag.mkTag Tag.OFFLINE_CONTRACT . Just . boolTagValue <$> (mbBapMetadata >>= (.offlineContract))

mkBapTermsTagGroup :: Maybe DBapMetadata.BapMetadata -> Maybe Spec.TagGroup
mkBapTermsTagGroup mbBapMetadata =
  case catMaybes [staticTermsTag, offlineContractTag] of
    [] -> Nothing
    tags -> Just $ Tag.getFullTagGroup Tag.BAP_TERMS tags
  where
    staticTermsTag = Tag.mkTag Tag.STATIC_TERMS . Just . showBaseUrl <$> (mbBapMetadata >>= (.staticTermsUrl))
    offlineContractTag = Tag.mkTag Tag.OFFLINE_CONTRACT . Just . boolTagValue <$> (mbBapMetadata >>= (.offlineContract))

-- | The single patch/fix operation for the order's top-level tag list.
-- Always adds BPP_TERMS; adds BAP_TERMS too when 'includeBapTerms' (on_confirm only).
-- Also drops any pre-existing order.tags group that isn't BAP_TERMS/BPP_TERMS.
patchOrderTags :: Bool -> Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Spec.Order -> Spec.Order
patchOrderTags includeBapTerms mbBapMetadata bppConfig order =
  conformingOrder {Spec.orderTags = withBapTerms . withBppTerms $ conformingOrder.orderTags}
  where
    conformingOrder = dropNonConformingOrderTags order
    withBppTerms = addTagGroup (mkBppTermsTagGroup mbBapMetadata bppConfig)
    withBapTerms
      | includeBapTerms = addTagGroup (mkBapTermsTagGroup mbBapMetadata)
      | otherwise = \tagGroups -> tagGroups

-- Drops any other tag group Layer 1 put in order.tags (e.g. BPP_INVOICE_INFO, SETTLEMENT_TERMS), since ONDC v2.1.0's TRV10 schema restricts order.tags[*].descriptor.code to BAP_TERMS/BPP_TERMS only.
dropNonConformingOrderTags :: Spec.Order -> Spec.Order
dropNonConformingOrderTags order = order {Spec.orderTags = fmap (filter isAllowedOrderTag) order.orderTags}
  where
    allowedOrderTagCodes = ["BAP_TERMS", "BPP_TERMS"]
    isAllowedOrderTag tagGroup = maybe False (`elem` allowedOrderTagCodes) (tagGroup.tagGroupDescriptor >>= (.descriptorCode))

-- | Same as patchOrderTags, for on_search's catalog-level tag list
-- (message.catalog.tags -- there's no Order to hang order.tags off of).
patchCatalogTags :: Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Spec.Catalog -> Spec.Catalog
patchCatalogTags mbBapMetadata bppConfig catalog =
  catalog {Spec.catalogTags = addTagGroup (mkBppTermsTagGroup mbBapMetadata bppConfig) catalog.catalogTags}

-- VehicleEnergyType --------------------------------------------------------

-- Passes through any of the seven valid ONDC energy_type codes, otherwise defaults to PETROL, since vehicle.energyType is free text from onboarding not validated against ONDC's vocabulary.
validEnergyTypes :: [Text]
validEnergyTypes = show <$> [Enums.ELECTRIC, Enums.PETROL, Enums.DIESEL, Enums.HYDROGEN, Enums.BIOFUELS, Enums.CNG, Enums.LPG]

overrideVehicleEnergyType :: Text -> Text
overrideVehicleEnergyType energyType
  | energyType `elem` validEnergyTypes = energyType
  | otherwise = show Enums.PETROL

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
-- together -- the two fixes SharedLogic.CallBAP applies to an already-built
-- on_confirm/on_update order for pilot merchants.
applyOrderCategoryAndFulfillmentStateOverrides :: Bool -> Spec.Order -> Spec.Order
applyOrderCategoryAndFulfillmentStateOverrides isScheduled =
  overrideOrderCategoryIds isScheduled . overrideOrderFulfillmentState

-- | The full ONDC-scheduled-ride on_confirm order patch: category ids and
-- fulfillment-state codes, BAP_TERMS + BPP_TERMS, fulfillment.type,
-- vehicle.energy_type, then ROUTE_INFO.
applyOnConfirmOrderOverrides :: (CacheFlow m r, MonadFlow m) => Bool -> Text -> Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Spec.Order -> m Spec.Order
applyOnConfirmOrderOverrides isScheduled transactionId mbBapMetadata bppConfig =
  patchOrderRouteInfo transactionId
    . patchOrderVehicleEnergyType
    . patchOrderFulfillmentTypes
    . patchOrderTags True mbBapMetadata bppConfig
    . applyOrderCategoryAndFulfillmentStateOverrides isScheduled

-- | The full ONDC-scheduled-ride on_init order patch: BPP_TERMS only,
-- fulfillment.type, vehicle.energy_type, then ROUTE_INFO.
applyOnInitOrderOverrides :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Spec.Order -> m Spec.Order
applyOnInitOrderOverrides transactionId mbBapMetadata bppConfig =
  patchOrderRouteInfo transactionId
    . patchOrderVehicleEnergyType
    . patchOrderFulfillmentTypes
    . patchOrderTags False mbBapMetadata bppConfig

-- | The ONDC-scheduled-ride on_select order patch: fulfillment.type,
-- vehicle.energy_type, then ROUTE_INFO.
applyOnSelectOrderOverrides :: (CacheFlow m r, MonadFlow m) => Text -> Spec.Order -> m Spec.Order
applyOnSelectOrderOverrides transactionId =
  patchOrderRouteInfo transactionId
    . patchOrderVehicleEnergyType
    . patchOrderFulfillmentTypes

-- | The ONDC-scheduled-ride on_status order patch: fulfillment.type then
-- vehicle.energy_type only.
applyOnStatusOrderOverrides :: Spec.Order -> Spec.Order
applyOnStatusOrderOverrides =
  patchOrderVehicleEnergyType . patchOrderFulfillmentTypes

-- Breakup --------------------------------------------------------

-- Remaps each breakup title to its closest ONDC-valid title, dropping lines with no valid target, since Layer 1 emits NammaYatri's internal fare-component vocabulary, not ONDC's fixed breakup-title enum.
remapBreakupTitle :: Text -> Maybe Text
remapBreakupTitle title
  | title == show Enums.TOTAL_FARE = Nothing
  | title == show Enums.BASE_FARE = Just "BASE_FARE"
  | title `elem` [show Enums.DISTANCE_FARE, show Enums.DEAD_KILOMETER_FARE, show Enums.TIME_BASED_FARE] = Just "DISTANCE_FARE"
  | title `elem` [show Enums.PARKING_CHARGE, show Enums.PARKING_CHARGE_TAX_EXCLUSIVE, show Enums.PARKING_CHARGE_TAX] = Just "PARKING_CHARGES"
  | title == show Enums.WAITING_OR_PICKUP_CHARGES = Just "WAITING_CHARGES"
  | title == show Enums.NIGHT_SHIFT_CHARGE = Just "NIGHT_CHARGES"
  | title `elem` [show Enums.TOLL_VAT, show Enums.TOLL_FARE_TAX_EXCLUSIVE, show Enums.TOLL_FARE_TAX, show Enums.TOLL_CHARGES] = Just "TOLL_CHARGES"
  | title `elem` [show Enums.CANCELLATION_CHARGES, show Enums.CANCELLATION_FEE_TAX_EXCLUSIVE, show Enums.CANCELLATION_TAX] = Just "CANCELLATION_CHARGES"
  | title == show Enums.CUSTOMER_SELECTED_FARE = Just "BUYER_ADDITIONAL_AMOUNT"
  | title == show Enums.DRIVER_ALLOWANCE = Just "DRIVER_BATA"
  | title `elem` [show Enums.SGST, show Enums.CGST, show Enums.FIXED_GOVERNMENT_RATE, show Enums.RIDE_VAT, show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE, show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX, show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE, show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX] = Just "TAX"
  | title `elem` [show Enums.SERVICE_CHARGE, show Enums.EXTRA_TIME_FARE, show Enums.RIDE_STOP_CHARGES, show Enums.PER_STOP_CHARGES, show Enums.LUGGAGE_CHARGE, show Enums.AIRPORT_CONVENIENCE_FEE, show Enums.RETURN_FEE, show Enums.BOOTH_CHARGE, show Enums.PLATFORM_FEE, show Enums.DRIVER_SELECTED_FARE] = Just "ADD_ONS"
  | otherwise = Nothing

-- | Applies remapBreakupTitle to every breakup line on an already-built
-- order's quote (on_init, on_confirm).
overrideOrderBreakupTitles :: Spec.Order -> Spec.Order
overrideOrderBreakupTitles order = order {Spec.orderQuote = fixQuote <$> Spec.orderQuote order}
  where
    fixQuote quotation = quotation {Spec.quotationBreakup = mapMaybe fixBreakup <$> Spec.quotationBreakup quotation}
    fixBreakup breakup = case Spec.quotationBreakupInnerTitle breakup >>= remapBreakupTitle of
      Nothing -> Nothing
      Just newTitle -> Just breakup {Spec.quotationBreakupInnerTitle = Just newTitle}

-- FulfillmentId --------------------------------------------------------

-- Overrides order.fulfillments[*].id and item.fulfillment_ids to the quote id on every later push, since ONDC Workbench requires fulfillment.id to stay the value announced at on_confirm (booking.quoteId).
overrideOrderFulfillmentId :: Text -> Spec.Order -> Spec.Order
overrideOrderFulfillmentId quoteId order =
  order
    { Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments,
      Spec.orderItems = map patchItem <$> order.orderItems
    }
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentId = Just quoteId}
    patchItem item = item {Spec.itemFulfillmentIds = Just [quoteId]}

-- ItemCompliance --------------------------------------------------------

-- Restricts an item's descriptor.code to RIDE/RENTAL and its tags to a fixed allow-list, shared by every ONDC-scheduled-ride transformer that patches items (order.items or catalog.providers[*].items).
fixItemCompliance :: Spec.Item -> Spec.Item
fixItemCompliance item =
  item
    { Spec.itemDescriptor = fixItemDescriptor <$> Spec.itemDescriptor item,
      Spec.itemTags = filter isAllowedTagGroup <$> Spec.itemTags item
    }
  where
    isRental = maybe False (any ("RENTAL" `isInfixOf`)) (Spec.itemCategoryIds item)
    fixItemDescriptor descriptor = descriptor {Spec.descriptorCode = Just (if isRental then "RENTAL" else "RIDE")}

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

-- Restricts item.descriptor.code to RIDE/RENTAL and item.tags to a fixed allow-list, since the dynamic-offer /select flow's on_select has no ONDC compliance fix for order.items.
overrideOrderItemCompliance :: Spec.Order -> Spec.Order
overrideOrderItemCompliance order = order {Spec.orderItems = map fixItemCompliance <$> Spec.orderItems order}

-- StopAuthorization --------------------------------------------------------

-- Flips the START stop's OTP authorization.status to CLAIMED once the ride has started (no-op before that), since Layer 1 always sends it as UNCLAIMED.
overrideOrderStopAuthorizationStatus :: Bool -> Spec.Order -> Spec.Order
overrideOrderStopAuthorizationStatus isRideStarted order
  | not isRideStarted = order
  | otherwise = order {Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentStops = map patchStop <$> fulfillment.fulfillmentStops}
    patchStop stop
      | stop.stopType == Just "START" = stop {Spec.stopAuthorization = claim <$> stop.stopAuthorization}
      | otherwise = stop
    claim auth = auth {Spec.authorizationStatus = Just "CLAIMED"}

-- RideAssigned (on_confirm / on_update / on_status pushes) ----------------

-- Applies fulfillment-state, category-id, breakup, tag, fulfillment-type, fulfillment-id and stop-authorization overrides together, since the on_confirm and on_update ride-assigned pushes both build through the same Layer 1 path and need the identical ONDC fix.
applyOndcScheduledRideAssignedOrderOverrides :: Bool -> Text -> Bool -> Spec.Order -> Spec.Order
applyOndcScheduledRideAssignedOrderOverrides isScheduled quoteId isRideStarted =
  dropNonConformingOrderTags
    . patchOrderFulfillmentTypes
    . overrideOrderCategoryIds isScheduled
    . overrideOrderFulfillmentState
    . overrideOrderBreakupTitles
    . overrideOrderFulfillmentId quoteId
    . overrideOrderStopAuthorizationStatus isRideStarted

-- Fetches the pilot gate and applies applyOndcScheduledRideAssignedOrderOverrides only when enabled, replacing the fetch+check+apply every on_update/on_status push needing this override used to duplicate.
applyOndcScheduledRideOrderOverridesIfEnabled ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  Text ->
  Bool ->
  Maybe Spec.ConfirmReqMessage ->
  m (Maybe Spec.ConfirmReqMessage)
applyOndcScheduledRideOrderOverridesIfEnabled merchantOperatingCityId isScheduled quoteId isRideStarted mbMsg = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOperatingCityId.getId)
  let isOndcScheduledRideSupportEnabled = fromMaybe False transporterConfig.enableOndcScheduledRideSupport
  pure $
    if isOndcScheduledRideSupportEnabled
      then (\msg -> msg {Spec.confirmReqMessageOrder = applyOndcScheduledRideAssignedOrderOverrides isScheduled quoteId isRideStarted msg.confirmReqMessageOrder}) <$> mbMsg
      else mbMsg
