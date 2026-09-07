module SharedLogic.AddOn
  ( getAddOn,
    resolveAddOnData,
    verifyAddOnEcho,
    mkAddOnCatalogEntries,
    buildSpecAddOn,
    buildSelectedSpecAddOns,
    getSelectedQuantity,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Map as M
import Domain.Types.AddOnConfig (AddOnConfig)
import qualified Domain.Types.AddOnConfig as DAddOnConfig
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AddOnConfig as QAddOnConfig

-- | Fetch every enabled add-on on offer in a city, grouped by the tier it is
-- scoped to. `Nothing` holds the city-wide offers (empty `vehicleServiceTier`
-- list); `Just tier` holds the ones scoped to that tier. A config whose
-- `vehicleServiceTier` lists more than one tier appears under each of them.
getAddOn ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  m (M.Map (Maybe ServiceTierType) [AddOnConfig])
getAddOn merchantOpCityId enabled = do
  configs <- QAddOnConfig.findAllByMerchantOpCityIdAndEnabled merchantOpCityId enabled
  pure $ M.fromListWith (<>) $ concatMap toEntries configs
  where
    toEntries cfg
      | null cfg.vehicleServiceTier = [(Nothing, [cfg])]
      | otherwise = [(Just tier, [cfg]) | tier <- cfg.vehicleServiceTier]

-- | The add-ons that should be attached to a single catalog item for the
-- given tier: the tier-specific offers plus whatever applies to every tier.
mkAddOnCatalogEntries :: M.Map (Maybe ServiceTierType) [AddOnConfig] -> ServiceTierType -> [AddOnConfig]
mkAddOnCatalogEntries addOnMap tier =
  M.findWithDefault [] (Just tier) addOnMap <> M.findWithDefault [] Nothing addOnMap

-- | Build the wire-level `Spec.AddOn` advertised in `on_search` for one
-- `add_on_config` row. The wire id is the row's own `id` (its primary key),
-- which is also what `resolveAddOnData` looks the selection back up by --
-- `addOnType` only shows up in the descriptor code, for the BAP to display.
buildSpecAddOn :: AddOnConfig -> Spec.AddOn
buildSpecAddOn cfg =
  Spec.AddOn
    { Spec.addOnId = Just $ getId cfg.id,
      Spec.addOnDescriptor =
        Just $
          Spec.Descriptor
            { Spec.descriptorCode = Just $ show cfg.addOnType,
              Spec.descriptorName = Just cfg.descriptorName,
              Spec.descriptorShortDesc = cfg.descriptorShortDesc,
              Spec.descriptorLongDesc = Nothing
            },
      Spec.addOnPrice =
        cfg.pricePerQuantity <&> \price ->
          Spec.Price
            { Spec.priceValue = Just $ highPrecMoneyToText price,
              Spec.priceCurrency = Nothing,
              Spec.priceComputedValue = Nothing,
              Spec.priceMaximumValue = Nothing,
              Spec.priceMinimumValue = Nothing,
              Spec.priceOfferedValue = Nothing
            },
      Spec.addOnQuantity =
        Just
          Spec.ItemQuantity
            { Spec.itemQuantityMaximum = Just Spec.ItemCount {Spec.itemCountCount = Just cfg.maxQuantity},
              Spec.itemQuantitySelected = Nothing
            }
    }

-- | How many units of an add-on the BAP actually selected -- from
-- `add_on.quantity.selected.count` on the wire, defaulting to 1 when the BAP
-- omits it (a bare opt-in with no explicit count).
getSelectedQuantity :: Spec.AddOn -> Int
getSelectedQuantity addOn =
  fromMaybe 1 $ addOn.addOnQuantity >>= (.itemQuantitySelected) >>= (.itemCountCount)

-- | The wire-level `Spec.AddOn`s to echo back on on_select/on_init/on_confirm
-- for what the BAP actually selected earlier (persisted as `AddOnData` on
-- Quote/SearchTry/Booking) -- unlike `buildSpecAddOn` (used on on_search to
-- advertise the whole catalog, with only `itemQuantityMaximum` set), this
-- returns only the ones actually selected, with `itemQuantitySelected` set to
-- what was picked. Batches the config lookup in one query; a config that's
-- since been deleted is silently dropped rather than failing the whole
-- response -- by this point the selection was already validated and charged,
-- so a stale catalog row shouldn't block the callback.
buildSelectedSpecAddOns :: (EsqDBFlow m r, CacheFlow m r) => [DAddOnConfig.AddOnData] -> m [Spec.AddOn]
buildSelectedSpecAddOns [] = pure []
buildSelectedSpecAddOns addOnData = do
  cfgs <- QAddOnConfig.findAllByIds (map (.configId) addOnData)
  let cfgById = M.fromList [(cfg.id, cfg) | cfg <- cfgs]
  pure $ mapMaybe (\d -> withSelectedQuantity d.selectedQuantity <$> M.lookup d.configId cfgById) addOnData
  where
    withSelectedQuantity selectedQuantity cfg =
      let addOn = buildSpecAddOn cfg
       in addOn
            { Spec.addOnQuantity =
                addOn.addOnQuantity <&> \q -> q {Spec.itemQuantitySelected = Just Spec.ItemCount {Spec.itemCountCount = Just selectedQuantity}}
            }

-- | Verify one incoming `add_on` selection against its already-fetched
-- `add_on_config` row: that it actually belongs to the given city, that it's
-- `enabled`, and -- if the row's `vehicleServiceTier` list is non-empty --
-- that the given tier is in it. On failure it throws directly (a synchronous
-- NACK, never an async on_x NACK).
validateAddOnConfig ::
  (MonadThrow m, Log m) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe ServiceTierType ->
  Spec.AddOn ->
  AddOnConfig ->
  m ()
validateAddOnConfig merchantOpCityId mbTier addOn cfg = do
  let addOnIdText = getId cfg.id
  unless (cfg.merchantOperatingCityId == merchantOpCityId) $
    throwError $ InvalidRequest $ "Add-on not available in this city: " <> addOnIdText
  unless cfg.enabled $
    throwError $ InvalidRequest $ "Add-on is not enabled: " <> addOnIdText
  unless (tierEligible) $
    throwError $ InvalidRequest $ "Add-on not available for this vehicle tier: " <> addOnIdText
  let selectedQuantity = getSelectedQuantity addOn
  unless (selectedQuantity >= 1 && selectedQuantity <= cfg.maxQuantity) $
    throwError $ InvalidRequest $ "Add-on quantity " <> show selectedQuantity <> " exceeds the allowed maximum: " <> addOnIdText
  where
    tierEligible = null cfg.vehicleServiceTier || maybe False (`elem` cfg.vehicleServiceTier) mbTier

-- | Validate every add-on the BAP selected on one item and resolve them into
-- the `AddOnData` rows persisted on Quote/SearchTry/Booking. A BAP can select
-- more than one add-on on the same item (e.g. rider insurance plus a future
-- second add-on), so this validates the whole list, not just one. Rejects
-- the same add-on being selected twice in one request -- a BAP has no way to
-- express "more of the same add-on" other than quantity, so a repeated id is
-- always a client bug, not a legitimate request for two of the same row.
--
-- Fetches all the selected configs in a single batched query rather than one
-- lookup per add-on: a fetched-count mismatch against the requested ids means
-- at least one id was invalid, so that's checked and thrown up front too,
-- before the per-config (city/enabled/tier/quantity) checks run.
resolveAddOnData ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe ServiceTierType ->
  [Spec.AddOn] ->
  m [DAddOnConfig.AddOnData]
resolveAddOnData merchantOpCityId mbTier addOns = do
  addOnIds <- forM addOns $ \addOn -> Id <$> (addOn.addOnId & fromMaybeM (InvalidRequest "add_on.id is required"))
  let duplicateIds = findDuplicates addOnIds
  unless (null duplicateIds) $
    throwError $ InvalidRequest $ "Add-on selected more than once in the same request: " <> show (getId <$> duplicateIds)
  cfgs <- QAddOnConfig.findAllByIds addOnIds
  let cfgById = M.fromList [(cfg.id, cfg) | cfg <- cfgs]
  when (length cfgs /= length addOnIds) $
    throwError $ InvalidRequest $ "Invalid add-on id(s): " <> show (getId <$> filter (`M.notMember` cfgById) addOnIds)
  forM (zip addOnIds addOns) $ \(addOnId, addOn) -> do
    cfg <- M.lookup addOnId cfgById & fromMaybeM (InvalidRequest $ "Invalid add-on id: " <> getId addOnId)
    validateAddOnConfig merchantOpCityId mbTier addOn cfg
    pure DAddOnConfig.AddOnData {configId = cfg.id, selectedQuantity = getSelectedQuantity addOn}

findDuplicates :: (Eq a, Hashable a) => [a] -> [a]
findDuplicates xs = HM.keys $ HM.filter (> 1) $ HM.fromListWith (+) [(x, 1 :: Int) | x <- xs]

-- | The opt-in is a one-way ratchet: once selected at /select, the same set
-- of add-ons (same config ids, same quantities) must be echoed at every
-- later step (/init, /confirm) -- and a step can't introduce add-ons that
-- were never selected at /select in the first place. If nothing was ever
-- selected, nothing may be echoed either; this is a no-op only when both
-- sides are empty, so existing integrations that never send an add_on are
-- unaffected. Throws a synchronous NACK on a missing, extra, or mismatched
-- (id or quantity) echo -- `AddOnData`'s derived `Eq` compares both.
verifyAddOnEcho ::
  (EsqDBFlow m r, CacheFlow m r) =>
  [DAddOnConfig.AddOnData] ->
  Id DMOC.MerchantOperatingCity ->
  Maybe ServiceTierType ->
  [Spec.AddOn] ->
  m ()
verifyAddOnEcho existingAddOnData merchantOpCityId mbTier addOns
  | null existingAddOnData =
    unless (null addOns) $
      throwError $ InvalidRequest "Add-ons were not selected earlier; none can be echoed now"
  | otherwise = do
    echoedAddOnData <- resolveAddOnData merchantOpCityId mbTier addOns
    let toHashMap = HM.fromList . map (\d -> (d.configId, d.selectedQuantity))
    unless
      ( length echoedAddOnData == length existingAddOnData
          && toHashMap echoedAddOnData == toHashMap existingAddOnData
      )
      $ throwError $
        InvalidRequest "The echoed add-ons do not match the ones selected earlier"
