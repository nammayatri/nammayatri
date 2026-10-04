module SharedLogic.AddOn
  ( groupAddOnsByTier,
    mkOfferedAddOnData,
    resolveAddOnData,
    verifyAddOnEcho,
    mkAddOnCatalogEntries,
    buildSpecAddOn,
    buildSelectedSpecAddOns,
    getSelectedQuantity,
    addOnChargesTotal,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Map as M
import Domain.Types.AddOnConfig (AddOnConfig)
import qualified Domain.Types.AddOnConfig as DAddOnConfig
import Domain.Types.Common (ServiceTierType)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AddOnConfig as QAddOnConfig

-- | Group the add-ons on offer by the tier they are scoped to. `Nothing`
-- holds the city-wide offers (empty `vehicleServiceTier` list); `Just tier`
-- holds the ones scoped to that tier. A config whose `vehicleServiceTier`
-- lists more than one tier appears under each of them.
groupAddOnsByTier :: [AddOnConfig] -> M.Map (Maybe ServiceTierType) [AddOnConfig]
groupAddOnsByTier configs = M.fromListWith (<>) $ concatMap toEntries configs
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
      Spec.addOnPrice = mkSpecAddOnPrice <$> cfg.pricePerQuantity,
      Spec.addOnQuantity =
        Just
          Spec.ItemQuantity
            { Spec.itemQuantityMaximum = Just Spec.ItemCount {Spec.itemCountCount = Just cfg.maxQuantity},
              Spec.itemQuantitySelected = Nothing
            }
    }

mkSpecAddOnPrice :: HighPrecMoney -> Spec.Price
mkSpecAddOnPrice price =
  Spec.Price
    { Spec.priceValue = Just $ highPrecMoneyToText price,
      Spec.priceCurrency = Nothing,
      Spec.priceComputedValue = Nothing,
      Spec.priceMaximumValue = Nothing,
      Spec.priceMinimumValue = Nothing,
      Spec.priceOfferedValue = Nothing
    }

-- | How many units of an add-on the BAP actually selected -- from
-- `add_on.quantity.selected.count` on the wire, defaulting to 1 when the BAP
-- omits it (a bare opt-in with no explicit count).
getSelectedQuantity :: Spec.AddOn -> Int
getSelectedQuantity addOn =
  fromMaybe 1 $ addOn.addOnQuantity >>= (.itemQuantitySelected) >>= (.itemCountCount)

-- | The catalogue snapshot stored on the search request
-- (`SearchRequest.offeredAddOns`) for one `add_on_config` row advertised in
-- on_search: nothing selected yet, with the max quantity and price frozen as
-- shown to the rider. Everything after search validates and prices against
-- these, never against the live table.
mkOfferedAddOnData :: AddOnConfig -> DAddOnConfig.AddOnData
mkOfferedAddOnData cfg =
  DAddOnConfig.AddOnData
    { configId = cfg.id,
      selectedQuantity = Nothing,
      maxQuantity = cfg.maxQuantity,
      pricePerQuantity = cfg.pricePerQuantity
    }

-- | The wire-level `Spec.AddOn`s to echo back on on_select/on_init/on_confirm
-- for what the BAP actually selected earlier (persisted as `AddOnData` on
-- Quote/SearchTry/Booking) -- unlike `buildSpecAddOn` (used on on_search to
-- advertise the whole catalog, with only `itemQuantityMaximum` set), this
-- returns only the ones actually selected, with `itemQuantitySelected`, the
-- maximum and the price taken from the `AddOnData` (what was shown and
-- charged), not the catalogue's current values. The config row is only read
-- for the descriptor text; one that's since been deleted is silently dropped
-- rather than failing the whole response -- by this point the selection was
-- already validated and charged, so a stale catalog row shouldn't block the
-- callback.
buildSelectedSpecAddOns :: (EsqDBFlow m r, CacheFlow m r) => [DAddOnConfig.AddOnData] -> m [Spec.AddOn]
buildSelectedSpecAddOns [] = pure []
buildSelectedSpecAddOns addOnData = do
  cfgs <- QAddOnConfig.findAllByIds (map (.configId) addOnData)
  let cfgById = M.fromList [(cfg.id, cfg) | cfg <- cfgs]
  pure $ mapMaybe (\d -> withSelection d <$> M.lookup d.configId cfgById) addOnData
  where
    withSelection d cfg =
      let addOn = buildSpecAddOn cfg
       in addOn
            { Spec.addOnPrice = mkSpecAddOnPrice <$> d.pricePerQuantity,
              Spec.addOnQuantity =
                Just
                  Spec.ItemQuantity
                    { Spec.itemQuantityMaximum = Just Spec.ItemCount {Spec.itemCountCount = Just d.maxQuantity},
                      Spec.itemQuantitySelected = d.selectedQuantity <&> \q -> Spec.ItemCount {Spec.itemCountCount = Just q}
                    }
            }

-- | The (config id, selected quantity) pairs the BAP sent, with the ids
-- parsed and duplicates rejected -- a BAP has no way to express "more of the
-- same add-on" other than quantity, so a repeated id is always a client bug,
-- not a legitimate request for two of the same row. Throws a synchronous
-- NACK.
parseSelectedAddOns :: (MonadThrow m, Log m) => [Spec.AddOn] -> m [(Id AddOnConfig, Int)]
parseSelectedAddOns addOns = do
  selected <- forM addOns $ \addOn -> do
    addOnId <- Id <$> (addOn.addOnId & fromMaybeM (InvalidRequest "add_on.id is required"))
    pure (addOnId, getSelectedQuantity addOn)
  let duplicateIds = findDuplicates (map fst selected)
  unless (null duplicateIds) $
    throwError $ InvalidRequest $ "Add-on selected more than once in the same request: " <> show (getId <$> duplicateIds)
  pure selected

-- | Validate every add-on the BAP selected on one item and resolve them into
-- the `AddOnData` rows persisted on Quote/SearchTry/Booking. A BAP can select
-- more than one add-on on the same item (e.g. rider insurance plus a future
-- second add-on), so this validates the whole list, not just one.
--
-- Validation is entirely against `offeredAddOns`, the snapshot on_search
-- stored on the search request (`SearchRequest.offeredAddOns`): a selected id
-- must be one that was offered on this search (which already implies it was
-- enabled and in this city at search time), and the quantity must be within
-- the `maxQuantity` shown then. The live `add_on_config` table is never
-- consulted, so a catalogue edit between search and select can neither
-- reject nor re-price a selection the rider was shown. Each returned row is
-- the offered one with `selectedQuantity` filled in, so it carries the
-- price frozen at search for the rest of the transaction.
resolveAddOnData ::
  (MonadThrow m, Log m) =>
  [DAddOnConfig.AddOnData] ->
  [Spec.AddOn] ->
  m [DAddOnConfig.AddOnData]
resolveAddOnData offeredAddOns addOns = do
  selected <- parseSelectedAddOns addOns
  let offeredById = M.fromList [(d.configId, d) | d <- offeredAddOns]
      notOffered = filter (`M.notMember` offeredById) (map fst selected)
  unless (null notOffered) $
    throwError $ InvalidRequest $ "Add-on(s) were not offered on this search: " <> show (getId <$> notOffered)
  forM selected $ \(addOnId, selectedQuantity) -> do
    offered <- M.lookup addOnId offeredById & fromMaybeM (InvalidRequest $ "Add-on was not offered on this search: " <> getId addOnId)
    unless (selectedQuantity >= 1 && selectedQuantity <= offered.maxQuantity) $
      throwError $ InvalidRequest $ "Add-on quantity " <> show selectedQuantity <> " exceeds the allowed maximum: " <> getId addOnId
    pure offered {DAddOnConfig.selectedQuantity = Just selectedQuantity}

-- | What the selected add-ons cost in total: pricePerQuantity x selectedQuantity,
-- summed over the selection. `Nothing` when none of them is priced (a free
-- opt-in) or nothing is selected, so an unpriced add-on leaves the fare alone.
--
-- Priced from the `pricePerQuantity` carried on each `AddOnData` (frozen at
-- search, selected at /select), never from the live catalogue -- so every
-- fare computed for the selection (the quote at /select, each driver's offer
-- fare in the Estimate flow) charges what the rider was shown, and a later
-- catalogue price edit can't change it.
addOnChargesTotal :: [DAddOnConfig.AddOnData] -> Maybe HighPrecMoney
addOnChargesTotal addOnData =
  let charges = mapMaybe (\d -> (*) <$> d.pricePerQuantity <*> (fromIntegral <$> d.selectedQuantity)) addOnData
   in if null charges then Nothing else Just (sum charges)

findDuplicates :: (Eq a, Hashable a) => [a] -> [a]
findDuplicates xs = HM.keys $ HM.filter (> 1) $ HM.fromListWith (+) [(x, 1 :: Int) | x <- xs]

-- | The opt-in is a one-way ratchet: once selected at /select, the same set
-- of add-ons (same config ids, same quantities) must be echoed at every
-- later step (/init, /confirm) -- and a step can't introduce add-ons that
-- were never selected at /select in the first place. If nothing was ever
-- selected, nothing may be echoed either; this is a no-op only when both
-- sides are empty, so existing integrations that never send an add_on are
-- unaffected. Compares the echo against the `AddOnData` persisted at
-- selection only -- no catalogue lookup -- and throws a synchronous NACK on
-- a missing, extra, or mismatched (id or quantity) echo.
verifyAddOnEcho :: (MonadThrow m, Log m) => [DAddOnConfig.AddOnData] -> [Spec.AddOn] -> m ()
verifyAddOnEcho existingAddOnData addOns
  | null existingAddOnData =
    unless (null addOns) $
      throwError $ InvalidRequest "Add-ons were not selected earlier; none can be echoed now"
  | otherwise = do
    echoed <- parseSelectedAddOns addOns
    let existing = [(d.configId, q) | d <- existingAddOnData, q <- maybeToList d.selectedQuantity]
    unless (length echoed == length existing && HM.fromList echoed == HM.fromList existing) $
      throwError $ InvalidRequest "The echoed add-ons do not match the ones selected earlier"
