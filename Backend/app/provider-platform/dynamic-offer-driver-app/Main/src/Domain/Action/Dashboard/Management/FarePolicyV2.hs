{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Typed fare-policy management (dev/docs/fare-policy-revamp-plan.md).
-- Replaces the 94-column CSV contract for the new editor. Invariants:
--   * writes validate the COMPLETE policy before any DB mutation (no wipe-then-validate);
--   * replace keeps the fare-policy id stable (delete+create under the same id);
--   * every write clears the Redis policy cache and the airport per-km derived cache;
--   * a policy row carrying a merchantOperatingCityId different from the caller's city
--     is rejected (the legacy update endpoint had no such scoping).
module Domain.Action.Dashboard.Management.FarePolicyV2
  ( getFarePolicyV2List,
    getFarePolicyV2Policy,
    postFarePolicyV2PolicyReplace,
    postFarePolicyV2BulkReplace,
    postFarePolicyV2Preview,
    postFarePolicyV2ProductCreate,
    postFarePolicyV2ProductUpdate,
    postFarePolicyV2ProductRemove,
    getFarePolicyV2ChangeRequestList,
    postFarePolicyV2ChangeRequestDecide,
    getFarePolicyV2AlertsSubscriptions,
    postFarePolicyV2AlertsSubscribe,
    postFarePolicyV2AlertsUnsubscribe,
  )
where

import qualified API.Types.ProviderPlatform.Management.FarePolicyV2 as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as DPM
import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.Common as DCommon
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub, sort, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.CancellationFarePolicy as DCFP
import Domain.Types.Common (TripCategory (..))
import qualified Domain.Types.ConditionalCharges as DTAC
import Domain.Types.Extra.ConditionalCharges (ConditionalChargesCategories (..))
import qualified Domain.Types.FareAlertSubscription as DFAS
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FarePolicy.Common as DFPC
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as DDriverExtraFeeBounds
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as FarePolicyD
import qualified Domain.Types.FarePolicyChangeRequest as DFPCR
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.ServiceTierType (ServiceTierType)
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Email.Flow as Email
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareCalculator as SFC
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.Cac.FarePolicy as CQFP
import qualified Storage.CachedQueries.CancellationFarePolicy as CQCFP
import qualified Storage.CachedQueries.FareProduct as CQFProduct
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CancellationFarePolicy as QCFP
import qualified Storage.Queries.ConditionalCharges as QCC
import qualified Storage.Queries.FareAlertSubscription as QFAS
import qualified Storage.Queries.FarePolicy.FarePolicyAmbulanceDetailsSlab as QFPAD
import qualified Storage.Queries.FarePolicyChangeRequest as QFPCR
import qualified Storage.Queries.FareProduct as QFareProduct
import qualified Storage.Queries.FareProductExtra as QFareProductExtra

--------------------------------------------------------------------------------
-- context resolution
--------------------------------------------------------------------------------

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  pure (merchant, merchantOpCity)

getTransporterConfig' :: Id DMOC.MerchantOperatingCity -> Flow TransporterConfig
getTransporterConfig' mocId =
  getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing
    >>= fromMaybeM (TransporterConfigNotFound mocId.getId)

-- legacy rows may carry no city; reject only a definite mismatch
checkCityScope :: Id DMOC.MerchantOperatingCity -> FarePolicyD.FarePolicy -> Flow ()
checkCityScope mocId policy =
  whenJust policy.merchantOperatingCityId $ \pcId ->
    unless (pcId == mocId) $ throwError (InvalidRequest "Fare policy belongs to a different operating city")

--------------------------------------------------------------------------------
-- list
--------------------------------------------------------------------------------

getFarePolicyV2List ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe TripCategory ->
  Maybe SL.Area ->
  Maybe ServiceTierType ->
  Maybe Bool ->
  Flow Common.FPV2ProductListRes
getFarePolicyV2List merchantShortId opCity mbTripCategory mbArea mbServiceTier mbEnabled = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  allProducts <- CQFProduct.findAllFareProductByMerchantOpCityId merchantOpCity.id
  tierConfigs <- CQVST.findAllByMerchantOpCityId merchantOpCity.id Nothing
  -- the city's display name for a tier; the UI falls back to the enum
  let tierName st = listToMaybe [t.name | t <- tierConfigs, t.serviceTierType == st]
  let filtered =
        filter
          ( \fp ->
              maybe True (\tc -> fp.tripCategory == tc) mbTripCategory
                && maybe True (\a -> fp.area == a) mbArea
                && maybe True (\st -> fp.vehicleServiceTier == st) mbServiceTier
                && maybe True (\e -> fp.enabled == e) mbEnabled
          )
          allProducts
  items <- mapM (mkItem tierName) filtered
  pure $ Common.FPV2ProductListRes {fareProducts = catMaybes items}
  where
    mkItem tierName fp = do
      mbPolicy <- CQFP.findById Nothing fp.farePolicyId
      case mbPolicy of
        Nothing -> do
          -- a dangling farePolicyId must be visible, never silently dropped
          logError $ "getFarePolicyV2List: dangling farePolicyId " <> fp.farePolicyId.getId <> " on fareProduct " <> fp.id.getId
          pure Nothing
        Just policy ->
          pure $
            Just
              Common.FPV2ProductListItem
                { fareProductId = cast fp.id,
                  farePolicyId = cast fp.farePolicyId,
                  serviceTier = fp.vehicleServiceTier,
                  serviceTierName = tierName fp.vehicleServiceTier,
                  tripCategory = fp.tripCategory,
                  area = fp.area,
                  timeBounds = fp.timeBounds,
                  searchSource = toApiSearchSource fp.searchSource,
                  enabled = fp.enabled,
                  disableRecompute = fp.disableRecompute,
                  summary = mkSummary policy
                }

mkSummary :: FarePolicyD.FarePolicy -> Common.FPV2PolicySummary
mkSummary policy =
  case policy.farePolicyDetails of
    FarePolicyD.ProgressiveDetails d ->
      base Common.Progressive (Just d.baseFare) (Just (NE.head d.perExtraKmRateSections).perExtraKmRate) ((.perMinRate.amount) . NE.head <$> d.perMinRateSections) (d.nightShiftCharge)
    FarePolicyD.SlabsDetails d ->
      base Common.Slabs (Just (NE.head d.slabs).baseFare) Nothing Nothing ((NE.head d.slabs).nightShiftCharge)
    FarePolicyD.RentalDetails d ->
      base Common.Rental (Just d.baseFare) (Just d.plannedPerKmRate) Nothing (d.nightShiftCharge)
    FarePolicyD.InterCityDetails d ->
      base Common.InterCity (Just d.baseFare) (Just d.perKmRateOneWay) Nothing (d.nightShiftCharge)
    FarePolicyD.AmbulanceDetails d ->
      base Common.Ambulance (Just (NE.head d.slabs).baseFare) (Just (NE.head d.slabs).perKmRate) Nothing ((NE.head d.slabs).nightShiftCharge)
  where
    base fpType baseFare perKm perMin nsc =
      Common.FPV2PolicySummary
        { farePolicyType = fpType,
          baseFare = baseFare,
          perKmRate = perKm,
          perMinRate = perMin,
          nightShiftCharge = toApiNightShiftCharge <$> nsc,
          congestionChargeMultiplier = toApiCongestionMultiplier <$> policy.congestionChargeMultiplier
        }

--------------------------------------------------------------------------------
-- read one policy
--------------------------------------------------------------------------------

getFarePolicyV2Policy :: ShortId DM.Merchant -> Context.City -> Id DCommon.FarePolicy -> Flow Common.FPV2PolicyRes
getFarePolicyV2Policy merchantShortId opCity reqFarePolicyId = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  let farePolicyId :: Id FarePolicyD.FarePolicy = cast reqFarePolicyId
  policy <- CQFP.findById Nothing farePolicyId >>= fromMaybeM (InvalidRequest $ "Fare policy not found: " <> farePolicyId.getId)
  checkCityScope merchantOpCity.id policy
  mbCancellation <- maybe (pure Nothing) CQCFP.findById policy.cancellationFarePolicyId
  pure
    Common.FPV2PolicyRes
      { farePolicyId = reqFarePolicyId,
        currency = policy.currency,
        distanceUnit = policy.distanceUnit,
        createdAt = policy.createdAt,
        updatedAt = policy.updatedAt,
        policy = toApiPolicy policy mbCancellation
      }

--------------------------------------------------------------------------------
-- replace (single + bulk)
--------------------------------------------------------------------------------

postFarePolicyV2PolicyReplace ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DCommon.FarePolicy ->
  Maybe Bool ->
  Common.FPV2ReplaceReq ->
  Flow Common.FPV2ReplaceRes
postFarePolicyV2PolicyReplace merchantShortId opCity reqFarePolicyId mbDryRun req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  transporterConfig <- getTransporterConfig' merchantOpCity.id
  let dryRun = fromMaybe False mbDryRun
  (issues, diff, apply) <- prepareReplace merchantOpCity transporterConfig (cast reqFarePolicyId) req.policy
  if dryRun || not (null issues)
    then pure Common.FPV2ReplaceRes {applied = False, issues, diff}
    else do
      apply
      pure Common.FPV2ReplaceRes {applied = True, issues = [], diff}

postFarePolicyV2BulkReplace ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Bool ->
  Common.FPV2BulkReplaceReq ->
  Flow Common.FPV2BulkReplaceRes
postFarePolicyV2BulkReplace merchantShortId opCity mbDryRun req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  transporterConfig <- getTransporterConfig' merchantOpCity.id
  let dryRun = fromMaybe False mbDryRun
  -- validate EVERYTHING first; apply only when every replacement is clean
  prepared <- forM req.replacements $ \r -> do
    (issues, diff, apply) <- prepareReplace merchantOpCity transporterConfig (cast r.farePolicyId) r.policy
    pure (r.farePolicyId, issues, diff, apply)
  let anyIssues = any (\(_, issues, _, _) -> not (null issues)) prepared
      results = map (\(fpId, issues, diff, _) -> Common.FPV2BulkReplaceItem {farePolicyId = fpId, issues, diff}) prepared
  if dryRun || anyIssues
    then pure Common.FPV2BulkReplaceRes {applied = False, results}
    else do
      -- serialize writes for the city so concurrent bulk saves cannot interleave
      Redis.withWaitOnLockRedisWithExpiry (farePolicyV2CityLockKey merchantOpCity.id) 10 60 $
        mapM_ (\(_, _, _, apply) -> apply) prepared
      pure Common.FPV2BulkReplaceRes {applied = True, results}

farePolicyV2CityLockKey :: Id DMOC.MerchantOperatingCity -> Text
farePolicyV2CityLockKey cityId = "FarePolicyV2:BulkReplace:CityId-" <> cityId.getId

-- | Validate + diff + build the deferred write. NOTHING is mutated until the
-- returned action runs, so a validation failure can never wipe existing config.
prepareReplace ::
  DMOC.MerchantOperatingCity ->
  TransporterConfig ->
  Id FarePolicyD.FarePolicy ->
  Common.FPV2Policy ->
  Flow ([Common.FPV2ValidationIssue], [Common.FPV2FieldDiff], Flow ())
prepareReplace merchantOpCity transporterConfig farePolicyId apiPolicy = do
  old <- CQFP.findById Nothing farePolicyId >>= fromMaybeM (InvalidRequest $ "Fare policy not found: " <> farePolicyId.getId)
  checkCityScope merchantOpCity.id old
  oldCancellation <- maybe (pure Nothing) CQCFP.findById old.cancellationFarePolicyId
  now <- getCurrentTime
  let issues = validatePolicy transporterConfig apiPolicy
      diff = computeDiff (toApiPolicy old oldCancellation) apiPolicy
  pure
    ( issues,
      diff,
      do
        mbCancellationId <- upsertInlineCancellation merchantOpCity old.cancellationFarePolicyId apiPolicy.cancellationFarePolicy now
        newPolicy <- fromApiPolicy (ExistingPolicyCtx old.id old.currency old.distanceUnit old.merchantId old.merchantOperatingCityId old.createdAt) mbCancellationId now apiPolicy
        CQFP.delete old.id
        CQFP.create newPolicy
        writeAmbulanceSlabs newPolicy
        replaceConditionalCharges newPolicy now apiPolicy
        SpecialZoneDriverDemand.clearAirportPerKmFareCacheForPolicy old.id
    )

-- inline cancellation policy: always mint a NEW row and re-point the policy;
-- the old row stays (rides may still reference it), which keeps this write safe
upsertInlineCancellation ::
  DMOC.MerchantOperatingCity ->
  Maybe (Id DCFP.CancellationFarePolicy) ->
  Maybe Common.FPV2CancellationFarePolicy ->
  UTCTime ->
  Flow (Maybe (Id DCFP.CancellationFarePolicy))
upsertInlineCancellation _ oldId Nothing _ = pure oldId
upsertInlineCancellation merchantOpCity _ (Just c) now = do
  newId <- generateGUID
  QCFP.create
    DCFP.CancellationFarePolicy
      { id = newId,
        currency = merchantOpCity.currency,
        description = c.description,
        freeCancellationTimeSeconds = c.freeCancellationTimeSeconds,
        maxWaitingTimeAtPickupSeconds = c.maxWaitingTimeAtPickupSeconds,
        minCancellationCharge = c.minCancellationCharge,
        maxCancellationCharge = c.maxCancellationCharge,
        perMetreCancellationCharge = c.perMetreCancellationCharge,
        perMinuteCancellationCharge = c.perMinuteCancellationCharge,
        percentageOfRideFareToBeCharged = c.percentageOfRideFareToBeCharged,
        createdAt = now,
        updatedAt = now
      }
  pure (Just newId)

-- Queries.FarePolicy.create skips ambulance slabs ("can be done with slabs"),
-- so replace/create writes them explicitly, mirroring the legacy CSV upsert
writeAmbulanceSlabs :: FarePolicyD.FarePolicy -> Flow ()
writeAmbulanceSlabs policy = case policy.farePolicyDetails of
  FarePolicyD.AmbulanceDetails d -> mapM_ (\slab -> QFPAD.create (policy.id, slab)) (NE.toList d.slabs)
  _ -> pure ()

-- Reconcile by category: update rows whose category persists (preserving
-- createdAt), create new ones, delete the ones dropped from the request —
-- replace semantics, matching how every other policy field clears on omission.
replaceConditionalCharges :: FarePolicyD.FarePolicy -> UTCTime -> Common.FPV2Policy -> Flow ()
replaceConditionalCharges policy now apiPolicy = do
  existing <- QCC.findAllByFp policy.id.getId
  let requested = fromMaybe [] apiPolicy.conditionalCharges
      requestedCategories = map (fromApiChargeCategory . (.chargeCategory)) requested
  forM_ existing $ \e ->
    unless (e.chargeCategory `elem` requestedCategories) $
      QCC.deleteByFpAndCategory policy.id.getId e.chargeCategory
  forM_ requested $ \r -> do
    let category = fromApiChargeCategory r.chargeCategory
        row =
          DTAC.ConditionalCharges
            { cgstPercentage = r.cgstPercentage,
              charge = r.charge,
              chargeCategory = category,
              farePolicyId = policy.id.getId,
              sgstPercentage = r.sgstPercentage,
              createdAt = now,
              updatedAt = now
            }
    case find (\e -> e.chargeCategory == category) existing of
      Just e -> QCC.updateByPrimaryKey row {DTAC.createdAt = e.createdAt}
      Nothing -> QCC.create row

--------------------------------------------------------------------------------
-- preview
--------------------------------------------------------------------------------

postFarePolicyV2Preview :: ShortId DM.Merchant -> Context.City -> Common.FPV2PreviewReq -> Flow Common.FPV2PreviewRes
postFarePolicyV2Preview merchantShortId opCity req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  transporterConfig <- getTransporterConfig' merchantOpCity.id
  now <- getCurrentTime
  (domainPolicy, mbCancellation, serviceTier, tripCategory) <- case (req.farePolicyId, req.policy) of
    (Just fpId, _) -> do
      policy <- CQFP.findById Nothing (cast fpId) >>= fromMaybeM (InvalidRequest $ "Fare policy not found: " <> fpId.getId)
      checkCityScope merchantOpCity.id policy
      mbCanc <- maybe (pure Nothing) CQCFP.findById policy.cancellationFarePolicyId
      refs <- CQFProduct.findAllFareProductByFarePolicyId policy.id
      let mbRef = listToMaybe refs
      serviceTier <- fromMaybeM (InvalidRequest "serviceTier required: policy is not bound to any fare product") (req.serviceTier <|> ((.vehicleServiceTier) <$> mbRef))
      tripCategory <- fromMaybeM (InvalidRequest "tripCategory required: policy is not bound to any fare product") (req.tripCategory <|> ((.tripCategory) <$> mbRef))
      pure (policy, mbCanc, serviceTier, tripCategory)
    (Nothing, Just apiPolicy) -> do
      let issues = validatePolicy transporterConfig apiPolicy
      unless (null issues) $
        throwError (InvalidRequest $ "Policy invalid: " <> T.intercalate "; " (map (\i -> i.field <> ": " <> i.message) issues))
      serviceTier <- req.serviceTier & fromMaybeM (InvalidRequest "serviceTier is required for inline policy preview")
      tripCategory <- req.tripCategory & fromMaybeM (InvalidRequest "tripCategory is required for inline policy preview")
      tempId <- generateGUID
      policy <- fromApiPolicy (NewPolicyCtx tempId merchantOpCity.currency merchantOpCity.distanceUnit merchant.id merchantOpCity.id) Nothing now apiPolicy
      pure (policy, Nothing, serviceTier, tripCategory)
    (Nothing, Nothing) -> throwError (InvalidRequest "either farePolicyId or an inline policy is required")
  let emptyCongestion = FarePolicyD.CongestionChargeDetails Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      fullFarePolicy = FarePolicyD.farePolicyToFullFarePolicy merchant.id serviceTier tripCategory mbCancellation emptyCongestion Nothing domainPolicy Nothing
  trips <- forM req.sampleTrips $ \trip -> do
    let params =
          SFC.CalculateFareParametersParams
            { farePolicy = fullFarePolicy,
              actualDistance = Just trip.distance,
              rideTime = fromMaybe now trip.rideTime,
              returnTime = Nothing,
              roundTrip = False,
              waitingTime = trip.waitingTimeMin,
              stopWaitingTimes = [],
              actualRideDuration = Nothing,
              vehicleAge = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = Nothing,
              nightShiftOverlapChecking = isJust trip.rideTime,
              estimatedDistance = Just trip.distance,
              estimatedRideDuration = Just trip.duration,
              estimatedRideStaticDuration = Nothing,
              estimatedCongestionCharge = Nothing,
              timeDiffFromUtc = Just transporterConfig.timeDiffFromUtc,
              tollCharges = Nothing,
              noOfStops = 0,
              currency = domainPolicy.currency,
              distanceUnit = domainPolicy.distanceUnit,
              petCharges = Nothing,
              shouldApplyBusinessDiscount = False,
              shouldApplyPersonalDiscount = False,
              merchantOperatingCityId = Just merchantOpCity.id,
              mbAdditonalChargeCategories = Nothing,
              numberOfLuggages = Nothing,
              govtChargesRate = Just transporterConfig.taxConfig.rideGst,
              pickupGateId = Nothing,
              fareSettlementType = Nothing
            }
    fareParams <- SFC.calculateFareParameters params
    let totalFare = SFC.fareSum fareParams (Just [])
        bounds = (\b -> DDriverExtraFeeBounds.findDriverExtraFeeBoundsByDistance trip.distance b) <$> domainPolicy.driverExtraFeeBounds
        minFee = maybe 0 (.minFee) bounds
        maxFee = maybe 0 (.maxFee) bounds
        pickupSpread = case domainPolicy.farePolicyDetails of
          FarePolicyD.ProgressiveDetails d -> d.pickupCharges.pickupChargesMax - d.pickupCharges.pickupChargesMin
          _ -> 0
        breakup = SFC.mkFareParamsDisplayBreakups True identity Common.FPV2FareBreakupItem fareParams
    pure
      Common.FPV2TripPreview
        { distance = trip.distance,
          duration = trip.duration,
          totalFare = totalFare,
          minFare = totalFare + minFee,
          maxFare = totalFare + maxFee + pickupSpread,
          breakup = breakup
        }
  pure $ Common.FPV2PreviewRes {trips = trips}

--------------------------------------------------------------------------------
-- product create / remove
--------------------------------------------------------------------------------

postFarePolicyV2ProductCreate :: ShortId DM.Merchant -> Context.City -> Common.FPV2CreateProductReq -> Flow Common.FPV2CreateProductRes
postFarePolicyV2ProductCreate merchantShortId opCity req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  transporterConfig <- getTransporterConfig' merchantOpCity.id
  now <- getCurrentTime
  -- reject a duplicate combo up front
  existing <- CQFProduct.findAllFareProductByMerchantOpCityId merchantOpCity.id
  let clash fp =
        fp.tripCategory == req.tripCategory
          && fp.area == req.area
          && fp.vehicleServiceTier == req.serviceTier
          && fp.timeBounds == req.timeBounds
          && fp.searchSource == fromApiSearchSource req.searchSource
  whenJust (find clash existing) $ \fp ->
    throwError (InvalidRequest $ "A fare product for this combo already exists: " <> fp.id.getId)
  newPolicyId <- generateGUID
  newPolicy <- case (req.sourceFarePolicyId, req.policy) of
    (_, Just apiPolicy) -> do
      let issues = validatePolicy transporterConfig apiPolicy
      unless (null issues) $
        throwError (InvalidRequest $ "Policy invalid: " <> T.intercalate "; " (map (\i -> i.field <> ": " <> i.message) issues))
      mbCancellationId <- upsertInlineCancellation merchantOpCity Nothing apiPolicy.cancellationFarePolicy now
      fromApiPolicy (NewPolicyCtx newPolicyId merchantOpCity.currency merchantOpCity.distanceUnit merchant.id merchantOpCity.id) mbCancellationId now apiPolicy
    (Just sourceId, Nothing) -> do
      source <- CQFP.findById Nothing (cast sourceId) >>= fromMaybeM (InvalidRequest $ "Source fare policy not found: " <> sourceId.getId)
      -- a legacy source may predate validation; refuse to propagate an invalid
      -- template into a new combo (same bar as the inline-policy path)
      let cloneIssues = validatePolicy transporterConfig (toApiPolicy source Nothing)
      unless (null cloneIssues) $
        throwError (InvalidRequest $ "Source policy invalid, fix it before cloning: " <> T.intercalate "; " (map (\i -> i.field <> ": " <> i.message) cloneIssues))
      pure $ (source :: FarePolicyD.FarePolicy) {FarePolicyD.id = newPolicyId, FarePolicyD.merchantId = Just merchant.id, FarePolicyD.merchantOperatingCityId = Just merchantOpCity.id, FarePolicyD.createdAt = now, FarePolicyD.updatedAt = now}
    (Nothing, Nothing) -> throwError (InvalidRequest "either sourceFarePolicyId or an inline policy is required")
  -- the variant must be able to price the trip category: a Progressive policy
  -- bound to a Rental combo (say, cloned from a OneWay donor) misprices or
  -- breaks end-ride slab resolution, and nothing downstream re-checks this.
  -- Allowed sets follow production data: Rental/Ambulance are strictly their
  -- own variant; InterCity and CrossCity run both InterCity and normal
  -- (Progressive/Slabs) policies, per destination city or as the generic
  -- (city = Nothing) fallback.
  let allowedVariants = case req.tripCategory of
        Rental _ -> [FarePolicyD.Rental]
        InterCity _ _ -> [FarePolicyD.InterCity, FarePolicyD.Progressive, FarePolicyD.Slabs]
        CrossCity _ _ -> [FarePolicyD.InterCity, FarePolicyD.Progressive, FarePolicyD.Slabs]
        Ambulance _ -> [FarePolicyD.Ambulance]
        _ -> [FarePolicyD.Progressive, FarePolicyD.Slabs]
      variant = FarePolicyD.getFarePolicyType newPolicy
  unless (variant `elem` allowedVariants) $
    throwError (InvalidRequest $ "A " <> show variant <> " policy cannot price " <> show req.tripCategory <> " trips; expected one of " <> show allowedVariants <> ". Pick a donor from the same trip category.")
  CQFP.create newPolicy
  writeAmbulanceSlabs newPolicy
  whenJust req.policy $ \apiPolicy -> replaceConditionalCharges newPolicy now apiPolicy
  -- conditional charges live in their own table keyed by policy id, so a clone
  -- must copy them explicitly or the new combo silently loses them
  whenJust req.sourceFarePolicyId $ \sourceId ->
    when (isNothing req.policy) $ do
      sourceCharges <- QCC.findAllByFp sourceId.getId
      forM_ sourceCharges $ \c ->
        QCC.create c {DTAC.farePolicyId = newPolicy.id.getId, DTAC.createdAt = now, DTAC.updatedAt = now}
  newProductId <- generateGUID
  let fareProduct =
        DFareProduct.FareProduct
          { id = newProductId,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCity.id,
            farePolicyId = newPolicy.id,
            tripCategory = req.tripCategory,
            vehicleServiceTier = req.serviceTier,
            area = req.area,
            timeBounds = req.timeBounds,
            searchSource = fromApiSearchSource req.searchSource,
            enabled = req.enabled,
            disableRecompute = req.disableRecompute
          }
  before <- activeCoverage merchantOpCity.id
  CQFProduct.create fareProduct
  CQFProduct.clearCache fareProduct
  CQFProduct.clearCacheById merchantOpCity.id
  after <- activeCoverage merchantOpCity.id
  sendCoverageAlerts merchantOpCity before after "dashboard (combo created)"
  pure Common.FPV2CreateProductRes {fareProductId = cast fareProduct.id, farePolicyId = cast newPolicy.id}

-- | In-place combo attribute update; toggling `enabled` or moving `timeBounds`
-- changes rider-visible coverage, so it feeds the same alert pipeline.
postFarePolicyV2ProductUpdate :: ShortId DM.Merchant -> Context.City -> Id DCommon.FareProduct -> Common.FPV2UpdateProductReq -> Flow APISuccess
postFarePolicyV2ProductUpdate merchantShortId opCity reqFareProductId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  let fareProductId :: Id DFareProduct.FareProduct = cast reqFareProductId
  fareProduct <- QFareProduct.findByPrimaryKey fareProductId >>= fromMaybeM (InvalidRequest $ "Fare product not found: " <> fareProductId.getId)
  unless (fareProduct.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Fare product belongs to a different operating city")
  -- moving timeBounds must not land on another product's combo slot (same
  -- duplicate rule as create; two products for one combo resolve arbitrarily)
  whenJust req.timeBounds $ \newBounds ->
    when (newBounds /= fareProduct.timeBounds) $ do
      existing <- CQFProduct.findAllFareProductByMerchantOpCityId merchantOpCity.id
      let clash fp =
            fp.id /= fareProduct.id
              && fp.tripCategory == fareProduct.tripCategory
              && fp.area == fareProduct.area
              && fp.vehicleServiceTier == fareProduct.vehicleServiceTier
              && fp.timeBounds == newBounds
              && fp.searchSource == fareProduct.searchSource
      whenJust (find clash existing) $ \fp ->
        throwError (InvalidRequest $ "A fare product for this combo already exists: " <> fp.id.getId)
  -- detach: legacy data binds one policy to many combos, so an in-place edit
  -- reprices all of them. detachFarePolicy forks this combo onto its own deep
  -- copy (policy row + ambulance slabs + conditional charges) so later edits
  -- stop affecting the others. No-op when the combo already owns its policy.
  mbDetachedPolicyId <-
    if req.detachFarePolicy == Just True
      then do
        boundProducts <- CQFProduct.findAllFareProductByFarePolicyId fareProduct.farePolicyId
        if length boundProducts <= 1
          then pure Nothing
          else do
            source <- CQFP.findById Nothing fareProduct.farePolicyId >>= fromMaybeM (InvalidRequest $ "Fare policy not found: " <> fareProduct.farePolicyId.getId)
            now <- getCurrentTime
            clonePolicyId <- generateGUID
            let clone = (source :: FarePolicyD.FarePolicy) {FarePolicyD.id = clonePolicyId, FarePolicyD.createdAt = now, FarePolicyD.updatedAt = now}
            CQFP.create clone
            writeAmbulanceSlabs clone
            sourceCharges <- QCC.findAllByFp source.id.getId
            forM_ sourceCharges $ \c ->
              QCC.create c {DTAC.farePolicyId = clonePolicyId.getId, DTAC.createdAt = now, DTAC.updatedAt = now}
            pure (Just clonePolicyId)
      else pure Nothing
  before <- activeCoverage merchantOpCity.id
  let updated =
        fareProduct
          { DFareProduct.enabled = fromMaybe fareProduct.enabled req.enabled,
            DFareProduct.disableRecompute = req.disableRecompute <|> fareProduct.disableRecompute,
            DFareProduct.timeBounds = fromMaybe fareProduct.timeBounds req.timeBounds,
            DFareProduct.farePolicyId = fromMaybe fareProduct.farePolicyId mbDetachedPolicyId
          }
  QFareProduct.updateByPrimaryKey updated
  CQFProduct.clearCache fareProduct
  CQFProduct.clearCache updated
  CQFProduct.clearCacheById merchantOpCity.id
  after <- activeCoverage merchantOpCity.id
  sendCoverageAlerts merchantOpCity before after "dashboard (combo updated)"
  pure Success

-- | Maker-checker: filing a removal deletes NOTHING — it creates a PENDING
-- change request that a different dashboard user must approve. Removing a
-- combo can silently take a whole vehicle tier off the city, hence the gate.
postFarePolicyV2ProductRemove :: ShortId DM.Merchant -> Context.City -> Id DCommon.FareProduct -> Common.FPV2RemoveProductReq -> Flow Common.FPV2ChangeRequestRes
postFarePolicyV2ProductRemove merchantShortId opCity reqFareProductId req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  let fareProductId :: Id DFareProduct.FareProduct = cast reqFareProductId
  fareProduct <- QFareProduct.findByPrimaryKey fareProductId >>= fromMaybeM (InvalidRequest $ "Fare product not found: " <> fareProductId.getId)
  unless (fareProduct.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Fare product belongs to a different operating city")
  requestedBy <- fromMaybeM (InvalidRequest "requestedBy missing (must be set by the dashboard proxy)") req.requestedBy
  pending <- QFPCR.findAllByFareProductId fareProduct.id
  when (any (\r -> r.status == DFPCR.PENDING) pending) $
    throwError (InvalidRequest "A pending removal request already exists for this combo")
  requestId <- generateGUID
  now <- getCurrentTime
  QFPCR.create
    DFPCR.FarePolicyChangeRequest
      { id = requestId,
        merchantId = merchant.id,
        merchantOperatingCityId = merchantOpCity.id,
        action = DFPCR.REMOVE_FARE_PRODUCT,
        fareProductId = fareProduct.id,
        fareProductSnapshot = decodeUtf8 (BSL.toStrict (A.encode fareProduct)),
        status = DFPCR.PENDING,
        requestedBy,
        checkedBy = Nothing,
        reason = req.reason,
        remarks = Nothing,
        createdAt = now,
        updatedAt = now
      }
  pure Common.FPV2ChangeRequestRes {requestId = cast requestId, status = Common.PENDING}

getFarePolicyV2ChangeRequestList :: ShortId DM.Merchant -> Context.City -> Maybe Common.FPV2ChangeRequestStatus -> Flow Common.FPV2ChangeRequestListRes
getFarePolicyV2ChangeRequestList merchantShortId opCity mbStatus = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  requests <- QFPCR.findAllByMerchantOperatingCityId merchantOpCity.id
  let filtered = filter (\r -> maybe True (\s -> fromApiChangeStatus s == r.status) mbStatus) requests
      sorted = sortOn (Down . (.createdAt)) filtered
  pure $ Common.FPV2ChangeRequestListRes {requests = map toApiChangeRequest sorted}
  where
    toApiChangeRequest r =
      Common.FPV2ChangeRequest
        { requestId = cast r.id,
          action = DCommon.REMOVE_FARE_PRODUCT,
          status = toApiChangeStatus r.status,
          fareProductId = cast r.fareProductId,
          comboSummary = r.fareProductSnapshot,
          requestedBy = r.requestedBy,
          checkedBy = r.checkedBy,
          reason = r.reason,
          remarks = r.remarks,
          createdAt = r.createdAt
        }

postFarePolicyV2ChangeRequestDecide :: ShortId DM.Merchant -> Context.City -> Id DCommon.FarePolicyChangeRequest -> Common.FPV2DecideChangeRequestReq -> Flow APISuccess
postFarePolicyV2ChangeRequestDecide merchantShortId opCity reqRequestId req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  let requestId :: Id DFPCR.FarePolicyChangeRequest = cast reqRequestId
  request <- QFPCR.findByPrimaryKey requestId >>= fromMaybeM (InvalidRequest $ "Change request not found: " <> requestId.getId)
  unless (request.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Change request belongs to a different operating city")
  unless (request.status == DFPCR.PENDING) $
    throwError (InvalidRequest "Change request is already decided")
  checkedBy <- fromMaybeM (InvalidRequest "checkedBy missing (must be set by the dashboard proxy)") req.checkedBy
  when (checkedBy == request.requestedBy) $
    throwError (InvalidRequest "Maker and checker must be different users")
  if req.approve
    then do
      executeProductRemoval merchantOpCity request.fareProductId checkedBy
      QFPCR.updateStatusById DFPCR.APPROVED (Just checkedBy) req.remarks request.id
    else QFPCR.updateStatusById DFPCR.REJECTED (Just checkedBy) req.remarks request.id
  pure Success

executeProductRemoval :: DMOC.MerchantOperatingCity -> Id DFareProduct.FareProduct -> Text -> Flow ()
executeProductRemoval merchantOpCity fareProductId actor = do
  fareProduct <- QFareProduct.findByPrimaryKey fareProductId >>= fromMaybeM (InvalidRequest $ "Fare product already removed: " <> fareProductId.getId)
  before <- activeCoverage merchantOpCity.id
  QFareProductExtra.delete fareProduct.id
  CQFProduct.clearCache fareProduct
  CQFProduct.clearCacheById merchantOpCity.id
  -- delete the policy only when nothing references it anymore (cross-city sharing)
  refs <- CQFProduct.findAllFareProductByFarePolicyId fareProduct.farePolicyId
  when (null refs) $ do
    CQFP.delete fareProduct.farePolicyId
    SpecialZoneDriverDemand.clearAirportPerKmFareCacheForPolicy fareProduct.farePolicyId
  after <- activeCoverage merchantOpCity.id
  sendCoverageAlerts merchantOpCity before after actor

--------------------------------------------------------------------------------
-- coverage alerts
--------------------------------------------------------------------------------

-- the city's active (area, vehicle tier) matrix; rendered as Text to stay
-- independent of Ord instances on domain enums
activeCoverage :: Id DMOC.MerchantOperatingCity -> Flow (Set.Set (Text, Text))
activeCoverage mocId = do
  products <- CQFProduct.findAllFareProductByMerchantOpCityId mocId
  pure $ Set.fromList [(T.pack (show p.area), T.pack (show p.vehicleServiceTier)) | p <- products, p.enabled]

sendCoverageAlerts :: DMOC.MerchantOperatingCity -> Set.Set (Text, Text) -> Set.Set (Text, Text) -> Text -> Flow ()
sendCoverageAlerts merchantOpCity before after actor = do
  let activated = Set.toList (Set.difference after before)
      deactivated = Set.toList (Set.difference before after)
  unless (null activated && null deactivated) $
    fork "farePolicyV2 coverage alert email" $ do
      subscriptions <- QFAS.findAllByMerchantOperatingCityId merchantOpCity.id
      let recipients = [s.email | s <- subscriptions, s.alertType == DFAS.AREA_VEHICLES]
      if null recipients
        then logInfo $ "coverage changed in " <> show merchantOpCity.city <> " but no AREA_VEHICLES subscribers"
        else do
          transporterConfig <- getTransporterConfig' merchantOpCity.id
          emailServiceConfig <- asks (.emailServiceConfig)
          let fromEmail = fromMaybe "no-reply@nammayatri.in" transporterConfig.tdsFromEmail
              section label cells =
                if null cells
                  then ""
                  else label <> ":\n" <> T.unlines (map (\(a, v) -> "  - area " <> a <> " / vehicle " <> v) cells) <> "\n"
              subject = "[Fare Coverage] Area-Vehicles changed in " <> T.pack (show merchantOpCity.city)
              body =
                "The fare coverage matrix of "
                  <> T.pack (show merchantOpCity.city)
                  <> " changed (by "
                  <> actor
                  <> ").\n\n"
                  <> section "Activated" activated
                  <> section "Deactivated" deactivated
                  <> "\nThis is a system-generated email from the fare policy dashboard."
          liftIO $ Email.sendPlainEmail emailServiceConfig fromEmail recipients subject body

--------------------------------------------------------------------------------
-- alert subscriptions
--------------------------------------------------------------------------------

getFarePolicyV2AlertsSubscriptions :: ShortId DM.Merchant -> Context.City -> Flow Common.FPV2SubscriptionListRes
getFarePolicyV2AlertsSubscriptions merchantShortId opCity = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  subscriptions <- QFAS.findAllByMerchantOperatingCityId merchantOpCity.id
  pure $
    Common.FPV2SubscriptionListRes
      { subscriptions =
          map
            (\s -> Common.FPV2Subscription {email = s.email, alertType = DCommon.AREA_VEHICLES, subscribedBy = s.subscribedBy, createdAt = s.createdAt})
            subscriptions
      }

postFarePolicyV2AlertsSubscribe :: ShortId DM.Merchant -> Context.City -> Common.FPV2SubscriptionReq -> Flow APISuccess
postFarePolicyV2AlertsSubscribe merchantShortId opCity req = do
  (merchant, merchantOpCity) <- resolveCity merchantShortId opCity
  let email = T.toLower (T.strip req.email)
  unless ("@" `T.isInfixOf` email) $ throwError (InvalidRequest "invalid email address")
  existing <- QFAS.findAllByMerchantOperatingCityId merchantOpCity.id
  -- idempotent: an existing identical subscription is a no-op
  unless (any (\s -> s.email == email && s.alertType == DFAS.AREA_VEHICLES) existing) $ do
    subscriptionId <- generateGUID
    now <- getCurrentTime
    QFAS.create
      DFAS.FareAlertSubscription
        { id = subscriptionId,
          merchantId = merchant.id,
          merchantOperatingCityId = merchantOpCity.id,
          email,
          alertType = DFAS.AREA_VEHICLES,
          subscribedBy = email,
          createdAt = now,
          updatedAt = now
        }
  pure Success

postFarePolicyV2AlertsUnsubscribe :: ShortId DM.Merchant -> Context.City -> Common.FPV2SubscriptionReq -> Flow APISuccess
postFarePolicyV2AlertsUnsubscribe merchantShortId opCity req = do
  (_, merchantOpCity) <- resolveCity merchantShortId opCity
  let email = T.toLower (T.strip req.email)
  existing <- QFAS.findAllByMerchantOperatingCityId merchantOpCity.id
  forM_ (filter (\s -> s.email == email && s.alertType == DFAS.AREA_VEHICLES) existing) $ \s ->
    QFAS.deleteById s.id
  pure Success

toApiChangeStatus :: DFPCR.FarePolicyChangeStatus -> Common.FPV2ChangeRequestStatus
toApiChangeStatus = \case
  DFPCR.PENDING -> Common.PENDING
  DFPCR.APPROVED -> Common.APPROVED
  DFPCR.REJECTED -> Common.REJECTED

fromApiChangeStatus :: Common.FPV2ChangeRequestStatus -> DFPCR.FarePolicyChangeStatus
fromApiChangeStatus = \case
  Common.PENDING -> DFPCR.PENDING
  Common.APPROVED -> DFPCR.APPROVED
  Common.REJECTED -> DFPCR.REJECTED

--------------------------------------------------------------------------------
-- validation
--------------------------------------------------------------------------------

validatePolicy :: TransporterConfig -> Common.FPV2Policy -> [Common.FPV2ValidationIssue]
validatePolicy transporterConfig p =
  concat
    [ validateDetails p.farePolicyDetails,
      validateBounds p.driverExtraFeeBounds,
      percentageIssue "businessDiscountPercentage" p.businessDiscountPercentage,
      percentageIssue "personalDiscountPercentage" p.personalDiscountPercentage,
      nonNegativeMaybe "serviceCharge" p.serviceCharge,
      nonNegativeMaybe "parkingCharge" p.parkingCharge,
      nonNegativeMaybe "perStopCharge" p.perStopCharge,
      nonNegativeMaybe "perLuggageCharge" p.perLuggageCharge,
      nonNegativeMaybe "tollCharges" p.tollCharges,
      nonNegativeMaybe "petCharges" p.petCharges,
      nonNegativeMaybe "platformFee" p.platformFee
    ]
  where
    issue field message = [Common.FPV2ValidationIssue {field, message}]
    percentageIssue field = maybe [] (\v -> if v < 0 || v > 100 then issue field "must be between 0 and 100" else [])
    nonNegativeMaybe field = maybe [] (\v -> if v < 0 then issue field "must not be negative" else [])
    minBaseFareIssue baseFare =
      case transporterConfig.minBaseFare of
        Just minBase
          | baseFare < minBase && not (fromMaybe False transporterConfig.allowFarePolicyUpdateBelowMinBaseFare) ->
            issue "baseFare" ("below the configured minimum base fare " <> show minBase)
        _ -> []
    sortedUnique :: Ord a => Text -> Text -> [a] -> [Common.FPV2ValidationIssue]
    sortedUnique field label keys =
      let sorted = sort keys
       in if length (nub sorted) /= length sorted then issue field ("duplicate " <> label) else []
    -- Rental/InterCity pricing-slab invariants (upstream commit 03c947988:
    -- "added slab checks for intercity and rentals"): slab resolution at
    -- end-ride errors out without a 0/0 base row, and a completed ride is
    -- undercharged unless the slab picked at 100/100 charges the whole fare.
    pricingSlabIssues :: [Common.FPV2PricingSlab] -> [Common.FPV2ValidationIssue]
    pricingSlabIssues slabs =
      concat
        [ if not (any isBaseSlab slabs)
            then issue "farePolicyDetails.pricingSlabs" "must include a base row with timePercentage = 0 and distancePercentage = 0; rides on this policy could not be ended otherwise"
            else [],
          case pickForCompletedRide slabs of
            Just s
              | not (isFullFareSlab s) ->
                issue "farePolicyDetails.pricingSlabs" "the slab picked for a fully completed ride must charge the whole fare: set farePercentage = 100 (or include actual time/distance) on the highest row"
            _ -> []
        ]
      where
        isBaseSlab s = s.timePercentage == 0 && s.distancePercentage == 0
        isFullFareSlab s = s.farePercentage >= 100 || s.includeActualTimePercentage || s.includeActualDistPercentage
        -- mirrors findFPXDetailsByTimeAndDistancePercentage 100 100: highest
        -- applicable row by (timePercentage, distancePercentage)
        pickForCompletedRide =
          listToMaybe
            . sortOn (\s -> Down (s.timePercentage, s.distancePercentage))
            . filter (\s -> s.timePercentage <= 100 && s.distancePercentage <= 100)
    validateBounds Nothing = []
    validateBounds (Just bounds) =
      concat
        [ concatMap (\b -> if b.minFee > b.maxFee then issue "driverExtraFeeBounds" "minFee must not exceed maxFee" else []) bounds,
          concatMap (\b -> if b.minFee < 0 || b.stepFee < 0 || b.defaultStepFee < 0 then issue "driverExtraFeeBounds" "fees must not be negative" else []) bounds,
          sortedUnique "driverExtraFeeBounds" "startDistance" (map (.startDistance) bounds)
        ]
    validateDetails = \case
      Common.FPV2Progressive d ->
        concat
          [ if null d.perExtraKmRateSections then issue "farePolicyDetails.perExtraKmRateSections" "at least one section is required" else [],
            sortedUnique "farePolicyDetails.perExtraKmRateSections" "startDistance" (map (.startDistance) d.perExtraKmRateSections),
            maybe [] (sortedUnique "farePolicyDetails.perMinRateSections" "rideDurationInMin" . map (.rideDurationInMin)) d.perMinRateSections,
            if d.pickupCharges.pickupChargesMin > d.pickupCharges.pickupChargesMax then issue "farePolicyDetails.pickupCharges" "min must not exceed max" else [],
            if d.baseFare < 0 then issue "farePolicyDetails.baseFare" "must not be negative" else [],
            minBaseFareIssue d.baseFare
          ]
      Common.FPV2Slabs d ->
        concat
          [ if null d.slabs then issue "farePolicyDetails.slabs" "at least one slab is required" else [],
            sortedUnique "farePolicyDetails.slabs" "startDistance" (map (.startDistance) d.slabs),
            concatMap (\s -> minBaseFareIssue s.baseFare) d.slabs
          ]
      Common.FPV2Rental d ->
        concat
          [ if null d.distanceBuffers then issue "farePolicyDetails.distanceBuffers" "at least one buffer is required" else [],
            if null d.pricingSlabs then issue "farePolicyDetails.pricingSlabs" "at least one pricing slab is required" else [],
            sortedUnique "farePolicyDetails.distanceBuffers" "rideDuration" (map (.rideDuration) d.distanceBuffers),
            pricingSlabIssues d.pricingSlabs,
            minBaseFareIssue d.baseFare
          ]
      Common.FPV2InterCity d ->
        concat
          [ if null d.pricingSlabs then issue "farePolicyDetails.pricingSlabs" "at least one pricing slab is required" else [],
            pricingSlabIssues d.pricingSlabs,
            minBaseFareIssue d.baseFare
          ]
      Common.FPV2Ambulance d ->
        concat
          [ if null d.slabs then issue "farePolicyDetails.slabs" "at least one slab is required" else [],
            sortedUnique "farePolicyDetails.slabs" "vehicleAge" (map (.vehicleAge) d.slabs),
            concatMap (\s -> minBaseFareIssue s.baseFare) d.slabs
          ]

--------------------------------------------------------------------------------
-- diff (flattened JSON compare of the API representation)
--------------------------------------------------------------------------------

computeDiff :: Common.FPV2Policy -> Common.FPV2Policy -> [Common.FPV2FieldDiff]
computeDiff old new =
  let oldM = Map.fromList (flattenValue "" (A.toJSON old))
      newM = Map.fromList (flattenValue "" (A.toJSON new))
      keys = nub (Map.keys oldM <> Map.keys newM)
   in mapMaybe
        ( \k ->
            let o = Map.lookup k oldM
                n = Map.lookup k newM
             in if o == n then Nothing else Just Common.FPV2FieldDiff {field = k, oldValue = o, newValue = n}
        )
        keys

flattenValue :: Text -> A.Value -> [(Text, Text)]
flattenValue prefix = \case
  A.Object o -> concatMap (\(k, v) -> flattenValue (joinKey prefix (AK.toText k)) v) (AKM.toList o)
  A.Array vals -> concat (zipWith (\i v -> flattenValue (prefix <> "[" <> show (i :: Int) <> "]") v) [0 ..] (toList vals))
  A.Null -> []
  leaf -> [(prefix, renderLeaf leaf)]
  where
    joinKey "" k = k
    joinKey p k = p <> "." <> k
    renderLeaf = \case
      A.String t -> t
      other -> decodeUtf8 (BSL.toStrict (A.encode other))

--------------------------------------------------------------------------------
-- domain <-> API mapping
--------------------------------------------------------------------------------

toApiPolicy :: FarePolicyD.FarePolicy -> Maybe DCFP.CancellationFarePolicy -> Common.FPV2Policy
toApiPolicy policy mbCancellation =
  Common.FPV2Policy
    { farePolicyDetails = toApiDetails policy.farePolicyDetails,
      driverExtraFeeBounds = map toApiBounds . NE.toList <$> policy.driverExtraFeeBounds,
      serviceCharge = policy.serviceCharge,
      parkingCharge = policy.parkingCharge,
      perStopCharge = policy.perStopCharge,
      perLuggageCharge = policy.perLuggageCharge,
      returnFee = toApiReturnFee <$> policy.returnFee,
      boothCharges = toApiBoothCharge <$> policy.boothCharges,
      nightShiftBounds = (\b -> Common.FPV2NightShiftBounds {nightShiftStart = b.nightShiftStart, nightShiftEnd = b.nightShiftEnd}) <$> policy.nightShiftBounds,
      allowedTripDistanceBounds = (\b -> Common.FPV2AllowedTripDistanceBounds {minAllowedTripDistance = b.minAllowedTripDistance, maxAllowedTripDistance = b.maxAllowedTripDistance}) <$> policy.allowedTripDistanceBounds,
      tollCharges = policy.tollCharges,
      petCharges = policy.petCharges,
      driverAllowance = policy.driverAllowance,
      airportConvenienceFee = policy.airportConvenienceFee,
      businessDiscountPercentage = policy.businessDiscountPercentage,
      personalDiscountPercentage = policy.personalDiscountPercentage,
      priorityCharges = policy.priorityCharges,
      pickupBufferInSecsForNightShiftCal = policy.pickupBufferInSecsForNightShiftCal,
      tipOptions = policy.tipOptions,
      additionalCongestionCharge = Just policy.additionalCongestionCharge,
      perMinuteRideExtraTimeCharge = policy.perMinuteRideExtraTimeCharge,
      rideExtraTimeChargeGracePeriod = policy.rideExtraTimeChargeGracePeriod,
      congestionChargeMultiplier = toApiCongestionMultiplier <$> policy.congestionChargeMultiplier,
      perDistanceUnitInsuranceCharge = policy.perDistanceUnitInsuranceCharge,
      cardCharge = (\c -> Common.FPV2CardCharge {perDistanceUnitMultiplier = c.perDistanceUnitMultiplier, fixed = c.fixed}) <$> policy.cardCharge,
      vatChargeConfig = toApiChargeConfig <$> policy.vatChargeConfig,
      commissionChargeConfig = toApiChargeConfig <$> policy.commissionChargeConfig,
      cancellationCommissionChargeConfig = toApiChargeConfig <$> policy.cancellationCommissionChargeConfig,
      tollTaxChargeConfig = toApiChargeConfig <$> policy.tollTaxChargeConfig,
      platformFee = policy.platformFee,
      platformFeeCgst = policy.cgst,
      platformFeeSgst = policy.sgst,
      platformFeeChargesBy = Just (toApiPlatformFeeMethod policy.platformFeeChargesBy),
      conditionalCharges = if null policy.conditionalCharges then Nothing else Just (map toApiConditionalCharge policy.conditionalCharges),
      driverCancellationNotAllowed = policy.driverCancellationNotAllowed,
      cancellationFarePolicy = toApiCancellation <$> mbCancellation,
      description = policy.description
    }
  where
    toApiBounds b =
      Common.FPV2DriverExtraFeeBounds
        { startDistance = b.startDistance,
          stepFee = b.stepFee,
          defaultStepFee = b.defaultStepFee,
          minFee = b.minFee,
          maxFee = b.maxFee
        }
    toApiConditionalCharge c =
      Common.FPV2ConditionalCharge
        { chargeCategory = toApiChargeCategory c.chargeCategory,
          charge = c.charge,
          cgstPercentage = c.cgstPercentage,
          sgstPercentage = c.sgstPercentage
        }
    toApiCancellation c =
      Common.FPV2CancellationFarePolicy
        { description = c.description,
          freeCancellationTimeSeconds = c.freeCancellationTimeSeconds,
          maxWaitingTimeAtPickupSeconds = c.maxWaitingTimeAtPickupSeconds,
          minCancellationCharge = c.minCancellationCharge,
          maxCancellationCharge = c.maxCancellationCharge,
          perMetreCancellationCharge = c.perMetreCancellationCharge,
          perMinuteCancellationCharge = c.perMinuteCancellationCharge,
          percentageOfRideFareToBeCharged = c.percentageOfRideFareToBeCharged
        }

toApiDetails :: FarePolicyD.FarePolicyDetails -> Common.FPV2FarePolicyDetails
toApiDetails = \case
  FarePolicyD.ProgressiveDetails d ->
    Common.FPV2Progressive
      Common.FPV2ProgressiveDetails
        { baseFare = d.baseFare,
          baseDistance = d.baseDistance,
          deadKmFare = d.deadKmFare,
          pickupCharges = Common.FPV2PickupCharges {pickupChargesMin = d.pickupCharges.pickupChargesMin, pickupChargesMax = d.pickupCharges.pickupChargesMax},
          perExtraKmRateSections = map (\s -> Common.FPV2PerExtraKmRateSection {startDistance = s.startDistance, perExtraKmRate = s.perExtraKmRate, baseFareDepreciation = s.baseFareDepreciation}) (NE.toList d.perExtraKmRateSections),
          perMinRateSections = map (\s -> Common.FPV2PerMinRateSection {rideDurationInMin = s.rideDurationInMin, perMinRate = s.perMinRate.amount}) . NE.toList <$> d.perMinRateSections,
          perMinRateDurationBasis = toApiDurationBasis <$> d.perMinRateDurationBasis,
          waitingChargeInfo = toApiWaitingChargeInfo <$> d.waitingChargeInfo,
          nightShiftCharge = toApiNightShiftCharge <$> d.nightShiftCharge
        }
  FarePolicyD.SlabsDetails d ->
    Common.FPV2Slabs
      Common.FPV2SlabsDetails
        { slabs =
            map
              ( \s ->
                  Common.FPV2Slab
                    { startDistance = s.startDistance,
                      baseFare = s.baseFare,
                      waitingChargeInfo = toApiWaitingChargeInfo <$> s.waitingChargeInfo,
                      platformFeeInfo = toApiPlatformFeeInfo <$> s.platformFeeInfo,
                      nightShiftCharge = toApiNightShiftCharge <$> s.nightShiftCharge
                    }
              )
              (NE.toList d.slabs)
        }
  FarePolicyD.RentalDetails d ->
    Common.FPV2Rental
      Common.FPV2RentalDetails
        { baseFare = d.baseFare,
          perHourCharge = d.perHourCharge,
          perExtraKmRate = d.perExtraKmRate,
          perExtraMinRate = d.perExtraMinRate,
          includedKmPerHr = d.includedKmPerHr,
          plannedPerKmRate = d.plannedPerKmRate,
          deadKmFare = d.deadKmFare,
          maxAdditionalKmsLimit = d.maxAdditionalKmsLimit,
          totalAdditionalKmsLimit = d.totalAdditionalKmsLimit,
          distanceBuffers = map (\b -> Common.FPV2RentalDistanceBuffer {rideDuration = b.rideDuration, bufferKms = b.bufferKms, bufferMeters = b.bufferMeters}) (NE.toList d.distanceBuffers),
          pricingSlabs = map (\s -> Common.FPV2PricingSlab {timePercentage = s.timePercentage, distancePercentage = s.distancePercentage, farePercentage = s.farePercentage, includeActualTimePercentage = s.includeActualTimePercentage, includeActualDistPercentage = s.includeActualDistPercentage}) (NE.toList d.pricingSlabs),
          waitingChargeInfo = toApiWaitingChargeInfo <$> d.waitingChargeInfo,
          nightShiftCharge = toApiNightShiftCharge <$> d.nightShiftCharge
        }
  FarePolicyD.InterCityDetails d ->
    Common.FPV2InterCity
      Common.FPV2InterCityDetails
        { baseFare = d.baseFare,
          perHourCharge = d.perHourCharge,
          perKmRateOneWay = d.perKmRateOneWay,
          perKmRateRoundTrip = d.perKmRateRoundTrip,
          perExtraKmRate = d.perExtraKmRate,
          perExtraMinRate = d.perExtraMinRate,
          kmPerPlannedExtraHour = d.kmPerPlannedExtraHour,
          deadKmFare = d.deadKmFare,
          perDayMaxHourAllowance = d.perDayMaxHourAllowance,
          perDayMaxAllowanceInMins = d.perDayMaxAllowanceInMins,
          defaultWaitTimeAtDestination = d.defaultWaitTimeAtDestination,
          stateEntryPermitCharges = d.stateEntryPermitCharges,
          pricingSlabs = map (\s -> Common.FPV2PricingSlab {timePercentage = s.timePercentage, distancePercentage = s.distancePercentage, farePercentage = s.farePercentage, includeActualTimePercentage = s.includeActualTimePercentage, includeActualDistPercentage = s.includeActualDistPercentage}) (NE.toList d.pricingSlabs),
          waitingChargeInfo = toApiWaitingChargeInfo <$> d.waitingChargeInfo,
          nightShiftCharge = toApiNightShiftCharge <$> d.nightShiftCharge
        }
  FarePolicyD.AmbulanceDetails d ->
    Common.FPV2Ambulance
      Common.FPV2AmbulanceDetails
        { slabs =
            map
              ( \s ->
                  Common.FPV2AmbulanceSlab
                    { vehicleAge = s.vehicleAge,
                      baseFare = s.baseFare,
                      baseDistance = s.baseDistance,
                      perKmRate = s.perKmRate,
                      waitingChargeInfo = toApiWaitingChargeInfo <$> s.waitingChargeInfo,
                      platformFeeInfo = toApiPlatformFeeInfo <$> s.platformFeeInfo,
                      nightShiftCharge = toApiNightShiftCharge <$> s.nightShiftCharge
                    }
              )
              (NE.toList d.slabs)
        }

data PolicyCtx
  = ExistingPolicyCtx (Id FarePolicyD.FarePolicy) Currency DistanceUnit (Maybe (Id DM.Merchant)) (Maybe (Id DMOC.MerchantOperatingCity)) UTCTime
  | NewPolicyCtx (Id FarePolicyD.FarePolicy) Currency DistanceUnit (Id DM.Merchant) (Id DMOC.MerchantOperatingCity)

fromApiPolicy :: PolicyCtx -> Maybe (Id DCFP.CancellationFarePolicy) -> UTCTime -> Common.FPV2Policy -> Flow FarePolicyD.FarePolicy
fromApiPolicy ctx mbCancellationId now p = do
  let (policyId, currency, distanceUnit, mbMerchantId, mbMocId, createdAt) = case ctx of
        ExistingPolicyCtx i c d m moc created -> (i, c, d, m, moc, created)
        NewPolicyCtx i c d m moc -> (i, c, d, Just m, Just moc, now)
  details <- fromApiDetails currency distanceUnit p.farePolicyDetails
  bounds <- forM p.driverExtraFeeBounds $ \bs -> do
    ne <- fromMaybeM (InvalidRequest "driverExtraFeeBounds must not be an empty list") (NE.nonEmpty bs)
    pure $ fmap (\b -> DDriverExtraFeeBounds.DriverExtraFeeBounds {startDistance = b.startDistance, distanceUnit, stepFee = b.stepFee, defaultStepFee = b.defaultStepFee, minFee = b.minFee, maxFee = b.maxFee}) ne
  pure
    FarePolicyD.FarePolicy
      { id = policyId,
        driverExtraFeeBounds = bounds,
        serviceCharge = p.serviceCharge,
        parkingCharge = p.parkingCharge,
        perStopCharge = p.perStopCharge,
        perLuggageCharge = p.perLuggageCharge,
        returnFee = fromApiReturnFee <$> p.returnFee,
        boothCharges = fromApiBoothCharge <$> p.boothCharges,
        currency,
        nightShiftBounds = (\b -> DPM.NightShiftBounds {nightShiftStart = b.nightShiftStart, nightShiftEnd = b.nightShiftEnd}) <$> p.nightShiftBounds,
        allowedTripDistanceBounds = (\b -> FarePolicyD.AllowedTripDistanceBounds {minAllowedTripDistance = b.minAllowedTripDistance, maxAllowedTripDistance = b.maxAllowedTripDistance, distanceUnit}) <$> p.allowedTripDistanceBounds,
        distanceUnit,
        tollCharges = p.tollCharges,
        petCharges = p.petCharges,
        driverAllowance = p.driverAllowance,
        airportConvenienceFee = p.airportConvenienceFee,
        businessDiscountPercentage = p.businessDiscountPercentage,
        personalDiscountPercentage = p.personalDiscountPercentage,
        priorityCharges = p.priorityCharges,
        pickupBufferInSecsForNightShiftCal = p.pickupBufferInSecsForNightShiftCal,
        tipOptions = p.tipOptions,
        additionalCongestionCharge = fromMaybe 0 p.additionalCongestionCharge,
        perMinuteRideExtraTimeCharge = p.perMinuteRideExtraTimeCharge,
        rideExtraTimeChargeGracePeriod = p.rideExtraTimeChargeGracePeriod,
        congestionChargeMultiplier = fromApiCongestionMultiplier <$> p.congestionChargeMultiplier,
        perDistanceUnitInsuranceCharge = p.perDistanceUnitInsuranceCharge,
        cardCharge = (\c -> FarePolicyD.CardCharge {perDistanceUnitMultiplier = c.perDistanceUnitMultiplier, fixed = c.fixed}) <$> p.cardCharge,
        vatChargeConfig = fromApiChargeConfig <$> p.vatChargeConfig,
        commissionChargeConfig = fromApiChargeConfig <$> p.commissionChargeConfig,
        cancellationCommissionChargeConfig = fromApiChargeConfig <$> p.cancellationCommissionChargeConfig,
        tollTaxChargeConfig = fromApiChargeConfig <$> p.tollTaxChargeConfig,
        farePolicyDetails = details,
        cancellationFarePolicyId = mbCancellationId,
        description = p.description,
        platformFee = p.platformFee,
        sgst = p.platformFeeSgst,
        cgst = p.platformFeeCgst,
        platformFeeChargesBy = maybe FarePolicyD.None fromApiPlatformFeeMethod p.platformFeeChargesBy,
        createdAt,
        updatedAt = now,
        merchantId = mbMerchantId,
        merchantOperatingCityId = mbMocId,
        conditionalCharges =
          map
            ( \c ->
                DTAC.ConditionalCharges
                  { cgstPercentage = c.cgstPercentage,
                    charge = c.charge,
                    chargeCategory = fromApiChargeCategory c.chargeCategory,
                    farePolicyId = policyId.getId,
                    sgstPercentage = c.sgstPercentage,
                    createdAt,
                    updatedAt = now
                  }
            )
            (fromMaybe [] p.conditionalCharges),
        driverCancellationNotAllowed = p.driverCancellationNotAllowed
      }

fromApiDetails :: Currency -> DistanceUnit -> Common.FPV2FarePolicyDetails -> Flow FarePolicyD.FarePolicyDetails
fromApiDetails currency distanceUnit = \case
  Common.FPV2Progressive d -> do
    sections <- fromMaybeM (InvalidRequest "perExtraKmRateSections must not be empty") (NE.nonEmpty d.perExtraKmRateSections)
    perMinSections <- forM d.perMinRateSections $ \ss ->
      fromMaybeM (InvalidRequest "perMinRateSections must not be an empty list") (NE.nonEmpty ss)
    pure $
      FarePolicyD.ProgressiveDetails
        FarePolicyD.FPProgressiveDetails
          { baseFare = d.baseFare,
            baseDistance = d.baseDistance,
            distanceUnit,
            perExtraKmRateSections = fmap (\s -> FarePolicyD.FPProgressiveDetailsPerExtraKmRateSection {startDistance = s.startDistance, distanceUnit, perExtraKmRate = s.perExtraKmRate, baseFareDepreciation = s.baseFareDepreciation}) sections,
            perMinRateSections = fmap (fmap (\s -> FarePolicyD.FPProgressiveDetailsPerMinRateSection {rideDurationInMin = s.rideDurationInMin, perMinRate = mkPrice (Just currency) s.perMinRate})) perMinSections,
            perMinRateDurationBasis = fromApiDurationBasis <$> d.perMinRateDurationBasis,
            deadKmFare = d.deadKmFare,
            pickupCharges = DFPC.PickupCharges {pickupChargesMin = d.pickupCharges.pickupChargesMin, pickupChargesMax = d.pickupCharges.pickupChargesMax},
            waitingChargeInfo = fromApiWaitingChargeInfo <$> d.waitingChargeInfo,
            nightShiftCharge = fromApiNightShiftCharge <$> d.nightShiftCharge,
            currency
          }
  Common.FPV2Slabs d -> do
    slabs <- fromMaybeM (InvalidRequest "slabs must not be empty") (NE.nonEmpty d.slabs)
    pure $
      FarePolicyD.SlabsDetails
        FarePolicyD.FPSlabsDetails
          { slabs =
              fmap
                ( \s ->
                    FarePolicyD.FPSlabsDetailsSlab
                      { startDistance = s.startDistance,
                        distanceUnit,
                        baseFare = s.baseFare,
                        waitingChargeInfo = fromApiWaitingChargeInfo <$> s.waitingChargeInfo,
                        platformFeeInfo = fromApiPlatformFeeInfo <$> s.platformFeeInfo,
                        nightShiftCharge = fromApiNightShiftCharge <$> s.nightShiftCharge,
                        currency
                      }
                )
                slabs
          }
  Common.FPV2Rental d -> do
    buffers <- fromMaybeM (InvalidRequest "distanceBuffers must not be empty") (NE.nonEmpty d.distanceBuffers)
    pricingSlabs <- fromMaybeM (InvalidRequest "pricingSlabs must not be empty") (NE.nonEmpty d.pricingSlabs)
    pure $
      FarePolicyD.RentalDetails
        FarePolicyD.FPRentalDetails
          { baseFare = d.baseFare,
            perHourCharge = d.perHourCharge,
            distanceBuffers = fmap (\b -> FarePolicyD.FPRentalDetailsDistanceBuffers {rideDuration = b.rideDuration, bufferKms = b.bufferKms, bufferMeters = b.bufferMeters}) buffers,
            perExtraKmRate = d.perExtraKmRate,
            perExtraMinRate = d.perExtraMinRate,
            includedKmPerHr = d.includedKmPerHr,
            plannedPerKmRate = d.plannedPerKmRate,
            currency,
            deadKmFare = d.deadKmFare,
            maxAdditionalKmsLimit = d.maxAdditionalKmsLimit,
            totalAdditionalKmsLimit = d.totalAdditionalKmsLimit,
            pricingSlabs = fmap (\s -> FarePolicyD.FPRentalDetailsPricingSlabs {timePercentage = s.timePercentage, distancePercentage = s.distancePercentage, farePercentage = s.farePercentage, includeActualTimePercentage = s.includeActualTimePercentage, includeActualDistPercentage = s.includeActualDistPercentage}) pricingSlabs,
            nightShiftCharge = fromApiNightShiftCharge <$> d.nightShiftCharge,
            waitingChargeInfo = fromApiWaitingChargeInfo <$> d.waitingChargeInfo
          }
  Common.FPV2InterCity d -> do
    pricingSlabs <- fromMaybeM (InvalidRequest "pricingSlabs must not be empty") (NE.nonEmpty d.pricingSlabs)
    pure $
      FarePolicyD.InterCityDetails
        FarePolicyD.FPInterCityDetails
          { baseFare = d.baseFare,
            perHourCharge = d.perHourCharge,
            perKmRateOneWay = d.perKmRateOneWay,
            perKmRateRoundTrip = d.perKmRateRoundTrip,
            perExtraKmRate = d.perExtraKmRate,
            perExtraMinRate = d.perExtraMinRate,
            kmPerPlannedExtraHour = d.kmPerPlannedExtraHour,
            deadKmFare = d.deadKmFare,
            perDayMaxHourAllowance = d.perDayMaxHourAllowance,
            perDayMaxAllowanceInMins = d.perDayMaxAllowanceInMins,
            pricingSlabs = fmap (\s -> FarePolicyD.FPInterCityDetailsPricingSlabs {timePercentage = s.timePercentage, distancePercentage = s.distancePercentage, farePercentage = s.farePercentage, includeActualTimePercentage = s.includeActualTimePercentage, includeActualDistPercentage = s.includeActualDistPercentage}) pricingSlabs,
            defaultWaitTimeAtDestination = d.defaultWaitTimeAtDestination,
            currency,
            stateEntryPermitCharges = d.stateEntryPermitCharges,
            nightShiftCharge = fromApiNightShiftCharge <$> d.nightShiftCharge,
            waitingChargeInfo = fromApiWaitingChargeInfo <$> d.waitingChargeInfo
          }
  Common.FPV2Ambulance d -> do
    slabs <- fromMaybeM (InvalidRequest "slabs must not be empty") (NE.nonEmpty d.slabs)
    pure $
      FarePolicyD.AmbulanceDetails
        FarePolicyD.FPAmbulanceDetails
          { slabs =
              fmap
                ( \(idx, s) ->
                    FarePolicyD.FPAmbulanceDetailsSlab
                      { id = idx,
                        vehicleAge = s.vehicleAge,
                        baseFare = s.baseFare,
                        baseDistance = s.baseDistance,
                        perKmRate = s.perKmRate,
                        currency,
                        waitingChargeInfo = fromApiWaitingChargeInfo <$> s.waitingChargeInfo,
                        platformFeeInfo = fromApiPlatformFeeInfo <$> s.platformFeeInfo,
                        nightShiftCharge = fromApiNightShiftCharge <$> s.nightShiftCharge
                      }
                )
                (NE.zip (NE.fromList [1 ..]) slabs)
          }

--------------------------------------------------------------------------------
-- enum bridges (generated API enums have no Read/Show; map constructors explicitly)
--------------------------------------------------------------------------------

toApiNightShiftCharge :: DFPC.NightShiftCharge -> Common.FPV2NightShiftCharge
toApiNightShiftCharge = \case
  DFPC.ProgressiveNightShiftCharge f -> Common.ProgressiveNightShiftCharge f
  DFPC.ConstantNightShiftCharge m -> Common.ConstantNightShiftCharge m

fromApiNightShiftCharge :: Common.FPV2NightShiftCharge -> DFPC.NightShiftCharge
fromApiNightShiftCharge = \case
  Common.ProgressiveNightShiftCharge f -> DFPC.ProgressiveNightShiftCharge f
  Common.ConstantNightShiftCharge m -> DFPC.ConstantNightShiftCharge m

toApiWaitingChargeInfo :: DFPC.WaitingChargeInfo -> Common.FPV2WaitingChargeInfo
toApiWaitingChargeInfo w =
  Common.FPV2WaitingChargeInfo
    { freeWaitingTime = w.freeWaitingTime,
      waitingCharge = case w.waitingCharge of
        DFPC.PerMinuteWaitingCharge m -> Common.PerMinuteWaitingCharge m
        DFPC.ConstantWaitingCharge m -> Common.ConstantWaitingCharge m
    }

fromApiWaitingChargeInfo :: Common.FPV2WaitingChargeInfo -> DFPC.WaitingChargeInfo
fromApiWaitingChargeInfo w =
  DFPC.WaitingChargeInfo
    { freeWaitingTime = w.freeWaitingTime,
      waitingCharge = case w.waitingCharge of
        Common.PerMinuteWaitingCharge m -> DFPC.PerMinuteWaitingCharge m
        Common.ConstantWaitingCharge m -> DFPC.ConstantWaitingCharge m
    }

toApiCongestionMultiplier :: FarePolicyD.CongestionChargeMultiplier -> Common.FPV2CongestionChargeMultiplier
toApiCongestionMultiplier = \case
  FarePolicyD.BaseFareAndExtraDistanceFare c -> Common.BaseFareAndExtraDistanceFare c
  FarePolicyD.ExtraDistanceFare c -> Common.ExtraDistanceFare c

fromApiCongestionMultiplier :: Common.FPV2CongestionChargeMultiplier -> FarePolicyD.CongestionChargeMultiplier
fromApiCongestionMultiplier = \case
  Common.BaseFareAndExtraDistanceFare c -> FarePolicyD.BaseFareAndExtraDistanceFare c
  Common.ExtraDistanceFare c -> FarePolicyD.ExtraDistanceFare c

toApiReturnFee :: FarePolicyD.ReturnFee -> Common.FPV2ReturnFee
toApiReturnFee = \case
  FarePolicyD.ReturnFeeFixed m -> Common.ReturnFeeFixed m
  FarePolicyD.ReturnFeePercentage d -> Common.ReturnFeePercentage d

fromApiReturnFee :: Common.FPV2ReturnFee -> FarePolicyD.ReturnFee
fromApiReturnFee = \case
  Common.ReturnFeeFixed m -> FarePolicyD.ReturnFeeFixed m
  Common.ReturnFeePercentage d -> FarePolicyD.ReturnFeePercentage d

toApiBoothCharge :: FarePolicyD.BoothCharge -> Common.FPV2BoothCharge
toApiBoothCharge = \case
  FarePolicyD.BoothChargeFixed m -> Common.BoothChargeFixed m
  FarePolicyD.BoothChargePercentage d -> Common.BoothChargePercentage d

fromApiBoothCharge :: Common.FPV2BoothCharge -> FarePolicyD.BoothCharge
fromApiBoothCharge = \case
  Common.BoothChargeFixed m -> FarePolicyD.BoothChargeFixed m
  Common.BoothChargePercentage d -> FarePolicyD.BoothChargePercentage d

toApiPlatformFeeMethod :: FarePolicyD.PlatformFeeMethods -> Common.FPV2PlatformFeeMethod
toApiPlatformFeeMethod = \case
  FarePolicyD.Subscription -> Common.Subscription
  FarePolicyD.FixedAmount -> Common.FixedAmount
  FarePolicyD.None -> Common.None
  FarePolicyD.SlabBased -> Common.SlabBased
  FarePolicyD.NoCharge -> Common.NoCharge

fromApiPlatformFeeMethod :: Common.FPV2PlatformFeeMethod -> FarePolicyD.PlatformFeeMethods
fromApiPlatformFeeMethod = \case
  Common.Subscription -> FarePolicyD.Subscription
  Common.FixedAmount -> FarePolicyD.FixedAmount
  Common.None -> FarePolicyD.None
  Common.SlabBased -> FarePolicyD.SlabBased
  Common.NoCharge -> FarePolicyD.NoCharge

toApiPlatformFeeInfo :: FarePolicyD.PlatformFeeInfo -> Common.FPV2PlatformFeeInfo
toApiPlatformFeeInfo p =
  Common.FPV2PlatformFeeInfo
    { platformFeeCharge = case p.platformFeeCharge of
        FarePolicyD.ProgressivePlatformFee m -> Common.ProgressivePlatformFee m
        FarePolicyD.ConstantPlatformFee m -> Common.ConstantPlatformFee m,
      cgst = p.cgst,
      sgst = p.sgst
    }

fromApiPlatformFeeInfo :: Common.FPV2PlatformFeeInfo -> FarePolicyD.PlatformFeeInfo
fromApiPlatformFeeInfo p =
  FarePolicyD.PlatformFeeInfo
    { platformFeeCharge = case p.platformFeeCharge of
        Common.ProgressivePlatformFee m -> FarePolicyD.ProgressivePlatformFee m
        Common.ConstantPlatformFee m -> FarePolicyD.ConstantPlatformFee m,
      cgst = p.cgst,
      sgst = p.sgst
    }

toApiDurationBasis :: DFPC.PerMinRateDurationBasis -> Common.FPV2PerMinRateDurationBasis
toApiDurationBasis = \case
  DFPC.TotalDuration -> Common.TotalDuration
  DFPC.TrafficDelayDuration -> Common.TrafficDelayDuration

fromApiDurationBasis :: Common.FPV2PerMinRateDurationBasis -> DFPC.PerMinRateDurationBasis
fromApiDurationBasis = \case
  Common.TotalDuration -> DFPC.TotalDuration
  Common.TrafficDelayDuration -> DFPC.TrafficDelayDuration

toApiChargeCategory :: ConditionalChargesCategories -> Common.FPV2ConditionalChargeCategory
toApiChargeCategory = \case
  SAFETY_PLUS_CHARGES -> Common.SAFETY_PLUS_CHARGES
  NYREGULAR_SUBSCRIPTION_CHARGE -> Common.NYREGULAR_SUBSCRIPTION_CHARGE
  NO_CHARGES -> Common.NO_CHARGES

fromApiChargeCategory :: Common.FPV2ConditionalChargeCategory -> ConditionalChargesCategories
fromApiChargeCategory = \case
  Common.SAFETY_PLUS_CHARGES -> SAFETY_PLUS_CHARGES
  Common.NYREGULAR_SUBSCRIPTION_CHARGE -> NYREGULAR_SUBSCRIPTION_CHARGE
  Common.NO_CHARGES -> NO_CHARGES

toApiSearchSource :: DFareProduct.SearchSource -> Common.FPV2SearchSource
toApiSearchSource = \case
  DFareProduct.ALL -> Common.ALL
  DFareProduct.DASHBOARD -> Common.DASHBOARD
  DFareProduct.MOBILE_APP -> Common.MOBILE_APP

fromApiSearchSource :: Common.FPV2SearchSource -> DFareProduct.SearchSource
fromApiSearchSource = \case
  Common.ALL -> DFareProduct.ALL
  Common.DASHBOARD -> DFareProduct.DASHBOARD
  Common.MOBILE_APP -> DFareProduct.MOBILE_APP

toApiChargeConfig :: FarePolicyD.FareChargeConfig -> Common.FPV2FareChargeConfig
toApiChargeConfig c = Common.FPV2FareChargeConfig {value = c.value, appliesOn = map toApiChargeComponent c.appliesOn}

fromApiChargeConfig :: Common.FPV2FareChargeConfig -> FarePolicyD.FareChargeConfig
fromApiChargeConfig c = FarePolicyD.FareChargeConfig {value = c.value, appliesOn = map fromApiChargeComponent c.appliesOn}

toApiChargeComponent :: FarePolicyD.FareChargeComponent -> Common.FPV2FareChargeComponent
toApiChargeComponent = \case
  FarePolicyD.RideFare -> Common.RideFare
  FarePolicyD.WaitingCharge -> Common.WaitingCharge
  FarePolicyD.ServiceChargeComponent -> Common.ServiceChargeComponent
  FarePolicyD.TollChargesComponent -> Common.TollChargesComponent
  FarePolicyD.CongestionChargeComponent -> Common.CongestionChargeComponent
  FarePolicyD.ParkingChargeComponent -> Common.ParkingChargeComponent
  FarePolicyD.PetChargeComponent -> Common.PetChargeComponent
  FarePolicyD.PriorityChargeComponent -> Common.PriorityChargeComponent
  FarePolicyD.NightShiftChargeComponent -> Common.NightShiftChargeComponent
  FarePolicyD.InsuranceChargeComponent -> Common.InsuranceChargeComponent
  FarePolicyD.StopChargeComponent -> Common.StopChargeComponent
  FarePolicyD.LuggageChargeComponent -> Common.LuggageChargeComponent
  FarePolicyD.PlatformFeeComponent -> Common.PlatformFeeComponent
  FarePolicyD.CustomerCancellationChargeComponent -> Common.CustomerCancellationChargeComponent
  FarePolicyD.CustomerExtraFeeComponent -> Common.CustomerExtraFeeComponent
  FarePolicyD.DeadKmFareComponent -> Common.DeadKmFareComponent
  FarePolicyD.ExtraKmFareComponent -> Common.ExtraKmFareComponent
  FarePolicyD.RideDurationFareComponent -> Common.RideDurationFareComponent
  FarePolicyD.TimeBasedFareComponent -> Common.TimeBasedFareComponent
  FarePolicyD.DistBasedFareComponent -> Common.DistBasedFareComponent
  FarePolicyD.TimeFareComponent -> Common.TimeFareComponent
  FarePolicyD.DistanceFareComponent -> Common.DistanceFareComponent
  FarePolicyD.PickupChargeComponent -> Common.PickupChargeComponent
  FarePolicyD.ExtraDistanceFareComponent -> Common.ExtraDistanceFareComponent
  FarePolicyD.ExtraTimeFareComponent -> Common.ExtraTimeFareComponent
  FarePolicyD.StateEntryPermitChargesComponent -> Common.StateEntryPermitChargesComponent
  FarePolicyD.AmbulanceDistBasedFareComponent -> Common.AmbulanceDistBasedFareComponent
  FarePolicyD.RideVatComponent -> Common.RideVatComponent
  FarePolicyD.TollVatComponent -> Common.TollVatComponent

fromApiChargeComponent :: Common.FPV2FareChargeComponent -> FarePolicyD.FareChargeComponent
fromApiChargeComponent = \case
  Common.RideFare -> FarePolicyD.RideFare
  Common.WaitingCharge -> FarePolicyD.WaitingCharge
  Common.ServiceChargeComponent -> FarePolicyD.ServiceChargeComponent
  Common.TollChargesComponent -> FarePolicyD.TollChargesComponent
  Common.CongestionChargeComponent -> FarePolicyD.CongestionChargeComponent
  Common.ParkingChargeComponent -> FarePolicyD.ParkingChargeComponent
  Common.PetChargeComponent -> FarePolicyD.PetChargeComponent
  Common.PriorityChargeComponent -> FarePolicyD.PriorityChargeComponent
  Common.NightShiftChargeComponent -> FarePolicyD.NightShiftChargeComponent
  Common.InsuranceChargeComponent -> FarePolicyD.InsuranceChargeComponent
  Common.StopChargeComponent -> FarePolicyD.StopChargeComponent
  Common.LuggageChargeComponent -> FarePolicyD.LuggageChargeComponent
  Common.PlatformFeeComponent -> FarePolicyD.PlatformFeeComponent
  Common.CustomerCancellationChargeComponent -> FarePolicyD.CustomerCancellationChargeComponent
  Common.CustomerExtraFeeComponent -> FarePolicyD.CustomerExtraFeeComponent
  Common.DeadKmFareComponent -> FarePolicyD.DeadKmFareComponent
  Common.ExtraKmFareComponent -> FarePolicyD.ExtraKmFareComponent
  Common.RideDurationFareComponent -> FarePolicyD.RideDurationFareComponent
  Common.TimeBasedFareComponent -> FarePolicyD.TimeBasedFareComponent
  Common.DistBasedFareComponent -> FarePolicyD.DistBasedFareComponent
  Common.TimeFareComponent -> FarePolicyD.TimeFareComponent
  Common.DistanceFareComponent -> FarePolicyD.DistanceFareComponent
  Common.PickupChargeComponent -> FarePolicyD.PickupChargeComponent
  Common.ExtraDistanceFareComponent -> FarePolicyD.ExtraDistanceFareComponent
  Common.ExtraTimeFareComponent -> FarePolicyD.ExtraTimeFareComponent
  Common.StateEntryPermitChargesComponent -> FarePolicyD.StateEntryPermitChargesComponent
  Common.AmbulanceDistBasedFareComponent -> FarePolicyD.AmbulanceDistBasedFareComponent
  Common.RideVatComponent -> FarePolicyD.RideVatComponent
  Common.TollVatComponent -> FarePolicyD.TollVatComponent
