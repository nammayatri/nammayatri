{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pure decision core for end-ride fare recomputation.
--
-- This module answers ONE question with no IO: given everything known at end
-- ride, which distance and duration do we bill, and under which fare policy
-- (the one quoted, or the latest)? Every branch of the legacy ladder in
-- "Domain.Action.UI.Ride.EndRide" maps to exactly one 'RecomputeReason', and
-- the post-pick adjustments ('recalculateFareForDistance' internals) map to
-- 'RecomputeModifier's.
--
-- SHADOW MODE: 'decideRecompute' currently runs alongside the legacy ladder;
-- the legacy ladder still bills. Mismatches between
-- 'predictedChargeableDistance' and the billed distance are logged with tag
-- @RecomputeDecisionShadow@. Cutover (billing from this module) is a separate
-- change once shadow mismatches are zero in production.
--
-- The time levers (forgiveness bands, the within-threshold floor, the
-- distance-forgiven gate) and the band predicate are mirrored here AND wired
-- into the live path ('getChargeableDistanceAndDuration' and the ladders in
-- EndRide.hs delegate to 'recomputeBandQualifies' /
-- 'forgivenessAdjustedDuration': single source of truth).
module Domain.Action.UI.Ride.EndRide.RecomputeDecision
  ( RequestSource (..),
    ProductFlags (..),
    ExtraKmBudgetState (..),
    RecomputeConfig (..),
    RecomputeInput (..),
    DistanceSource (..),
    DurationSource (..),
    PricingSource (..),
    RecomputeReason (..),
    RecomputeModifier (..),
    RecomputeDecision (..),
    ShadowRecord (..),
    decideRecompute,
    mkRecomputeConfig,
    mkShadowRecordText,
    defaultNoRecomputeTripCategories,
    defaultRecomputeBands,
    defaultTimeForgivenessBands,
    recomputeBandQualifies,
    forgivenessAdjustedDuration,
    reasonText,
  )
where

import qualified Data.Aeson as Ae
import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Domain.Types as DTC
import qualified Domain.Types.Extra.TransporterConfig as ExtraTC
import qualified Domain.Types.TransporterConfig as DTConf
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Common
import Kernel.Utils.Text (encodeToText)

data RequestSource = DriverSource | DashboardSource | CallBasedSource | CronJobSource
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Recompute-relevant facts carried on the FareProduct / FullFarePolicy.
data ProductFlags = ProductFlags
  { disableRecompute :: Bool,
    disableDownwardRecompute :: Bool,
    -- | The fare policy defines perMinRateSections. They bill time on the
    -- chargeable (actual) minutes at recompute — and the within-threshold
    -- time floor binds — ONLY when 'extraTimeChargeConfigured' is False;
    -- a configured extra-time charge wins and pins the sections to the
    -- estimated minutes, exactly as quoted.
    hasPerMinRateSections :: Bool,
    -- | The fare policy configures perMinuteRideExtraTimeCharge: time overage
    -- is billed through the extra-time charge (grace + forgiveness window
    -- deducted), never through perMinRateSections.
    extraTimeChargeConfigured :: Bool,
    -- | City pricing (Progressive/Slabs): the duration levers
    -- (forgiveness/gating) apply. Rental/intercity/ambulance time billing is
    -- contractual and exempt.
    timeLeversApplicable :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Per-driver rolling extra-km budget, values as of AFTER this ride's diff
-- was added (that is what the legacy path compares against).
data ExtraKmBudgetState = ExtraKmBudgetState
  { dailyExtraKms :: HighPrecMeters,
    weeklyExtraKms :: HighPrecMeters
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Every config lever that influences the recompute decision, in one record.
-- Assembled from TransporterConfig via 'mkRecomputeConfig'; tests build it
-- directly.
data RecomputeConfig = RecomputeConfig
  { cfgRecomputeIfPickupDropNotOutsideOfThreshold :: Bool,
    -- | One band table gates recompute in BOTH directions: within the
    -- threshold it qualifies upward recompute; outside the threshold the
    -- actual (or approx) is billed only when |delta| clears the band,
    -- shortfalls included.
    cfgRecomputeThresholds :: Maybe [ExtraTC.RecomputeBand],
    cfgUpwardsRecomputeBuffer :: HighPrecMeters,
    cfgUpwardsRecomputeBufferPercentage :: Maybe Int,
    cfgFareRecomputeDailyExtraKmsThreshold :: HighPrecMeters,
    cfgFareRecomputeWeeklyExtraKmsThreshold :: HighPrecMeters,
    cfgEnableDownwardRecomputeForDifferentDestination :: Maybe Bool,
    cfgMinThresholdForPassThroughDestination :: Maybe Meters,
    cfgDownwardRecomputeDistanceThreshold :: Maybe HighPrecMeters,
    cfgNoRecomputeTripCategories :: [DTC.TripCategory],
    -- | Estimate-relative time forgiveness: the tightest matching band (by
    -- the ride's estimated duration) supplies the forgiveness window. The
    -- window is DEDUCTED from an overrun's billed duration (clamped at the
    -- estimate): overage inside the window bills the estimate, overage beyond
    -- it bills only the excess. Empty list = no forgiveness. Fleet default:
    -- one catch-all 5-min band.
    cfgTimeForgivenessBands :: [ExtraTC.TimeForgivenessBand],
    -- | When True, rides billed at the ESTIMATED distance also bill the
    -- estimated duration, so the per-minute extra-time charge cannot fire on
    -- a ride the distance ladder decided to forgive. Default False.
    cfgGateExtraTimeChargeByRecompute :: Bool,
    -- | Within the pickup/drop threshold, never bill BELOW the estimated
    -- duration (no time refunds at the booked destination). Default True,
    -- but it only binds while the fare policy bills time via
    -- perMinRateSections — otherwise a below-estimate duration has no
    -- downward fare effect on city rides, and flooring would wrongly inflate
    -- rental (actual-duration) billing.
    cfgFloorTimeAtEstimateWithinThreshold :: Bool,
    -- | Re-run the congestion model at end ride. Default False.
    cfgRecomputeCongestionOnEndRide :: Bool,
    -- | Estimated-toll fallback when GPS was dark at the gates. Default False.
    cfgEstimatedTollFallback :: Bool,
    -- | Overlay the driver on extra-km budget exhaustion. Default True.
    cfgNotifyDriverOnBudgetExceeded :: Bool
  }
  deriving (Show, Eq, Generic)

data RecomputeInput = RecomputeInput
  { requestSource :: RequestSource,
    tripCategory :: DTC.TripCategory,
    isOdometerBilled :: Bool,
    isRectificationCategory :: Bool,
    estimatedDistance :: Maybe Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedDuration :: Maybe Seconds,
    traveledDistance :: HighPrecMeters,
    odometerDistance :: Maybe Meters,
    -- | Directions-API approximation used on the failed path. Nothing when it
    -- was not (or cannot be) computed; the decision then reports
    -- 'FailedOutsideUnknownApprox' and predicts no concrete distance.
    approxTraveledDistance :: Maybe Meters,
    actualDuration :: Maybe Seconds,
    distanceCalculationFailed :: Bool,
    -- | The ride row's own flag (tri-state on purpose: the pass-through-drop
    -- override requires it to be literally @Just False@, which is never true
    -- on the cron/odometer paths where the flag was not yet written).
    rideFlagDistanceCalculationFailed :: Maybe Bool,
    pickupDropOutsideOfThreshold :: Bool,
    dropOutsideOfThreshold :: Bool,
    passedThroughDrop :: Bool,
    -- | Nothing = unknown; treated as within budget (mirrors legacy
    -- 'checkExtraKmsThreshold' which defaults to True on partial state).
    budgetState :: Maybe ExtraKmBudgetState,
    productFlags :: ProductFlags,
    cfg :: RecomputeConfig
  }
  deriving (Show, Generic)

data DistanceSource
  = UseEstimate
  | UseActual
  | UseActualCapped
  | UseOdometer
  | UseApproxRoute
  | UseEstimatePlusBuffer
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data DurationSource = UseActualDuration | UseEstimatedDuration | UseFlooredDuration | UseForgivenessAdjustedDuration
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PricingSource = QuotedPolicy | LatestPolicy
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | One constructor per leaf of the legacy ladder. See the inventory table in
-- docs/backend/design/fare-recompute-unification-plan.md.
data RecomputeReason
  = CronJobEstimate
  | OdometerBilled
  | RectificationActual
  | RectificationEstimateOnFailure
  | WithinThresholdUpwardRecompute
  | WithinThresholdUpwardDisabled
  | WithinThresholdNoUpwardBand
  | WithinThresholdBudgetExhausted
  | OutsideThresholdShorterActual
  | OutsideThresholdDownwardDisabled
  | OutsideThresholdSmallShortfallEstimate
  | OutsideThresholdSmallOverageEstimate
  | OutsideThresholdLargeOverageActual
  | FailedWithinThresholdEstimate
  | FailedOutsideShorterApprox
  | FailedOutsideDownwardDisabled
  | FailedOutsideSmallShortfallEstimate
  | FailedOutsideSmallOverageEstimate
  | FailedOutsideApproxRecompute
  | FailedOutsideCappedBuffer
  | FailedOutsideUnknownApprox
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Post-pick adjustments applied inside the fare recompute step.
data RecomputeModifier
  = PassThroughDropOverride
  | DisableRecomputeBypass
  | DownwardRecomputeFloored
  | DownwardToleranceForgiven
  | DurationOverageForgiven
  | DurationForgivenessDeducted
  | ExtraTimeGatedOnEstimateBilledRide
  | TimeFlooredAtEstimate
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RecomputeDecision = RecomputeDecision
  { distanceSource :: DistanceSource,
    durationSource :: DurationSource,
    pricingSource :: PricingSource,
    reason :: RecomputeReason,
    modifiers :: [RecomputeModifier],
    -- | Concrete billed distance when the decision is fully determined by the
    -- inputs; Nothing only for 'FailedOutsideUnknownApprox'.
    predictedChargeableDistance :: Maybe Meters
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Trip categories pinned to the estimate in the pass-through-drop and
-- downward-recompute rules; overridable per city via the policy's
-- pinnedTripCategories.
defaultNoRecomputeTripCategories :: [DTC.TripCategory]
defaultNoRecomputeTripCategories = [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer]

-- | Fleet-wide default upward bands, mirroring the DB defaults the legacy
-- recompute_distance_thresholds column shipped with. Used only when a city
-- has no policy (or a policy without bands).
defaultRecomputeBands :: [ExtraTC.RecomputeBand]
defaultRecomputeBands =
  [ mkBand 5000 40,
    mkBand 10000 30,
    mkBand 15000 20,
    mkBand 9999999 10
  ]
  where
    mkBand upper pct =
      ExtraTC.RecomputeBand
        { estimatedDistanceUpper = upper,
          minThresholdPercentage = pct,
          minThresholdDistance = 1000
        }

-- | Resolve the EFFECTIVE recompute config from the unified
-- 'ExtraTC.RecomputePolicy' column, falling back to the fleet-wide code
-- defaults (which mirror the old legacy-column DB defaults) for any absent
-- field. The legacy scattered columns are NO LONGER read: cities must carry
-- their custom values in the policy (see
-- dev/sql-seed/fare-recompute-policy-backfill.sql — run it BEFORE deploying
-- this resolver, or custom legacy values silently revert to the defaults).
mkRecomputeConfig :: DTConf.TransporterConfig -> RecomputeConfig
mkRecomputeConfig tc = do
  let pol = tc.fareRecomputePolicy
      up = pol >>= (.upward)
      down = pol >>= (.downward)
      tim = pol >>= (.time)
  RecomputeConfig
    { cfgRecomputeIfPickupDropNotOutsideOfThreshold = fromMaybe False (up >>= (.allowWithinThreshold)),
      cfgRecomputeThresholds = Just $ fromMaybe defaultRecomputeBands (up >>= (.bands)),
      cfgUpwardsRecomputeBuffer = fromMaybe 2000 (up >>= (.bufferMeters)),
      cfgUpwardsRecomputeBufferPercentage = up >>= (.bufferPercentage),
      cfgFareRecomputeDailyExtraKmsThreshold = fromMaybe 5000 (up >>= (.dailyExtraKmsBudget)),
      cfgFareRecomputeWeeklyExtraKmsThreshold = fromMaybe 20000 (up >>= (.weeklyExtraKmsBudget)),
      cfgEnableDownwardRecomputeForDifferentDestination = down >>= (.allowForChangedDestination),
      cfgMinThresholdForPassThroughDestination = down >>= (.passThroughMinEstimateMeters),
      cfgDownwardRecomputeDistanceThreshold = down >>= (.forgivenessMeters),
      cfgNoRecomputeTripCategories = fromMaybe defaultNoRecomputeTripCategories (pol >>= (.pinnedTripCategories)),
      cfgTimeForgivenessBands =
        case (tim >>= (.forgivenessBands), tim >>= (.overageForgivenessSeconds)) of
          (Just bands, _) -> bands
          (Nothing, Just flat) -> [ExtraTC.TimeForgivenessBand {estimatedDurationUpper = 9999999, forgivenessSeconds = flat}]
          (Nothing, Nothing) -> defaultTimeForgivenessBands,
      cfgGateExtraTimeChargeByRecompute = fromMaybe False (tim >>= (.gateExtraTimeOnEstimateBilled)),
      cfgFloorTimeAtEstimateWithinThreshold = fromMaybe True (tim >>= (.floorAtEstimateWithinThreshold)),
      cfgRecomputeCongestionOnEndRide = fromMaybe False (pol >>= (.recomputeCongestionOnEndRide)),
      cfgEstimatedTollFallback = fromMaybe False (pol >>= (.estimatedTollFallback)),
      cfgNotifyDriverOnBudgetExceeded = fromMaybe True (up >>= (.notifyDriverOnBudgetExceeded))
    }

reasonText :: RecomputeDecision -> Text
reasonText d = Text.intercalate "+" (show d.reason : map show d.modifiers)

-- | The full shadow-mode comparison record persisted to
-- ride.recompute_reason as compact JSON, so divergence analysis is a DB/CH
-- query instead of log-diving. The billed fare itself is NOT duplicated here:
-- it already lives on ride.fare / fare_parameters, and the shadow fare is
-- identical by construction whenever mismatch = false (same distance,
-- duration, and policy into the same calculator).
--
-- Fields carry an sr prefix so they don't clash with RecomputeDecision's
-- selectors in this module; the JSON keys stay unprefixed via
-- 'shadowRecordJsonOptions'.
data ShadowRecord = ShadowRecord
  { srReason :: RecomputeReason,
    srModifiers :: [RecomputeModifier],
    srDistanceSource :: DistanceSource,
    srDurationSource :: DurationSource,
    srPricingSource :: PricingSource,
    srPredictedDistance :: Maybe Meters,
    srBilledDistance :: Meters,
    srMismatch :: Bool
  }
  deriving (Show, Eq, Generic)

shadowRecordJsonOptions :: Ae.Options
shadowRecordJsonOptions =
  Ae.defaultOptions
    { Ae.fieldLabelModifier = \field -> case drop 2 field of
        (c : rest) -> Char.toLower c : rest
        [] -> field
    }

instance ToJSON ShadowRecord where
  toJSON = Ae.genericToJSON shadowRecordJsonOptions
  toEncoding = Ae.genericToEncoding shadowRecordJsonOptions

instance FromJSON ShadowRecord where
  parseJSON = Ae.genericParseJSON shadowRecordJsonOptions

mkShadowRecordText :: RecomputeDecision -> Meters -> Text
mkShadowRecordText d billed =
  encodeToText
    ShadowRecord
      { srReason = d.reason,
        srModifiers = d.modifiers,
        srDistanceSource = d.distanceSource,
        srDurationSource = d.durationSource,
        srPricingSource = d.pricingSource,
        srPredictedDistance = d.predictedChargeableDistance,
        srBilledDistance = billed,
        srMismatch = maybe False (/= billed) d.predictedChargeableDistance
      }

-- | Band predicate shared by the live ladder and the shadow decision: the
-- tightest band for the ride's estimate decides when a distance DELTA
-- qualifies for recompute. Pass the absolute delta — the same bands gate the
-- upward (overage) and, outside the pickup/drop threshold, the downward
-- (shortfall) direction. Distance-only by design: TIME overage is billed
-- through the forgiveness bands + per-min sections and must never unlock
-- sub-band distance deltas.
recomputeBandQualifies ::
  Maybe [ExtraTC.RecomputeBand] ->
  Meters ->
  Meters ->
  Bool
recomputeBandQualifies mbBands estimatedDist distanceDelta = do
  let filteredThresholds = maybe [] (filter (\band -> band.estimatedDistanceUpper > estimatedDist)) mbBands
      mbBand = listToMaybe $ sortBy (comparing \band -> band.estimatedDistanceUpper - estimatedDist) filteredThresholds
  case mbBand of
    Nothing -> False
    Just band ->
      distanceDelta > band.minThresholdDistance
        && distanceDelta.getMeters > (estimatedDist.getMeters * band.minThresholdPercentage) `div` 100

-- | Fleet default time forgiveness: a 5-minute window on every ride (single
-- catch-all band) — sub-5-min overruns bill the estimate, larger overruns
-- bill with the first 5 minutes deducted.
defaultTimeForgivenessBands :: [ExtraTC.TimeForgivenessBand]
defaultTimeForgivenessBands = [ExtraTC.TimeForgivenessBand {estimatedDurationUpper = 9999999, forgivenessSeconds = 300}]

-- | Forgiveness window of the tightest band matching the ride's estimated
-- duration (band upper strictly above the estimate, mirroring the
-- distance-band selection). Nothing = no matching band or no estimate.
matchingForgivenessSeconds :: [ExtraTC.TimeForgivenessBand] -> Maybe Seconds -> Maybe Seconds
matchingForgivenessSeconds bands mbEstimatedDur = do
  estDur <- mbEstimatedDur
  let matching = filter (\band -> band.estimatedDurationUpper > estDur) bands
  band <- listToMaybe $ sortBy (comparing \band -> band.estimatedDurationUpper - estDur) matching
  pure band.forgivenessSeconds

-- | The billed duration after applying the forgiveness window to an overrun:
-- the window is deducted from the actual duration, never going below the
-- estimate. Overage within the window therefore bills the estimate outright;
-- overage beyond it bills only the part past the window. Nothing = no
-- overrun, or no matching band (bill the input duration unchanged).
forgivenessAdjustedDuration :: [ExtraTC.TimeForgivenessBand] -> Maybe Seconds -> Maybe Seconds -> Maybe Seconds
forgivenessAdjustedDuration bands mbEstimatedDur mbActualDur = do
  window <- matchingForgivenessSeconds bands mbEstimatedDur
  estDur <- mbEstimatedDur
  actDur <- mbActualDur
  if actDur > estDur
    then pure (max estDur (actDur - window))
    else Nothing

decideRecompute :: RecomputeInput -> RecomputeDecision
decideRecompute input = applyFareStepAdjustments input (basePick input)

-- | Mirrors the branch point in endRideHandler plus the two
-- calculateFinalValuesFor* ladders.
basePick :: RecomputeInput -> RecomputeDecision
basePick RecomputeInput {cfg = RecomputeConfig {..}, ..}
  | requestSource == CronJobSource = (pick UseEstimate QuotedPolicy CronJobEstimate (Just estimate)) {durationSource = UseEstimatedDuration}
  | isOdometerBilled = pick UseOdometer QuotedPolicy OdometerBilled odometerDistance
  | isRectificationCategory =
    if distanceCalculationFailed
      then pick UseEstimate QuotedPolicy RectificationEstimateOnFailure (Just (fromMaybe traveledRounded estimatedDistance))
      else pick UseActual QuotedPolicy RectificationActual (Just traveledRounded)
  | distanceCalculationFailed = failedLadder
  | otherwise = correctLadder
  where
    pick src pricing why dist =
      RecomputeDecision
        { distanceSource = src,
          durationSource = UseActualDuration,
          pricingSource = pricing,
          reason = why,
          modifiers = [],
          predictedChargeableDistance = dist
        }
    estimate = fromMaybe 0 estimatedDistance
    traveledRounded = roundToIntegral traveledDistance :: Meters
    downwardEnabled =
      tripCategory `notElem` cfgNoRecomputeTripCategories
        || fromMaybe True cfgEnableDownwardRecomputeForDifferentDestination
    shouldRecompute diff = recomputeBandQualifies cfgRecomputeThresholds estimate diff
    -- Same fallback semantics as legacy checkExtraKmsThreshold: unknown = ok.
    budgetOk = case budgetState of
      Nothing -> True
      Just b ->
        cfgFareRecomputeDailyExtraKmsThreshold >= b.dailyExtraKms
          && cfgFareRecomputeWeeklyExtraKmsThreshold >= b.weeklyExtraKms

    correctLadder = do
      let distanceDiff = metersToHighPrecMeters (highPrecMetersToMeters traveledDistance - estimate)
          upwardQualifies = shouldRecompute (highPrecMetersToMeters distanceDiff)
          thresholdChecks = cfgRecomputeIfPickupDropNotOutsideOfThreshold && upwardQualifies
          maxUpwardBuffer = case (estimatedDistance, cfgUpwardsRecomputeBufferPercentage) of
            (Just estDistance, Just percentage) -> HighPrecMeters (max (fromRational $ toRational estDistance.getMeters * (toRational percentage / 100)) cfgUpwardsRecomputeBuffer.getHighPrecMeters)
            _ -> cfgUpwardsRecomputeBuffer
          maxDistance = fromMaybe traveledDistance maxEstimatedDistance + maxUpwardBuffer
      if not pickupDropOutsideOfThreshold
        then
          if thresholdChecks && budgetOk
            then pick UseActualCapped QuotedPolicy WithinThresholdUpwardRecompute (Just (roundToIntegral $ min traveledDistance maxDistance))
            else do
              let why
                    | not cfgRecomputeIfPickupDropNotOutsideOfThreshold = WithinThresholdUpwardDisabled
                    | not upwardQualifies = WithinThresholdNoUpwardBand
                    | otherwise = WithinThresholdBudgetExhausted
              pick UseEstimate QuotedPolicy why (Just estimate)
        else do
          -- Outside the threshold the SAME bands gate both directions: the
          -- actual is billed only when |traveled - estimate| clears the
          -- tightest band for the estimate; sub-band deltas keep the
          -- estimate (repriced on the latest policy, destination changed).
          let deltaQualifies = shouldRecompute (abs (highPrecMetersToMeters distanceDiff))
          if distanceDiff < 0
            then
              if not downwardEnabled
                then pick UseEstimate QuotedPolicy OutsideThresholdDownwardDisabled (Just estimate)
                else
                  if deltaQualifies
                    then pick UseActual LatestPolicy OutsideThresholdShorterActual (Just traveledRounded)
                    else pick UseEstimate LatestPolicy OutsideThresholdSmallShortfallEstimate (Just estimate)
            else
              if deltaQualifies
                then pick UseActual LatestPolicy OutsideThresholdLargeOverageActual (Just traveledRounded)
                else pick UseEstimate LatestPolicy OutsideThresholdSmallOverageEstimate (Just estimate)

    failedLadder = do
      let maxDistanceFailed = case (estimatedDistance, cfgUpwardsRecomputeBufferPercentage) of
            (Just estDistance, Just percentage) -> Meters $ max (round $ toRational estDistance.getMeters * (toRational percentage / 100)) (round cfgUpwardsRecomputeBuffer.getHighPrecMeters)
            _ -> highPrecMetersToMeters cfgUpwardsRecomputeBuffer
      if not pickupDropOutsideOfThreshold
        then pick UseEstimate QuotedPolicy FailedWithinThresholdEstimate (Just estimate)
        else case approxTraveledDistance of
          Nothing -> pick UseApproxRoute LatestPolicy FailedOutsideUnknownApprox Nothing
          Just approx -> do
            let distanceDiff = metersToHighPrecMeters (approx - estimate)
                deltaQualifies = shouldRecompute (abs (highPrecMetersToMeters distanceDiff))
            if distanceDiff < 0
              then
                if not downwardEnabled
                  then pick UseEstimate QuotedPolicy FailedOutsideDownwardDisabled (Just estimate)
                  else
                    if deltaQualifies
                      then pick UseApproxRoute LatestPolicy FailedOutsideShorterApprox (Just approx)
                      else pick UseEstimate LatestPolicy FailedOutsideSmallShortfallEstimate (Just estimate)
              else
                if not deltaQualifies
                  then pick UseEstimate LatestPolicy FailedOutsideSmallOverageEstimate (Just estimate)
                  else
                    if highPrecMetersToMeters distanceDiff < maxDistanceFailed
                      then pick UseApproxRoute LatestPolicy FailedOutsideApproxRecompute (Just approx)
                      else pick UseEstimatePlusBuffer LatestPolicy FailedOutsideCappedBuffer (Just (estimate + maxDistanceFailed))

-- | Mirrors the inside of recalculateFareForDistance: the pass-through-drop
-- override, the disableRecompute bypass, and getChargeableDistanceAndDuration
-- (downward floor, downward tolerance, duration levers).
applyFareStepAdjustments :: RecomputeInput -> RecomputeDecision -> RecomputeDecision
applyFareStepAdjustments RecomputeInput {cfg = RecomputeConfig {..}, ..} base = do
  let estimate = fromMaybe 0 estimatedDistance
      passThroughApplies =
        passedThroughDrop
          && dropOutsideOfThreshold
          && tripCategory `elem` cfgNoRecomputeTripCategories
          && rideFlagDistanceCalculationFailed == Just False
          && maybe True (estimate >) cfgMinThresholdForPassThroughDestination
      afterPassThrough =
        if passThroughApplies
          then
            base
              { distanceSource = UseEstimate,
                durationSource = UseEstimatedDuration,
                modifiers = base.modifiers <> [PassThroughDropOverride],
                predictedChargeableDistance = Just estimate
              }
          else base
  if productFlags.disableRecompute
    then
      afterPassThrough
        { distanceSource = UseEstimate,
          durationSource = UseEstimatedDuration,
          modifiers = afterPassThrough.modifiers <> [DisableRecomputeBypass],
          predictedChargeableDistance = Just estimate
        }
    else do
      let recalc = afterPassThrough.predictedChargeableDistance
          -- Pass-through already pinned the duration to the estimate; the
          -- floor must not relabel it (live's finalDuration is the estimate
          -- there, so its max-floor is an identity).
          flooredSrc = if afterPassThrough.durationSource == UseEstimatedDuration then UseEstimatedDuration else UseFlooredDuration
          (withFloor, flooredDistance) = case recalc of
            Just d
              | productFlags.disableDownwardRecompute && estimate > d ->
                ( afterPassThrough
                    { distanceSource = UseEstimate,
                      durationSource = flooredSrc,
                      modifiers = afterPassThrough.modifiers <> [DownwardRecomputeFloored]
                    },
                  Just estimate
                )
              | productFlags.disableDownwardRecompute ->
                (afterPassThrough {durationSource = flooredSrc}, Just d)
              | d < estimate && metersToHighPrecMeters (estimate - d) < fromMaybe 0 cfgDownwardRecomputeDistanceThreshold ->
                ( afterPassThrough
                    { distanceSource = UseEstimate,
                      durationSource = flooredSrc,
                      modifiers = afterPassThrough.modifiers <> [DownwardToleranceForgiven]
                    },
                  Just estimate
                )
            _ -> (afterPassThrough, recalc)
          -- Duration levers: city pricing only (rental/intercity time is
          -- contractual). Billing the estimated duration zeroes the
          -- extra-time component because calculateExtraTimeFare only charges
          -- actual > estimated + grace.
          estimateBilled = flooredDistance == Just estimate && isJust flooredDistance
          gated =
            productFlags.timeLeversApplicable
              && cfgGateExtraTimeChargeByRecompute
              && estimateBilled
              && withFloor.durationSource /= UseEstimatedDuration
              && maybe False (\act -> maybe False (act >) estimatedDuration) actualDuration
          -- The forgiveness window is deducted from an overrun's billed
          -- duration (clamped at the estimate): fully inside the window ->
          -- estimate billed; beyond it -> only the excess past the window.
          mbForgivenessAdjusted =
            if productFlags.timeLeversApplicable
              && not gated
              && withFloor.durationSource == UseActualDuration
              then forgivenessAdjustedDuration cfgTimeForgivenessBands estimatedDuration actualDuration
              else Nothing
          -- Within the pickup/drop threshold, never bill below the estimated
          -- duration. Default-on, but binds only when perMinRateSections
          -- actually bill the chargeable minutes — i.e. sections present and
          -- no extra-time charge configured (a configured extra-time charge
          -- pins sections to the estimated minutes, and below-estimate
          -- durations have no fare effect on city rides anyway).
          timeFloored =
            cfgFloorTimeAtEstimateWithinThreshold
              && productFlags.hasPerMinRateSections
              && not productFlags.extraTimeChargeConfigured
              && not dropOutsideOfThreshold
              && withFloor.durationSource == UseActualDuration
              && maybe False (\act -> maybe False (act <) estimatedDuration) actualDuration
          withDuration
            | gated =
              withFloor
                { durationSource = UseEstimatedDuration,
                  modifiers = withFloor.modifiers <> [ExtraTimeGatedOnEstimateBilledRide]
                }
            | Just adj <- mbForgivenessAdjusted =
              if Just adj == estimatedDuration
                then
                  withFloor
                    { durationSource = UseEstimatedDuration,
                      modifiers = withFloor.modifiers <> [DurationOverageForgiven]
                    }
                else
                  withFloor
                    { durationSource = UseForgivenessAdjustedDuration,
                      modifiers = withFloor.modifiers <> [DurationForgivenessDeducted]
                    }
            | timeFloored =
              withFloor
                { durationSource = UseEstimatedDuration,
                  modifiers = withFloor.modifiers <> [TimeFlooredAtEstimate]
                }
            | otherwise = withFloor
      withDuration {predictedChargeableDistance = flooredDistance}
