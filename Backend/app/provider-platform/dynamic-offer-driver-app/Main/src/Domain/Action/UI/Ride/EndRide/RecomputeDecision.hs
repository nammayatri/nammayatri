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
-- The duration levers ('cfgActualRideDurationDiffThreshold',
-- 'cfgGateExtraTimeChargeByRecompute', duration criteria on recompute bands)
-- are mirrored here AND wired into the live path (see
-- 'getChargeableDistanceAndDuration' / 'shouldUpwardRecompute' in EndRide.hs,
-- both of which delegate their predicates to this module so there is a single
-- source of truth).
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
    upwardBandQualifies,
    forgivenDurationOverage,
    reasonText,
  )
where

import qualified Data.Aeson as Ae
import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Domain.Types as DTC
import qualified Domain.Types.TransporterConfig as DTConf
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Common
import Kernel.Utils.Text (encodeToText)

data RequestSource = DriverSource | DashboardSource | CallBasedSource | CronJobSource
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Recompute kill-switches carried on the FareProduct / FullFarePolicy.
data ProductFlags = ProductFlags
  { disableRecompute :: Bool,
    disableDownwardRecompute :: Bool
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
    cfgRecomputeThresholds :: Maybe [DTConf.DistanceRecomputeConfigs],
    cfgActualRideDistanceDiffThreshold :: HighPrecMeters,
    cfgUpwardsRecomputeBuffer :: HighPrecMeters,
    cfgUpwardsRecomputeBufferPercentage :: Maybe Int,
    cfgFareRecomputeDailyExtraKmsThreshold :: HighPrecMeters,
    cfgFareRecomputeWeeklyExtraKmsThreshold :: HighPrecMeters,
    cfgEnableDownwardRecomputeForDifferentDestination :: Maybe Bool,
    cfgMinThresholdForPassThroughDestination :: Maybe Meters,
    cfgDownwardRecomputeDistanceThreshold :: Maybe HighPrecMeters,
    cfgNoRecomputeTripCategories :: [DTC.TripCategory],
    -- | Duration overage strictly below this is forgiven: the ride is billed
    -- at the estimated duration (so no extra-time charge). Nothing = lever off
    -- (today's behavior: actual duration always billed).
    cfgActualRideDurationDiffThreshold :: Maybe Seconds,
    -- | When True, rides billed at the ESTIMATED distance also bill the
    -- estimated duration, so the per-minute extra-time charge cannot fire on
    -- a ride the distance ladder decided to forgive. Default False.
    cfgGateExtraTimeChargeByRecompute :: Bool
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

data DurationSource = UseActualDuration | UseEstimatedDuration | UseFlooredDuration
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
  | OutsideThresholdSmallOverageEstimate
  | OutsideThresholdLargeOverageActual
  | FailedWithinThresholdEstimate
  | FailedOutsideShorterApprox
  | FailedOutsideDownwardDisabled
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
  | ExtraTimeGatedOnEstimateBilledRide
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

-- | What 'tripCategoriesForNoRecalc' hardcodes today; overridable per city via
-- TransporterConfig.noRecomputeTripCategories.
defaultNoRecomputeTripCategories :: [DTC.TripCategory]
defaultNoRecomputeTripCategories = [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer]

mkRecomputeConfig :: DTConf.TransporterConfig -> RecomputeConfig
mkRecomputeConfig tc =
  RecomputeConfig
    { cfgRecomputeIfPickupDropNotOutsideOfThreshold = tc.recomputeIfPickupDropNotOutsideOfThreshold,
      cfgRecomputeThresholds = tc.recomputeDistanceThresholds,
      cfgActualRideDistanceDiffThreshold = tc.actualRideDistanceDiffThreshold,
      cfgUpwardsRecomputeBuffer = tc.upwardsRecomputeBuffer,
      cfgUpwardsRecomputeBufferPercentage = tc.upwardsRecomputeBufferPercentage,
      cfgFareRecomputeDailyExtraKmsThreshold = tc.fareRecomputeDailyExtraKmsThreshold,
      cfgFareRecomputeWeeklyExtraKmsThreshold = tc.fareRecomputeWeeklyExtraKmsThreshold,
      cfgEnableDownwardRecomputeForDifferentDestination = tc.enableDownwardRecomputeForDifferentDestination,
      cfgMinThresholdForPassThroughDestination = tc.minThresholdForPassThroughDestination,
      cfgDownwardRecomputeDistanceThreshold = tc.downwardRecomputeDistanceThreshold,
      cfgNoRecomputeTripCategories = fromMaybe defaultNoRecomputeTripCategories tc.noRecomputeTripCategories,
      cfgActualRideDurationDiffThreshold = tc.actualRideDurationDiffThreshold,
      cfgGateExtraTimeChargeByRecompute = tc.gateExtraTimeChargeByRecompute == Just True
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

-- | Band predicate shared by the live 'shouldUpwardRecompute' and the shadow
-- decision. A band matches when the DISTANCE criteria hold, OR (new, opt-in)
-- when its duration criteria are configured and the DURATION overage clears
-- them. Absent duration criteria keep today's distance-only behavior.
upwardBandQualifies ::
  Maybe [DTConf.DistanceRecomputeConfigs] ->
  Meters ->
  Meters ->
  Maybe Seconds ->
  Maybe Seconds ->
  Bool
upwardBandQualifies mbBands estimatedDist distanceDiff mbEstimatedDur mbActualDur = do
  let filteredThresholds = maybe [] (filter (\band -> band.estimatedDistanceUpper > estimatedDist)) mbBands
      mbBand = listToMaybe $ sortBy (comparing \band -> band.estimatedDistanceUpper - estimatedDist) filteredThresholds
  case mbBand of
    Nothing -> False
    Just band -> do
      let distanceQualifies =
            distanceDiff > band.minThresholdDistance
              && distanceDiff.getMeters > (estimatedDist.getMeters * band.minThresholdPercentage) `div` 100
          durationQualifies = case (band.minThresholdDurationSeconds, mbEstimatedDur, mbActualDur) of
            (Just minDurDiff, Just estDur, Just actDur) -> do
              let durDiff = actDur - estDur
                  pctOk = case band.minThresholdDurationPercentage of
                    Just pct -> durDiff.getSeconds > (estDur.getSeconds * pct) `div` 100
                    Nothing -> True
              durDiff > minDurDiff && pctOk
            _ -> False
      distanceQualifies || durationQualifies

-- | True when the actual duration overran the estimate by strictly less than
-- the configured forgiveness threshold (lever off when unset).
forgivenDurationOverage :: Maybe Seconds -> Maybe Seconds -> Maybe Seconds -> Bool
forgivenDurationOverage mbThreshold mbEstimatedDur mbActualDur =
  case (mbThreshold, mbEstimatedDur, mbActualDur) of
    (Just threshold, Just estDur, Just actDur) -> actDur > estDur && (actDur - estDur) < threshold
    _ -> False

decideRecompute :: RecomputeInput -> RecomputeDecision
decideRecompute input = applyFareStepAdjustments input (basePick input)

-- | Mirrors the branch point in endRideHandler plus the two
-- calculateFinalValuesFor* ladders.
basePick :: RecomputeInput -> RecomputeDecision
basePick RecomputeInput {cfg = RecomputeConfig {..}, ..}
  | requestSource == CronJobSource = pick UseEstimate QuotedPolicy CronJobEstimate (Just estimate)
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
    shouldRecompute diff = upwardBandQualifies cfgRecomputeThresholds estimate diff estimatedDuration actualDuration
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
        else
          if distanceDiff < 0
            then
              if downwardEnabled
                then pick UseActual LatestPolicy OutsideThresholdShorterActual (Just traveledRounded)
                else pick UseEstimate QuotedPolicy OutsideThresholdDownwardDisabled (Just estimate)
            else
              if distanceDiff < cfgActualRideDistanceDiffThreshold
                then pick UseEstimate LatestPolicy OutsideThresholdSmallOverageEstimate (Just estimate)
                else pick UseActual LatestPolicy OutsideThresholdLargeOverageActual (Just traveledRounded)

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
            if distanceDiff < 0
              then
                if downwardEnabled
                  then pick UseApproxRoute LatestPolicy FailedOutsideShorterApprox (Just approx)
                  else pick UseEstimate QuotedPolicy FailedOutsideDownwardDisabled (Just estimate)
              else
                if distanceDiff < cfgActualRideDistanceDiffThreshold
                  then pick UseEstimate LatestPolicy FailedOutsideSmallOverageEstimate (Just estimate)
                  else
                    if highPrecMetersToMeters distanceDiff < maxDistanceFailed && shouldRecompute (highPrecMetersToMeters distanceDiff)
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
          (withFloor, flooredDistance) = case recalc of
            Just d
              | productFlags.disableDownwardRecompute && estimate > d ->
                ( afterPassThrough
                    { distanceSource = UseEstimate,
                      durationSource = UseFlooredDuration,
                      modifiers = afterPassThrough.modifiers <> [DownwardRecomputeFloored]
                    },
                  Just estimate
                )
              | productFlags.disableDownwardRecompute ->
                (afterPassThrough {durationSource = UseFlooredDuration}, Just d)
              | d < estimate && metersToHighPrecMeters (estimate - d) < fromMaybe 0 cfgDownwardRecomputeDistanceThreshold ->
                ( afterPassThrough
                    { distanceSource = UseEstimate,
                      durationSource = UseFlooredDuration,
                      modifiers = afterPassThrough.modifiers <> [DownwardToleranceForgiven]
                    },
                  Just estimate
                )
            _ -> (afterPassThrough, recalc)
          -- Duration levers, applied on top (both default-off). Billing the
          -- estimated duration zeroes the extra-time component because
          -- calculateExtraTimeFare only charges actual > estimated + grace.
          estimateBilled = flooredDistance == Just estimate && isJust flooredDistance
          gated =
            cfgGateExtraTimeChargeByRecompute
              && estimateBilled
              && withFloor.durationSource /= UseEstimatedDuration
              && maybe False (\act -> maybe False (act >) estimatedDuration) actualDuration
          forgiven =
            not gated
              && withFloor.durationSource == UseActualDuration
              && forgivenDurationOverage cfgActualRideDurationDiffThreshold estimatedDuration actualDuration
          withDuration
            | gated =
              withFloor
                { durationSource = UseEstimatedDuration,
                  modifiers = withFloor.modifiers <> [ExtraTimeGatedOnEstimateBilledRide]
                }
            | forgiven =
              withFloor
                { durationSource = UseEstimatedDuration,
                  modifiers = withFloor.modifiers <> [DurationOverageForgiven]
                }
            | otherwise = withFloor
      withDuration {predictedChargeableDistance = flooredDistance}
