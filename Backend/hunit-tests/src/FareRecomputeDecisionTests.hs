{-# LANGUAGE OverloadedStrings #-}

-- | Golden table for the pure end-ride fare recompute decision core. One test
-- per row of the behavior inventory in
-- docs/backend/design/fare-recompute-unification-plan.md — if any of these
-- change, the shadow decision has diverged from the legacy ladder's contract.
module FareRecomputeDecisionTests (tests) where

import qualified Data.Text as T
import qualified Domain.Action.UI.Ride.EndRide.RecomputeDecision as RD
import qualified Domain.Types as DTC
import Domain.Types.TransporterConfig (DistanceRecomputeConfigs (..))
import Kernel.Prelude
import Kernel.Types.Common
import Test.Tasty
import Test.Tasty.HUnit

band :: DistanceRecomputeConfigs
band =
  DistanceRecomputeConfigs
    { estimatedDistanceUpper = 999999,
      minThresholdPercentage = 10,
      minThresholdDistance = 1000,
      minThresholdDurationSeconds = Nothing,
      minThresholdDurationPercentage = Nothing
    }

baseCfg :: RD.RecomputeConfig
baseCfg =
  RD.RecomputeConfig
    { RD.cfgRecomputeIfPickupDropNotOutsideOfThreshold = True,
      RD.cfgRecomputeThresholds = Just [band],
      RD.cfgActualRideDistanceDiffThreshold = 1200,
      RD.cfgUpwardsRecomputeBuffer = 2000,
      RD.cfgUpwardsRecomputeBufferPercentage = Nothing,
      RD.cfgFareRecomputeDailyExtraKmsThreshold = 5000,
      RD.cfgFareRecomputeWeeklyExtraKmsThreshold = 20000,
      RD.cfgEnableDownwardRecomputeForDifferentDestination = Nothing,
      RD.cfgMinThresholdForPassThroughDestination = Nothing,
      RD.cfgDownwardRecomputeDistanceThreshold = Nothing,
      RD.cfgNoRecomputeTripCategories = RD.defaultNoRecomputeTripCategories,
      RD.cfgActualRideDurationDiffThreshold = Nothing,
      RD.cfgGateExtraTimeChargeByRecompute = False
    }

-- Estimated 10km / 30min ride that actually ran 12km; endpoints matched.
baseInput :: RD.RecomputeInput
baseInput =
  RD.RecomputeInput
    { RD.requestSource = RD.DriverSource,
      RD.tripCategory = DTC.OneWay DTC.OneWayOnDemandStaticOffer,
      RD.isOdometerBilled = False,
      RD.isRectificationCategory = False,
      RD.estimatedDistance = Just 10000,
      RD.maxEstimatedDistance = Just 11000,
      RD.estimatedDuration = Just 1800,
      RD.traveledDistance = 12000,
      RD.odometerDistance = Nothing,
      RD.approxTraveledDistance = Nothing,
      RD.actualDuration = Just 2000,
      RD.distanceCalculationFailed = False,
      RD.rideFlagDistanceCalculationFailed = Just False,
      RD.pickupDropOutsideOfThreshold = False,
      RD.dropOutsideOfThreshold = False,
      RD.passedThroughDrop = False,
      RD.budgetState = Just (RD.ExtraKmBudgetState 2000 4000),
      RD.productFlags = RD.ProductFlags False False,
      RD.cfg = baseCfg
    }

summary :: RD.RecomputeDecision -> (RD.RecomputeReason, RD.DistanceSource, RD.PricingSource, Maybe Meters)
summary d = (RD.reason d, RD.distanceSource d, RD.pricingSource d, RD.predictedChargeableDistance d)

decide :: RD.RecomputeInput -> RD.RecomputeDecision
decide = RD.decideRecompute

tests :: TestTree
tests =
  testGroup
    "FareRecomputeDecision"
    [ testGroup
        "request-source short circuits"
        [ testCase "cron job bills the estimate on the quoted policy" $
            summary (decide baseInput {RD.requestSource = RD.CronJobSource})
              @?= (RD.CronJobEstimate, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "odometer trip bills the odometer delta" $
            summary (decide baseInput {RD.isOdometerBilled = True, RD.odometerDistance = Just 9000})
              @?= (RD.OdometerBilled, RD.UseOdometer, RD.QuotedPolicy, Just 9000),
          testCase "rectification category bills traveled distance" $
            summary (decide baseInput {RD.isRectificationCategory = True})
              @?= (RD.RectificationActual, RD.UseActual, RD.QuotedPolicy, Just 12000),
          testCase "rectification category falls back to estimate on GPS failure" $
            summary (decide baseInput {RD.isRectificationCategory = True, RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True})
              @?= (RD.RectificationEstimateOnFailure, RD.UseEstimate, RD.QuotedPolicy, Just 10000)
        ],
      testGroup
        "correct distance, endpoints within threshold"
        [ testCase "upward recompute: actual capped at maxEstimated + buffer" $
            -- traveled 12000 <= 11000 + 2000 cap, so actual is billed
            summary (decide baseInput)
              @?= (RD.WithinThresholdUpwardRecompute, RD.UseActualCapped, RD.QuotedPolicy, Just 12000),
          testCase "upward recompute caps at maxEstimated + buffer" $
            summary (decide baseInput {RD.traveledDistance = 15000})
              @?= (RD.WithinThresholdUpwardRecompute, RD.UseActualCapped, RD.QuotedPolicy, Just 13000),
          testCase "estimate when upward recompute config is off" $
            summary (decide baseInput {RD.cfg = baseCfg {RD.cfgRecomputeIfPickupDropNotOutsideOfThreshold = False}})
              @?= (RD.WithinThresholdUpwardDisabled, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "estimate when the diff misses every band" $
            summary (decide baseInput {RD.traveledDistance = 10500})
              @?= (RD.WithinThresholdNoUpwardBand, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "estimate when no bands configured at all" $
            summary (decide baseInput {RD.cfg = baseCfg {RD.cfgRecomputeThresholds = Nothing}})
              @?= (RD.WithinThresholdNoUpwardBand, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "estimate when driver's extra-km budget is exhausted" $
            summary (decide baseInput {RD.budgetState = Just (RD.ExtraKmBudgetState 6000 4000)})
              @?= (RD.WithinThresholdBudgetExhausted, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "unknown budget state counts as within budget" $
            summary (decide baseInput {RD.budgetState = Nothing})
              @?= (RD.WithinThresholdUpwardRecompute, RD.UseActualCapped, RD.QuotedPolicy, Just 12000)
        ],
      testGroup
        "correct distance, endpoints outside threshold"
        [ testCase "shorter ride bills actual on latest pricing" $
            summary (decide baseInput {RD.pickupDropOutsideOfThreshold = True, RD.traveledDistance = 8000})
              @?= (RD.OutsideThresholdShorterActual, RD.UseActual, RD.LatestPolicy, Just 8000),
          testCase "shorter OTP ride keeps estimate when downward recompute disabled" $
            summary
              ( decide
                  baseInput
                    { RD.pickupDropOutsideOfThreshold = True,
                      RD.traveledDistance = 8000,
                      RD.tripCategory = DTC.OneWay DTC.OneWayRideOtp,
                      RD.cfg = baseCfg {RD.cfgEnableDownwardRecomputeForDifferentDestination = Just False}
                    }
              )
              @?= (RD.OutsideThresholdDownwardDisabled, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "small overage keeps estimate but reprices on latest policy" $
            summary (decide baseInput {RD.pickupDropOutsideOfThreshold = True, RD.traveledDistance = 11000})
              @?= (RD.OutsideThresholdSmallOverageEstimate, RD.UseEstimate, RD.LatestPolicy, Just 10000),
          testCase "large overage bills actual on latest pricing" $
            summary (decide baseInput {RD.pickupDropOutsideOfThreshold = True, RD.traveledDistance = 12000})
              @?= (RD.OutsideThresholdLargeOverageActual, RD.UseActual, RD.LatestPolicy, Just 12000)
        ],
      testGroup
        "failed distance calculation"
        [ testCase "within threshold bills the estimate" $
            summary (decide baseInput {RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True})
              @?= (RD.FailedWithinThresholdEstimate, RD.UseEstimate, RD.QuotedPolicy, Just 10000),
          testCase "outside threshold without approx distance predicts nothing" $
            summary (decide baseInput {RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True, RD.pickupDropOutsideOfThreshold = True})
              @?= (RD.FailedOutsideUnknownApprox, RD.UseApproxRoute, RD.LatestPolicy, Nothing),
          testCase "shorter approx route bills the approximation" $
            summary (decide baseInput {RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True, RD.pickupDropOutsideOfThreshold = True, RD.approxTraveledDistance = Just 8000})
              @?= (RD.FailedOutsideShorterApprox, RD.UseApproxRoute, RD.LatestPolicy, Just 8000),
          testCase "small approx overage keeps estimate on latest pricing" $
            summary (decide baseInput {RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True, RD.pickupDropOutsideOfThreshold = True, RD.approxTraveledDistance = Just 11000})
              @?= (RD.FailedOutsideSmallOverageEstimate, RD.UseEstimate, RD.LatestPolicy, Just 10000),
          testCase "qualifying approx overage bills the approximation" $
            -- diff 2000 < maxDistance 2000? no: must be strictly below; use 11500 (diff 1500 >= 1200, < 2000, band ok)
            summary (decide baseInput {RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True, RD.pickupDropOutsideOfThreshold = True, RD.approxTraveledDistance = Just 11500})
              @?= (RD.FailedOutsideApproxRecompute, RD.UseApproxRoute, RD.LatestPolicy, Just 11500),
          testCase "runaway approx overage is capped at estimate + buffer" $
            summary (decide baseInput {RD.distanceCalculationFailed = True, RD.rideFlagDistanceCalculationFailed = Just True, RD.pickupDropOutsideOfThreshold = True, RD.approxTraveledDistance = Just 20000})
              @?= (RD.FailedOutsideCappedBuffer, RD.UseEstimatePlusBuffer, RD.LatestPolicy, Just 12000)
        ],
      testGroup
        "fare-step modifiers"
        [ testCase "pass-through-drop override pins OTP ride to the estimate" $ do
            let d =
                  decide
                    baseInput
                      { RD.pickupDropOutsideOfThreshold = True,
                        RD.dropOutsideOfThreshold = True,
                        RD.passedThroughDrop = True,
                        RD.tripCategory = DTC.OneWay DTC.OneWayRideOtp
                      }
            RD.predictedChargeableDistance d @?= Just 10000
            RD.distanceSource d @?= RD.UseEstimate
            RD.durationSource d @?= RD.UseEstimatedDuration
            RD.modifiers d @?= [RD.PassThroughDropOverride],
          testCase "disableRecompute bypasses everything" $ do
            let d = decide baseInput {RD.productFlags = RD.ProductFlags True False}
            RD.predictedChargeableDistance d @?= Just 10000
            RD.modifiers d @?= [RD.DisableRecomputeBypass],
          testCase "disableDownwardRecompute floors a shorter ride at the estimate" $ do
            let d = decide baseInput {RD.pickupDropOutsideOfThreshold = True, RD.traveledDistance = 8000, RD.productFlags = RD.ProductFlags False True}
            RD.reason d @?= RD.OutsideThresholdShorterActual
            RD.predictedChargeableDistance d @?= Just 10000
            RD.durationSource d @?= RD.UseFlooredDuration
            RD.modifiers d @?= [RD.DownwardRecomputeFloored],
          testCase "downward tolerance forgives a small shortfall" $ do
            let d = decide baseInput {RD.pickupDropOutsideOfThreshold = True, RD.traveledDistance = 8000, RD.cfg = baseCfg {RD.cfgDownwardRecomputeDistanceThreshold = Just 3000}}
            RD.predictedChargeableDistance d @?= Just 10000
            RD.modifiers d @?= [RD.DownwardToleranceForgiven]
        ],
      testGroup
        "duration levers (opt-in)"
        [ testCase "duration band criteria alone can qualify upward recompute" $ do
            let durBand = band {minThresholdDurationSeconds = Just 600}
                d =
                  decide
                    baseInput
                      { RD.traveledDistance = 10000, -- zero distance diff
                        RD.actualDuration = Just 3000, -- 20 min over the 30 min estimate
                        RD.cfg = baseCfg {RD.cfgRecomputeThresholds = Just [durBand]}
                      }
            RD.reason d @?= RD.WithinThresholdUpwardRecompute,
          testCase "duration overage below forgiveness threshold bills estimated duration" $ do
            let d =
                  decide
                    baseInput
                      { RD.pickupDropOutsideOfThreshold = True,
                        RD.actualDuration = Just 2100, -- 300s over, threshold 600s
                        RD.cfg = baseCfg {RD.cfgActualRideDurationDiffThreshold = Just 600}
                      }
            RD.durationSource d @?= RD.UseEstimatedDuration
            RD.modifiers d @?= [RD.DurationOverageForgiven],
          testCase "extra-time gating pins estimate-billed rides to estimated duration" $ do
            let d =
                  decide
                    baseInput
                      { RD.traveledDistance = 10500, -- misses the band: estimate billed
                        RD.actualDuration = Just 3000,
                        RD.cfg = baseCfg {RD.cfgGateExtraTimeChargeByRecompute = True}
                      }
            RD.reason d @?= RD.WithinThresholdNoUpwardBand
            RD.durationSource d @?= RD.UseEstimatedDuration
            RD.modifiers d @?= [RD.ExtraTimeGatedOnEstimateBilledRide],
          testCase "levers off: actual duration is billed" $
            RD.durationSource (decide baseInput) @?= RD.UseActualDuration
        ],
      testGroup
        "shadow record"
        [ testCase "agreeing billed distance -> mismatch false" $
            assertBool "expected mismatch:false" ("\"mismatch\":false" `T.isInfixOf` RD.mkShadowRecordText (decide baseInput) 12000),
          testCase "diverging billed distance -> mismatch true" $
            assertBool "expected mismatch:true" ("\"mismatch\":true" `T.isInfixOf` RD.mkShadowRecordText (decide baseInput) 9999),
          testCase "record carries predicted and billed distances" $ do
            let rec = RD.mkShadowRecordText (decide baseInput) 12000
            assertBool "predictedDistance present" ("\"predictedDistance\":12000" `T.isInfixOf` rec)
            assertBool "billedDistance present" ("\"billedDistance\":12000" `T.isInfixOf` rec)
        ]
    ]
