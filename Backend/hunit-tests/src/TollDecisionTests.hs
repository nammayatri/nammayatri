{-# LANGUAGE OverloadedStrings #-}

-- | Truth table for the pure end-ride toll reconciliation
-- (Domain.Action.UI.Ride.EndRide.TollDecision) — behavior-identical extraction
-- of the matrix that used to live inline in EndRide.hs.
module TollDecisionTests (tests) where

import Domain.Action.UI.Ride.EndRide.TollDecision
import Kernel.Prelude
import Kernel.Types.Confidence
import Test.Tasty
import Test.Tasty.HUnit

base :: TollInput
base =
  TollInput
    { distanceCalculationFailed = False,
      numberOfSelfTuned = Just 0,
      pickupDropOutsideOfThreshold = False,
      estimatedTollCharges = Nothing,
      estimatedTollNames = Nothing,
      estimatedTollIds = Nothing,
      detectedTollCharges = Nothing,
      detectedTollNames = Nothing,
      detectedTollIds = Nothing,
      driverDeviatedToTollRoute = Nothing,
      validatedPendingToll = Nothing,
      enableEstimatedTollFallback = False
    }

detected :: TollInput -> TollInput
detected i = i {detectedTollCharges = Just 60, detectedTollNames = Just ["NICE Road"], detectedTollIds = Just ["t1"]}

estimated :: TollInput -> TollInput
estimated i = i {estimatedTollCharges = Just 80, estimatedTollNames = Just ["NICE Road"], estimatedTollIds = Just ["t1"]}

pending :: TollInput -> TollInput
pending i = i {validatedPendingToll = Just (40, ["Elevated Expressway"], ["t2"])}

tests :: TestTree
tests =
  testGroup
    "TollDecision"
    [ testGroup
        "reliable GPS"
        [ testCase "detected only -> billed as Sure" $
            decideTollBilling (detected base)
              @?= TollBilling (Just 60) (Just ["NICE Road"]) (Just ["t1"]) (Just Sure),
          testCase "detected + pending within threshold -> combined, Neutral" $
            decideTollBilling (pending (detected base))
              @?= TollBilling (Just 100) (Just ["NICE Road", "Elevated Expressway"]) (Just ["t1", "t2"]) (Just Neutral),
          testCase "pending only within threshold -> pending billed, Neutral" $
            decideTollBilling (pending base)
              @?= TollBilling (Just 40) (Just ["Elevated Expressway"]) (Just ["t2"]) (Just Neutral),
          testCase "pending outside threshold is dropped" $
            decideTollBilling ((pending base) {pickupDropOutsideOfThreshold = True})
              @?= TollBilling Nothing Nothing Nothing Nothing,
          testCase "nothing detected but toll was estimated -> empty bill, Sure" $
            decideTollBilling (estimated base)
              @?= TollBilling Nothing Nothing Nothing (Just Sure),
          testCase "nothing anywhere -> empty bill, no confidence" $
            decideTollBilling base
              @?= TollBilling Nothing Nothing Nothing Nothing
        ],
      testGroup
        "unreliable GPS (failed or self-tuned)"
        [ testCase "self-tuned batches count as unreliable; zero estimate wipes the bill" $
            decideTollBilling ((detected (estimated base)) {numberOfSelfTuned = Just 2, estimatedTollCharges = Just 0})
              @?= TollBilling Nothing Nothing Nothing Nothing,
          testCase "estimate present + detected -> detected billed, Neutral" $
            decideTollBilling ((detected (estimated base)) {distanceCalculationFailed = True})
              @?= TollBilling (Just 60) (Just ["NICE Road"]) (Just ["t1"]) (Just Neutral),
          testCase "estimate present + detected + pending -> combined, Neutral" $
            decideTollBilling ((pending (detected (estimated base))) {distanceCalculationFailed = True})
              @?= TollBilling (Just 100) (Just ["NICE Road", "Elevated Expressway"]) (Just ["t1", "t2"]) (Just Neutral),
          testCase "nothing detected but driver deviated to toll route -> estimated, Neutral" $
            decideTollBilling ((estimated base) {distanceCalculationFailed = True, driverDeviatedToTollRoute = Just True})
              @?= TollBilling (Just 80) (Just ["NICE Road"]) (Just ["t1"]) (Just Neutral),
          testCase "GPS dark at gates, fallback enabled, within threshold -> estimated, Unsure" $
            decideTollBilling ((estimated base) {distanceCalculationFailed = True, enableEstimatedTollFallback = True})
              @?= TollBilling (Just 80) (Just ["NICE Road"]) (Just ["t1"]) (Just Unsure),
          testCase "GPS dark at gates, fallback disabled -> nothing billed, Unsure" $
            decideTollBilling ((estimated base) {distanceCalculationFailed = True})
              @?= TollBilling Nothing Nothing Nothing (Just Unsure),
          testCase "no estimate, pending within threshold -> pending billed, Unsure" $
            decideTollBilling ((pending base) {distanceCalculationFailed = True})
              @?= TollBilling (Just 40) (Just ["Elevated Expressway"]) (Just ["t2"]) (Just Unsure),
          testCase "no estimate, nothing pending -> nothing billed, no confidence" $
            decideTollBilling base {distanceCalculationFailed = True}
              @?= TollBilling Nothing Nothing Nothing Nothing
        ]
    ]
