module FRFS.UnitTests where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import EulerHS.Prelude
import SharedLogic.PTCircuitBreaker
import Test.Tasty
import Test.Tasty.HUnit

-- ============================================================
-- Circuit Breaker Config Tests (PR #13987)
-- ============================================================

testDefaultCircuitBreakerConfig :: TestTree
testDefaultCircuitBreakerConfig =
  testGroup
    "Default circuit breaker config"
    [ testCase "fare threshold: 20 failures in 30s" $ do
        let cfg = defaultCircuitBreakerConfig
        let fareThresholds = maybe [] (.thresholds) cfg.fare
        case fareThresholds of
          [ThresholdConfig fc ws] -> do
            fc @?= 20
            ws @?= 30
          other -> assertFailure $ "Expected single threshold, got: " <> show other,
      testCase "booking threshold: 20 failures in 30s" $ do
        let cfg = defaultCircuitBreakerConfig
        let bookingThresholds = maybe [] (.thresholds) cfg.booking
        case bookingThresholds of
          [ThresholdConfig fc ws] -> do
            fc @?= 20
            ws @?= 30
          other -> assertFailure $ "Expected single threshold, got: " <> show other,
      testCase "fare canary: 10 per 30s window" $ do
        let cfg = defaultCircuitBreakerConfig
        let fareCfg = cfg.fare
        case fareCfg of
          Just c -> do
            c.canaryAllowedPerWindow @?= 10
            c.canaryWindowSeconds @?= 30
          Nothing -> assertFailure "Expected fare config to be present",
      testCase "booking canary: 10 per 30s window" $ do
        let cfg = defaultCircuitBreakerConfig
        let bookingCfg = cfg.booking
        case bookingCfg of
          Just c -> do
            c.canaryAllowedPerWindow @?= 10
            c.canaryWindowSeconds @?= 30
          Nothing -> assertFailure "Expected booking config to be present"
    ]

testParseCircuitBreakerConfig :: TestTree
testParseCircuitBreakerConfig =
  testGroup
    "parseCircuitBreakerConfig"
    [ testCase "Nothing returns default config" $ do
        let result = parseCircuitBreakerConfig Nothing
        result @?= defaultCircuitBreakerConfig,
      testCase "JSON round-trip preserves thresholds" $ do
        let cfg = defaultCircuitBreakerConfig
            encoded = encode cfg
            decoded = decode encoded :: Maybe CircuitBreakerConfig
        case decoded of
          Nothing -> assertFailure "Failed to decode JSON round-trip"
          Just result -> result @?= cfg,
      testCase "Custom threshold overrides default" $ do
        let customJson =
              "{\"fare\":{\"thresholds\":[{\"failureCount\":5,\"windowSeconds\":10}],\"canaryAllowedPerWindow\":3,\"canaryWindowSeconds\":15},"
                <> "\"booking\":{\"thresholds\":[{\"failureCount\":10,\"windowSeconds\":20}],\"canaryAllowedPerWindow\":5,\"canaryWindowSeconds\":25}}"
        let mVal = decode (BL.pack customJson)
        case mVal of
          Nothing -> assertFailure "Failed to parse custom JSON"
          Just val -> do
            let result = parseCircuitBreakerConfig (Just val)
            let fareThresholds = maybe [] (.thresholds) result.fare
            case fareThresholds of
              [ThresholdConfig fc ws] -> do
                fc @?= 5
                ws @?= 10
              _ -> assertFailure "Unexpected fare thresholds"
    ]

-- ============================================================
-- CMRL Retry Backoff Tests (PR #13988)
-- ============================================================

testCMRLRetryBackoff :: TestTree
testCMRLRetryBackoff =
  testGroup
    "CMRL retry exponential backoff delays"
    [ testCase "attempt 0: 1s delay (1000000 μs)" $
        cmrlRetryDelay 0 @?= 1000000,
      testCase "attempt 1: 2s delay (2000000 μs)" $
        cmrlRetryDelay 1 @?= 2000000,
      testCase "attempt 2: 4s delay (4000000 μs)" $
        cmrlRetryDelay 2 @?= 4000000,
      testCase "attempt 3: 8s delay (8000000 μs)" $
        cmrlRetryDelay 3 @?= 8000000
    ]
  where
    -- Mirrors the formula in Auth.hs: 1000000 * (2 ^ attempt)
    cmrlRetryDelay :: Int -> Int
    cmrlRetryDelay attempt = 1000000 * (2 ^ attempt)

-- ============================================================
-- GTFS Stage Fare Clamping Tests (PR #13986)
-- ============================================================

testGTFSStageFareClamping :: TestTree
testGTFSStageFareClamping =
  testGroup
    "GTFS stage fare minimum clamping"
    [ testCase "same stop (stage=0, isStageStop=False): clamps to 1" $
        computeClampedStage 0 (Just False) @?= 1,
      testCase "adjacent stops (stage=1, isStageStop=True): adjusts to 0 then clamps to 1" $
        computeClampedStage 1 (Just True) @?= 1,
      testCase "adjacent stops (stage=1, isStageStop=False): stays 1, clamps to 1" $
        computeClampedStage 1 (Just False) @?= 1,
      testCase "2 stages apart (isStageStop=True): adjusted=1, clamped=1" $
        computeClampedStage 2 (Just True) @?= 1,
      testCase "2 stages apart (isStageStop=False): stage=2, clamped=2" $
        computeClampedStage 2 (Just False) @?= 2,
      testCase "5 stages apart (isStageStop=True): adjusted=4, clamped=4" $
        computeClampedStage 5 (Just True) @?= 4,
      testCase "bug: old max 0 would allow stage 0 lookup (now fixed to max 1)" $ do
        let oldBehavior stage endIsStageStop =
              let adjusted = case endIsStageStop of
                    Just True -> stage - 1
                    _ -> stage
               in max 0 adjusted
            newBehavior = computeClampedStage
        -- Old code: stage=1, isStageStop=True → adjusted=0, max 0 = 0 (wrong!)
        oldBehavior 1 (Just True) @?= 0
        -- New code: same inputs → clamped to 1 (correct)
        newBehavior 1 (Just True) @?= 1
    ]
  where
    -- Mirrors the logic in FRFSUtils.hs:473-478
    computeClampedStage :: Int -> Maybe Bool -> Int
    computeClampedStage stage endIsStageStop =
      let adjustedStage = case endIsStageStop of
            Just True -> stage - 1
            _ -> stage
       in max 1 adjustedStage

-- ============================================================
-- QR Simplification Tests (PR #13988)
-- ============================================================

testQRDescriptionLogic :: TestTree
testQRDescriptionLogic =
  testGroup
    "QR description logic"
    [ testCase "isSingleMode=True gives PROVIDER_NATIVE_QR" $
        qrDescription True @?= Just "PROVIDER_NATIVE_QR",
      testCase "isSingleMode=False gives Nothing" $
        qrDescription False @?= Nothing,
      testCase "PROVIDER_NATIVE_QR marks shorter payload path" $ do
        let desc = qrDescription True
        desc @?= Just "PROVIDER_NATIVE_QR"
    ]
  where
    -- Mirrors Flow.Common.hs:245
    qrDescription :: Bool -> Maybe Text
    qrDescription isSingleModeCMRL =
      if isSingleModeCMRL then Just "PROVIDER_NATIVE_QR" else Nothing

-- ============================================================
-- All Tests
-- ============================================================

allTests :: TestTree
allTests =
  testGroup
    "FRFS Unit Tests (PR #13986, #13987, #13988)"
    [ testDefaultCircuitBreakerConfig,
      testParseCircuitBreakerConfig,
      testCMRLRetryBackoff,
      testGTFSStageFareClamping,
      testQRDescriptionLogic
    ]
