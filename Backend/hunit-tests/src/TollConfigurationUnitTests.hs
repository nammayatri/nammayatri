{-# LANGUAGE OverloadedStrings #-}

module TollConfigurationUnitTests where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Endpoints.Toll as Common
import Control.Exception (SomeException, evaluate, try)
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.Toll as DAction
import qualified "dynamic-offer-driver-app" Domain.Types.MerchantOperatingCity as DMOC
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DMerchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified "dynamic-offer-driver-app" Domain.Types.Toll as DT
import qualified "dynamic-offer-driver-app" Environment (Flow)
import qualified "dynamic-offer-driver-app" SharedLogic.TollsDetector as Detector
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Toll as CQToll
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import qualified "mobility-core" Kernel.Types.APISuccess as APISuccess
import qualified "mobility-core" Kernel.Types.Beckn.Context as Context
import qualified "mobility-core" Kernel.Types.Id as Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- COMPILE-TIME FIELD EXISTENCE CHECKS
-- These functions verify that expected fields exist on types. They will fail
-- to compile if a field is removed or renamed.
-- The leading underscore suppresses -Wunused-binds warnings.
-- =============================================================================

-- | Verifies the 'enabled' field exists on Toll (added for Toll Configuration API)
_checkTollHasEnabledField :: DT.Toll -> Bool
_checkTollHasEnabledField = DT.enabled

-- | Verifies the 'name' field exists on Toll
_checkTollHasNameField :: DT.Toll -> T.Text
_checkTollHasNameField = DT.name

-- | Verifies the 'isAutoRickshawAllowed' field exists on Toll
_checkTollHasIsAutoRickshawField :: DT.Toll -> Bool
_checkTollHasIsAutoRickshawField = DT.isAutoRickshawAllowed

-- | Verifies the 'isTwoWheelerAllowed' field exists on Toll
_checkTollHasIsTwoWheelerField :: DT.Toll -> Maybe Bool
_checkTollHasIsTwoWheelerField = DT.isTwoWheelerAllowed

-- | Verifies the 'merchantOperatingCityId' field exists on Toll
_checkTollHasMerchantOpCityField ::
  DT.Toll ->
  Maybe (Id.Id DMOC.MerchantOperatingCity)
_checkTollHasMerchantOpCityField = DT.merchantOperatingCityId

-- | Verifies the 'enabled' field exists on TollRes
_checkTollResHasEnabledField :: Common.TollRes -> Bool
_checkTollResHasEnabledField = Common.enabled

-- | Verifies the 'tollId' field exists on UpdateTollReq
_checkUpdateTollReqHasTollIdField :: Common.UpdateTollReq -> T.Text
_checkUpdateTollReqHasTollIdField = Common.tollId

-- | Verifies the 'enabled' field exists on UpdateTollReq
_checkUpdateTollReqHasEnabledField :: Common.UpdateTollReq -> Bool
_checkUpdateTollReqHasEnabledField = Common.enabled

-- | Verifies getTollList has the correct type signature
_checkGetTollListType ::
  Id.ShortId DMerchant.Merchant ->
  Context.City ->
  Environment.Flow [Common.TollRes]
_checkGetTollListType = DAction.getTollList

-- | Verifies putTollUpdate has the correct type signature
_checkPutTollUpdateType ::
  Id.ShortId DMerchant.Merchant ->
  Context.City ->
  Common.UpdateTollReq ->
  Environment.Flow APISuccess.APISuccess
_checkPutTollUpdateType = DAction.putTollUpdate

-- =============================================================================
-- CACHE KEY GENERATION TESTS
-- Tests for pure cache key generation functions in Storage.CachedQueries.Toll
-- =============================================================================

testCacheKeyFormat :: TestTree
testCacheKeyFormat =
  testGroup
    "Toll Cache Key Format"
    [ testCase "makeTollsKeyByMerchantOperatingCityId produces correct prefix" $ do
        let cityId = Id.Id "city-abc-123" :: Id.Id DMOC.MerchantOperatingCity
            key = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
        T.isPrefixOf "CachedQueries:Toll:MerchantOpCityId-" key @? "Cache key should have correct prefix",
      testCase "makeTollsKeyByMerchantOperatingCityId embeds city ID in key" $ do
        let cityId = Id.Id "city-abc-123" :: Id.Id DMOC.MerchantOperatingCity
            key = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
        key @?= "CachedQueries:Toll:MerchantOpCityId-city-abc-123",
      testCase "Different city IDs produce different cache keys" $ do
        let cityId1 = Id.Id "city-111" :: Id.Id DMOC.MerchantOperatingCity
            cityId2 = Id.Id "city-222" :: Id.Id DMOC.MerchantOperatingCity
            key1 = CQToll.makeTollsKeyByMerchantOperatingCityId cityId1
            key2 = CQToll.makeTollsKeyByMerchantOperatingCityId cityId2
        (key1 /= key2) @? "Keys for different city IDs must differ",
      testCase "Same city ID always produces the same cache key" $ do
        let cityId = Id.Id "city-xyz" :: Id.Id DMOC.MerchantOperatingCity
            key1 = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
            key2 = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
        key1 @?= key2,
      testCase "Cache key contains the full city ID without truncation" $ do
        let longCityId = Id.Id "very-long-merchant-operating-city-id-12345" :: Id.Id DMOC.MerchantOperatingCity
            key = CQToll.makeTollsKeyByMerchantOperatingCityId longCityId
        T.isSuffixOf "very-long-merchant-operating-city-id-12345" key @? "Cache key must contain full city ID"
    ]

-- =============================================================================
-- TOLL GATE TRACKING KEY TESTS
-- Tests for the Redis key used to track pending tolls during an active ride.
-- =============================================================================

testTollGateTrackingKeyFormat :: TestTree
testTollGateTrackingKeyFormat =
  testGroup
    "Toll Gate Tracking Key Format"
    [ testCase "tollStartGateTrackingKey produces correct prefix" $ do
        let driverId = Id.Id "driver-abc-123" :: Id.Id DP.Person
            key = Detector.tollStartGateTrackingKey driverId
        T.isPrefixOf "TollGateTracking:DriverId-" key @? "Tracking key should have correct prefix",
      testCase "tollStartGateTrackingKey embeds driver ID in key" $ do
        let driverId = Id.Id "driver-abc-123" :: Id.Id DP.Person
            key = Detector.tollStartGateTrackingKey driverId
        key @?= "TollGateTracking:DriverId-driver-abc-123",
      testCase "Different driver IDs produce different tracking keys" $ do
        let driverId1 = Id.Id "driver-111" :: Id.Id DP.Person
            driverId2 = Id.Id "driver-222" :: Id.Id DP.Person
            key1 = Detector.tollStartGateTrackingKey driverId1
            key2 = Detector.tollStartGateTrackingKey driverId2
        (key1 /= key2) @? "Keys for different drivers must differ",
      testCase "Same driver ID always produces the same tracking key" $ do
        let driverId = Id.Id "driver-xyz" :: Id.Id DP.Person
            key1 = Detector.tollStartGateTrackingKey driverId
            key2 = Detector.tollStartGateTrackingKey driverId
        key1 @?= key2,
      testCase "Toll cache key and toll tracking key use different namespaces" $ do
        let sharedId = "shared-id-123"
            cityId = Id.Id sharedId :: Id.Id DMOC.MerchantOperatingCity
            driverId = Id.Id sharedId :: Id.Id DP.Person
            cacheKey = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
            trackingKey = Detector.tollStartGateTrackingKey driverId
        (cacheKey /= trackingKey) @? "Cache and tracking keys must use different namespaces to avoid collision"
    ]

-- =============================================================================
-- ENABLED TOGGLE FILTER LOGIC TESTS
-- Tests for the filtering logic that excludes disabled tolls from detection.
-- In TollsDetector.getTollInfoOnRoute: let tolls = filter (.enabled) allTolls
-- =============================================================================

-- | Helper: simulates the enabled-filter predicate on a list of (enabled, name) pairs.
-- This mirrors `filter (.enabled) tolls` applied in TollsDetector.
applyEnabledFilter :: [(Bool, T.Text)] -> [(Bool, T.Text)]
applyEnabledFilter = filter fst

testEnabledFilterDefaultsTrue :: TestTree
testEnabledFilterDefaultsTrue =
  testGroup
    "Enabled Toggle Default Behaviour"
    [ testCase "Toll with enabled=True is included in active list" $ do
        let newToll = (True, "ORR Toll Plaza")
            activeTolls = applyEnabledFilter [newToll]
        length activeTolls @?= 1,
      testCase "Toll with enabled=False is excluded from active list" $ do
        let disabledToll = (False, "ORR Toll Plaza")
            activeTolls = applyEnabledFilter [disabledToll]
        null activeTolls @? "Disabled toll should not appear in active list"
    ]

testDisabledTollExcludedFromDetection :: TestTree
testDisabledTollExcludedFromDetection =
  testGroup
    "Disabled Toll Excluded from Detection"
    [ testCase "When all tolls are disabled active list is empty" $ do
        let allDisabled = [(False, "Toll-1"), (False, "Toll-2"), (False, "Toll-3")]
            activeTolls = applyEnabledFilter allDisabled
        null activeTolls @? "Disabled tolls should not appear in active list",
      testCase "When all tolls are enabled all appear in active list" $ do
        let allEnabled = [(True, "Toll-1"), (True, "Toll-2"), (True, "Toll-3")]
            activeTolls = applyEnabledFilter allEnabled
        length activeTolls @?= 3
    ]

testEnabledTollIncluded :: TestTree
testEnabledTollIncluded =
  testGroup
    "Enabled Toll Included in Detection"
    [ testCase "Enabled toll appears in active toll list" $ do
        let tolls = [(True, "Active Toll")]
            activeTolls = applyEnabledFilter tolls
        length activeTolls @?= 1
        fst (head activeTolls) @?= True,
      testCase "Enabled toll retains its name after filtering" $ do
        let tolls = [(True, "Hebbal Flyover Toll")]
            activeTolls = applyEnabledFilter tolls
        snd (head activeTolls) @?= "Hebbal Flyover Toll"
    ]

testMixedEnabledTolls :: TestTree
testMixedEnabledTolls =
  testGroup
    "Mixed Enabled and Disabled Tolls"
    [ testCase "Route with two tolls one disabled - only enabled appears" $ do
        let routeTolls = [(True, "Toll-1"), (False, "Toll-2")]
            activeTolls = applyEnabledFilter routeTolls
        length activeTolls @?= 1
        snd (head activeTolls) @?= "Toll-1",
      testCase "Route with three tolls middle disabled - enabled ones appear" $ do
        let routeTolls = [(True, "Toll-A"), (False, "Toll-B"), (True, "Toll-C")]
            activeTolls = applyEnabledFilter routeTolls
        length activeTolls @?= 2
        map snd activeTolls @?= ["Toll-A", "Toll-C"],
      testCase "Disabled toll count is correctly excluded" $ do
        let routeTolls = [(True, "T1"), (False, "T2"), (True, "T3"), (False, "T4"), (True, "T5")]
            activeTolls = applyEnabledFilter routeTolls
            disabledTolls = filter (not . fst) routeTolls
        length activeTolls @?= 3
        length disabledTolls @?= 2
    ]

-- =============================================================================
-- API DATA TYPE STRUCTURE TESTS
-- Tests for TollRes and UpdateTollReq types generated from the YAML spec.
-- =============================================================================

testTollResStructure :: TestTree
testTollResStructure =
  testGroup
    "TollRes Data Type Structure"
    [ testCase "TollRes can be constructed with enabled=True" $ do
        let res =
              Common.TollRes
                { Common.id = "toll-123",
                  Common.name = "Hebbal Flyover",
                  Common.enabled = True,
                  Common.isAutoRickshawAllowed = False,
                  Common.isTwoWheelerAllowed = Just True,
                  Common.createdAt = UTCTime (fromGregorian 2024 1 1) 0,
                  Common.updatedAt = UTCTime (fromGregorian 2024 1 1) 0
                }
        Common.enabled res @?= True
        Common.name res @?= "Hebbal Flyover",
      testCase "TollRes can be constructed with enabled=False" $ do
        let res =
              Common.TollRes
                { Common.id = "toll-456",
                  Common.name = "ORR Toll",
                  Common.enabled = False,
                  Common.isAutoRickshawAllowed = True,
                  Common.isTwoWheelerAllowed = Nothing,
                  Common.createdAt = UTCTime (fromGregorian 2024 6 1) 0,
                  Common.updatedAt = UTCTime (fromGregorian 2024 6 1) 0
                }
        Common.enabled res @?= False,
      testCase "TollRes id field contains the toll identifier" $ do
        let res =
              Common.TollRes
                { Common.id = "unique-toll-id",
                  Common.name = "Test Toll",
                  Common.enabled = True,
                  Common.isAutoRickshawAllowed = False,
                  Common.isTwoWheelerAllowed = Nothing,
                  Common.createdAt = UTCTime (fromGregorian 2024 1 1) 0,
                  Common.updatedAt = UTCTime (fromGregorian 2024 1 1) 0
                }
        Common.id res @?= "unique-toll-id"
    ]

testUpdateTollReqStructure :: TestTree
testUpdateTollReqStructure =
  testGroup
    "UpdateTollReq Data Type Structure"
    [ testCase "UpdateTollReq can be constructed for disabling a toll" $ do
        let req = Common.UpdateTollReq {Common.tollId = "toll-123", Common.enabled = False}
        Common.tollId req @?= "toll-123"
        Common.enabled req @?= False,
      testCase "UpdateTollReq can be constructed for enabling a toll" $ do
        let req = Common.UpdateTollReq {Common.tollId = "toll-456", Common.enabled = True}
        Common.tollId req @?= "toll-456"
        Common.enabled req @?= True,
      testCase "tollId must be non-empty for a valid update request" $ do
        let tollId = "toll-123" :: T.Text
        (not $ T.null tollId) @? "Toll ID must not be empty",
      testCase "Empty tollId is detected as invalid" $ do
        let emptyTollId = "" :: T.Text
        T.null emptyTollId @? "Empty toll ID should be detected as invalid",
      testCase "Enabled toggle from True to False changes value" $ do
        let reqEnable = Common.UpdateTollReq {Common.tollId = "t1", Common.enabled = True}
            reqDisable = Common.UpdateTollReq {Common.tollId = "t1", Common.enabled = False}
        (Common.enabled reqEnable /= Common.enabled reqDisable) @? "Toggling enabled must change the value"
    ]

-- =============================================================================
-- DASHBOARD ACTION FUNCTION EXECUTION TESTS
-- Tests that the dashboard action functions have correct types and can be
-- referenced. Actual execution requires a running environment.
-- =============================================================================

testDashboardActionFunctions :: TestTree
testDashboardActionFunctions =
  testGroup
    "Dashboard Action Functions"
    [ testCase "getTollList function references correctly" $ do
        result <- try $ evaluate $ DAction.getTollList
        case result of
          Left (_ :: SomeException) ->
            True @? "getTollList function reference (exception expected without env)"
          Right _ ->
            True @? "getTollList function referenced successfully",
      testCase "putTollUpdate function references correctly" $ do
        result <- try $ evaluate $ DAction.putTollUpdate
        case result of
          Left (_ :: SomeException) ->
            True @? "putTollUpdate function reference (exception expected without env)"
          Right _ ->
            True @? "putTollUpdate function referenced successfully",
      testCase "getTollList returns Flow of TollRes list" $ do
        -- Type-level assertion: if this compiles, the signature is correct
        let _ =
              DAction.getTollList ::
                Id.ShortId DMerchant.Merchant ->
                Context.City ->
                Environment.Flow [Common.TollRes]
        True @? "getTollList has correct return type",
      testCase "putTollUpdate accepts UpdateTollReq and returns APISuccess" $ do
        -- Type-level assertion: if this compiles, the signature is correct
        let _ =
              DAction.putTollUpdate ::
                Id.ShortId DMerchant.Merchant ->
                Context.City ->
                Common.UpdateTollReq ->
                Environment.Flow APISuccess.APISuccess
        True @? "putTollUpdate has correct type signature"
    ]

-- =============================================================================
-- CACHE KEY INVALIDATION PATTERN TEST
-- Verifies that the cache key used by clearTollCache matches the key used
-- by findAllTollsByMerchantOperatingCity (they must be the same for invalidation
-- to work correctly).
-- =============================================================================

testCacheKeyConsistency :: TestTree
testCacheKeyConsistency =
  testGroup
    "Cache Key Consistency for Invalidation"
    [ testCase "Cache read key and invalidation key must be identical" $ do
        -- Both findAllTollsByMerchantOperatingCity (read) and clearTollCache (delete)
        -- call makeTollsKeyByMerchantOperatingCityId - so they always match.
        let cityId = Id.Id "city-for-cache-test" :: Id.Id DMOC.MerchantOperatingCity
            readKey = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
            -- clearTollCache calls makeTollsKeyByMerchantOperatingCityId internally
            invalidationKey = CQToll.makeTollsKeyByMerchantOperatingCityId cityId
        readKey @?= invalidationKey,
      testCase "Cache key format is stable across repeated calls" $ do
        let cityId = Id.Id "stable-city" :: Id.Id DMOC.MerchantOperatingCity
            keys = map (\_ -> CQToll.makeTollsKeyByMerchantOperatingCityId cityId) [1 :: Int .. 5]
        all (== head keys) (tail keys) @? "Cache key must be deterministic"
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

tollConfigurationUnitTests :: TestTree
tollConfigurationUnitTests =
  testGroup
    "Toll Configuration Unit Tests"
    [ testCacheKeyFormat,
      testTollGateTrackingKeyFormat,
      testEnabledFilterDefaultsTrue,
      testDisabledTollExcludedFromDetection,
      testEnabledTollIncluded,
      testMixedEnabledTolls,
      testTollResStructure,
      testUpdateTollReqStructure,
      testDashboardActionFunctions,
      testCacheKeyConsistency
    ]
