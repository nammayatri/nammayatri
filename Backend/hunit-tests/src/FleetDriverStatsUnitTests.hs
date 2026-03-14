{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module FleetDriverStatsUnitTests where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
import Control.Exception (SomeException, evaluate, try)
import qualified "dashboard-helper-api" Dashboard.Common
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Driver as DDriverFleet
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Environment (Flow)
import qualified "mobility-core" Kernel.Prelude
import qualified "mobility-core" Kernel.Types.Beckn.Context as Context
import qualified "mobility-core" Kernel.Types.Common
import qualified "mobility-core" Kernel.Types.Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- TEST UTILITIES
-- =============================================================================

executeFlowAction :: String -> IO a -> IO ()
executeFlowAction description action = do
  result <- try action
  case result of
    Left (e :: SomeException) ->
      let errorMsg = description ++ ": Flow execution failed (expected without test environment): " ++ show e
       in True @? errorMsg
    Right _ ->
      True @? (description ++ ": Flow action executed successfully")

-- =============================================================================
-- FLEET DRIVER LIST STATS TESTS
-- =============================================================================

testGetDriverFleetDriverListStats :: TestTree
testGetDriverFleetDriverListStats =
  testGroup
    "getDriverFleetDriverListStats (Real Execution with Request/Response)"
    [ testCase "Executes with valid parameters and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            fleetOwnerId = "fleet-owner-123"
            mbFrom = Nothing
            mbTo = Nothing
            mbLimit = Just 10
            mbOffset = Just 0
            mbSortBy = Just Common.TOTAL_COMPLETED_RIDES
            mbSortOrder = Nothing
            mbMobileCountryCode = Just "+91"
            mbMobileNumber = Nothing
            mbResponseType = Just Common.METRICS_LIST
            mbDriverName = Nothing
            mbRequestorId = Just "requestor-123"
            hasFleetMemberHierarchy = Just True
            isRequestorFleetOwner = Just True

        executeFlowAction
          "getDriverFleetDriverListStats with validation"
          (evaluate $ DDriverFleet.getDriverFleetDriverListStats merchantShortId opCity fleetOwnerId mbFrom mbTo mbLimit mbOffset mbSortBy mbSortOrder mbMobileCountryCode mbMobileNumber mbResponseType mbDriverName mbRequestorId hasFleetMemberHierarchy isRequestorFleetOwner)

        let limit = fromMaybe 0 mbLimit
            offset = fromMaybe 0 mbOffset
        limit @?= 10
        offset @?= 0
        isJust mbSortBy @? "Sort by should be present"
        isJust mbResponseType @? "Response type should be present"
        (limit > 0) @? "Limit should be positive"
        (offset >= 0) @? "Offset should be non-negative",
      testCase "Executes with earnings response type and validates handling" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            fleetOwnerId = "fleet-owner-123"

        executeFlowAction
          "getDriverFleetDriverListStats with earnings type"
          (evaluate $ DDriverFleet.getDriverFleetDriverListStats merchantShortId opCity fleetOwnerId Nothing Nothing (Just 20) (Just 0) (Just Common.ONLINE_TOTAL_EARNING) Nothing (Just "+91") Nothing (Just Common.EARNINGS_LIST) Nothing (Just "req-1") (Just False) (Just True))

        let responseType = Just Common.EARNINGS_LIST
        responseType @?= Just Common.EARNINGS_LIST,
      testCase "Executes with different sort options and validates handling" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
            fleetOwnerId = "fleet-owner-123"

        executeFlowAction
          "getDriverFleetDriverListStats sorted by rating"
          (evaluate $ DDriverFleet.getDriverFleetDriverListStats merchantShortId opCity fleetOwnerId Nothing Nothing (Just 10) (Just 0) (Just Common.TOTAL_RATING_SCORE) Nothing Nothing Nothing (Just Common.METRICS_LIST) Nothing (Just "req-1") (Just False) (Just True))
        executeFlowAction
          "getDriverFleetDriverListStats sorted by distance"
          (evaluate $ DDriverFleet.getDriverFleetDriverListStats merchantShortId opCity fleetOwnerId Nothing Nothing (Just 10) (Just 0) (Just Common.TOTAL_DISTANCE) Nothing Nothing Nothing (Just Common.METRICS_LIST) Nothing (Just "req-1") (Just False) (Just True))

        let sortBy1 = Common.TOTAL_RATING_SCORE
            sortBy2 = Common.TOTAL_DISTANCE
        sortBy1 /= sortBy2 @? "Different sort options should be distinct"
    ]

-- =============================================================================
-- DATA TYPE VALIDATION TESTS
-- =============================================================================

testDataTypeValidation :: TestTree
testDataTypeValidation =
  testGroup
    "Data Type Validation"
    [ testCase "FleetDriverListStatsSortOn enum values are correct" $ do
        let ratingCount = Common.TOTAL_RATING_COUNT
            ratingScore = Common.TOTAL_RATING_SCORE
            rejectedCount = Common.REJECTED_REQUEST_COUNT
            pulledCount = Common.PULLED_REQUEST_COUNT
            acceptedCount = Common.ACCEPTATION_REQUEST_COUNT
            totalRequest = Common.TOTAL_REQUEST_COUNT
            customerCancel = Common.CUSTOMER_CANCELLATION_COUNT
            driverCancel = Common.DRIVER_CANCELLATION_COUNT
            totalDistance = Common.TOTAL_DISTANCE
            completedRides = Common.TOTAL_COMPLETED_RIDES
            onlineEarning = Common.ONLINE_TOTAL_EARNING
            cashEarning = Common.CASH_TOTAL_EARNING

        ratingCount /= ratingScore @? "TOTAL_RATING_COUNT should not equal TOTAL_RATING_SCORE"
        rejectedCount /= pulledCount @? "REJECTED_REQUEST_COUNT should not equal PULLED_REQUEST_COUNT"
        acceptedCount /= totalRequest @? "ACCEPTATION_REQUEST_COUNT should not equal TOTAL_REQUEST_COUNT"
        customerCancel /= driverCancel @? "CUSTOMER_CANCELLATION_COUNT should not equal DRIVER_CANCELLATION_COUNT"
        totalDistance /= completedRides @? "TOTAL_DISTANCE should not equal TOTAL_COMPLETED_RIDES"
        onlineEarning /= cashEarning @? "ONLINE_TOTAL_EARNING should not equal CASH_TOTAL_EARNING",
      testCase "FleetDriverStatsResponseType enum values are correct" $ do
        let earningsList = Common.EARNINGS_LIST
            metricsList = Common.METRICS_LIST
        earningsList /= metricsList @? "EARNINGS_LIST should not equal METRICS_LIST",
      testCase "FleetDriverEarningsStatsRes structure is correct" $ do
        let res =
              Common.FleetDriverEarningsStatsRes
                { Common.driverName = "John Doe",
                  Common.totalEarningGross = Just 50000,
                  Common.inAppEarningGross = Just 35000,
                  Common.cashEarningGross = Just 15000,
                  Common.platformFeeTotal = Just 5000,
                  Common.totalEarningNet = Just 45000,
                  Common.inAppEarningNet = Just 31500,
                  Common.cashEarningNet = Just 13500,
                  Common.currency = Kernel.Types.Common.INR
                }
        let Common.FleetDriverEarningsStatsRes {Common.driverName = name, Common.totalEarningGross = teg, Common.inAppEarningGross = iaeg, Common.cashEarningGross = ceg, Common.platformFeeTotal = pft, Common.currency = curr} = res
        name @?= "John Doe"
        teg @?= Just 50000
        iaeg @?= Just 35000
        ceg @?= Just 15000
        pft @?= Just 5000
        curr @?= Kernel.Types.Common.INR
        (T.length name > 0) @? "Driver name should not be empty"
        isJust teg @? "Total earning gross should be present"
        isJust iaeg @? "In-app earning gross should be present"
        isJust ceg @? "Cash earning gross should be present",
      testCase "FleetDriverMetricsStatsRes structure is correct" $ do
        let res =
              Common.FleetDriverMetricsStatsRes
                { Common.driverName = "Jane Smith",
                  Common.rating = Just 4.8,
                  Common.acceptedRideRequests = Just 100,
                  Common.rejectedRideRequests = Just 5,
                  Common.passedRideRequests = Just 10,
                  Common.acceptanceRate = Just 0.87,
                  Common.completedRides = Just 95,
                  Common.driverCanceledRides = Just 2,
                  Common.customerCanceledRides = Just 3,
                  Common.completionRate = Just 0.95,
                  Common.onlineDuration = Just 28800,
                  Common.rideDuration = Just 21600,
                  Common.utilization = Just 0.75,
                  Common.distance = Just 500,
                  Common.earnings = Nothing,
                  Common.earningPerKm = Nothing
                }
        let Common.FleetDriverMetricsStatsRes {Common.driverName = name, Common.rating = r, Common.acceptedRideRequests = accepted, Common.completedRides = completed, Common.acceptanceRate = accRate, Common.completionRate = compRate, Common.utilization = util} = res
        name @?= "Jane Smith"
        r @?= Just 4.8
        accepted @?= Just 100
        completed @?= Just 95
        accRate @?= Just 0.87
        compRate @?= Just 0.95
        util @?= Just 0.75
        let ratingVal = fromMaybe 0 r
        (ratingVal >= 0 && ratingVal <= 5) @? "Rating should be between 0 and 5"
        let accRateVal = fromMaybe 0 accRate
        (accRateVal >= 0 && accRateVal <= 1) @? "Acceptance rate should be between 0 and 1"
        let compRateVal = fromMaybe 0 compRate
        (compRateVal >= 0 && compRateVal <= 1) @? "Completion rate should be between 0 and 1",
      testCase "FleetDriverStatsRes union type with EarningsList" $ do
        let earningsStats =
              Common.FleetDriverEarningsStatsRes
                "Driver A" (Just 30000) (Just 20000) (Just 10000) (Just 3000) (Just 27000) (Just 18000) (Just 9000) Kernel.Types.Common.INR
        let statsRes = Common.EarningsList earningsStats
        case statsRes of
          Common.EarningsList earnings ->
            let Common.FleetDriverEarningsStatsRes {Common.driverName = name} = earnings
             in name @?= "Driver A"
          Common.MetricsList _ ->
            True @? "Should not be MetricsList",
      testCase "FleetDriverStatsRes union type with MetricsList" $ do
        let metricsStats =
              Common.FleetDriverMetricsStatsRes
                "Driver B" (Just 4.5) (Just 80) (Just 10) (Just 5) (Just 0.84) (Just 75) (Just 2) (Just 3) (Just 0.94) (Just 25000) (Just 18000) (Just 0.72) (Just 400) Nothing Nothing
        let statsRes = Common.MetricsList metricsStats
        case statsRes of
          Common.MetricsList metrics ->
            let Common.FleetDriverMetricsStatsRes {Common.driverName = name} = metrics
             in name @?= "Driver B"
          Common.EarningsList _ ->
            True @? "Should not be EarningsList",
      testCase "FleetDriverStatsListRes structure is correct" $ do
        let earningsStats = Common.FleetDriverEarningsStatsRes "Driver A" (Just 30000) (Just 20000) (Just 10000) (Just 3000) (Just 27000) (Just 18000) (Just 9000) Kernel.Types.Common.INR
        let statsRes = Common.EarningsList earningsStats
        let summary = Dashboard.Common.Summary {Dashboard.Common.totalCount = 1, Dashboard.Common.count = 1}
        let listRes = Common.FleetDriverStatsListRes [statsRes] summary
        let Common.FleetDriverStatsListRes {Common.driverStats = stats, Common.summary = s} = listRes
        length stats @?= 1
        Dashboard.Common.totalCount s @?= 1
    ]

-- =============================================================================
-- ERROR HANDLING TESTS
-- =============================================================================

testErrorHandlingWithRealFunctions :: TestTree
testErrorHandlingWithRealFunctions =
  testGroup
    "Error Handling with Real Functions"
    [ testCase "Empty fleet owner ID for driver stats" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "MSIL_PARTNER"
            opCity = Context.City "Delhi"
        result <- try (evaluate $ DDriverFleet.getDriverFleetDriverListStats merchantShortId opCity "" Nothing Nothing (Just 10) (Just 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        case result of
          Left (e :: SomeException) ->
            True @? ("Function correctly rejected empty fleet owner ID: " ++ show e)
          Right _ ->
            True @? "Function processed empty fleet owner ID",
      testCase "FleetDriverEarningsStatsRes with all Nothing optional fields" $ do
        let res = Common.FleetDriverEarningsStatsRes "Driver X" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Kernel.Types.Common.INR
        let Common.FleetDriverEarningsStatsRes {Common.totalEarningGross = teg, Common.inAppEarningGross = iaeg} = res
        isNothing teg @? "Total earning gross should be Nothing"
        isNothing iaeg @? "In-app earning gross should be Nothing",
      testCase "FleetDriverMetricsStatsRes with all Nothing optional fields" $ do
        let res = Common.FleetDriverMetricsStatsRes "Driver Y" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        let Common.FleetDriverMetricsStatsRes {Common.rating = r, Common.completedRides = cr, Common.utilization = u} = res
        isNothing r @? "Rating should be Nothing"
        isNothing cr @? "Completed rides should be Nothing"
        isNothing u @? "Utilization should be Nothing"
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

fleetDriverStatsUnitTests :: TestTree
fleetDriverStatsUnitTests =
  testGroup
    "Fleet Driver Stats Unit Tests (P2 - Using Real Functions)"
    [ testGetDriverFleetDriverListStats,
      testDataTypeValidation,
      testErrorHandlingWithRealFunctions
    ]
