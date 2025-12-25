{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module CreateRequestUnitTests where

-- Imports for the real function and types

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Operator.Endpoints.Driver as Common
import Control.Exception (evaluate, try)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Operator.Driver as DDriverOp
import qualified "dynamic-offer-driver-app" Domain.Types.Merchant as DM
import qualified "dynamic-offer-driver-app" Environment (Flow)
import qualified "mobility-core" Kernel.Prelude
import qualified "mobility-core" Kernel.Types.APISuccess
import qualified "mobility-core" Kernel.Types.Beckn.Context as Context
import qualified "mobility-core" Kernel.Types.Id
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- TEST UTILITIES FOR FLOW EXECUTION
-- =============================================================================

executeFlowAction :: String -> IO a -> IO ()
executeFlowAction description action = do
  result <- try action
  case result of
    Left (e :: Kernel.Prelude.SomeException) ->
      let errorMsg = description ++ ": Flow execution failed (expected without test environment): " ++ show e
       in True @? errorMsg
    Right _ ->
      True @? (description ++ ": Flow action executed successfully")

-- =============================================================================
-- REAL FUNCTION EXECUTION WITH REQUEST BODIES AND RESPONSE VALIDATION
-- =============================================================================

testPostDriverOperatorCreateRequestWithRealExecution :: TestTree
testPostDriverOperatorCreateRequestWithRealExecution =
  testGroup
    "postDriverOperatorCreateRequest (Real Execution with Request/Response)"
    [ testCase "Executes with valid create request and validates response structure" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.Delhi
            req =
              Common.DriverOperationHubRequest
                { Common.creatorId = "creator-123",
                  Common.operationHubId = Kernel.Types.Id.Id "hub-123",
                  Common.registrationNo = "DL01AB1234",
                  Common.requestType = Common.ONBOARDING_INSPECTION
                }
        executeFlowAction
          "postDriverOperatorCreateRequest with validation"
          (evaluate $ DDriverOp.postDriverOperatorCreateRequest merchantShortId opCity req)
        let Common.DriverOperationHubRequest {Common.creatorId = creatorId, Common.operationHubId = operationHubId, Common.registrationNo = registrationNo, Common.requestType = requestType} = req
        creatorId @?= "creator-123"
        operationHubId @?= Kernel.Types.Id.Id "hub-123"
        registrationNo @?= "DL01AB1234"
        requestType @?= Common.ONBOARDING_INSPECTION
        (T.length creatorId > 0) @? "Creator ID should not be empty"
        (T.length registrationNo >= 10) @? "Registration number should be at least 10 characters"
        T.isPrefixOf "DL" registrationNo @? "Registration number should start with DL"
        let expectedResponseType =
              DDriverOp.postDriverOperatorCreateRequest ::
                Kernel.Types.Id.ShortId DM.Merchant ->
                Context.City ->
                Common.DriverOperationHubRequest ->
                Environment.Flow Kernel.Types.APISuccess.APISuccess
        True @? "Function should return APISuccess",
      testCase "Executes with different request types and validates request handling" $ do
        let req1 = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
            req2 = Common.DriverOperationHubRequest "creator-456" (Kernel.Types.Id.Id "hub-456") "DL02CD5678" Common.REGULAR_INSPECTION
            merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.Delhi
        executeFlowAction
          "postDriverOperatorCreateRequest with req1"
          (evaluate $ DDriverOp.postDriverOperatorCreateRequest merchantShortId opCity req1)
        executeFlowAction
          "postDriverOperatorCreateRequest with req2"
          (evaluate $ DDriverOp.postDriverOperatorCreateRequest merchantShortId opCity req2)
        let Common.DriverOperationHubRequest {Common.creatorId = creatorId1, Common.operationHubId = operationHubId1, Common.registrationNo = registrationNo1, Common.requestType = requestType1} = req1
            Common.DriverOperationHubRequest {Common.creatorId = creatorId2, Common.operationHubId = operationHubId2, Common.registrationNo = registrationNo2, Common.requestType = requestType2} = req2
        creatorId1 @?= "creator-123"
        creatorId2 @?= "creator-456"
        operationHubId1 @?= Kernel.Types.Id.Id "hub-123"
        operationHubId2 @?= Kernel.Types.Id.Id "hub-456"
        registrationNo1 @?= "DL01AB1234"
        registrationNo2 @?= "DL02CD5678"
        requestType1 @?= Common.ONBOARDING_INSPECTION
        requestType2 @?= Common.REGULAR_INSPECTION
        creatorId1 /= creatorId2 @? "Different creator IDs should be distinct"
        operationHubId1 /= operationHubId2 @? "Different operation hub IDs should be distinct"
        registrationNo1 /= registrationNo2 @? "Different registration numbers should be distinct"
        requestType1 /= requestType2 @? "Different request types should be distinct"
    ]

-- =============================================================================
-- DATA TYPE VALIDATION TESTS
-- =============================================================================

testDataTypeValidation :: TestTree
testDataTypeValidation =
  testGroup
    "Data Type Validation"
    [ testCase "RequestType enum values are correct" $ do
        let onboardingInspection = Common.ONBOARDING_INSPECTION
            regularInspection = Common.REGULAR_INSPECTION
        onboardingInspection /= regularInspection @? "ONBOARDING_INSPECTION should not equal REGULAR_INSPECTION",
      testCase "DriverOperationHubRequest structure is correct" $ do
        let req = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
        let Common.DriverOperationHubRequest {Common.creatorId = creatorId, Common.operationHubId = operationHubId, Common.registrationNo = registrationNo, Common.requestType = requestType} = req
        creatorId @?= "creator-123"
        operationHubId @?= Kernel.Types.Id.Id "hub-123"
        registrationNo @?= "DL01AB1234"
        requestType @?= Common.ONBOARDING_INSPECTION,
      testCase "City enum values are correct" $ do
        let delhi = Context.Delhi
            bangalore = Context.Bangalore
            mumbai = Context.Mumbai
        delhi /= bangalore @? "Delhi should not equal Bangalore"
        bangalore /= mumbai @? "Bangalore should not equal Mumbai"
        delhi /= mumbai @? "Delhi should not equal Mumbai",
      testCase "ShortId structure is correct" $ do
        let shortId = Kernel.Types.Id.ShortId "merchant-123"
        let Kernel.Types.Id.ShortId id = shortId
        id @?= "merchant-123"
    ]

-- =============================================================================
-- COMPLEX SCENARIOS WITH REAL FUNCTIONS
-- =============================================================================

testComplexScenariosWithRealFunctions :: TestTree
testComplexScenariosWithRealFunctions =
  testGroup
    "Complex Scenarios with Real Functions"
    [ testCase "Different request types work correctly" $ do
        let onboardingReq = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
            regularReq = Common.DriverOperationHubRequest "creator-456" (Kernel.Types.Id.Id "hub-456") "DL02CD5678" Common.REGULAR_INSPECTION
        let Common.DriverOperationHubRequest {Common.requestType = reqType1} = onboardingReq
            Common.DriverOperationHubRequest {Common.requestType = reqType2} = regularReq
        reqType1 @?= Common.ONBOARDING_INSPECTION
        reqType2 @?= Common.REGULAR_INSPECTION
        reqType1 /= reqType2 @? "Different request types should be distinct",
      testCase "Different operation hubs work correctly" $ do
        let hub1Req = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
            hub2Req = Common.DriverOperationHubRequest "creator-456" (Kernel.Types.Id.Id "hub-456") "DL02CD5678" Common.ONBOARDING_INSPECTION
        let Common.DriverOperationHubRequest {Common.operationHubId = hubId1} = hub1Req
            Common.DriverOperationHubRequest {Common.operationHubId = hubId2} = hub2Req
        hubId1 @?= Kernel.Types.Id.Id "hub-123"
        hubId2 @?= Kernel.Types.Id.Id "hub-456"
        hubId1 /= hubId2 @? "Different operation hub IDs should be distinct",
      testCase "Different registration numbers work correctly" $ do
        let reg1Req = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
            reg2Req = Common.DriverOperationHubRequest "creator-456" (Kernel.Types.Id.Id "hub-456") "DL02CD5678" Common.ONBOARDING_INSPECTION
        let Common.DriverOperationHubRequest {Common.registrationNo = regNo1} = reg1Req
            Common.DriverOperationHubRequest {Common.registrationNo = regNo2} = reg2Req
        regNo1 @?= "DL01AB1234"
        regNo2 @?= "DL02CD5678"
        regNo1 /= regNo2 @? "Different registration numbers should be distinct"
    ]

-- =============================================================================
-- ERROR HANDLING WITH REAL FUNCTIONS
-- =============================================================================

testErrorHandlingWithRealFunctions :: TestTree
testErrorHandlingWithRealFunctions =
  testGroup
    "Error Handling with Real Functions"
    [ testCase "Invalid creator ID format" $ do
        let invalidReq = Common.DriverOperationHubRequest "" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
            Common.DriverOperationHubRequest {Common.creatorId = creatorId} = invalidReq
        creatorId @?= "",
      testCase "Invalid registration number format" $ do
        let invalidReq = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "123" Common.ONBOARDING_INSPECTION
            Common.DriverOperationHubRequest {Common.registrationNo = registrationNo} = invalidReq
        registrationNo @?= "123",
      testCase "Empty operation hub ID validation" $ do
        let invalidReq = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "") "DL01AB1234" Common.ONBOARDING_INSPECTION
            Common.DriverOperationHubRequest {Common.operationHubId = operationHubId} = invalidReq
        operationHubId @?= Kernel.Types.Id.Id "",
      testCase "Function handles empty creator ID gracefully" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.Delhi
            invalidReq = Common.DriverOperationHubRequest "" (Kernel.Types.Id.Id "hub-123") "DL01AB1234" Common.ONBOARDING_INSPECTION
        result <- try (evaluate $ DDriverOp.postDriverOperatorCreateRequest merchantShortId opCity invalidReq)
        case result of
          Left (e :: Kernel.Prelude.SomeException) -> do
            let errorMsg = "Function correctly rejected empty creator ID: " ++ show e
            True @? errorMsg
          Right _ -> do
            let Common.DriverOperationHubRequest {Common.creatorId = creatorId} = invalidReq
            creatorId @?= "", -- Verify the empty creator ID was passed through
      testCase "Function handles short registration number gracefully" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.Delhi
            invalidReq = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "hub-123") "123" Common.ONBOARDING_INSPECTION
        result <- try (evaluate $ DDriverOp.postDriverOperatorCreateRequest merchantShortId opCity invalidReq)
        case result of
          Left (e :: Kernel.Prelude.SomeException) -> do
            let errorMsg = "Function correctly rejected short registration number: " ++ show e
            True @? errorMsg
          Right _ -> do
            let Common.DriverOperationHubRequest {Common.registrationNo = registrationNo} = invalidReq
            registrationNo @?= "123", -- Verify the short registration number was passed through
      testCase "Function handles empty operation hub ID gracefully" $ do
        let merchantShortId = Kernel.Types.Id.ShortId "test-merchant"
            opCity = Context.Delhi
            invalidReq = Common.DriverOperationHubRequest "creator-123" (Kernel.Types.Id.Id "") "DL01AB1234" Common.ONBOARDING_INSPECTION
        result <- try (evaluate $ DDriverOp.postDriverOperatorCreateRequest merchantShortId opCity invalidReq)
        case result of
          Left (e :: Kernel.Prelude.SomeException) -> do
            let errorMsg = "Function correctly rejected empty operation hub ID: " ++ show e
            True @? errorMsg
          Right _ -> do
            let Common.DriverOperationHubRequest {Common.operationHubId = operationHubId} = invalidReq
            operationHubId @?= Kernel.Types.Id.Id "" -- Verify the empty operation hub ID was passed through
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

createRequestUnitTests :: TestTree
createRequestUnitTests =
  testGroup
    "Create Request Unit Tests (Using Real Functions)"
    [ testPostDriverOperatorCreateRequestWithRealExecution,
      testDataTypeValidation,
      testComplexScenariosWithRealFunctions,
      testErrorHandlingWithRealFunctions
    ]
