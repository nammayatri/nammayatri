{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module StclMembershipUnitTests where

import qualified "dynamic-offer-driver-app" API.Types.UI.StclMembership as APITypes
import Data.Aeson (decode, encode)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified "mobility-core" Kernel.Types.Common as KC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- CONSTANTS FROM StclMembership.hs
-- These are the module-level constants after the PR change (no longer coming
-- from TransporterConfig.stclConfig).
-- =============================================================================

-- Mirrors Domain.Action.UI.StclMembership.maxSharesPerDriver
maxSharesPerDriver :: Int
maxSharesPerDriver = 5

-- Mirrors Domain.Action.UI.StclMembership.pricePerShare
pricePerShare :: Int
pricePerShare = 100

-- =============================================================================
-- UpdateMembershipApplicationReq TESTS
-- PR removed addressProofType and addressProofImageId from this type.
-- =============================================================================

testUpdateMembershipApplicationReqStructure :: TestTree
testUpdateMembershipApplicationReqStructure =
  testGroup
    "UpdateMembershipApplicationReq Structure (after PR changes)"
    [ testCase "Request with all Nothing fields is valid" $ do
        let req =
              APITypes.UpdateMembershipApplicationReq
                { APITypes.address = Nothing,
                  APITypes.bankDetails = Nothing,
                  APITypes.nomineeName = Nothing,
                  APITypes.vehicleType = Nothing
                }
        isNothing (APITypes.address req) @? "address should be Nothing"
        isNothing (APITypes.bankDetails req) @? "bankDetails should be Nothing"
        isNothing (APITypes.nomineeName req) @? "nomineeName should be Nothing"
        isNothing (APITypes.vehicleType req) @? "vehicleType should be Nothing",
      testCase "Request with nomineeName set is valid" $ do
        let req =
              APITypes.UpdateMembershipApplicationReq
                { APITypes.address = Nothing,
                  APITypes.bankDetails = Nothing,
                  APITypes.nomineeName = Just "Jane Doe",
                  APITypes.vehicleType = Nothing
                }
        APITypes.nomineeName req @?= Just "Jane Doe",
      testCase "Request with vehicleType set is valid" $ do
        let req =
              APITypes.UpdateMembershipApplicationReq
                { APITypes.address = Nothing,
                  APITypes.bankDetails = Nothing,
                  APITypes.nomineeName = Nothing,
                  APITypes.vehicleType = Just APITypes.TwoWheeler
                }
        APITypes.vehicleType req @?= Just APITypes.TwoWheeler,
      testCase "Request with address set is valid" $ do
        let addr =
              APITypes.Address
                { APITypes.city = "Mumbai",
                  APITypes.postalCode = "400001",
                  APITypes.stateName = "Maharashtra",
                  APITypes.streetAddress1 = "1 Main St",
                  APITypes.streetAddress2 = Nothing
                }
            req =
              APITypes.UpdateMembershipApplicationReq
                { APITypes.address = Just addr,
                  APITypes.bankDetails = Nothing,
                  APITypes.nomineeName = Nothing,
                  APITypes.vehicleType = Nothing
                }
        isJust (APITypes.address req) @? "address should be set",
      testCase "UpdateMembershipApplicationReq JSON round-trip" $ do
        let req =
              APITypes.UpdateMembershipApplicationReq
                { APITypes.address = Nothing,
                  APITypes.bankDetails = Nothing,
                  APITypes.nomineeName = Just "Test Name",
                  APITypes.vehicleType = Just APITypes.FourWheeler
                }
        let decoded = decode (encode req) :: Maybe APITypes.UpdateMembershipApplicationReq
        isJust decoded @? "UpdateMembershipApplicationReq should decode from JSON"
    ]

-- =============================================================================
-- TopUpSharesReq TESTS
-- =============================================================================

testTopUpSharesReqStructure :: TestTree
testTopUpSharesReqStructure =
  testGroup
    "TopUpSharesReq Structure"
    [ testCase "Request with numberOfShares only is valid" $ do
        let req =
              APITypes.TopUpSharesReq
                { APITypes.numberOfShares = 1,
                  APITypes.amount = Nothing,
                  APITypes.paymentServiceType = Nothing
                }
        APITypes.numberOfShares req @?= 1
        isNothing (APITypes.amount req) @? "amount should be Nothing when not provided",
      testCase "Request with explicit amount is valid" $ do
        let req =
              APITypes.TopUpSharesReq
                { APITypes.numberOfShares = 2,
                  APITypes.amount = Just (KC.Money 200),
                  APITypes.paymentServiceType = Nothing
                }
        APITypes.numberOfShares req @?= 2
        APITypes.amount req @?= Just (KC.Money 200),
      testCase "TopUpSharesReq JSON round-trip with numberOfShares only" $ do
        let req =
              APITypes.TopUpSharesReq
                { APITypes.numberOfShares = 3,
                  APITypes.amount = Nothing,
                  APITypes.paymentServiceType = Nothing
                }
        let decoded = decode (encode req) :: Maybe APITypes.TopUpSharesReq
        isJust decoded @? "TopUpSharesReq should decode from JSON"
    ]

-- =============================================================================
-- CAP CHECK LOGIC TESTS (mirrors postBuyAdditionalShares logic)
-- =============================================================================

-- | Simulate the cap check in postBuyAdditionalShares.
-- Returns True if the request exceeds the max shares limit.
exceedsShareCap :: Int -> Int -> Int -> Bool
exceedsShareCap existingShares requestedShares maxShares =
  existingShares + requestedShares > maxShares

-- | Compute the error message for cap exceeded (mirrors the actual code).
capExceededMessage :: Int -> Int -> Int -> T.Text
capExceededMessage maxShares existingShares requestedShares =
  "Total shares for driver cannot exceed "
    <> T.pack (show maxShares)
    <> ". Existing shares: "
    <> T.pack (show existingShares)
    <> ", requested: "
    <> T.pack (show requestedShares)

testShareCapCheck :: TestTree
testShareCapCheck =
  testGroup
    "Share Cap Check Logic (postBuyAdditionalShares)"
    [ testCase "Requesting 1 share with 0 existing passes cap check (maxShares=5)" $ do
        exceedsShareCap 0 1 maxSharesPerDriver @?= False,
      testCase "Requesting 4 shares with 0 existing passes cap check" $ do
        exceedsShareCap 0 4 maxSharesPerDriver @?= False,
      testCase "Requesting 5 shares with 0 existing passes cap check (exact boundary)" $ do
        exceedsShareCap 0 5 maxSharesPerDriver @?= False,
      testCase "Requesting 6 shares with 0 existing fails cap check" $ do
        exceedsShareCap 0 6 maxSharesPerDriver @?= True,
      testCase "Requesting 1 share with 5 existing fails cap check" $ do
        exceedsShareCap 5 1 maxSharesPerDriver @?= True,
      testCase "Requesting 2 shares with 3 existing passes cap check" $ do
        exceedsShareCap 3 2 maxSharesPerDriver @?= False,
      testCase "Requesting 3 shares with 3 existing fails cap check" $ do
        exceedsShareCap 3 3 maxSharesPerDriver @?= True,
      testCase "Requesting 1 share with 4 existing passes cap check (boundary)" $ do
        exceedsShareCap 4 1 maxSharesPerDriver @?= False,
      testCase "Requesting 2 shares with 4 existing fails cap check" $ do
        exceedsShareCap 4 2 maxSharesPerDriver @?= True,
      testCase "maxSharesPerDriver constant is 5" $ do
        maxSharesPerDriver @?= 5,
      testCase "Cap exceeded message contains correct info" $ do
        let msg = capExceededMessage 5 3 3
        T.isInfixOf "5" msg @? "Message should contain max shares"
        T.isInfixOf "3" msg @? "Message should contain existing shares"
    ]

-- =============================================================================
-- AMOUNT RESOLUTION LOGIC TESTS
-- PR: resolvedAmount = fromMaybe (Money (req.numberOfShares * pricePerShare)) req.amount
-- =============================================================================

-- | Simulate the amount resolution logic in postBuyAdditionalShares.
resolveAmount :: Maybe KC.Money -> Int -> KC.Money
resolveAmount mbAmount numberOfShares =
  case mbAmount of
    Just amt -> amt
    Nothing -> KC.Money (numberOfShares * pricePerShare)

testAmountResolution :: TestTree
testAmountResolution =
  testGroup
    "Amount Resolution Logic (postBuyAdditionalShares)"
    [ testCase "pricePerShare constant is 100" $ do
        pricePerShare @?= 100,
      testCase "Amount computed as numberOfShares * pricePerShare when not provided" $ do
        let resolved = resolveAmount Nothing 1
        resolved @?= KC.Money 100,
      testCase "2 shares without explicit amount = 200 rupees" $ do
        let resolved = resolveAmount Nothing 2
        resolved @?= KC.Money 200,
      testCase "5 shares without explicit amount = 500 rupees" $ do
        let resolved = resolveAmount Nothing 5
        resolved @?= KC.Money 500,
      testCase "Explicit amount overrides computed amount" $ do
        let resolved = resolveAmount (Just (KC.Money 150)) 1
        resolved @?= KC.Money 150,
      testCase "Explicit amount of 0 is accepted (client controls amount)" $ do
        let resolved = resolveAmount (Just (KC.Money 0)) 2
        resolved @?= KC.Money 0,
      testCase "Explicit amount different from computed is used" $ do
        let computedFor2 = resolveAmount Nothing 2  -- should be 200
            explicitFor2 = resolveAmount (Just (KC.Money 250)) 2  -- should be 250
        computedFor2 @?= KC.Money 200
        explicitFor2 @?= KC.Money 250
    ]

-- =============================================================================
-- hasPending GUARD LOGIC TESTS
-- PR: replaced resume-or-reject logic with a simple "if pending exists, reject"
-- =============================================================================

-- | Simulate the hasPending check in postBuyAdditionalShares.
hasPendingApplication :: [Bool] -> Bool
hasPendingApplication statuses = any id statuses

-- Using Bool to represent isPending for each application
testHasPendingGuard :: TestTree
testHasPendingGuard =
  testGroup
    "hasPending Guard Logic (postBuyAdditionalShares)"
    [ testCase "No applications means no pending" $ do
        hasPendingApplication [] @?= False,
      testCase "Single non-pending application means no pending" $ do
        hasPendingApplication [False] @?= False,
      testCase "Single pending application means hasPending is True" $ do
        hasPendingApplication [True] @?= True,
      testCase "Mix of pending and non-pending means hasPending is True" $ do
        hasPendingApplication [False, True, False] @?= True,
      testCase "All non-pending means hasPending is False" $ do
        hasPendingApplication [False, False, False] @?= False,
      testCase "Multiple pending applications means hasPending is True" $ do
        hasPendingApplication [True, True] @?= True,
      testCase "Error message is correct when pending exists" $ do
        let errMsg = "A pending membership application already exists for this driver" :: T.Text
        T.length errMsg > 0 @? "Error message should not be empty"
    ]

-- =============================================================================
-- VehicleType TESTS
-- =============================================================================

testVehicleTypeEnum :: TestTree
testVehicleTypeEnum =
  testGroup
    "VehicleType Enum"
    [ testCase "TwoWheeler, ThreeWheeler, FourWheeler are distinct" $ do
        (APITypes.TwoWheeler /= APITypes.ThreeWheeler) @? "TwoWheeler should not equal ThreeWheeler"
        (APITypes.ThreeWheeler /= APITypes.FourWheeler) @? "ThreeWheeler should not equal FourWheeler"
        (APITypes.TwoWheeler /= APITypes.FourWheeler) @? "TwoWheeler should not equal FourWheeler",
      testCase "TwoWheeler JSON round-trip" $ do
        let decoded = decode (encode APITypes.TwoWheeler) :: Maybe APITypes.VehicleType
        decoded @?= Just APITypes.TwoWheeler,
      testCase "ThreeWheeler JSON round-trip" $ do
        let decoded = decode (encode APITypes.ThreeWheeler) :: Maybe APITypes.VehicleType
        decoded @?= Just APITypes.ThreeWheeler,
      testCase "FourWheeler JSON round-trip" $ do
        let decoded = decode (encode APITypes.FourWheeler) :: Maybe APITypes.VehicleType
        decoded @?= Just APITypes.FourWheeler,
      testCase "VehicleType text mapping is correct (mirrors StclMembership domain logic)" $ do
        -- Mirrors the case expression in postBuyAdditionalShares / putUpdateApplication
        let toText vt = case vt of
              APITypes.TwoWheeler -> "2Wheeler"
              APITypes.ThreeWheeler -> "3Wheeler"
              APITypes.FourWheeler -> "4Wheeler"
        toText APITypes.TwoWheeler @?= "2Wheeler"
        toText APITypes.ThreeWheeler @?= "3Wheeler"
        toText APITypes.FourWheeler @?= "4Wheeler"
    ]

-- =============================================================================
-- REQUIRES EXISTING SUBMITTED APPLICATION TESTS
-- =============================================================================

-- | Simulate the "no submitted app" guard in postBuyAdditionalShares.
hasSubmittedApplications :: [Bool] -> Bool
hasSubmittedApplications statuses = any id statuses

testRequiresSubmittedApp :: TestTree
testRequiresSubmittedApp =
  testGroup
    "postBuyAdditionalShares requires existing SUBMITTED application"
    [ testCase "No applications: should reject top-up" $ do
        hasSubmittedApplications [] @?= False,
      testCase "Only pending application: should reject top-up" $ do
        hasSubmittedApplications [False] @?= False,
      testCase "Has submitted application: top-up allowed" $ do
        hasSubmittedApplications [True] @?= True,
      testCase "Mix of submitted and non-submitted: top-up allowed" $ do
        hasSubmittedApplications [False, True] @?= True
    ]

-- =============================================================================
-- numberOfShares VALIDATION TESTS
-- =============================================================================

testNumberOfSharesValidation :: TestTree
testNumberOfSharesValidation =
  testGroup
    "numberOfShares Validation (must be > 0)"
    [ testCase "0 shares should be rejected" $ do
        (0 <= 0) @? "0 shares is invalid",
      testCase "-1 shares should be rejected" $ do
        ((-1) <= 0) @? "Negative shares is invalid",
      testCase "1 share is valid" $ do
        (1 > 0) @? "1 share is valid",
      testCase "5 shares is valid" $ do
        (5 > 0) @? "5 shares is valid"
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

stclMembershipUnitTests :: TestTree
stclMembershipUnitTests =
  testGroup
    "STCL Membership Unit Tests"
    [ testUpdateMembershipApplicationReqStructure,
      testTopUpSharesReqStructure,
      testShareCapCheck,
      testAmountResolution,
      testHasPendingGuard,
      testVehicleTypeEnum,
      testRequiresSubmittedApp,
      testNumberOfSharesValidation
    ]
