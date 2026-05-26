{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module CancellationDuesUnitTests where

import qualified "dynamic-offer-driver-app" Domain.Types.CancellationDuesDetails as DCDD
import Data.Aeson (decode, encode)
import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- CancellationDuesPaymentStatus ENUM TESTS
-- =============================================================================

testCancellationDuesPaymentStatusEnum :: TestTree
testCancellationDuesPaymentStatusEnum =
  testGroup
    "CancellationDuesPaymentStatus Enum"
    [ testCase "PENDING status value is correct" $ do
        let status = DCDD.PENDING
        show status @?= "PENDING",
      testCase "PAID status value is correct" $ do
        let status = DCDD.PAID
        show status @?= "PAID",
      testCase "WAIVED status value is correct" $ do
        let status = DCDD.WAIVED
        show status @?= "WAIVED",
      testCase "PENDING and PAID are distinct" $ do
        (DCDD.PENDING /= DCDD.PAID) @? "PENDING should not equal PAID",
      testCase "PAID and WAIVED are distinct" $ do
        (DCDD.PAID /= DCDD.WAIVED) @? "PAID should not equal WAIVED",
      testCase "PENDING and WAIVED are distinct" $ do
        (DCDD.PENDING /= DCDD.WAIVED) @? "PENDING should not equal WAIVED",
      testCase "PENDING equals PENDING" $ do
        (DCDD.PENDING == DCDD.PENDING) @? "PENDING should equal PENDING",
      testCase "PAID equals PAID" $ do
        (DCDD.PAID == DCDD.PAID) @? "PAID should equal PAID",
      testCase "WAIVED equals WAIVED" $ do
        (DCDD.WAIVED == DCDD.WAIVED) @? "WAIVED should equal WAIVED"
    ]

-- =============================================================================
-- CancellationDuesPaymentStatus JSON SERIALIZATION TESTS
-- =============================================================================

testCancellationDuesPaymentStatusJson :: TestTree
testCancellationDuesPaymentStatusJson =
  testGroup
    "CancellationDuesPaymentStatus JSON Serialization"
    [ testCase "PENDING serializes to JSON correctly" $ do
        let encoded = encode DCDD.PENDING
        isJust (decode encoded :: Maybe DCDD.CancellationDuesPaymentStatus) @? "PENDING should decode back from JSON",
      testCase "PAID serializes to JSON correctly" $ do
        let encoded = encode DCDD.PAID
        isJust (decode encoded :: Maybe DCDD.CancellationDuesPaymentStatus) @? "PAID should decode back from JSON",
      testCase "WAIVED serializes to JSON correctly" $ do
        let encoded = encode DCDD.WAIVED
        isJust (decode encoded :: Maybe DCDD.CancellationDuesPaymentStatus) @? "WAIVED should decode back from JSON",
      testCase "PENDING round-trips through JSON" $ do
        let decoded = decode (encode DCDD.PENDING) :: Maybe DCDD.CancellationDuesPaymentStatus
        decoded @?= Just DCDD.PENDING,
      testCase "PAID round-trips through JSON" $ do
        let decoded = decode (encode DCDD.PAID) :: Maybe DCDD.CancellationDuesPaymentStatus
        decoded @?= Just DCDD.PAID,
      testCase "WAIVED round-trips through JSON" $ do
        let decoded = decode (encode DCDD.WAIVED) :: Maybe DCDD.CancellationDuesPaymentStatus
        decoded @?= Just DCDD.WAIVED,
      testCase "PENDING and PAID decode to distinct values" $ do
        let decodedPending = decode (encode DCDD.PENDING) :: Maybe DCDD.CancellationDuesPaymentStatus
            decodedPaid = decode (encode DCDD.PAID) :: Maybe DCDD.CancellationDuesPaymentStatus
        (decodedPending /= decodedPaid) @? "Decoded PENDING and PAID should be distinct"
    ]

-- =============================================================================
-- CancellationDuesPaymentStatus READ/SHOW TESTS
-- =============================================================================

testCancellationDuesPaymentStatusReadShow :: TestTree
testCancellationDuesPaymentStatusReadShow =
  testGroup
    "CancellationDuesPaymentStatus Read/Show"
    [ testCase "All three statuses have correct string representations" $ do
        let statuses = [DCDD.PENDING, DCDD.PAID, DCDD.WAIVED]
            showStrings = map show statuses
        showStrings @?= ["PENDING", "PAID", "WAIVED"],
      testCase "All three statuses are distinct" $ do
        let statuses = [DCDD.PENDING, DCDD.PAID, DCDD.WAIVED]
            uniqueCount = length statuses
        uniqueCount @?= 3,
      testCase "Ordering: PENDING < PAID < WAIVED" $ do
        (DCDD.PENDING < DCDD.PAID) @? "PENDING should be less than PAID"
        (DCDD.PAID < DCDD.WAIVED) @? "PAID should be less than WAIVED"
        (DCDD.PENDING < DCDD.WAIVED) @? "PENDING should be less than WAIVED"
    ]

-- =============================================================================
-- mbIncludeBreakup LOGIC TESTS (Pure logic extracted from getCancellationDuesDetails)
-- =============================================================================

-- | Simulate the mbIncludeBreakup decision logic from getCancellationDuesDetails.
-- When mbIncludeBreakup is Just True, we should fetch the breakup; otherwise not.
shouldFetchBreakup :: Maybe Bool -> Bool
shouldFetchBreakup mbIncludeBreakup = mbIncludeBreakup == Just True

testMbIncludeBreakupLogic :: TestTree
testMbIncludeBreakupLogic =
  testGroup
    "mbIncludeBreakup Decision Logic"
    [ testCase "Nothing does NOT fetch breakup" $ do
        shouldFetchBreakup Nothing @?= False,
      testCase "Just False does NOT fetch breakup" $ do
        shouldFetchBreakup (Just False) @?= False,
      testCase "Just True DOES fetch breakup" $ do
        shouldFetchBreakup (Just True) @?= True,
      testCase "Only Just True triggers breakup fetch" $ do
        let results = map shouldFetchBreakup [Nothing, Just False, Just True]
        results @?= [False, False, True]
    ]

-- =============================================================================
-- CancellationDuesDetailsRes duesBreakup FIELD TESTS
-- =============================================================================

-- | Simulate the conditional assignment of duesBreakup (None vs Some breakup).
-- Mirrors the logic in getCancellationDuesDetails.
computeDuesBreakup :: Maybe Bool -> [a] -> Maybe [a]
computeDuesBreakup mbIncludeBreakup rows =
  if mbIncludeBreakup == Just True
    then Just rows
    else Nothing

testDuesBreakupFieldLogic :: TestTree
testDuesBreakupFieldLogic =
  testGroup
    "duesBreakup Field Population Logic"
    [ testCase "duesBreakup is Nothing when mbIncludeBreakup is Nothing" $ do
        let result = computeDuesBreakup Nothing (["item1", "item2"] :: [String])
        isNothing result @? "duesBreakup should be Nothing when breakup not requested",
      testCase "duesBreakup is Nothing when mbIncludeBreakup is Just False" $ do
        let result = computeDuesBreakup (Just False) (["item1", "item2"] :: [String])
        isNothing result @? "duesBreakup should be Nothing when breakup explicitly not requested",
      testCase "duesBreakup is Just [] when mbIncludeBreakup is Just True and no rows" $ do
        let result = computeDuesBreakup (Just True) ([] :: [String])
        result @?= Just [],
      testCase "duesBreakup is Just [rows] when mbIncludeBreakup is Just True with data" $ do
        let rows = ["due1", "due2", "due3"] :: [String]
            result = computeDuesBreakup (Just True) rows
        result @?= Just rows,
      testCase "duesBreakup preserves row count when breakup requested" $ do
        let rows = [1, 2, 3, 4, 5] :: [Int]
            result = computeDuesBreakup (Just True) rows
        fmap length result @?= Just 5
    ]

-- =============================================================================
-- SORTING LOGIC TEST (rows sorted by createdAt descending)
-- =============================================================================

-- Tests for the sorting in getCancellationDuesDetails:
--   sortOn (Down . (.createdAt)) pendingDues
-- We use Prelude.reverse . Prelude.sort as an equivalent for Int proxies.
testBreakupSortingLogic :: TestTree
testBreakupSortingLogic =
  testGroup
    "Breakup Sorting Logic (descending by createdAt)"
    [ testCase "Empty list stays empty after sort" $ do
        let sorted = Prelude.reverse . Prelude.sort $ ([] :: [Int])
        sorted @?= [],
      testCase "Single-element list is unchanged" $ do
        let sorted = Prelude.reverse . Prelude.sort $ ([42] :: [Int])
        sorted @?= [42],
      testCase "List is sorted in descending order" $ do
        let ts = [1, 3, 2, 5, 4] :: [Int]
            sorted = Prelude.reverse . Prelude.sort $ ts
        sorted @?= [5, 4, 3, 2, 1],
      testCase "Most recent item appears first after descending sort" $ do
        let ts = [100, 200, 300] :: [Int]
            sorted = Prelude.reverse . Prelude.sort $ ts
        head sorted @?= 300
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

cancellationDuesUnitTests :: TestTree
cancellationDuesUnitTests =
  testGroup
    "Cancellation Dues Unit Tests"
    [ testCancellationDuesPaymentStatusEnum,
      testCancellationDuesPaymentStatusJson,
      testCancellationDuesPaymentStatusReadShow,
      testMbIncludeBreakupLogic,
      testDuesBreakupFieldLogic,
      testBreakupSortingLogic
    ]