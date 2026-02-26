module PPF.ReconTest (reconTests) where

import qualified Beckn.ACL.Recon as ACLRecon
import Data.Time (UTCTime (..), fromGregorian)
import qualified Data.List as L
import qualified Domain.Action.PPF.Recon as DRecon
import Domain.Types.PPFRecon
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), mkPrice)
import Kernel.Types.Id (Id (..))
import Test.Tasty
import Test.Tasty.HUnit

reconTests :: TestTree
reconTests =
  testGroup
    "PPF Recon Logic"
    [ groupByTests,
      wireFormatTests
    ]

-- ---------------------------------------------------------------------------
-- groupByReceiverSubscriberId tests
-- ---------------------------------------------------------------------------

groupByTests :: TestTree
groupByTests =
  testGroup
    "groupByReceiverSubscriberId"
    [ testCase "Empty list returns empty result" $ do
        let result = DRecon.groupByReceiverSubscriberId []
        assertEqual "Should return empty list" 0 (length result),
      testCase "Single entry returns one group" $ do
        let entry = mkTestRecon "order-1" "bpp-1"
            result = DRecon.groupByReceiverSubscriberId [entry]
        assertEqual "Should have one group" 1 (length result),
      testCase "Multiple entries same receiver form one group" $ do
        let entries =
              [ mkTestRecon "order-1" "bpp-1",
                mkTestRecon "order-2" "bpp-1",
                mkTestRecon "order-3" "bpp-1"
              ]
            result = DRecon.groupByReceiverSubscriberId entries
        assertEqual "Should have one group" 1 (length result)
        case result of
          [(receiverId, grouped)] -> do
            assertEqual "Receiver ID" "bpp-1" receiverId
            assertEqual "Group size" 3 (length grouped)
          _ -> assertFailure "Expected exactly one group",
      testCase "Different receivers form separate groups" $ do
        let entries =
              [ mkTestRecon "order-1" "bpp-1",
                mkTestRecon "order-2" "bpp-2",
                mkTestRecon "order-3" "bpp-1"
              ]
            result = DRecon.groupByReceiverSubscriberId entries
        assertEqual "Should have two groups" 2 (length result)
        let grouped = L.sortOn fst result
        case grouped of
          [(r1, g1), (r2, g2)] -> do
            assertEqual "First receiver" "bpp-1" r1
            assertEqual "First group size" 2 (length g1)
            assertEqual "Second receiver" "bpp-2" r2
            assertEqual "Second group size" 1 (length g2)
          _ -> assertFailure "Expected exactly two groups",
      testCase "Preserves all entries (no data loss)" $ do
        let entries =
              [ mkTestRecon "order-1" "bpp-1",
                mkTestRecon "order-2" "bpp-2",
                mkTestRecon "order-3" "bpp-3",
                mkTestRecon "order-4" "bpp-1",
                mkTestRecon "order-5" "bpp-2"
              ]
            result = DRecon.groupByReceiverSubscriberId entries
            totalEntries = sum $ map (length . snd) result
        assertEqual "Total entries preserved" 5 totalEntries
        assertEqual "Should have three groups" 3 (length result)
    ]

-- ---------------------------------------------------------------------------
-- settlementStatusToWire tests
-- ---------------------------------------------------------------------------

wireFormatTests :: TestTree
wireFormatTests =
  testGroup
    "settlementStatusToWire (ONDC wire format)"
    [ testCase "SETTLEMENT_SETTLED -> SETTLED" $
        assertEqual "Wire format" "SETTLED" (ACLRecon.settlementStatusToWire SETTLEMENT_SETTLED),
      testCase "SETTLEMENT_FAILED -> FAILED" $
        assertEqual "Wire format" "FAILED" (ACLRecon.settlementStatusToWire SETTLEMENT_FAILED),
      testCase "PENDING -> PENDING (unchanged)" $
        assertEqual "Wire format" "PENDING" (ACLRecon.settlementStatusToWire PENDING),
      testCase "IN_PROGRESS -> IN_PROGRESS (unchanged)" $
        assertEqual "Wire format" "IN_PROGRESS" (ACLRecon.settlementStatusToWire IN_PROGRESS)
    ]

-- ---------------------------------------------------------------------------
-- Test data helpers
-- ---------------------------------------------------------------------------

-- | Create a minimal PPFRecon for testing groupByReceiverSubscriberId.
-- Only the fields accessed by that function (receiverSubscriberId) matter.
mkTestRecon :: Text -> Text -> PPFRecon
mkTestRecon orderId receiverId =
  let testPrice = mkPrice (Just INR) 100
      testTime = UTCTime (fromGregorian 2026 1 1) 0
   in PPFRecon
        { id = Id "test-recon-id",
          domain = MOBILITY,
          networkOrderId = orderId,
          transactionId = "test-txn-id",
          collectorSubscriberId = "test-collector",
          receiverSubscriberId = receiverId,
          paymentTransactionId = Nothing,
          paymentReference = Nothing,
          orderAmount = testPrice,
          paymentAmount = testPrice,
          sellerShare = testPrice,
          buyerAppCommission = testPrice,
          networkFee = Nothing,
          gstAmount = Nothing,
          withholdingAmount = Nothing,
          tds = Nothing,
          tcs = Nothing,
          orderStatus = "ACTIVE",
          paymentStatus = INITIATED,
          settlementStatus = PENDING,
          settlementRefNo = Nothing,
          settlementAmount = Nothing,
          settlementDate = Nothing,
          fulfilledTimestamp = Nothing,
          settledTimestamp = Nothing,
          entityId = orderId,
          entityType = RIDE_BOOKING,
          collectorIFSC = Nothing,
          collectorBankAccount = Nothing,
          beneficiaryIFSC = Nothing,
          beneficiaryBankAccount = Nothing,
          reconInitiatedAt = Nothing,
          reconCompletedAt = Nothing,
          differenceAmount = Nothing,
          message = Nothing,
          merchantId = Nothing,
          merchantOperatingCityId = Nothing,
          createdAt = testTime,
          updatedAt = testTime
        }
