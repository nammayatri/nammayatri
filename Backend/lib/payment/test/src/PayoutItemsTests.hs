module PayoutItemsTests (payoutItemsTests) where

import qualified Data.Time as Time
import Kernel.External.Encryption (DbHash (..), Encrypted (..), EncryptedHashed (..))
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Juspay
import Kernel.Prelude
import Kernel.Types.Common (mkPrice)
import Kernel.Types.Id (Id (..), ShortId (..))
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPO
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.PayoutItems (toPayoutItem)
import Test.Tasty
import Test.Tasty.HUnit

payoutItemsTests :: TestTree
payoutItemsTests =
  testGroup
    "Payout history items (Lib.Payment.Payout.PayoutItems)"
    [ testCase "transaction id is the order's short id" $ do
        let item = toPayoutItem paidRequest (Just (mkOrder (Just "iS3zcqCzdp") Nothing Nothing))
        item.transactionId @?= Just "iS3zcqCzdp",
      testCase "transaction id falls back to the order id" $ do
        let item = toPayoutItem paidRequest (Just (mkOrder Nothing Nothing Nothing))
        item.transactionId @?= Just "order-1",
      testCase "no order: no transaction id or reference" $ do
        let item = toPayoutItem paidRequest Nothing
        item.transactionId @?= Nothing
        item.bankReference @?= Nothing
        item.bankReferenceType @?= Nothing,
      testCase "a paid order's reference and its type" $ do
        let item = toPayoutItem paidRequest (Just (mkOrder Nothing (Just "HDFCN52033069283") (Just Payout.UTR)))
        item.bankReference @?= Just "HDFCN52033069283"
        item.bankReferenceType @?= Just "UTR",
      testCase "an empty reference is no reference, and its type is dropped" $ do
        let item = toPayoutItem paidRequest (Just (mkOrder Nothing (Just "") (Just Payout.UTR)))
        item.bankReference @?= Nothing
        item.bankReferenceType @?= Nothing,
      testCase "status updated at is the request's last update" $ do
        let item = toPayoutItem paidRequest Nothing
        item.statusUpdatedAt @?= finishedAt,
      testCase "request fields pass through unchanged" $ do
        let item = toPayoutItem paidRequest Nothing
        item.payoutType @?= Just PR.ADHOC
        item.bankName @?= Just "SBIN"
        item.bankAccountLast4 @?= Just "0001"
        item.timestamp @?= createdAt,
      testCase "payout method: UPI with a VPA, CASH when settled in cash, else BANK" $ do
        let upi = toPayoutItem (mkRequest PR.CREDITED (Just "driver@upi")) Nothing
            cash = toPayoutItem (mkRequest PR.CASH_PAID Nothing) Nothing
            bank = toPayoutItem paidRequest Nothing
        upi.payoutMethod @?= "UPI"
        cash.payoutMethod @?= "CASH"
        bank.payoutMethod @?= "BANK"
    ]

createdAt :: UTCTime
createdAt = Time.UTCTime (Time.fromGregorian 2026 10 18) (Time.secondsToDiffTime (9 * 3600))

finishedAt :: UTCTime
finishedAt = Time.addUTCTime 1260 createdAt

-- A credited bulk (HDFC) payout.
paidRequest :: PR.PayoutRequest
paidRequest = mkRequest PR.CREDITED Nothing

mkRequest :: PR.PayoutRequestStatus -> Maybe Text -> PR.PayoutRequest
mkRequest status customerVpa =
  PR.PayoutRequest
    { amount = Just 500,
      bankAccountLast4 = Just "0001",
      bankName = Just "SBIN",
      batchId = Nothing,
      beneficiaryId = "driver-1",
      cashMarkedAt = Nothing,
      cashMarkedById = Nothing,
      cashMarkedByName = Nothing,
      city = Nothing,
      coverageFrom = Nothing,
      coverageTo = Nothing,
      createdAt,
      customerEmail = Nothing,
      customerName = Nothing,
      customerPhone = Nothing,
      customerVpa,
      entityId = "driver-1",
      entityName = Nothing,
      entityRefId = Nothing,
      expectedCreditTime = Nothing,
      failureReason = Nothing,
      id = Id "request-1",
      ledgerEntryIds = Nothing,
      merchantId = "merchant-1",
      merchantOperatingCityId = "city-1",
      orderType = Nothing,
      payoutFee = Nothing,
      payoutTransactionId = Just "order-1",
      payoutType = Just PR.ADHOC,
      remark = Nothing,
      retryCount = Nothing,
      scheduledAt = Nothing,
      status,
      updatedAt = finishedAt
    }

mkOrder :: Maybe Text -> Maybe Text -> Maybe Payout.SettlementRefType -> DPO.PayoutOrder
mkOrder mbShortId settlementRef settlementRefType =
  DPO.PayoutOrder
    { accountDetailsType = Nothing,
      amount = mkPrice Nothing 500,
      batchId = Nothing,
      city = "Hyderabad",
      createdAt,
      customerEmail = encrypted,
      customerId = "driver-1",
      entityIds = Nothing,
      entityName = Nothing,
      id = Id "order-1",
      idAssignedByServiceProvider = Nothing,
      lastStatusCheckedAt = Nothing,
      merchantId = "merchant-1",
      merchantOperatingCityId = Nothing,
      merchantTopUpAmount = Nothing,
      mobileNo = encrypted,
      orderId = "order-1",
      pgBaseFee = Nothing,
      pgGst = Nothing,
      responseCode = Nothing,
      responseMessage = Nothing,
      retriedOrderId = Nothing,
      settlementRef,
      settlementRefType,
      shortId = ShortId <$> mbShortId,
      status = Juspay.FULFILLMENTS_SUCCESSFUL,
      transferAmount = Nothing,
      transferId = Nothing,
      transferStatus = Nothing,
      updatedAt = finishedAt,
      vpa = Nothing
    }
  where
    encrypted = EncryptedHashed (Encrypted "") (DbHash "")
