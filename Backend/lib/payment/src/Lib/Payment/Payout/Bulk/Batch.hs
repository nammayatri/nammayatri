-- | Writes a batch's own rows: the batch, EXCLUDED requests and the items to send. The app's claim
-- writes the payable requests and orders.
module Lib.Payment.Payout.Bulk.Batch
  ( nextClientRefNo,
    openBulkBatch,
    closeBatchWithNothingToSend,
    recordExclusion,
    assembleBulkItems,
  )
where

import qualified Data.Time as Time
import Kernel.Beam.Functions (runInMasterDbAndRedis)
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Types.Common as DCommon
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Bulk.Schedule (ensureCityJob, firstCheckAt)
import Lib.Payment.Payout.Bulk.Types
import Lib.Payment.Payout.Request (createPayoutRequest)
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder

-- | The partner's six-digit file reference. HDFC requires it unique per (filerefno, reqdexctndt),
--   so it is a Redis counter per execution date, seeded from the database when the key is missing.
nextClientRefNo ::
  (MonadFlow m, BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Handle m ->
  Time.Day ->
  m Text
nextClientRefNo h executionDate = Redis.runInMasterCloudRedisCell $ do
  let key = h.keyPrefix <> "PayoutBatch:FileRefNo:" <> show executionDate
  mbCurrent :: Maybe Int <- Redis.get key
  when (isNothing mbCurrent) $ do
    -- Master, not the replica: a missing key is when two batches can race, and a stale max collides
    -- at the bank.
    dbMax <- runInMasterDbAndRedis $ QPayoutBatchExtra.findMaxClientRefNo executionDate
    -- First seed wins. The +1000 skips refs a just-written batch may hold that Postgres does not show
    -- yet (payout_batch is KV-backed); gaps are fine. 25 h outlives the date with an hour of slack.
    void $ Redis.setNxExpire key (25 * 3600) (max 100000 (dbMax + 1000))
  n <- Redis.incr key
  -- Stop before anything is claimed: HDFC would refuse a seven-digit reference.
  when (n > 999999) $
    throwError $ InternalError ("Bulk payout file references exhausted for execution date " <> show executionDate)
  pure (show n)

-- | Open a payout_batch before anything is sent, and make sure the city has its status-check job.
openBulkBatch ::
  (MonadFlow m, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadMask m) =>
  Handle m ->
  Payout.BulkStatusCheckPlan ->
  Text -> -- the payout service name, as the app stores it
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  DPayoutBatch.PayoutBatchOrigin ->
  DPayoutBatch.PayoutBatchRail ->
  Time.Day ->
  Int -> -- items expected in this batch
  HighPrecMoney -> -- their total
  Int -> -- beneficiaries excluded for want of bank details
  m DPayoutBatch.PayoutBatch
openBulkBatch h plan payoutServiceName merchantId merchantOpCityId origin rail executionDate itemCount totalAmount excludedCount = do
  now <- getCurrentTime
  batchIdRaw <- generateGUID
  clientRefNo <- nextClientRefNo h executionDate
  let batch =
        DPayoutBatch.PayoutBatch
          { id = Id batchIdRaw,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            payoutServiceName = payoutServiceName,
            origin = origin,
            status = DPayoutBatch.CREATED,
            payoutRail = rail,
            executionDate = executionDate,
            clientRefNo = clientRefNo,
            partnerBatchRef = Nothing,
            itemCount = itemCount,
            totalAmount = totalAmount,
            excludedCount = excludedCount,
            submittedAt = Nothing,
            statusCheckRound = 0,
            statusCheckCalls = 0,
            -- Set before the submit: if the process dies mid-submit, the job still asks the partner
            -- whether the batch arrived instead of leaving money held on a row nothing checks.
            nextStatusCallAt = Just (firstCheckAt plan now),
            statusNoDataReplies = 0,
            failureReason = Nothing,
            failureCode = Nothing,
            resolvedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QPayoutBatch.create batch
  ensureCityJob h merchantId merchantOpCityId
  pure batch

-- | Close a batch with nothing to send. A non-FAILURE order under it (a claim that failed after its
--   hold) may mean money is held, so the batch goes to manual review instead of COMPLETED.
closeBatchWithNothingToSend :: (MonadFlow m, BeamFlow m r) => DPayoutBatch.PayoutBatch -> m ()
closeBatchWithNothingToSend batch = do
  now <- getCurrentTime
  orders <- QPayoutOrder.findAllByBatchId (Just batch.id)
  let unsentOrders = filter ((/= Payout.FAILURE) . (.status)) orders
  if null unsentOrders
    then do
      logInfo $ "BulkPayout: batch " <> batch.id.getId <> " has no payable items; closing it without submitting"
      QPayoutBatch.updateFailure DPayoutBatch.COMPLETED (Just "No payable items in this batch") Nothing (Just now) Nothing batch.id
    else do
      logError $ "BulkPayout: batch " <> batch.id.getId <> " has " <> show (length unsentOrders) <> " order(s) that were not submitted; their money may be held"
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just ("Not submitted, but " <> show (length unsentOrders) <> " order(s) exist under this batch")) (Just "UNSUBMITTED_ORDERS") (Just now) Nothing batch.id

-- | Write an EXCLUDED request (with its batch id, as there is no order) for a beneficiary with no
--   usable bank details. Nothing is held, so the amount stays payable once the details are fixed.
recordExclusion ::
  (MonadFlow m, BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  DCommon.EntityName ->
  Text -> -- beneficiary id
  DPayoutBatch.PayoutBatch ->
  HighPrecMoney ->
  PR.PayoutType ->
  Text -> -- which detail is missing, shown on the excluded worklist
  m (Id PR.PayoutRequest)
recordExclusion merchantId merchantOpCityId entityName beneficiaryId batch amount payoutType reason = do
  now <- getCurrentTime
  reqId <- generateGUID
  -- The shared create, so the request gets its first history row.
  createPayoutRequest
    PR.PayoutRequest
      { id = Id reqId,
        batchId = Just batch.id,
        beneficiaryId = beneficiaryId,
        amount = Just amount,
        status = PR.EXCLUDED,
        failureReason = Just reason,
        payoutType = Just payoutType,
        entityName = Just entityName,
        entityId = beneficiaryId,
        entityRefId = Nothing,
        ledgerEntryIds = Nothing,
        bankName = Nothing,
        bankAccountLast4 = Nothing,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        city = Nothing,
        coverageFrom = Nothing,
        coverageTo = Nothing,
        customerEmail = Nothing,
        customerName = Nothing,
        customerPhone = Nothing,
        customerVpa = Nothing,
        cashMarkedAt = Nothing,
        cashMarkedById = Nothing,
        cashMarkedByName = Nothing,
        expectedCreditTime = Nothing,
        orderType = Nothing,
        payoutFee = Nothing,
        payoutTransactionId = Nothing,
        remark = Nothing,
        retryCount = Nothing,
        scheduledAt = Nothing,
        createdAt = now,
        updatedAt = now
      }
  logInfo $ "BulkPayoutClaim: excluded " <> beneficiaryId <> " from batch " <> batch.id.getId <> " -- " <> reason
  pure (Id reqId)

-- | Turn claimed orders into the partner's line items. Uses the bank details the claim already
--   checked, not a fresh read, so no order is dropped here.
assembleBulkItems ::
  (MonadFlow m) =>
  [(DPayoutOrder.PayoutOrder, BeneficiaryBank)] ->
  m [(DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem)]
assembleBulkItems claimed = forM claimed $ \(order, bank) -> do
  -- Send the short id, not the 36-char order id: HDFC rejects custrefno over 20 characters. It comes
  -- back on every inquiry row, which is how a response is matched to its order.
  shortId <-
    order.shortId
      & fromMaybeM (InternalError $ "Payout order " <> order.orderId <> " has no short reference; a bulk item cannot be built without one")
  pure
    ( order,
      Payout.BulkPayoutItem
        { itemRef = getShortId shortId,
          amount = order.amount.amount,
          currency = order.amount.currency,
          bankAccountNumber = bank.accountNumber,
          bankIfscCode = bank.ifscCode,
          beneficiaryName = bank.holderName,
          beneficiaryCode = Nothing,
          beneficiaryEmail = Nothing
        }
    )
