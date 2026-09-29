-- NOTE (reviewer, remove before merge): NEW module, not on main (bulk lifecycle: "open batch" and the rows
--   under it). Writes the payout_batch row (a new table), the HDFC file number, EXCLUDED requests and the
--   lines of the file.
--   Juspay/Stripe impact: bulk-only. Callers: Bulk.Cycle (runBulkCycle for the sweep and adhoc;
--   openSingleBulkPayout / sendSingleBulkPayout for instant payout in a bulk city) and the app's bulk claim
--   SharedLogic.Payout.Bulk.Claim.claimBeneficiary (recordExclusion). All of these run only for a BulkFlow
--   city, so a Juspay/Stripe city never gets here.

-- | Writing the rows of a batch: the batch itself, an EXCLUDED request for a member who cannot be
-- paid, and the line items to send. The app's claim writes the payable members' requests and orders.
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

-- NOTE (reviewer, remove before merge): the HDFC file number (filerefno): 6 digits, unique per execution
--   date, or HDFC refuses the file as a duplicate. One Redis counter per date, in the master cloud, shared by
--   every city and cloud. A missing key is seeded from the largest number in the master DB + 1000 (to skip
--   KV rows not yet in Postgres). Past 999999 it throws, before anything is claimed. Known gap: only this
--   counter keeps the numbers unique; there is no unique DB index. Bulk-only.

-- | The partner's file reference: six digits, unique per value date.
--
--   A counter per date in Redis, keyed on the city's own day -- the caller passes the same
--   executionDate that goes on the wire as @reqdexctndt@, because the bank's uniqueness is on
--   that pair. When its key is missing -- the first batch of the day, or a lost key -- it is
--   seeded from the largest reference already stored for that date, skipping 1000 ahead:
--   payout_batch is KV-backed, so a batch written seconds ago may not be readable from Postgres
--   yet. There are ~900,000 references a day and no need for them to be consecutive, so a gap
--   costs nothing.
nextClientRefNo ::
  (MonadFlow m, BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Handle m ->
  Time.Day ->
  m Text
nextClientRefNo h executionDate = Redis.runInMasterCloudRedisCell $ do
  let key = h.keyPrefix <> "PayoutBatch:FileRefNo:" <> show executionDate
  mbCurrent :: Maybe Int <- Redis.get key
  when (isNothing mbCurrent) $ do
    -- Master, not the replica: the seed only runs when the key is missing, which is exactly when two
    -- batches can race, and a stale read there collides on (filerefno, reqdexctndt) at the bank.
    dbMax <- runInMasterDbAndRedis $ QPayoutBatchExtra.findMaxClientRefNo executionDate
    -- Only the first caller's seed wins; the rest just increment it. 25 hours: the reference
    -- only has to be unique within its own execution date, so the key needs to outlive that date
    -- by a little -- an hour of slack covers a batch opened at 23:59 local and any clock skew --
    -- and then go, rather than accumulate one key per date forever.
    void $ Redis.setNxExpire key (25 * 3600) (max 100000 (dbMax + 1000))
  n <- Redis.incr key
  -- The file reference is six digits. Sending a seventh would be refused at best; stop here
  -- instead, before anything is claimed against the reference.
  when (n > 999999) $
    throwError $ InternalError ("Bulk payout file references exhausted for execution date " <> show executionDate)
  pure (show n)

-- NOTE (reviewer, remove before merge): opens the batch BEFORE anything is claimed or sent, so every order
--   and EXCLUDED request carries its batchId from birth. A crash before the submit answer leaves a CREATED
--   row whose nextStatusCallAt makes the status check ask HDFC whether the file arrived (the file-number
--   lookup in Bulk.Resolve). It also calls ensureCityJob: this is the ONLY place a city's status-check job
--   is created. A cycle with nothing payable still opens a batch and uses a file number, on purpose.
--   Bulk-only: called by runBulkCycle (sweep, adhoc) and by Cycle.openSingleBulkPayout (instant payout in a
--   bulk city, one batch per payout).

-- | Open a payout_batch. Written before anything is sent, so a crash in between leaves something
--   to recover from: the row starts with a recovery time on it, which makes the status-check job
--   ask the partner whether the batch ever arrived. Makes sure the city has that job right here,
--   for the same reason.
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
            -- Set before the call, not after: if this process dies mid-submit, the job finds the
            -- batch here and asks the partner whether it arrived, instead of leaving the money held
            -- behind a row nothing looks at. On the plan's own first check, so a batch that never
            -- got as far as a submit answer is chased on the same cadence as everything else.
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

-- NOTE (reviewer, remove before merge): a batch where nobody is payable (all excluded, all dropped at
--   claim, or an instant payout that made no order) is closed COMPLETED on the spot and never sent to HDFC.
--   updateFailure's last Nothing clears nextStatusCallAt, so the status check never looks at it. Bulk-only.

-- | A batch with nothing to send -- everyone in it was excluded, or the claim-time re-check
--   dropped them all. Resolved on the spot rather than left with a status check scheduled against
--   a batch the partner was never told about.
closeBatchWithNothingToSend :: (MonadFlow m, BeamFlow m r) => DPayoutBatch.PayoutBatch -> m ()
closeBatchWithNothingToSend batch = do
  now <- getCurrentTime
  logInfo $ "BulkPayout: batch " <> batch.id.getId <> " has no payable items; closing it without submitting"
  QPayoutBatch.updateFailure DPayoutBatch.COMPLETED (Just "No payable items in this batch") Nothing (Just now) Nothing batch.id

-- NOTE (reviewer, remove before merge): a person with missing bank details gets an EXCLUDED payout_request
--   (a new status) tagged with the batch, with no order and no hold, so the money stays in the wallet for
--   the next run. Its created_at is the time it is written. It goes through main's createPayoutRequest, so
--   it gets the normal first history row (EXCLUDE / EXCLUDED). Bulk-only: only the bulk claim calls it, and
--   no Juspay/Stripe request is ever EXCLUDED.

-- | Record a beneficiary dropped for want of usable bank details. No payout_order is created --
--   nothing was submitted for them -- and nothing is held, so the amount stays payable on the next
--   run once the details are fixed. The request carries the batch it was dropped from, which is
--   what makes the per-batch excluded list answerable without a payout_order to join through, and
--   the reason as given: it is what the excluded endpoint shows an operator.
--
--   Its created_at is the moment it was written, so every exclusion has its own time and the city's
--   excluded worklist pages in a stable order. The batch's excluded list reads the same
--   (city, created_at) index from the batch's creation onward, so excluded rows need no index of
--   their own on batch_id.
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
  -- Through the same create every payout request uses, so it gets its first history row too.
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
        -- An exclusion has no usable bank account on file -- that is why it is excluded.
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

-- NOTE (reviewer, remove before merge): turns claimed orders into the lines of the HDFC file. custrefno is
--   the order's SHORT id, because HDFC allows at most 20 characters there. The name goes out in full, with
--   no length cut: HDFC may reject a NEFT/RTGS name over 40 characters, and that item then fails and its
--   money is given back. Bulk-only (called by runBulkCycle and sendSingleBulkPayout).

-- | Turn claimed orders into the line items the partner is sent.
--
--   Total by construction: every check that can reject a beneficiary already ran before their order
--   existed, and the bank details that passed those checks are carried here rather than read again,
--   so nothing can disagree and no path creates an order and then drops it.
--
--   The one impossible case, an order with no short reference, throws: order creation always
--   assigns one, so a missing one is our own invariant broken rather than a problem with this
--   beneficiary.
assembleBulkItems ::
  (MonadFlow m) =>
  [(DPayoutOrder.PayoutOrder, BeneficiaryBank)] ->
  m [(DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem)]
assembleBulkItems claimed = forM claimed $ \(order, bank) -> do
  -- The reference we send is the order's SHORT id, not its id: HDFC cap custrefno at 20 characters
  -- and reject the whole item above it ("Length should not be more then 20"), and an order id is a
  -- 36-character UUID. The short id is what comes back on every inquiry row, so it is also how a
  -- response is matched to this order.
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
