-- NOTE (reviewer, remove before merge): NEW module, not on main (bulk lifecycle: "submit"). Sends one batch
--   file to HDFC once and records the answer; it never re-sends.
--   Juspay/Stripe impact: bulk-only. submitBatch is called only by Bulk.Cycle (runBulkCycle and
--   sendSingleBulkPayout); markSubmitted only from here and from Bulk.Resolve (after HDFC confirms a file we
--   had no answer for). Juspay/Stripe orders go to their partner one at a time in main's create path
--   (Tools/Payout.createPayoutOrder, the non-HDFC branch), not through this module.

-- | Sending a batch to the payout partner and recording what came back. A batch is sent once: an
-- answer we cannot read puts it on the status-check plan, where recovery asks whether it arrived.
module Lib.Payment.Payout.Bulk.Submit
  ( submitBatch,
    markSubmitted,
  )
where

import qualified Data.Time as Time
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import Lib.Payment.Payout.Bulk.Schedule (firstCheckAt)
import Lib.Payment.Payout.Bulk.Types
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra

-- | Mark a batch submitted and schedule its first status check, in one write.
--
--   Shared by a fresh submission and by one whose reference we only recovered later. The plan is
--   anchored to when we first called HDFC, never to now, so a late recovery is already overdue
--   for its early checks instead of starting the schedule over.
markSubmitted ::
  (MonadFlow m, BeamFlow m r) =>
  Payout.BulkStatusCheckPlan ->
  UTCTime -> -- when we first called HDFC about this batch
  Id DPayoutBatch.PayoutBatch ->
  Maybe Text ->
  m ()
markSubmitted plan submittedAt batchId mbPartnerRef =
  QPayoutBatch.updateAfterSubmit
    DPayoutBatch.SUBMITTED
    mbPartnerRef
    (Just submittedAt)
    1 -- the first check of the plan
    0 -- no calls made in it yet
    (Just (firstCheckAt plan submittedAt))
    Nothing -- accepted, so nothing to explain
    Nothing
    batchId

-- NOTE (reviewer, remove before merge): SUBMIT_UNKNOWN = we don't know whether HDFC got the file (the call
--   errored or timed out, HDFC accepted it without a batch number, or its answer did not prove a refusal).
--   The money stays held and the status check asks HDFC by file number (Bulk.Resolve.recoverMissingBatchRef).
--   A submit that failed before anything left us is also treated as "maybe sent", on purpose. If the plan
--   runs out, the batch goes to MANUAL_REVIEW_REQUIRED with the money still held. Bulk-only.

-- | A batch we could not get an answer about. The reservations stay held and the batch is put on
--   the ordinary status-check plan, where the recovery call asks whether the partner received it.
--
--   On the plan rather than a delay of its own, so a batch waiting to be placed and a batch being
--   polled share one cadence, one call budget and one way of giving up.
markSubmitUnknown ::
  (MonadFlow m, BeamFlow m r) =>
  Payout.BulkStatusCheckPlan ->
  UTCTime -> -- when we first called HDFC about this batch
  Text -> -- why we do not know
  Maybe Text -> -- the partner's own code, when its answer carried one
  Id DPayoutBatch.PayoutBatch ->
  m ()
markSubmitUnknown plan submittedAt reason mbCode batchId =
  QPayoutBatch.updateAfterSubmit
    DPayoutBatch.SUBMIT_UNKNOWN
    Nothing
    (Just submittedAt)
    1
    0
    (Just (firstCheckAt plan submittedAt))
    (Just reason)
    mbCode
    batchId

-- NOTE (reviewer, remove before merge): what each HDFC submit answer does:
--   * error / timeout, accepted without a batch number, or an answer that does not prove a refusal
--     (5xx, a note on a 2xx, 408/409) -> SUBMIT_UNKNOWN (see markSubmitUnknown);
--   * accepted -> SUBMITTED, first check at submit time + firstCheckDelayMinutes;
--   * duplicate file number, or HDFC refused the file -> MANUAL_REVIEW_REQUIRED with the money still held
--     and no more checks (updateFailure clears nextStatusCallAt). Not re-sent under a new number: that could
--     pay a file that did land twice. There is no API to give this money back; ops finish these by hand;
--   * gateway refusal (HDFC's gateway refused the file before its banking layer saw it, e.g. TH99400 for
--     special characters in a name) -> SUBMIT_FAILED, and every order is settled as failed through the
--     app's settleItem (the shared settle gives the money back, request AUTO_PAY_FAILED). If our settle
--     fails for any order (e.g. wallet lock busy), the batch goes to MANUAL_REVIEW_REQUIRED with code
--     LEDGER_SETTLE_FAILED, because nothing will check this batch again.
--   For the three refusal answers, submitted_at = when we sent the file (updateSubmittedAt) and
--   resolved_at = when HDFC answered (`answeredAt`). Bulk-only.

-- | Send an already-open batch's items to HDFC and record what came back.
submitBatch ::
  (BulkFlow m r) =>
  Handle m ->
  Payout.PayoutServiceConfig ->
  DPayoutBatch.PayoutBatchRail ->
  Time.Day ->
  DPayoutBatch.PayoutBatch ->
  [(DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem)] ->
  m ()
submitBatch h partner rail executionDate batch items = do
  now <- getCurrentTime
  let (orders, lineItems) = unzip items
      plan = Payout.bulkStatusCheckPlanOf partner
      batchId = batch.id
      clientRefNo = batch.clientRefNo
  let req = Payout.BulkPayoutReq {clientRefNo = clientRefNo, executionDate = executionDate, rail = toPayoutRail rail, items = lineItems}
  submitResult <- try $ Payout.submitBulkPayout partner req
  -- When the partner answered. The answers that don't go through markSubmitted save `now` (when we
  -- sent the file) as submitted_at and this as resolved_at.
  answeredAt <- getCurrentTime
  case submitResult of
    -- The call never answered. It may still have landed, so nothing is failed and nothing is
    -- released: recovery asks the partner whether they have our file reference.
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX bulk submit errored/timed out for batch " <> batchId.getId <> ": " <> show e
      markSubmitUnknown plan now "Could not reach the bank to submit this batch" Nothing batchId
    Right (Payout.BulkAccepted partnerBatchRef) -> markSubmitted plan now batchId (Just partnerBatchRef)
    -- Acknowledged without a reference. The submission landed, so this is emphatically not a
    -- failure -- we simply cannot track it until recovery tells us which batch it became.
    Right (Payout.BulkAcceptedNoRef reason) -> do
      logWarning $ "HDFC CBX accepted batch " <> batchId.getId <> " but quoted no batchnum: " <> reason
      markSubmitUnknown plan now "Batch number missing" Nothing batchId
    -- An answer that does not prove the file was refused (a server error, a note on a success status,
    -- a 408/409). It may be processing, so it is handled like a timeout: nothing is released, and
    -- recovery asks the partner whether it holds the file. The partner's code is kept on the batch.
    Right (Payout.BulkOutcomeUnknown code reason) -> do
      logError $ "HDFC CBX answer for batch " <> batchId.getId <> " does not confirm a refusal: " <> code <> " " <> reason
      markSubmitUnknown plan now ("Bank's answer did not confirm the batch was refused: " <> reason) (Just code) batchId
    -- A duplicate says the partner holds a file under this reference. It does not say whose, or
    -- whether it should be paid -- so the money stays reserved and a human decides. Resending
    -- under a new reference would pay a landed file twice.
    Right (Payout.BulkDuplicate _ reason) -> do
      logError $ "HDFC CBX already holds file reference " <> clientRefNo <> " for batch " <> batchId.getId <> ": " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) Nothing (Just answeredAt) Nothing batchId
    -- The partner refused the file inside an ordinary response: nothing was accepted. Parked in
    -- manual review with the money still held rather than given back. Their code is kept in its
    -- own column, so "every batch refused with this code" is answerable without reading prose.
    Right (Payout.BulkRejected code reason) -> do
      logError $ "HDFC CBX refused batch " <> batchId.getId <> ": " <> code <> " " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) (Just code) (Just answeredAt) Nothing batchId
    -- Refused (a client-error answer), so the file cannot be executing. This is the one submit
    -- outcome that releases: the items go back to payable for the next sweep.
    Right (Payout.BulkGatewayFailed code reason) -> do
      logError $ "HDFC CBX gateway refused batch " <> batchId.getId <> ": " <> code <> " " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      QPayoutBatch.updateFailure DPayoutBatch.SUBMIT_FAILED (Just reason) (Just code) (Just answeredAt) Nothing batchId
      -- Code and text in their own columns, as the gateway sent them, so response_code is filled
      -- and the code can be queried.
      -- Each order is failed through the app's settlement, which reverses its hold.
      requests <- QPayoutRequestExtra.findByIds (mapMaybe (listToMaybe <=< (.entityIds)) orders)
      let requestOf order = find (\r -> Just r.id.getId == (listToMaybe =<< order.entityIds)) requests
      failures <- fmap (length . filter (== True)) . forM orders $ \order -> do
        res <- try $ h.settleItem batch.merchantOperatingCityId order (requestOf order) (BulkRejected (Just code) (Just reason) Nothing Nothing)
        case res of
          Right () -> pure False
          Left (e :: SomeException) -> do
            logError $ "Bulk payout settle failed for order " <> order.orderId <> ": " <> show e
            pure True
      -- A batch nobody will ask HDFC about again: an item our own settle could not finish is handed
      -- to a human rather than left with its hold in place and no signal.
      when (failures > 0) $
        QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just ("Ledger settle failed for " <> show failures <> " item(s)")) (Just "LEDGER_SETTLE_FAILED") (Just answeredAt) Nothing batchId
