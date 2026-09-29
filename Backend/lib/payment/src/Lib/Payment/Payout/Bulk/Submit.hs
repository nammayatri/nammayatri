-- | Sends a batch to HDFC and records the answer. A batch is sent once: an answer we cannot read
-- puts it on the status-check plan, where recovery asks whether it arrived.
module Lib.Payment.Payout.Bulk.Submit
  ( submitBatch,
    markSubmitted,
    batchRejectedPrefix,
    gatewayRefusedPrefix,
  )
where

import qualified Data.Text as T
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
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra

-- | Start of the reason when HDFC refused the file in its answer: nothing was accepted.
batchRejectedPrefix :: Text
batchRejectedPrefix = "HDFC rejected the batch"

-- | Start of the reason when HDFC's gateway refused the file (auth, IP whitelist), so the bank
--   never saw it.
gatewayRefusedPrefix :: Text
gatewayRefusedPrefix = "HDFC gateway refused the batch"

-- | Mark a batch submitted and schedule its first status check. The plan is anchored to the first
--   call to HDFC, not to now, so a batch recovered late does not restart its schedule.
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

-- | A batch we got no clear answer about. Holds stay, and it goes on the normal status-check plan
--   so recovery and polling share one cadence, one call budget and one give-up rule.
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
  -- Answers that stop the batch store `now` (send time) as submitted_at and this as resolved_at.
  answeredAt <- getCurrentTime
  case submitResult of
    -- No answer. The file may still have landed, so nothing is released; recovery asks HDFC.
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX bulk submit errored/timed out for batch " <> batchId.getId <> ": " <> show e
      markSubmitUnknown plan now "Could not reach the bank to submit this batch" Nothing batchId
    Right (Payout.BulkAccepted partnerBatchRef) -> markSubmitted plan now batchId (Just partnerBatchRef)
    -- Accepted without a batch number: not a failure, just untrackable until recovery finds it.
    Right (Payout.BulkAcceptedNoRef reason) -> do
      logWarning $ "HDFC CBX accepted batch " <> batchId.getId <> " but quoted no batchnum: " <> reason
      markSubmitUnknown plan now "Batch number missing" Nothing batchId
    -- Does not prove a refusal (5xx, 408/409, a note on a success status). The file may be
    -- processing, so it is treated like a timeout and HDFC's code is kept.
    Right (Payout.BulkOutcomeUnknown code reason) -> do
      logError $ "HDFC CBX answer for batch " <> batchId.getId <> " does not confirm a refusal: " <> code <> " " <> reason
      markSubmitUnknown plan now ("Bank's answer did not confirm the batch was refused: " <> reason) (Just code) batchId
    -- HDFC already holds a file under this reference. Money stays held for a person to decide:
    -- resending under a new reference could pay a landed file twice.
    Right (Payout.BulkDuplicate _ reason) -> do
      logError $ "HDFC CBX already holds file reference " <> clientRefNo <> " for batch " <> batchId.getId <> ": " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just ("HDFC already has a file with reference " <> clientRefNo <> "; not re-sent, money still held: " <> reason)) (Just "DUPLICATE_FILE_REF") (Just answeredAt) Nothing batchId
    -- Refused inside a normal response, so nothing was accepted. Money stays held for manual review;
    -- HDFC's code goes in its own column so it can be queried.
    Right (Payout.BulkRejected code reason) -> do
      logError $ "HDFC CBX refused batch " <> batchId.getId <> ": " <> code <> " " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just (batchRejectedPrefix <> "; money still held: " <> reason)) (Just code) (Just answeredAt) Nothing batchId
    -- Neither accepted nor refused, so it proves nothing and money stays held for a person. Not worded
    -- as a rejection: release-hold treats a rejected batch as never taken, and this one may have been.
    Right (Payout.BulkUnrecognisedAck code reason) -> do
      logError $ "HDFC CBX answered batch " <> batchId.getId <> " with an unrecognised status: " <> code <> " " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just ("HDFC answered the batch with an unrecognised status; money still held: " <> reason)) (Just code) (Just answeredAt) Nothing batchId
    -- A client-error refusal, so the file cannot be executing. This is the only submit outcome
    -- that releases holds.
    Right (Payout.BulkGatewayFailed code reason) -> do
      logError $ "HDFC CBX gateway refused batch " <> batchId.getId <> ": " <> code <> " " <> reason
      QPayoutBatchExtra.updateSubmittedAt now batchId
      let gatewayError = code <> ": " <> reason
      -- SUBMIT_FAILED means every hold was given back, so it is written only after the release.
      -- A crash mid-release leaves the batch in manual review.
      QPayoutBatch.updateFailure
        DPayoutBatch.MANUAL_REVIEW_REQUIRED
        (Just (gatewayRefusedPrefix <> " (" <> gatewayError <> "); giving back held money. If the batch stays in this state, the release was interrupted"))
        (Just "RELEASE_INCOMPLETE")
        (Just answeredAt)
        Nothing
        batchId
      -- Settlement reverses each order's hold and stores the gateway's code in response_code.
      let settleRefused order mbRequest = do
            res <- try $ h.settleItem batch.merchantOperatingCityId order mbRequest (BulkRejected (Just code) (Just reason) Nothing Nothing)
            case res of
              Right () -> pure Nothing
              Left (e :: SomeException) -> do
                logError $ "Release of refused batch " <> batchId.getId <> ", order " <> order.orderId <> " failed: " <> show e
                pure (Just (order, T.take 200 (show e)))
      requests <- QPayoutRequestExtra.findByIds (mapMaybe requestIdOf orders)
      firstPass <- fmap catMaybes . forM orders $ \order -> settleRefused order (requestOf requests order)
      -- Retry failures once (often a busy wallet lock) from fresh rows. An attempt may have run its
      -- ledger step already, so final requests are skipped and only unreversed holds are given back.
      remaining <-
        if null firstPass
          then pure []
          else do
            let failedIds = map ((.id) . fst) firstPass
            freshOrders <- filter (\o -> o.id `elem` failedIds) <$> QPayoutOrder.findAllByBatchId (Just batchId)
            freshRequests <- QPayoutRequestExtra.findByIds (mapMaybe requestIdOf freshOrders)
            let stillOpen o = not (maybe False (isPayoutRequestFinal . (.status)) (requestOf freshRequests o))
            fmap catMaybes . forM (filter stillOpen freshOrders) $ \order -> settleRefused order (requestOf freshRequests order)
      releasedAt <- getCurrentTime
      case remaining of
        [] -> QPayoutBatch.updateFailure DPayoutBatch.SUBMIT_FAILED (Just reason) (Just code) (Just releasedAt) Nothing batchId
        _ -> do
          let failedOrders = T.intercalate "; " [o.orderId <> ": " <> err | (o, err) <- take 3 remaining]
              more = if length remaining > 3 then "; ..." else ""
              why =
                gatewayRefusedPrefix <> " (" <> gatewayError <> "), and held money could not be given back for "
                  <> show (length remaining)
                  <> " of "
                  <> show (length orders)
                  <> " item(s): "
                  <> failedOrders
                  <> more
          logError $ "Payout batch " <> batchId.getId <> " to manual review: " <> why
          QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just why) (Just "LEDGER_SETTLE_FAILED") (Just releasedAt) Nothing batchId
  where
    requestIdOf order = listToMaybe =<< order.entityIds
    requestOf requests order = find (\r -> Just r.id.getId == requestIdOf order) requests
