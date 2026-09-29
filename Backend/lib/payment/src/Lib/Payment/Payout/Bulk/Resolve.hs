-- | Drives a submitted batch to a final state: ask the partner, apply each item's answer, and
-- schedule the next call. Run by the status-check job.
module Lib.Payment.Payout.Bulk.Resolve
  ( advanceBatch,
    allOrdersFinal,
    unsettledCount,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (incrementGenericMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.Bulk.Schedule as BSC
import Lib.Payment.Payout.Bulk.Submit (markSubmitted)
import Lib.Payment.Payout.Bulk.Types
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra

-- | One status-check step for one batch, run under the batch lock. Every path ends in one write of
--   both the new status and the next call time, so a crash just leaves the same call due again.
advanceBatch ::
  (BulkFlow m r) =>
  Handle m ->
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime) -- next call time, or Nothing when no more calls are needed
advanceBatch h batch = do
  mbPartner <- h.partnerFor batch
  case mbPartner of
    Left reason -> do
      -- Without the partner config there are no credentials to call with.
      logError $ "Payout batch " <> batch.id.getId <> ": " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) (Just "CONFIG") Nothing Nothing batch.id
      pure Nothing
    Right partner ->
      if batch.status `elem` [DPayoutBatch.SUBMIT_UNKNOWN, DPayoutBatch.CREATED]
        then recoverMissingBatchRef partner batch
        else pollBatchStatus h partner batch

-- | For a batch with no submit answer, ask HDFC whether it has our file reference. Never
--   resubmits: the money may already be moving.
recoverMissingBatchRef ::
  (BulkFlow m r) =>
  Payout.PayoutServiceConfig ->
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime)
recoverMissingBatchRef partner batch = do
  now <- getCurrentTime
  let plan = Payout.bulkStatusCheckPlanOf partner
      submittedAt = fromMaybe batch.createdAt batch.submittedAt
      req = Payout.BatchRefRecoveryReq {clientRefNo = batch.clientRefNo, executionDate = batch.executionDate}
  result <- try $ Payout.recoverBatchRef partner req
  case result of
    -- Still unknown, so nothing is failed or released; it stays on the plan until that runs out.
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX batch-ref recovery errored for batch " <> batch.id.getId <> ": " <> show e
      scheduleNextStatusCall plan (withCallError "Batch number not confirmed by the bank" batch) now BSC.CallFailed batch.status 0
    Right (Payout.BatchRefFound partnerBatchRef) ->
      -- It arrived: poll it like any submitted batch, timed from when we sent it.
      markSubmitted plan submittedAt batch.id (Just partnerBatchRef) >> pure (Just (BSC.firstCheckAt plan submittedAt))
    -- "No record" is not proof: HDFC may not have ingested it yet, and releasing could pay twice.
    -- Stay on the plan; when it runs out the batch goes to manual review with the money still held.
    Right Payout.BatchRefNotFound -> do
      logInfo $ "HDFC CBX has no record of batch " <> batch.id.getId <> " yet; staying on the plan"
      scheduleNextStatusCall plan (batch {DPayoutBatch.failureReason = Just "Partner has no record of this batch yet", DPayoutBatch.failureCode = Just "NO_RECORD"}) now BSC.Answered batch.status 0
    -- An answer that places the batch nowhere counts as a failed call.
    Right (Payout.BatchRefUnknown mbCode reason) -> do
      logError $ "HDFC CBX batch-ref recovery for batch " <> batch.id.getId <> " was not answered: " <> show mbCode <> " " <> reason
      scheduleNextStatusCall plan (withPartnerRefusal mbCode reason batch) now BSC.CallFailed batch.status 0

-- | Ask HDFC how a batch is doing, apply what it says, and schedule the next call.
pollBatchStatus ::
  (BulkFlow m r) =>
  Handle m ->
  Payout.PayoutServiceConfig ->
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime)
pollBatchStatus h partner batch = do
  let plan = Payout.bulkStatusCheckPlanOf partner
      req = Payout.BulkStatusCheckReq {partnerBatchRef = batch.partnerBatchRef, clientRefNo = batch.clientRefNo, executionDate = batch.executionDate}
  result <- try $ Payout.checkBulkPayoutStatus partner req
  now <- getCurrentTime
  case result of
    Left (e :: SomeException) -> do
      -- Still counts as a call, so an unreachable partner uses up the check instead of being hammered.
      logError $ "HDFC CBX status check failed for batch " <> batch.id.getId <> ": " <> show e
      scheduleNextStatusCall plan (withCallError "Could not reach the bank for a status update" batch) now BSC.CallFailed batch.status 0
    Right Payout.StatusCheckNotReady -> do
      when (batch.statusCheckCalls + 1 >= plan.maxCallsPerCheck) $
        logWarning $ "HDFC CBX asked us to enquire again on the last call of a check for batch " <> batch.id.getId
      scheduleNextStatusCall plan (clearCallError batch) now BSC.NotReady batch.status 0
    Right Payout.StatusCheckNoData -> do
      -- Not a retry signal: count it (evidence for HDFC) and carry on with the plan.
      logWarning $ "HDFC CBX has no data for batch " <> batch.id.getId
      scheduleNextStatusCall plan (clearCallError batch) now BSC.Answered batch.status 1
    -- HDFC refused the inquiry itself (e.g. "Invalid Combination"): counts as a failed call.
    Right (Payout.StatusCheckRefused mbCode reason) -> do
      logError $ "HDFC CBX refused the status inquiry for batch " <> batch.id.getId <> ": " <> show mbCode <> " " <> reason
      scheduleNextStatusCall plan (withPartnerRefusal mbCode reason batch) now BSC.CallFailed batch.status 0
    Right (Payout.StatusCheckResolved outcomes) -> do
      -- HDFC echoes our custrefno (the order's short id). Matching only within this batch avoids a
      -- lookup on a column with no secondary key, and a stray ref cannot reach another batch's order.
      batchOrders <- QPayoutOrder.findAllByBatchId (Just batch.id)
      requestsById <- loadRequests batchOrders
      let ordersByRef = Map.fromList [(getShortId sid, o) | o <- batchOrders, Just sid <- [o.shortId]]
      applied <- forM outcomes $ \(itemRef, outcome, mbSettleStatus) ->
        case Map.lookup itemRef ordersByRef of
          Nothing -> do
            logError $ "HDFC CBX inquiry referenced an unknown order: " <> itemRef
            pure Nothing
          Just order -> do
            newStatus <- applyItemOutcome h batch.merchantOperatingCityId requestsById order outcome mbSettleStatus
            pure (Just (order.orderId, newStatus))
      -- Read what is left from the orders, not HDFC's rows: an item missing from the response is
      -- still in flight. AWAITING_PARTNER_APPROVAL only while every item waits for the approver.
      let statusAfter = Map.fromList (catMaybes applied)
          orderStatusAfter o = Map.findWithDefault o.status o.orderId statusAfter
          waitingOnApproval = not (null batchOrders) && all (\o -> orderStatusAfter o == Payout.AWAITING_APPROVAL) batchOrders
      -- No orders for a batch sent with items is a read problem (e.g. a KV miss): keep it in flight.
      when (null batchOrders && batch.itemCount > 0) $
        logError $ "Payout batch " <> batch.id.getId <> ": no orders found for a batch submitted with " <> show batch.itemCount <> " item(s); checking again"
      if allOrdersFinal batch.itemCount (map orderStatusAfter batchOrders)
        then do
          -- COMPLETED only if every request is also final on our side. Otherwise a person finishes
          -- the settle; HDFC is not asked again and no payout is retried.
          requestsAfter <- loadRequests batchOrders
          let unsettled = unsettledCount (map (fmap (.status) . requestOf requestsAfter) batchOrders)
          if unsettled == 0
            then QPayoutBatch.updateAfterStatusCall DPayoutBatch.COMPLETED (Just now) Nothing Nothing batch.statusCheckRound 0 Nothing batch.statusNoDataReplies batch.id
            else do
              let unsettledOrders = [o.orderId | o <- batchOrders, not (isSettled requestsAfter o)]
                  reason =
                    "HDFC gave a final answer for every item, but our wallet settle did not finish for "
                      <> show unsettled
                      <> " item(s); re-check this batch to finish them. Orders: "
                      <> T.intercalate ", " (take 10 unsettledOrders)
                      <> (if length unsettledOrders > 10 then ", ..." else "")
              logError $ "Payout batch " <> batch.id.getId <> ": " <> reason
              QPayoutBatch.updateAfterStatusCall DPayoutBatch.MANUAL_REVIEW_REQUIRED Nothing (Just reason) (Just "LEDGER_SETTLE_FAILED") batch.statusCheckRound 0 Nothing batch.statusNoDataReplies batch.id
          pure Nothing
        else scheduleNextStatusCall plan (clearCallError batch) now BSC.Answered (if waitingOnApproval then DPayoutBatch.AWAITING_PARTNER_APPROVAL else DPayoutBatch.SUBMITTED) 0

-- | Save why the call failed on the batch row, so a waiting batch shows why. Only fixed text is
--   saved; the raw exception stays in the log.
withCallError :: Text -> DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatch
withCallError whatWeWereDoing batch =
  batch
    { DPayoutBatch.failureReason = Just whatWeWereDoing,
      DPayoutBatch.failureCode = Just "CALL_FAILED"
    }

-- | Save the partner's own code and text on the batch, as sent.
withPartnerRefusal :: Maybe Text -> Text -> DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatch
withPartnerRefusal mbCode reason batch =
  batch
    { DPayoutBatch.failureReason = Just reason,
      DPayoutBatch.failureCode = Just (fromMaybe "PARTNER_REFUSED" mbCode)
    }

-- | Clear the last call's error once a call gets an answer.
clearCallError :: DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatch
clearCallError batch = batch {DPayoutBatch.failureReason = Nothing, DPayoutBatch.failureCode = Nothing}

-- | Record one call and schedule the next: another call in this check, the next check, or manual
--   review once the plan runs out.
scheduleNextStatusCall ::
  (MonadFlow m, BeamFlow m r) =>
  Payout.BulkStatusCheckPlan ->
  DPayoutBatch.PayoutBatch ->
  UTCTime ->
  BSC.CallOutcome ->
  DPayoutBatch.PayoutBatchStatus ->
  Int -> -- 1 when HDFC answered "no data", 0 otherwise
  m (Maybe UTCTime) -- the next call's time, as written
scheduleNextStatusCall plan batch now outcome newStatus noDataBump = do
  let calls = batch.statusCheckCalls + 1
      noData = batch.statusNoDataReplies + noDataBump
      submittedAt = fromMaybe batch.createdAt batch.submittedAt
  case BSC.nextStepAfterCall plan calls outcome of
    BSC.CallAgainIn delay -> do
      let dueAt = addUTCTime delay now
      QPayoutBatch.updateAfterStatusCall newStatus Nothing batch.failureReason batch.failureCode batch.statusCheckRound calls (Just dueAt) noData batch.id
      pure (Just dueAt)
    BSC.EndCheck -> case BSC.nextCheck plan submittedAt batch.statusCheckRound now of
      Just (checkNo, dueAt) -> do
        QPayoutBatch.updateAfterStatusCall newStatus Nothing batch.failureReason batch.failureCode checkNo 0 (Just dueAt) noData batch.id
        pure (Just dueAt)
      Nothing -> do
        logWarning $ "Payout batch " <> batch.id.getId <> " finished its status-check plan with items still in flight"
        -- Keep the last call's reason: it tells whoever picks this up whether HDFC kept answering
        -- "in flight" or was never reached.
        QPayoutBatch.updateAfterStatusCall DPayoutBatch.MANUAL_REVIEW_REQUIRED Nothing (Just (planExhausted <> maybe "" (" Last call: " <>) batch.failureReason)) batch.failureCode batch.statusCheckRound 0 Nothing noData batch.id
        pure Nothing
  where
    planExhausted = "Status checks finished with items still in flight; ask HDFC about this batch"

-- | Apply one inquiry row to its order. The outcome drives @status@; @mbSettleStatus@ (rbistatus)
--   is stored as @transferStatus@.
applyItemOutcome ::
  (BulkFlow m r) =>
  Handle m ->
  Text -> -- the batch's merchantOperatingCityId
  Map.Map Text PR.PayoutRequest ->
  DPayoutOrder.PayoutOrder ->
  Payout.BulkItemOutcome ->
  Maybe Payout.TransferStatus ->
  m Payout.PayoutOrderStatus -- the order's status after this row
applyItemOutcome h merchantOpCityId requestsById order outcome mbSettleStatus
  -- HDFC repeats every row on every inquiry, so a final order is not re-applied; only an unfinished
  -- settle is re-run. Keyed on status, since an intra-bank success has no transferStatus.
  | order.status `elem` [Payout.SUCCESS, Payout.FAILURE] = do
    -- A final order is never reversed by a later row; a contradicting row is logged instead.
    let paidAfterWeFailedIt = case (order.status, outcome) of
          (Payout.FAILURE, Payout.ItemProcessed {}) -> True
          _ -> False
    -- Paid on our side, but HDFC now reports a rejection or return: the person may not have the money.
    case (order.status, outcome) of
      (Payout.SUCCESS, Payout.ItemRejected mbCode detail _) -> do
        logError $ "BULK_PAYOUT_RETURNED_AFTER_SUCCESS: HDFC reports a rejection for order " <> order.orderId <> ", which we already settled as paid -- not applied: " <> show mbCode <> " " <> show detail
        incrementGenericMetrics "bulk_payout_returned_after_success"
      _ -> pure ()
    -- Failed and refunded on our side, but HDFC now says paid: a possible double payment. The money
    -- is not given back a second time.
    when paidAfterWeFailedIt $ do
      logError $ "BULK_PAYOUT_PAID_AFTER_FAILURE: HDFC says order " <> order.orderId <> " was PAID, but we failed it. The person may have been paid twice."
      incrementGenericMetrics "bulk_payout_paid_after_failure"
    unless (isSettled requestsById order || paidAfterWeFailedIt) $
      void $ settle (outcomeFromOrder order)
    pure order.status
  | otherwise = case outcome of
    -- Still in flight. 'Nothing' is a status we do not recognise: keep the old one rather than
    -- invent a state HDFC never reported.
    Payout.ItemInterim mbStatus mbCode mbNote mbRef ->
      recordInFlight mbStatus mbCode mbNote mbRef
    Payout.ItemPendingApproval mbStatus mbCode mbNote mbRef ->
      recordInFlight mbStatus mbCode mbNote mbRef
    Payout.ItemProcessed settlementRef refType mbCode mbNote ->
      settle (BulkPaid (Just settlementRef) refType mbSettleStatus mbCode mbNote)
    -- A failure reverses its hold, so the money is payable again in a later batch.
    Payout.ItemRejected mbCode detail mbRef ->
      settle (BulkRejected mbCode detail mbSettleStatus mbRef)
  where
    mbRequest = requestOf requestsById order

    -- A settle failure is ours (lock, ledger, config), not HDFC's: the item stays unsettled and is
    -- re-run on the next inquiry, or the batch goes to manual review.
    settle finalOutcome = do
      res <- try $ h.settleItem merchantOpCityId order mbRequest finalOutcome
      case res of
        Right () -> pure (if isPaid finalOutcome then Payout.SUCCESS else Payout.FAILURE)
        Left (e :: SomeException) -> do
          logError $ "Bulk payout settle failed for order " <> order.orderId <> ": " <> show e
          -- Report the status from before this pass, so the item does not count as done.
          pure order.status

    isPaid = \case
      BulkPaid {} -> True
      BulkRejected {} -> False

    -- Skip the write when nothing changed, so 'updatedAt' means "HDFC said something new". Keeps
    -- the stored partner ref when this row has none.
    recordInFlight mbStatus mbCode mbNote mbRef = do
      let newStatus = fromMaybe order.status mbStatus
          (newRef, newRefType) = case mbRef of
            Just (ref, refType) -> (Just ref, Just refType)
            Nothing -> (order.settlementRef, order.settlementRefType)
          unchanged =
            newStatus == order.status
              && mbSettleStatus == order.transferStatus
              && mbCode == order.responseCode
              && mbNote == order.responseMessage
              && newRef == order.settlementRef
              && newRefType == order.settlementRefType
      unless unchanged $
        QPayoutOrder.updateBulkSettled newStatus mbSettleStatus newRef newRefType mbCode mbNote order.id
      pure newStatus

-- | The requests behind a batch's orders, by id.
loadRequests :: (BeamFlow m r) => [DPayoutOrder.PayoutOrder] -> m (Map.Map Text PR.PayoutRequest)
loadRequests orders = do
  requests <- QPayoutRequestExtra.findByIds (mapMaybe (listToMaybe <=< (.entityIds)) orders)
  pure $ Map.fromList [(r.id.getId, r) | r <- requests]

requestOf :: Map.Map Text PR.PayoutRequest -> DPayoutOrder.PayoutOrder -> Maybe PR.PayoutRequest
requestOf requestsById order = (`Map.lookup` requestsById) =<< listToMaybe =<< order.entityIds

-- | Settled on our side: the request is final. A missing request counts as unsettled, since
--   treating it as settled would close the batch with money still held.
isSettled :: Map.Map Text PR.PayoutRequest -> DPayoutOrder.PayoutOrder -> Bool
isSettled requestsById order = maybe False (isPayoutRequestFinal . (.status)) (requestOf requestsById order)

-- | Every order is SUCCESS or FAILURE. No orders for a batch sent with items is a read problem, so
--   it does not count as finished.
allOrdersFinal :: Int -> [Payout.PayoutOrderStatus] -> Bool
allOrdersFinal itemCount statuses =
  not (null statuses && itemCount > 0) && all (`elem` [Payout.SUCCESS, Payout.FAILURE]) statuses

-- | Orders not settled on our side. A missing request ('Nothing') counts as unsettled.
unsettledCount :: [Maybe PR.PayoutRequestStatus] -> Int
unsettledCount = length . filter (maybe True (not . isPayoutRequestFinal))
