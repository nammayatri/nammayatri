-- NOTE (reviewer, remove before merge): bulk-only module, not on main. One step for one batch: ask HDFC
--   about it (or, for a batch we never got a submit answer for, ask whether our file number exists),
--   apply each item's answer to its order, settle the final items through the app, then save when the
--   next call is due -- or hand the batch to a human (MANUAL_REVIEW_REQUIRED). The only caller is
--   Bulk.StatusCheck.stepOneBatch (under the batch lock), i.e. the BulkPayoutStatusCheck job.
--   Juspay/Stripe orders are settled as on main (webhook, per-order status job, order refresh). The
--   refresh returns an order that has a batch as stored, so an HDFC order is settled only by bulk code:
--   here, or by Bulk.Submit when HDFC's gateway refuses the file.

-- | Driving a submitted batch to a terminal state: ask the partner where it is, apply what it says to
-- each order, and schedule the next call. This is what the status-check job runs.
module Lib.Payment.Payout.Bulk.Resolve
  ( advanceBatch,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
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

-- NOTE (reviewer, remove before merge): the partner config is loaded from the service name saved on the
--   batch (Handle.partnerFor), so a city that switches partner still checks its old batches with the old
--   one. If the saved name is not a known service, the batch goes to MANUAL_REVIEW_REQUIRED with code
--   CONFIG and is not checked again; if the config lookup itself fails, the run stops and the batch stays
--   due. CREATED (crash before the submit answer) and SUBMIT_UNKNOWN go to the file-number lookup; every
--   other status is polled. Bulk-only.

-- | One step of the plan for one batch: either find out whether a submission we never heard back
--   about arrived, or ask HDFC how the batch is doing.
--
--   Called by the status-check job, one batch at a time and under that batch's lock, so it can
--   assume nothing else is touching this batch. Every path ends in exactly one write, which
--   carries both the new status and when the next call is due -- so the two can never disagree,
--   and a crash anywhere leaves the batch due for the same call again.
advanceBatch ::
  (BulkFlow m r) =>
  Handle m ->
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime) -- when the next call is now due, or Nothing when the batch needs no more
advanceBatch h batch = do
  mbPartner <- h.partnerFor batch
  case mbPartner of
    Left reason -> do
      -- Nothing to call: without the partner config we cannot even pick credentials.
      logError $ "Payout batch " <> batch.id.getId <> ": " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) (Just "CONFIG") Nothing Nothing batch.id
      pure Nothing
    Right partner ->
      if batch.status `elem` [DPayoutBatch.SUBMIT_UNKNOWN, DPayoutBatch.CREATED]
        then recoverMissingBatchRef partner batch
        else pollBatchStatus h partner batch

-- NOTE (reviewer, remove before merge): NO_RECORD recovery. We never re-send a file we did not hear back
--   about; we only ask HDFC whether our file number (filerefno + execution date) exists:
--   * found -> SUBMITTED with HDFC's batch number, the plan still timed from when we first sent it;
--   * "no record" -> failure_code NO_RECORD and stay on the plan (HDFC may not have loaded the file yet);
--   * no usable answer -> counted as a failed call.
--   When the plan runs out, the batch goes to MANUAL_REVIEW_REQUIRED with the money still held, on
--   purpose. There is no API to release a "no record" batch; that is done by hand. Bulk-only.

-- | A batch we never got a submission answer for. Ask HDFC whether it has it.
--
--   Never resubmits blindly: the money may already be moving, so the only safe question is
--   whether our file reference is known to them.
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
    -- No usable answer -- a transport failure, or a note that told us nothing. We still do not know
    -- whether the partner has the batch, so nothing is failed and nothing is released: it goes back
    -- on the plan, and the plan running out is what hands it to a human.
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX batch-ref recovery errored for batch " <> batch.id.getId <> ": " <> show e
      scheduleNextStatusCall plan (withCallError "Batch number not confirmed by the bank" e batch) now BSC.CallFailed batch.status 0
    Right (Payout.BatchRefFound partnerBatchRef) ->
      -- It did arrive: check it like any other submitted batch, on the plan from when we sent it.
      markSubmitted plan submittedAt batch.id (Just partnerBatchRef) >> pure (Just (BSC.firstCheckAt plan submittedAt))
    -- "No record" is not proof on its own, and it is never a reason to send the money again.
    -- Early in the plan the partner may simply not have ingested the file yet; later it may still
    -- be a gap on their side. Either way we stay on the plan, and the plan running out is what
    -- hands the batch to a human -- with every reservation still held, because a batch we cannot
    -- place is exactly the case where releasing could pay twice.
    Right Payout.BatchRefNotFound -> do
      logInfo $ "HDFC CBX has no record of batch " <> batch.id.getId <> " yet; staying on the plan"
      scheduleNextStatusCall plan (batch {DPayoutBatch.failureReason = Just "Partner has no record of this batch yet", DPayoutBatch.failureCode = Just "NO_RECORD"}) now BSC.Answered batch.status 0
    -- An answer that places the batch nowhere: treated like a failed call, with the partner's own
    -- code and text on the row.
    Right (Payout.BatchRefUnknown mbCode reason) -> do
      logError $ "HDFC CBX batch-ref recovery for batch " <> batch.id.getId <> " was not answered: " <> show mbCode <> " " <> reason
      scheduleNextStatusCall plan (withPartnerRefusal mbCode reason batch) now BSC.CallFailed batch.status 0

-- NOTE (reviewer, remove before merge): one inquiry call and what each answer does (the answers without
--   rows never touch an item or any money):
--   * 202 "enquire again" (NotReady) -> call again readCallDelaySeconds (default 15 s) later, if the
--     check has a call left;
--   * "no data" -> this check ends, counted in statusNoDataReplies;
--   * HDFC refused the inquiry (e.g. 422 "Invalid Combination"), or the call failed -> HDFC's code and text
--     saved on the batch (code PARTNER_REFUSED for a refusal without one, CALL_FAILED for a failed call), and
--     the call retried after errorRetryDelaySeconds if the check has a call left;
--   * rows -> each one matched to THIS batch's order by short id (our custrefno), applied by applyItemOutcome.
--   DEBITED (money left HDFC, RBI has not confirmed) is not final, so such a batch keeps being checked.
--   Bulk-only.

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
      -- No usable answer. It still counts as a call, so a partner we cannot reach costs the
      -- check rather than letting us hammer them inside it.
      logError $ "HDFC CBX status check failed for batch " <> batch.id.getId <> ": " <> show e
      scheduleNextStatusCall plan (withCallError "Could not reach the bank for a status update" e batch) now BSC.CallFailed batch.status 0
    Right Payout.StatusCheckNotReady -> do
      when (batch.statusCheckCalls + 1 >= plan.maxCallsPerCheck) $
        logWarning $ "HDFC CBX asked us to enquire again on the last call of a check for batch " <> batch.id.getId
      scheduleNextStatusCall plan (clearCallError batch) now BSC.NotReady batch.status 0
    Right Payout.StatusCheckNoData -> do
      -- Not a retry signal. Count it as evidence for the bank and carry on with the plan.
      logWarning $ "HDFC CBX has no data for batch " <> batch.id.getId
      scheduleNextStatusCall plan (clearCallError batch) now BSC.Answered batch.status 1
    -- The partner refused the inquiry itself (e.g. "Invalid Combination" for a batch number it does
    -- not know). Counted like a failed call; its code and text go on the row exactly as sent.
    Right (Payout.StatusCheckRefused mbCode reason) -> do
      logError $ "HDFC CBX refused the status inquiry for batch " <> batch.id.getId <> ": " <> show mbCode <> " " <> reason
      scheduleNextStatusCall plan (withPartnerRefusal mbCode reason batch) now BSC.CallFailed batch.status 0
    Right (Payout.StatusCheckResolved outcomes) -> do
      -- HDFC echo back the custrefno we sent, which is the order's short id. Resolving it against
      -- this batch's own orders keeps the lookup off a column with no secondary key, and means a
      -- row quoting someone else's reference cannot reach another batch's order.
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
      -- Whether anything is left is read from the orders, not from counting the rows HDFC sent: an
      -- item missing from the response is still in flight. Worked out from the list already in hand
      -- plus what this call just wrote -- nothing else writes these orders while the batch is locked.
      --
      -- The batch is AWAITING_PARTNER_APPROVAL only while every one of its items is still waiting for
      -- the approver. Once any item has moved -- paid, debited, or rejected -- the approver has acted
      -- on the batch, so it is SUBMITTED (in progress) even if some items are still pending approval.
      let statusAfter = Map.fromList (catMaybes applied)
          orderStatusAfter o = Map.findWithDefault o.status o.orderId statusAfter
          stillPending = filter (\o -> orderStatusAfter o `notElem` [Payout.SUCCESS, Payout.FAILURE]) batchOrders
          waitingOnApproval = not (null batchOrders) && all (\o -> orderStatusAfter o == Payout.AWAITING_APPROVAL) batchOrders
      -- NOTE (reviewer, remove before merge): a batch is COMPLETED only when every order is final at HDFC
      --   AND every request is final on our side (written after the ledger step). If our settle did not run
      --   for an order (e.g. wallet lock busy), the order normally stays not final, so the next planned
      --   check runs it again (or, if the plan is over, the batch goes to MANUAL_REVIEW_REQUIRED). If an order
      --   is final but its request is not, and nothing is left in flight, the batch goes to
      --   MANUAL_REVIEW_REQUIRED with code LEDGER_SETTLE_FAILED; nothing is re-sent. There is no re-settle
      --   API, so such an order is finished by hand. Bulk-only.
      if null stillPending
        then do
          -- Every order has HDFC's final answer. The batch is COMPLETED only if every one of them was
          -- also settled on our side (its request is final -- written after the ledger step). If our
          -- own settle failed for some, the batch is handed to a human instead: nothing more will be
          -- asked of HDFC, and no payout is retried.
          requestsAfter <- loadRequests batchOrders
          let unsettled = length (filter (not . isSettled requestsAfter) batchOrders)
          if unsettled == 0
            then QPayoutBatch.updateAfterStatusCall DPayoutBatch.COMPLETED (Just now) Nothing Nothing batch.statusCheckRound 0 Nothing batch.statusNoDataReplies batch.id
            else do
              logError $ "Payout batch " <> batch.id.getId <> ": ledger settle failed for " <> show unsettled <> " item(s)"
              QPayoutBatch.updateAfterStatusCall DPayoutBatch.MANUAL_REVIEW_REQUIRED Nothing (Just ("Ledger settle failed for " <> show unsettled <> " item(s)")) (Just "LEDGER_SETTLE_FAILED") batch.statusCheckRound 0 Nothing batch.statusNoDataReplies batch.id
          pure Nothing
        else scheduleNextStatusCall plan (clearCallError batch) now BSC.Answered (if waitingOnApproval then DPayoutBatch.AWAITING_PARTNER_APPROVAL else DPayoutBatch.SUBMITTED) 0

-- NOTE (reviewer, remove before merge): only HDFC's own code / reason text (or a fixed text) is saved on the
--   batch row. The raw exception, which can include request headers, stays in the logs only, so no admin
--   API that reads the batch can show the apikey. Bulk-only.

-- | Put the reason a call could not be made onto the batch, so a row that is merely waiting says
--   why on the row instead of only in a log.
--
--   Prefers the partner's own words. Every non-200 they send is the same problem document, and its
--   reason ("Attempted login from unauthorized IP") is both more specific and more readable than
--   the exception wrapping it; @whatWeWereDoing@ is the fallback for a failure that carries no note
--   at all, such as a timeout. The raw exception stays in the log for whoever needs it.
withCallError :: Text -> SomeException -> DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatch
withCallError whatWeWereDoing e batch =
  batch
    { DPayoutBatch.failureReason = Just (fromMaybe whatWeWereDoing (noteField "noteReason = " rendered)),
      DPayoutBatch.failureCode = Just (fromMaybe "CALL_FAILED" (noteField "noteCode = " rendered))
    }
  where
    rendered = show e
    -- The note is rendered into the error text rather than carried as a value, so its fields are
    -- lifted back out here. Keeping the code in its own column is what makes "every batch that hit
    -- TH99401" answerable without reading prose.
    -- The text may have been shown more than once, in which case its quotes arrive escaped
    -- (\"1\"); the escape backslashes are not part of the value.
    noteField label txt = case T.splitOn label txt of
      (_ : rest : _) -> nonEmptyText' (T.dropWhileEnd (== '\\') (T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') rest))))
      _ -> Nothing
    nonEmptyText' t = if T.null t then Nothing else Just t

-- | The partner's own refusal, carried as data: its code and text go on the batch as sent.
withPartnerRefusal :: Maybe Text -> Text -> DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatch
withPartnerRefusal mbCode reason batch =
  batch
    { DPayoutBatch.failureReason = Just reason,
      DPayoutBatch.failureCode = Just (fromMaybe "PARTNER_REFUSED" mbCode)
    }

-- | A call that answered: whatever stopped the previous one no longer describes this batch.
clearCallError :: DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatch
clearCallError batch = batch {DPayoutBatch.failureReason = Nothing, DPayoutBatch.failureCode = Nothing}

-- NOTE (reviewer, remove before merge): one DB write per call, carrying both the new status and the next
--   due time, so the two never disagree. When the plan is finished with items still in flight, the batch
--   goes to MANUAL_REVIEW_REQUIRED (no more checks) and keeps the last call's reason. Bulk-only.

-- | Record one call and schedule what comes next: another call in this check, the next check in
--   the plan, or -- when the plan is finished with items still in flight -- a human.
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
        -- Keep whatever the last call said. "The plan ran out" is only half the story: it reads the
        -- same whether the partner kept saying "still in flight" or we never reached them at all,
        -- and the difference is the first thing whoever picks this up needs.
        QPayoutBatch.updateAfterStatusCall DPayoutBatch.MANUAL_REVIEW_REQUIRED Nothing (Just (planExhausted <> maybe "" (" Last call: " <>) batch.failureReason)) batch.failureCode batch.statusCheckRound 0 Nothing noData batch.id
        pure Nothing
  where
    planExhausted = "Status checks finished with items still in flight; ask HDFC about this batch"

-- NOTE (reviewer, remove before merge): settle-on-final-answer. Only SUCCESS / FAILURE are final.
--   * In-flight answers (processing, waiting for HDFC's approver, debited) only update the order and save
--     HDFC's bank reference (an answer without one never clears a saved one).
--   * Paid / rejected -> settle through the app's settleItem = driver app settleBulkItem -> the shared
--     UIPayout.payoutSettlementActionWith (the same ledger code the Juspay/Stripe webhook settle uses: hold
--     -> paid out, or money given back). A failed settle is our own error: it is logged, the item stays
--     open, and a later check runs it again.
--   * An order that is already final is not applied again; only its settle is re-run, from the answer saved
--     on the order (outcomeFromOrder), when its request is not final yet.
--   Bulk-only: Juspay/Stripe reach payoutSettlementActionWith through their own webhook / status paths.

-- | Apply one (outcome, settlementStatus) row from an inquiry to the payout_order it was matched
--   to. @mbSettleStatus@ is the rbistatus mirror stored as @transferStatus@ (the settlement axis);
--   the outcome drives @status@ (the order axis).
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
  -- Idempotent: HDFC repeats every row on every inquiry, so only act -- and only count as
  -- newly resolved -- if this order is still in flight. An already-terminal order (SUCCESS or
  -- FAILURE, from a prior pass) is reported as its terminal state, not re-applied. Keyed on the
  -- order status, not transferStatus, because an intra-bank success has no transferStatus.
  -- The one exception is our own side: if an earlier pass wrote the order but its ledger step
  -- failed (the request is not final), the settle is run again. Nothing is asked of HDFC for it.
  | order.status `elem` [Payout.SUCCESS, Payout.FAILURE] = do
    -- A settled item is settled for good: we do not un-pay a driver on a later row. Logged rather
    -- than silently swallowed, because it would mean the assumption behind that is wrong.
    let paidAfterWeFailedIt = case (order.status, outcome) of
          (Payout.FAILURE, Payout.ItemProcessed {}) -> True
          _ -> False
    case (order.status, outcome) of
      (Payout.SUCCESS, Payout.ItemRejected mbCode detail _) ->
        logWarning $ "HDFC CBX reported a rejection for order " <> order.orderId <> ", already settled -- ignoring: " <> show mbCode <> " " <> show detail
      _ -> pure ()
    -- NOTE (reviewer, remove before merge): BULK_PAYOUT_PAID_AFTER_FAILURE is a log tag only (no metric),
    --   so an alert must be set on it. The settle is not run again for such an order.
    -- We failed this order and gave the money back, but HDFC now says it was paid: the person may
    -- have been paid twice. Alert on this log tag; the money is not given back a second time.
    when paidAfterWeFailedIt $
      logError $ "BULK_PAYOUT_PAID_AFTER_FAILURE: HDFC says order " <> order.orderId <> " was PAID, but we failed it. The person may have been paid twice."
    unless (isSettled requestsById order || paidAfterWeFailedIt) $
      void $ settle (outcomeFromOrder order)
    pure order.status
  | otherwise = case outcome of
    -- Still in flight. The status now follows the partner's debit axis -- processing, waiting on their
    -- checker, or debited-but-unsettled -- and 'Nothing' means they said something we do not recognise,
    -- where the row keeps whatever it had rather than being given a state the bank never reported.
    Payout.ItemInterim mbStatus mbCode mbNote mbRef ->
      recordInFlight mbStatus mbCode mbNote mbRef
    Payout.ItemPendingApproval mbStatus mbCode mbNote mbRef ->
      recordInFlight mbStatus mbCode mbNote mbRef
    Payout.ItemProcessed settlementRef refType mbCode mbNote ->
      settle (BulkPaid settlementRef refType mbSettleStatus mbCode mbNote)
    -- A failure reverses its hold, so the money is payable again in a later batch.
    Payout.ItemRejected mbCode detail mbRef ->
      settle (BulkRejected mbCode detail mbSettleStatus mbRef)
  where
    mbRequest = requestOf requestsById order

    -- The app's settlement for this item. A failure here is our own (lock, ledger, config),
    -- not HDFC's: it is logged and the item stays unsettled, to be run again by the next inquiry
    -- of this batch if there is one, else the batch goes to MANUAL_REVIEW_REQUIRED.
    settle finalOutcome = do
      res <- try $ h.settleItem merchantOpCityId order mbRequest finalOutcome
      case res of
        Right () -> pure (if isPaid finalOutcome then Payout.SUCCESS else Payout.FAILURE)
        Left (e :: SomeException) -> do
          logError $ "Bulk payout settle failed for order " <> order.orderId <> ": " <> show e
          -- The order may or may not have been written before the failure; report what it is.
          maybe order.status (.status) <$> QPayoutOrder.findByPrimaryKey order.id order.orderId

    isPaid = \case
      BulkPaid {} -> True
      BulkRejected {} -> False

    -- One write, skipped when it would change nothing: HDFC repeats every row on every inquiry, so
    -- an unchanged item costs nothing and 'updatedAt' comes to mean "the partner said something new"
    -- instead of "we asked again". Also saves the partner's reference for the item, keeping the one
    -- we already have if this row carries none.
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
        QPayoutOrder.updateBulkSettled newStatus mbSettleStatus newRef newRefType mbCode mbNote order.id order.orderId
      pure newStatus

-- | The requests behind a batch's orders, by id.
loadRequests :: (BeamFlow m r) => [DPayoutOrder.PayoutOrder] -> m (Map.Map Text PR.PayoutRequest)
loadRequests orders = do
  requests <- QPayoutRequestExtra.findByIds (mapMaybe (listToMaybe <=< (.entityIds)) orders)
  pure $ Map.fromList [(r.id.getId, r) | r <- requests]

requestOf :: Map.Map Text PR.PayoutRequest -> DPayoutOrder.PayoutOrder -> Maybe PR.PayoutRequest
requestOf requestsById order = (`Map.lookup` requestsById) =<< listToMaybe =<< order.entityIds

-- | Settled on our side: the request was written final after the ledger step. An order with no
--   request has nothing to settle.
isSettled :: Map.Map Text PR.PayoutRequest -> DPayoutOrder.PayoutOrder -> Bool
isSettled requestsById order = maybe True (isPayoutRequestFinal . (.status)) (requestOf requestsById order)
