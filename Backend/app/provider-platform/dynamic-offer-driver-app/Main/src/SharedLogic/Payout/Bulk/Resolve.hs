{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driving a submitted batch to a terminal state: ask the partner where it is, apply what it says to
-- each order, and schedule the next call. This is what the status-check job runs.
module SharedLogic.Payout.Bulk.Resolve
  ( advanceBatch,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Lib.Scheduler
import SharedLogic.Payout.Bulk.Order
import SharedLogic.Payout.Bulk.Submit
import SharedLogic.Payout.Bulk.Types
import qualified SharedLogic.Payout.BulkStatusCheck as BSC
import qualified Tools.Payout as TPayout

-- | One step of the plan for one batch: either find out whether a submission we never heard back
--   about arrived, or ask HDFC how the batch is doing.
--
--   Called by the status-check job, one batch at a time and under that batch's lock, so it can
--   assume nothing else is touching this batch. Every path ends in exactly one write, which
--   carries both the new status and when the next call is due -- so the two can never disagree,
--   and a crash anywhere leaves the batch due for the same call again.
advanceBatch ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime) -- when the next call is now due, or Nothing when the batch needs no more
advanceBatch batch = case readMaybe (T.unpack batch.payoutServiceName) of
  Nothing -> do
    -- Nothing to call: without the service name we cannot even pick credentials.
    logError $ "Payout batch " <> batch.id.getId <> " has an unreadable payout service name: " <> batch.payoutServiceName
    QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just ("Unknown payout service: " <> batch.payoutServiceName)) (Just "CONFIG") Nothing Nothing batch.id
    pure Nothing
  Just payoutServiceName -> do
    partner <- TPayout.getPayoutServiceConfig payoutServiceName (Id batch.merchantOperatingCityId)
    if batch.status `elem` [DPayoutBatch.SUBMIT_UNKNOWN, DPayoutBatch.CREATED]
      then recoverMissingBatchRef partner batch
      else pollBatchStatus partner batch

-- | A batch we never got a submission answer for. Ask HDFC whether it has it.
--
--   Never resubmits blindly: the money may already be moving, so the only safe question is
--   whether our file reference is known to them.
recoverMissingBatchRef ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
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

-- | Ask HDFC how a batch is doing, apply what it says, and schedule the next call.
pollBatchStatus ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Payout.PayoutServiceConfig ->
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime)
pollBatchStatus partner batch = do
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
      ctx <- loadBulkOutcomeCtx batch.merchantOperatingCityId batchOrders
      let ordersByRef = Map.fromList [(getShortId sid, o) | o <- batchOrders, Just sid <- [o.shortId]]
      applied <- forM outcomes $ \(itemRef, outcome, mbSettleStatus) ->
        case Map.lookup itemRef ordersByRef of
          Nothing -> do
            logError $ "HDFC CBX inquiry referenced an unknown order: " <> itemRef
            pure (ItemOutcomeRejected, Nothing)
          Just order -> do
            (itemResult, newStatus) <- applyItemOutcome ctx order outcome mbSettleStatus
            pure (itemResult, Just (order.orderId, newStatus))
      -- Whether anything is left is read from the orders, not from counting the rows HDFC sent: an
      -- item missing from the response is still in flight. Worked out from the list already in hand
      -- plus what this call just wrote -- nothing else writes these orders while the batch is locked.
      let results = map fst applied
          statusAfter = Map.fromList (mapMaybe snd applied)
          stillPending = filter (\o -> Map.findWithDefault o.status o.orderId statusAfter `notElem` [Payout.SUCCESS, Payout.FAILURE]) batchOrders
          inFlight = filter (`elem` [ItemOutcomePending, ItemOutcomePendingApproval]) results
          waitingOnApproval = not (null inFlight) && all (== ItemOutcomePendingApproval) inFlight
      if null stillPending
        then QPayoutBatch.updateAfterStatusCall DPayoutBatch.COMPLETED (Just now) Nothing Nothing batch.statusCheckRound 0 Nothing batch.statusNoDataReplies batch.id >> pure Nothing
        else scheduleNextStatusCall plan (clearCallError batch) now BSC.Answered (if waitingOnApproval then DPayoutBatch.AWAITING_PARTNER_APPROVAL else DPayoutBatch.SUBMITTED) 0

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

-- | Record one call and schedule what comes next: another call in this check, the next check in
--   the plan, or -- when the plan is finished with items still in flight -- a human.
scheduleNextStatusCall ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r) =>
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

-- | Apply one (outcome, settlementStatus) row from an inquiry to the payout_order it was matched
--   to. @mbSettleStatus@ is the rbistatus mirror stored as @transferStatus@ (the settlement axis);
--   the outcome drives @status@ (the order axis).
applyItemOutcome ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  BulkOutcomeCtx ->
  DPayoutOrder.PayoutOrder ->
  Payout.BulkItemOutcome ->
  Maybe Payout.TransferStatus ->
  m (BulkItemResult, Payout.PayoutOrderStatus) -- and the order's status after this row
applyItemOutcome ctx order outcome mbSettleStatus
  -- Idempotent: HDFC repeats every row on every inquiry, so only act -- and only count as
  -- newly resolved -- if this order is still in flight. An already-terminal order (SUCCESS or
  -- FAILURE, from a prior pass) is reported as its terminal state, not re-applied. Keyed on the
  -- order status, not transferStatus, because an intra-bank success has no transferStatus.
  | order.status == Payout.SUCCESS = do
    -- A settled item is settled for good: we do not un-pay a driver on a later row. Logged rather
    -- than silently swallowed, because it would mean the assumption behind that is wrong.
    case outcome of
      Payout.ItemRejected mbCode detail ->
        logWarning $ "HDFC CBX reported a rejection for order " <> order.orderId <> ", already settled -- ignoring: " <> show mbCode <> " " <> show detail
      _ -> pure ()
    pure (ItemOutcomeProcessed, Payout.SUCCESS)
  -- Every failure releases its reservation, so a failed order is always deferred to a later batch.
  | order.status == Payout.FAILURE = pure (ItemOutcomeDeferred, Payout.FAILURE)
  | otherwise = case outcome of
    -- Still in flight. The status now follows the partner's debit axis -- processing, waiting on their
    -- checker, or debited-but-unsettled -- and 'Nothing' means they said something we do not recognise,
    -- where the row keeps whatever it had rather than being given a state the bank never reported.
    Payout.ItemInterim mbStatus mbCode mbNote -> do
      newStatus <- recordInFlight mbStatus mbCode mbNote
      pure (ItemOutcomePending, newStatus)
    Payout.ItemPendingApproval mbStatus mbCode mbNote -> do
      newStatus <- recordInFlight mbStatus mbCode mbNote
      pure (ItemOutcomePendingApproval, newStatus)
    Payout.ItemProcessed settlementRef refType mbCode mbNote -> do
      settleClaimedOrder ctx order settlementRef refType mbSettleStatus mbCode mbNote
      pure (ItemOutcomeProcessed, Payout.SUCCESS)
    Payout.ItemRejected mbCode detail -> do
      failClaimedOrder ctx order mbCode detail mbSettleStatus
      pure (ItemOutcomeDeferred, Payout.FAILURE)
  where
    -- One write, skipped when it would change nothing: HDFC repeats every row on every inquiry, so
    -- an unchanged item costs nothing and 'updatedAt' comes to mean "the partner said something new"
    -- instead of "we asked again".
    recordInFlight mbStatus mbCode mbNote = do
      let newStatus = fromMaybe order.status mbStatus
          unchanged =
            newStatus == order.status
              && mbSettleStatus == order.transferStatus
              && mbCode == order.responseCode
              && mbNote == order.responseMessage
      unless unchanged $
        QPayoutOrder.updateBulkStatusAndRespInfo newStatus mbSettleStatus mbCode mbNote order.orderId
      pure newStatus
