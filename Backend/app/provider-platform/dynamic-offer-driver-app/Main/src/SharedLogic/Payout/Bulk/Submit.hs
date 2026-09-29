{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Sending a batch to the payout partner and recording what came back. A batch is sent once: an
-- answer we cannot read puts it on the status-check plan, where recovery asks whether it arrived.
module SharedLogic.Payout.Bulk.Submit
  ( submitBatch,
    markSubmitted,
    markSubmitUnknown,
  )
where

import qualified Data.Time as Time
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import Lib.Scheduler
import SharedLogic.Payout.Bulk.Order
import SharedLogic.Payout.Bulk.Types
import SharedLogic.Payout.BulkStatusCheck (ensureStatusCheckJob)
import qualified SharedLogic.Payout.BulkStatusCheck as BSC

-- | Mark a batch submitted and schedule its first status check, in one write.
--
--   Shared by a fresh submission and by one whose reference we only recovered later. The plan is
--   anchored to when we first called HDFC, never to now, so a late recovery is already overdue
--   for its early checks instead of starting the schedule over.
markSubmitted ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r) =>
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
    (Just (BSC.firstCheckAt plan submittedAt))
    Nothing -- accepted, so nothing to explain
    Nothing
    batchId

-- | A batch we could not get an answer about. The reservations stay held and the batch is put on
--   the ordinary status-check plan, where the recovery call asks whether the partner received it.
--
--   On the plan rather than a delay of its own, so a batch waiting to be placed and a batch being
--   polled share one cadence, one call budget and one way of giving up.
markSubmitUnknown ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r) =>
  Payout.BulkStatusCheckPlan ->
  UTCTime -> -- when we first called HDFC about this batch
  Text -> -- why we do not know
  Id DPayoutBatch.PayoutBatch ->
  m ()
markSubmitUnknown plan submittedAt reason batchId =
  QPayoutBatch.updateAfterSubmit
    DPayoutBatch.SUBMIT_UNKNOWN
    Nothing
    (Just submittedAt)
    1
    0
    (Just (BSC.firstCheckAt plan submittedAt))
    (Just reason)
    Nothing -- no code: the partner gave us none in these cases
    batchId

-- | Send an already-open batch's items to HDFC and record what came back.
submitBatch ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
  Payout.PayoutServiceConfig ->
  DPayoutBatch.PayoutBatchRail ->
  Time.Day ->
  DPayoutBatch.PayoutBatch ->
  [(DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem)] ->
  m ()
submitBatch partner rail executionDate batch items = do
  now <- getCurrentTime
  let (orders, lineItems) = unzip items
      plan = Payout.bulkStatusCheckPlanOf partner
      batchId = batch.id
      clientRefNo = batch.clientRefNo
  ensureStatusCheckJob
  let req = Payout.BulkPayoutReq {clientRefNo = clientRefNo, executionDate = executionDate, rail = toPayoutRail rail, items = lineItems}
  submitResult <- try $ Payout.submitBulkPayout partner req
  case submitResult of
    -- The call never answered. It may still have landed, so nothing is failed and nothing is
    -- released: recovery asks the partner whether they have our file reference.
    Left (e :: SomeException) -> do
      logError $ "HDFC CBX bulk submit errored/timed out for batch " <> batchId.getId <> ": " <> show e
      markSubmitUnknown plan now "Could not reach the bank to submit this batch" batchId
    Right (Payout.BulkAccepted partnerBatchRef) -> markSubmitted plan now batchId (Just partnerBatchRef)
    -- Acknowledged without a reference. The submission landed, so this is emphatically not a
    -- failure -- we simply cannot track it until recovery tells us which batch it became.
    Right (Payout.BulkAcceptedNoRef reason) -> do
      logWarning $ "HDFC CBX accepted batch " <> batchId.getId <> " but quoted no batchnum: " <> reason
      markSubmitUnknown plan now "Batch number missing" batchId
    -- A duplicate says the partner holds a file under this reference. It does not say whose, or
    -- whether it should be paid -- so the money stays reserved and a human decides. Resending
    -- under a new reference, which is what this used to do, pays a landed file twice.
    Right (Payout.BulkDuplicate _ reason) -> do
      logError $ "HDFC CBX already holds file reference " <> clientRefNo <> " for batch " <> batchId.getId <> ": " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) Nothing Nothing Nothing batchId
    -- The partner refused the file inside an ordinary response: nothing was accepted. Parked
    -- rather than released, so that someone confirms before the money is freed. Their code is kept
    -- in its own column, so "every batch refused with this code" is answerable without reading prose.
    Right (Payout.BulkRejected code reason) -> do
      logError $ "HDFC CBX refused batch " <> batchId.getId <> ": " <> code <> " " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) (Just code) Nothing Nothing batchId
    -- Refused before the partner's banking layer saw it, so the file cannot be executing. This is
    -- the one submit outcome that releases: the items go back to payable for the next sweep.
    Right (Payout.BulkGatewayFailed code reason) -> do
      logError $ "HDFC CBX gateway refused batch " <> batchId.getId <> ": " <> code <> " " <> reason
      QPayoutBatch.updateFailure DPayoutBatch.SUBMIT_FAILED (Just reason) (Just code) (Just now) Nothing batchId
      -- Code and text in their own columns, as the gateway sent them: gluing them into one string
      -- left response_code empty and made the code unqueryable.
      ctx <- loadBulkOutcomeCtx batch.merchantOperatingCityId orders
      for_ orders $ \order -> failClaimedOrder ctx order (Just code) (Just reason) Nothing
