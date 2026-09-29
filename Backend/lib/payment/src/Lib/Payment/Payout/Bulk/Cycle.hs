-- NOTE (reviewer, remove before merge): NEW module, not on main. The entry point of the bulk lifecycle:
--   open batch -> claim each person (app callback) -> build the file lines -> submit. The status-check job
--   then takes over (Bulk.StatusCheck -> Bulk.Resolve -> settle).
--   Juspay/Stripe impact: bulk-only. runBulkCycle's only caller is SharedLogic.Payout.Bulk.Driver.runBulkPayoutCycle,
--   which is called only from (a) the ScheduledBatchPayout sweep inside `when (payoutServiceFlow ==
--   Payout.BulkFlow)` -- Juspay/Stripe cities keep main's per-person `fork processOneWalletPayout` -- and
--   (b) the adhoc admin API, which refuses any non-BulkFlow city. The batch-of-one functions at the bottom
--   (openSingleBulkPayout / sendSingleBulkPayout) are called only by the instant payout in a bulk city
--   (SharedLogic.Payout.Bulk.Driver.runInstantBulkPayout); a Juspay/Stripe city's instant payout never
--   reaches them.

-- | One payout cycle end to end: the batch, then the rows underneath it, then the submission. The
-- app has already run its eligibility pass; its claim (lock, re-check, hold) is a callback.
module Lib.Payment.Payout.Bulk.Cycle
  ( BulkCycleConfig (..),
    runBulkCycle,
    SingleBulkPayout (..),
    openSingleBulkPayout,
    sendSingleBulkPayout,
  )
where

import Data.List (partition)
import qualified Data.Time as Time
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import Lib.Payment.Payout.Bulk.Batch
import Lib.Payment.Payout.Bulk.Submit (submitBatch)
import Lib.Payment.Payout.Bulk.Types
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra

-- | What the app decided about this cycle before any row exists.
data BulkCycleConfig = BulkCycleConfig
  { -- | The payout service name, as the app stores it on the batch.
    payoutServiceName :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    origin :: DPayoutBatch.PayoutBatchOrigin,
    rail :: DPayoutBatch.PayoutBatchRail,
    -- | The date the partner executes on, in the city's own day.
    executionDate :: Time.Day,
    -- | Items per batch: the partner's cap, or less if the app's config says so.
    chunkSize :: Int
  }

-- NOTE (reviewer, remove before merge): payable people are split by the per-file cap (chunkSize: the city's
--   limit, never above HDFC's maxItemsPerBatch). Excluded people ride on the first batch only; a cycle with
--   nothing payable still opens one batch so they have one to be listed under, on purpose. The claim
--   callback is the app's claimBeneficiary: wallet lock, re-check, then the shared wallet payout writes the
--   hold before the order, the same hold logic as Juspay/Stripe.

-- | Run one bulk payout cycle over beneficiaries that have already passed the eligibility pass:
--   the batch first, then the rows underneath it -- so an order and an excluded request both carry
--   their batchId from birth -- and only then the submission.
--
--   The item cap splits the payable beneficiaries into one batch each; everyone excluded for want
--   of bank details rides on the first batch, counted once per cycle.
runBulkCycle ::
  (BulkFlow m r) =>
  Handle m ->
  Payout.PayoutServiceConfig ->
  BulkCycleConfig ->
  -- | The app's claim: re-check under its lock, then hold and create the order, or record an
  --   exclusion, or drop. It gets the whole batch, so every row it writes carries the batch's id.
  (DPayoutBatch.PayoutBatch -> BulkCandidate b -> m ClaimResult) ->
  [BulkCandidate b] ->
  m [(BulkCandidate b, BulkClaimOutcome)]
runBulkCycle h partner cfg claim candidates = do
  let (excludedCandidates, payableCandidates) = partition (isJust . (.exclusionReason)) candidates
      -- The cap counts submitted items, and an excluded beneficiary is never sent -- so exclusions
      -- take no item slots. A cycle with nothing payable still opens one batch, so the people
      -- dropped for missing bank details have a batch to be listed under.
      chunks = case chunksOf (max 1 cfg.chunkSize) payableCandidates of
        [] -> [[]]
        cs -> cs
  fmap concat $
    forM (zip [0 :: Int ..] chunks) $ \(i, chunk) -> do
      let chunkExcluded = if i == 0 then excludedCandidates else []
      batch <-
        openBulkBatch
          h
          (Payout.bulkStatusCheckPlanOf partner)
          cfg.payoutServiceName
          cfg.merchantId
          cfg.merchantOperatingCityId
          cfg.origin
          cfg.rail
          cfg.executionDate
          (length chunk)
          (sum (map (.amount) chunk))
          (length chunkExcluded)
      claims <- forM (chunk <> chunkExcluded) $ \candidate -> do
        result <- claim batch candidate
        pure (candidate, result)
      items <- assembleBulkItems [(order, bank) | (_, Claimed order bank) <- claims]
      -- The batch was opened on the eligibility pass's numbers; anyone the claim-time re-check
      -- dropped never became a row under it, so correct the counts before the submit writes status.
      let claimedExcluded = length [() | (_, Excluded _) <- claims]
      QPayoutBatchExtra.updateCounts (length items) (sum (map ((.amount) . snd) items)) claimedExcluded batch.id
      if null items
        then closeBatchWithNothingToSend batch
        else submitBatch h partner cfg.rail cfg.executionDate batch items
      pure
        [ ( candidate,
            case result of
              Dropped reason -> ClaimDropped reason
              -- The reason the eligibility pass recorded, which is also what the excluded worklist
              -- shows: the adhoc caller reports it back to whoever asked for the payout.
              Excluded _ -> ClaimExcluded (fromMaybe "Bank account not added" candidate.exclusionReason)
              Claimed order _ -> ClaimSubmitted order
          )
          | (candidate, result) <- claims
        ]

-- | Split the payable beneficiaries by the partner's per-call item cap, so each chunk becomes its
--   own batch instead of one oversized call the partner would refuse.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- NOTE (reviewer, remove before merge): instant payout on the bulk rail is a batch of one (origin INSTANT),
--   in two steps so the HDFC call happens AFTER the wallet lock is released, as in the sweep. Under the lock
--   the app opens the batch and writes the request, hold and order; once the lock is gone, the file is sent.
--   The lock lasts 10 s (Juspay/Stripe's value) and the HDFC call can take up to 60 s, so holding the lock
--   across it could let it expire mid-call. A crash between the two steps leaves the batch with
--   nextStatusCallAt set, so the status check chases it like any batch that died before its submit. Bulk-only.

-- | A batch of one: opened, its order written and its money held, not yet sent.
data SingleBulkPayout = SingleBulkPayout
  { singlePartner :: Payout.PayoutServiceConfig,
    singleCycleConfig :: BulkCycleConfig,
    singleBatch :: DPayoutBatch.PayoutBatch,
    singleOrder :: DPayoutOrder.PayoutOrder,
    singleBank :: BeneficiaryBank
  }

-- | Open a batch of one around a payout the app makes under its own wallet lock. The app's
--   callback writes the request, the hold and the order against the batch, or makes none (giving
--   back anything it held). With no order the batch is closed with nothing in it; if the callback
--   throws, the batch is closed the same way and the error is passed on. Nothing is sent here: see
--   'sendSingleBulkPayout'.
openSingleBulkPayout ::
  (BulkFlow m r) =>
  Handle m ->
  Payout.PayoutServiceConfig ->
  BulkCycleConfig ->
  HighPrecMoney -> -- the amount expected, shown on the batch until the order is written
  (DPayoutBatch.PayoutBatch -> m (Maybe (DPayoutOrder.PayoutOrder, BeneficiaryBank))) ->
  m (Maybe SingleBulkPayout)
openSingleBulkPayout h partner cfg expectedAmount createOrder = do
  batch <-
    openBulkBatch
      h
      (Payout.bulkStatusCheckPlanOf partner)
      cfg.payoutServiceName
      cfg.merchantId
      cfg.merchantOperatingCityId
      cfg.origin
      cfg.rail
      cfg.executionDate
      1
      expectedAmount
      0
  let closeEmpty = do
        QPayoutBatchExtra.updateCounts 0 0 0 batch.id
        closeBatchWithNothingToSend batch
  created <- try (createOrder batch)
  case created of
    Left (e :: SomeException) -> closeEmpty >> throwM e
    Right Nothing -> closeEmpty >> pure Nothing
    Right (Just (order, bank)) ->
      pure . Just $
        SingleBulkPayout
          { singlePartner = partner,
            singleCycleConfig = cfg,
            singleBatch = batch,
            singleOrder = order,
            singleBank = bank
          }

-- | Send a batch of one opened by 'openSingleBulkPayout'. Called once the app's wallet lock is
--   released: the money is already held, so nothing here touches the payee's balance.
sendSingleBulkPayout :: (BulkFlow m r) => Handle m -> SingleBulkPayout -> m ()
sendSingleBulkPayout h single = do
  items <- assembleBulkItems [(single.singleOrder, single.singleBank)]
  QPayoutBatchExtra.updateCounts (length items) (sum (map ((.amount) . snd) items)) 0 single.singleBatch.id
  submitBatch h single.singlePartner single.singleCycleConfig.rail single.singleCycleConfig.executionDate single.singleBatch items
