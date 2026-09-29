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

-- | Run one bulk cycle over beneficiaries that passed eligibility. Each batch is opened before its
--   rows so they carry its id from birth; the excluded ride on the first batch only.
runBulkCycle ::
  (BulkFlow m r) =>
  Handle m ->
  Payout.PayoutServiceConfig ->
  BulkCycleConfig ->
  -- | The app's claim: re-check under its lock, then hold and create the order, record an
  --   exclusion, or drop.
  (DPayoutBatch.PayoutBatch -> BulkCandidate b -> m ClaimResult) ->
  [BulkCandidate b] ->
  m [(BulkCandidate b, BulkClaimOutcome)]
runBulkCycle h partner cfg claim candidates = do
  let (excludedCandidates, payableCandidates) = partition (isJust . (.exclusionReason)) candidates
      -- Excluded people are never sent, so they take no item slots. A cycle with nothing payable
      -- still opens one batch so the excluded have one to be listed under.
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
      -- The claim-time re-check may drop people counted when the batch opened, so fix the counts.
      let claimedExcluded = length [() | (_, Excluded _ _) <- claims]
      QPayoutBatchExtra.updateCounts (length items) (sum (map ((.amount) . snd) items)) claimedExcluded batch.id
      if null items
        then closeBatchWithNothingToSend batch
        else submitBatch h partner cfg.rail cfg.executionDate batch items
      pure
        [ ( candidate,
            case result of
              Dropped reason -> ClaimDropped reason
              -- The reason saved on the EXCLUDED request; the adhoc caller reports it back.
              Excluded _ reason -> ClaimExcluded reason
              Claimed order _ -> ClaimSubmitted order
          )
          | (candidate, result) <- claims
        ]

-- | Split by the partner's per-call item cap, one batch per chunk; an oversized call is refused.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | A batch of one: opened, its order written and its money held, not yet sent.
data SingleBulkPayout = SingleBulkPayout
  { singlePartner :: Payout.PayoutServiceConfig,
    singleCycleConfig :: BulkCycleConfig,
    singleBatch :: DPayoutBatch.PayoutBatch,
    singleOrder :: DPayoutOrder.PayoutOrder,
    singleBank :: BeneficiaryBank
  }

-- | Open a batch of one around a payout the app makes under its own wallet lock. If the callback
--   writes no order or throws, the batch is closed empty. Nothing is sent here.
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
