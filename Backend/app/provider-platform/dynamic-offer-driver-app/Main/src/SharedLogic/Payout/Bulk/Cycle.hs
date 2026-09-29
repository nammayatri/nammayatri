{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One payout cycle end to end: every check first, then the batch, then the rows underneath it, then
-- the submission. Shared by the scheduled sweep and the ad-hoc admin flow.
module SharedLogic.Payout.Bulk.Cycle
  ( runBulkPayoutCycle,
  )
where

import Data.List (partition)
import qualified Data.Time as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import Lib.Scheduler
import SharedLogic.Payout.Bulk.Claim
import SharedLogic.Payout.Bulk.Submit
import SharedLogic.Payout.Bulk.Types
import qualified Tools.Payout as TPayout

-- | Run one bulk payout cycle over beneficiaries that have already passed the eligibility pass, in
--   the order the doc (section 3.1) asks for: all checks first, then the batch, then the rows
--   underneath it -- so an order and an excluded request both carry their batchId from birth --
--   and only then the submission to HDFC.
--
--   HDFC's per-call item cap splits the payable beneficiaries into one batch each; everyone
--   excluded for want of bank details rides on the first batch, counted once per cycle.
runBulkPayoutCycle ::
  ( EncFlow m r,
    ServiceFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
  DSPC.ScheduledPayoutConfig ->
  DEMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DPayoutBatch.PayoutBatchOrigin ->
  PR.PayoutType ->
  DTConf.TransporterConfig ->
  [BulkCandidate] ->
  m [(BulkCandidate, BulkClaimOutcome)]
runBulkPayoutCycle config payoutServiceName merchantId merchantOpCityId origin payoutType transporterConfig candidates = do
  partner <- TPayout.getPayoutServiceConfig payoutServiceName merchantOpCityId
  -- Read the partner's limits without naming the partner. A payout service with no bulk API is
  -- refused here, once, instead of silently inheriting someone else's ceiling.
  caps <-
    Payout.bulkPartnerCapsOf partner
      & fromMaybeM (InternalError $ "Payout service " <> show payoutServiceName <> " is not a bulk payout partner")
  now <- getCurrentTime
  let -- The partner's cap is authoritative; config.itemsPerBatchLimit can only shrink it, never exceed it.
      partnerCap = caps.maxItemsPerBatch
      chunkSize = max 1 (maybe partnerCap (min partnerCap) config.itemsPerBatchLimit)
      -- NEFT unless the city says otherwise. HDFC may still execute an item intra-bank when the
      -- beneficiary banks with them; that is read per item from the response.
      rail = fromMaybe DPayoutBatch.NEFT config.defaultPayoutRail
      -- The date HDFC executes on, in the city's own day: taking the UTC day would send
      -- yesterday's date for anything submitted before 05:30 IST.
      executionDate = Time.utctDay (addUTCTime (secondsToNominalDiffTime config.timeDiffFromUtc) now)
      (excludedCandidates, payableCandidates) = partition (isJust . (.exclusionReason)) candidates
      -- The cap counts submitted items, and an excluded beneficiary is never sent -- so exclusions
      -- take no item slots. A cycle with nothing payable still opens one batch, so the people
      -- dropped for missing bank details have a batch to be listed under.
      chunks = case chunksOf chunkSize payableCandidates of
        [] -> [[]]
        cs -> cs
  fmap concat $
    forM (zip [0 :: Int ..] chunks) $ \(i, chunk) -> do
      let chunkExcluded = if i == 0 then excludedCandidates else []
      batch <-
        openBulkBatch
          (Payout.bulkStatusCheckPlanOf partner)
          payoutServiceName
          merchantId
          merchantOpCityId
          origin
          rail
          executionDate
          (length chunk)
          (sum (map (.amount) chunk))
          (length chunkExcluded)
      claims <- forM (chunk <> chunkExcluded) $ \candidate -> do
        mbClaim <- claimBeneficiary config payoutType transporterConfig payoutServiceName merchantId merchantOpCityId batch.id candidate
        pure (candidate, mbClaim)
      items <- assembleBulkItems [claimed | (_, Just (Right claimed)) <- claims]
      -- The batch was opened on the eligibility pass's numbers; anyone the claim-time re-check
      -- dropped never became a row under it, so correct the counts before the submit writes status.
      let claimedExcluded = length [() | (_, Just (Left _)) <- claims]
      QPayoutBatchExtra.updateCounts (length items) (sum (map ((.amount) . snd) items)) claimedExcluded batch.id
      if null items
        then closeBatchWithNothingToSend batch
        else submitBatch partner rail executionDate batch items
      pure
        [ ( candidate,
            case mbClaim of
              Nothing -> ClaimDropped "No longer eligible when the batch claimed it"
              -- The reason the eligibility pass recorded, which is also what the excluded worklist
              -- shows: the adhoc caller reports it back to whoever asked for the payout.
              Just (Left _) -> ClaimExcluded (fromMaybe "Bank account not added" candidate.exclusionReason)
              Just (Right (order, _)) -> ClaimSubmitted order
          )
          | (candidate, mbClaim) <- claims
        ]

-- | A batch with nothing to send -- everyone in it was excluded, or the claim-time re-check
--   dropped them all. Resolved on the spot rather than left with a status check scheduled against
--   a batch HDFC was never told about.
closeBatchWithNothingToSend :: (MonadFlow m, PaymentBeamFlow.BeamFlow m r) => DPayoutBatch.PayoutBatch -> m ()
closeBatchWithNothingToSend batch = do
  now <- getCurrentTime
  logInfo $ "BulkPayout: batch " <> batch.id.getId <> " has no payable items; closing it without submitting"
  QPayoutBatch.updateFailure DPayoutBatch.COMPLETED (Just "No payable items in this batch") Nothing (Just now) Nothing batch.id
