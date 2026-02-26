{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.PPF.Recon
  ( processOnRecon,
    initiateReconBatch,
    groupByReceiverSubscriberId,
  )
where

import qualified Beckn.ACL.Recon as ACLRecon
import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.Merchant (Merchant)
import Domain.Types.PPFRecon
import qualified Beckn.ACL.OnRecon as ACLOnRecon
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.PPF.PaymentStateMachine (validateSettlementStatusTransition)
import qualified SharedLogic.PPF as PPF
import qualified Storage.Queries.PPFRecon as QPPFRecon
import TransactionLogs.PushLogs (pushPPFLog)
import TransactionLogs.Types (KeyConfig, TokenConfig)

-- | Maximum number of pending recon entries to process per batch.
-- Prevents loading too many entries into memory for busy platforms.
reconBatchLimit :: Int
reconBatchLimit = 500

-- | Process on_recon response from BPP.
-- Called by the BAP (Collector) when receiving on_recon callback.
processOnRecon ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ACLOnRecon.DOnReconReq ->
  m ()
processOnRecon onReconReq = do
  forM_ onReconReq.orderReconResults $ \result -> do
    mbRecon <- QPPFRecon.findByNetworkOrderId result.orderId
    case mbRecon of
      Nothing -> logWarning $ "PPF Recon entry not found for orderId: " <> result.orderId
      Just recon -> do
        now <- getCurrentTime
        case result.status of
          "ACCEPTED" -> do
            -- Idempotency: skip if already past PENDING
            when (recon.settlementStatus == PENDING) $ do
              logInfo $ "PPF Recon ACCEPTED for orderId: " <> result.orderId
              validateSettlementStatusTransition recon.settlementStatus IN_PROGRESS
              QPPFRecon.updateReconStatusById
                IN_PROGRESS
                (Just now)
                (Just now)
                (Just "Recon accepted by receiver")
                recon.id
          "DISPUTED" -> do
            -- Idempotency: skip if already failed
            when (recon.settlementStatus /= SETTLEMENT_FAILED) $ do
              logWarning $ "PPF Recon DISPUTED for orderId: " <> result.orderId <> " message: " <> fromMaybe "" result.message
              validateSettlementStatusTransition recon.settlementStatus SETTLEMENT_FAILED
              QPPFRecon.updateReconStatusById
                SETTLEMENT_FAILED
                (Just $ fromMaybe now recon.reconInitiatedAt)
                (Just now)
                result.message
                recon.id
          "SETTLED" -> do
            -- Idempotency: skip if already settled
            when (recon.settlementStatus /= SETTLEMENT_SETTLED) $ do
              logInfo $ "PPF Recon SETTLED for orderId: " <> result.orderId <> " refNo: " <> fromMaybe "" result.settlementRefNo
              validateSettlementStatusTransition recon.settlementStatus SETTLEMENT_SETTLED
              QPPFRecon.updateSettlementStatusById
                SETTLEMENT_SETTLED
                result.settlementRefNo
                (Just now)
                (Just now)
                recon.id
          _ -> logWarning $ "Unknown recon status: " <> result.status <> " for orderId: " <> result.orderId

-- | Initiate a batch of recon requests.
-- Called by the scheduler job to process pending reconciliations.
-- Groups recon entries by BPP subscriber ID and sends recon to each.
initiateReconBatch ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  DBC.BecknConfig ->
  Id Merchant ->
  m [ReconSpec.ReconReq]
initiateReconBatch bapConfig merchantId = do
  now <- getCurrentTime
  pendingRecons <- QPPFRecon.findPendingReconciliation PENDING (Just merchantId)
  let limitedRecons = take reconBatchLimit pendingRecons
  if null limitedRecons
    then do
      logInfo "No pending PPF reconciliation entries found"
      pure []
    else do
      logInfo $ "Found " <> show (length limitedRecons) <> " pending PPF reconciliation entries"
      let grouped = groupByReceiverSubscriberId limitedRecons
      reconReqs <- forM grouped $ \(receiverSubId, entries) -> do
        logInfo $ "Building recon request for receiver: " <> receiverSubId <> " with " <> show (length entries) <> " entries"
        -- Mark entries as IN_PROGRESS
        forM_ entries $ \entry ->
          QPPFRecon.updateReconStatusById
            IN_PROGRESS
            (Just now)
            Nothing
            Nothing
            entry.id
        -- Build BECKN recon request
        reconReq <- ACLRecon.buildReconReq bapConfig entries
        -- Push Network Observability logs for each entry
        forM_ entries $ \entry -> do
          let noLog = PPF.buildPPFNetworkObservabilityLog entry
          pushPPFLog noLog merchantId.getId (show entry.domain)
        pure reconReq
      pure reconReqs

-- | Group recon entries by receiver subscriber ID.
-- Uses Map.fromListWith for O(n log n) performance.
groupByReceiverSubscriberId :: [PPFRecon] -> [(Text, [PPFRecon])]
groupByReceiverSubscriberId entries =
  Map.toList $ Map.fromListWith (<>) [(entry.receiverSubscriberId, [entry]) | entry <- entries]
