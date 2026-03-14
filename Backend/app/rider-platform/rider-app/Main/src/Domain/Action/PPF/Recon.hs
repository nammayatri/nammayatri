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

import qualified Beckn.ACL.OnRecon as ACLOnRecon
import qualified Beckn.ACL.Recon as ACLRecon
import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.PPFRecon
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.PPFRecon as QPPFRecon
import TransactionLogs.PushLogs (pushLogs)
import TransactionLogs.Types (KeyConfig, TokenConfig)

reconBatchLimit :: Int
reconBatchLimit = 500

-- | Process on_recon response from BPP.
-- Per ONDC NTS RSF 2.0.0: recon_accord=true means accepted, false means disputed.
processOnRecon ::
  ( BeamFlow.BeamFlow m r
  ) =>
  ACLOnRecon.DOnReconReq ->
  m ()
processOnRecon onReconReq = do
  forM_ onReconReq.orderReconResults $ \result -> do
    mbRecon <- QPPFRecon.findByNetworkOrderId result.orderId
    case mbRecon of
      Nothing -> logWarning $ "PPF Recon entry not found for orderId: " <> result.orderId
      Just recon -> do
        if result.reconAccord
          then do
            when (recon.settlementStatus == PENDING) $ do
              logInfo $ "PPF Recon accepted for orderId: " <> result.orderId
              QPPFRecon.updateSettlementStatusById TO_BE_INITIATED recon.id
          else do
            when (recon.settlementStatus /= PENDING) $ do
              logWarning $ "PPF Recon disputed for orderId: " <> result.orderId
              QPPFRecon.updateSettlementStatusById PENDING recon.id

-- | Initiate a batch of recon requests.
-- Groups recon entries by BPP subscriber ID and sends recon to each.
initiateReconBatch ::
  ( BeamFlow.BeamFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  DBC.BecknConfig ->
  Id Merchant ->
  m [ReconSpec.ReconReq]
initiateReconBatch bapConfig merchantId = do
  pendingRecons <- QPPFRecon.findPendingReconciliation PENDING merchantId.getId
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
        forM_ entries $ \entry ->
          QPPFRecon.updateSettlementStatusById TO_BE_INITIATED entry.id
        reconReq <- ACLRecon.buildReconReq bapConfig entries
        void $ pushLogs "recon" (A.toJSON reconReq) merchantId.getId "MOBILITY"
        pure reconReq
      pure reconReqs

groupByReceiverSubscriberId :: [PPFRecon] -> [(Text, [PPFRecon])]
groupByReceiverSubscriberId entries =
  Map.toList $ Map.fromListWith (<>) [(entry.receiverSubscriberId, [entry]) | entry <- entries]
