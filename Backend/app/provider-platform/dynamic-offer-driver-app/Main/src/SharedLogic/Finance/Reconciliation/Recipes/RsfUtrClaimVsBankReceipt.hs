{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.RsfUtrClaimVsBankReceipt
  ( recipe,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Types as A
import qualified Data.HashSet as HS
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Time (nominalDay)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import Lib.Finance.Reconciliation.Recipe (Recipe (..))
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Storage.Queries.RsfLedgerAggregates as QAgg

-- Phase 2, stage C (check 5): a UTR already known to us must still have the money to cover the
-- new claims the message puts on it. Source = Σ this message's PENDING legs on the UTR; target =
-- what the bank credited that is not yet allocated (received − allocated, one GROUP BY). Target
-- below source rejects those legs as REJECTED_UTR_IMBALANCE. The recipe is built per message so a
-- concurrent pass for another message never has its legs counted or rejected here.
recipe :: (BeamFlow m r) => Text -> Recipe m
recipe messageId =
  Recipe
    { spec = ReconT.ReconciliationSpec ReconT.ONDC_RSF ReconT.RSF_CLAIM ReconT.BANK_DEPOSIT,
      chunkPlan = ReconT.ByDay,
      settlementBuffer = 2 * nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources messageId,
      fetchTargetsById = fetchTargets,
      fetchSourcesByIds = fetchSourcesById messageId,
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = poolCovers,
      syncSourceStatus = Just syncClaimStatus
    }

fetchSources :: (BeamFlow m r) => Text -> ReconT.MerchantScope -> ReconT.DateRange -> m [ReconT.SourceRecord]
fetchSources messageId scope range = do
  claims <- QLedger.findClaimsByMerchantStatusAndCreatedAtRange scope.merchantId L.PENDING range.from range.to
  fetchSourcesById messageId scope (nub $ mapMaybe (.utr) claims)

-- One source per UTR: this message's PENDING legs on it.
fetchSourcesById :: (BeamFlow m r) => Text -> ReconT.MerchantScope -> [Text] -> m [ReconT.SourceRecord]
fetchSourcesById messageId scope utrs = do
  entries <- QLedger.findAllByMerchantAndMessageId scope.merchantId messageId
  let wanted = HS.fromList utrs
      pending = [c | c <- entries, c.entryType == L.BAP_CLAIM, c.claimStatus == Just L.PENDING, maybe False (`HS.member` wanted) c.utr]
      byUtr = Map.fromListWith (<>) [(u, [c]) | c <- pending, Just u <- [c.utr]]
  pure
    [ ReconT.SourceRecord
        { srcId = utr,
          srcEntityId = Just utr,
          srcPartyId = Nothing,
          srcAmount = sum (map (.amount) claims),
          srcMatchKey = Just utr,
          srcComponent = Nothing,
          srcMeta = Just $ A.object ["newClaims" .= sum (map (.amount) claims), "pendingClaimIds" .= map (.id.getId) claims],
          srcTimestamp = minimum (map (.createdAt) claims),
          srcLifecycle = ReconT.Settled
        }
      | (utr, claims) <- Map.toList byUtr
    ]

-- The target is the UTR's unallocated money: receipt + adjustments − allocations, from one GROUP BY.
fetchTargets :: (BeamFlow m r) => ReconT.MerchantScope -> HS.HashSet Text -> m [ReconT.TargetRecord]
fetchTargets scope utrs = do
  sums <- QAgg.sumByUtrAndEntryType scope.merchantId [L.BANK_RECEIPT, L.MANUAL_ADJUSTMENT, L.BANK_ALLOCATION] (HS.toList utrs)
  now <- getCurrentTime
  let amount u t = fromMaybe 0 (Map.lookup (u, t) sums)
  pure
    [ ReconT.TargetRecord
        { tgtId = utr,
          tgtMatchKey = utr,
          tgtAmount = amount utr L.BANK_RECEIPT + amount utr L.MANUAL_ADJUSTMENT - amount utr L.BANK_ALLOCATION,
          tgtMeta = Nothing,
          tgtSettlementId = Nothing,
          tgtSettlementDate = Just now,
          tgtSettlementMode = Nothing,
          tgtRrn = Just utr,
          tgtTransactionDate = Just now
        }
      | utr <- HS.toList utrs,
        Map.member (utr, L.BANK_RECEIPT) sums
    ]

-- Covered when the unallocated money is at least the new claims; otherwise the claims exceed it.
poolCovers :: [ReconT.SourceRecord] -> [ReconT.TargetRecord] -> ReconT.ReconResult
poolCovers srcs tgts
  | null srcs = ReconT.ReconResult ReconT.MISSING_IN_SOURCE (Just "No pending claims on this UTR")
  | null tgts = ReconT.ReconResult ReconT.MISSING_IN_TARGET (Just "UTR has no bank receipt")
  | available >= newClaims = ReconT.ReconResult ReconT.MATCHED Nothing
  | otherwise = ReconT.ReconResult ReconT.LOWER_IN_TARGET (Just $ "UTR_IMBALANCE: new claims " <> show newClaims <> " exceed unallocated " <> show available)
  where
    newClaims = sum (map (.srcAmount) srcs)
    available = sum (map (.tgtAmount) tgts)

-- Write-back: anything but MATCHED rejects the UTR's PENDING claims.
syncClaimStatus :: (BeamFlow m r) => ReconT.SourceRecord -> ReconT.ReconciliationStatus -> m ()
syncClaimStatus src status =
  unless (status `elem` [ReconT.MATCHED, ReconT.AWAITING_SETTLEMENT]) $ do
    let claimIds = fromMaybe [] (src.srcMeta >>= extractField "pendingClaimIds") :: [Text]
    QLedger.updateClaimStatusByIds (map Id claimIds) L.REJECTED_UTR_IMBALANCE Nothing

extractField :: (A.FromJSON a) => Text -> A.Value -> Maybe a
extractField key (A.Object o) = AKM.lookup (AK.fromText key) o >>= A.parseMaybe A.parseJSON
extractField _ _ = Nothing
