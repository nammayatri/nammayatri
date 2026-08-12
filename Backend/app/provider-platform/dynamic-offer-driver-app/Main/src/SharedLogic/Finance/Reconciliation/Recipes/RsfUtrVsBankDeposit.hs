{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.RsfUtrVsBankDeposit
  ( recipe,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as A
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Time (nominalDay)
import Kernel.Prelude
import Kernel.Types.Common as KTC
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as RUS
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlement as QRUS
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlementExtra as QRUSExtra

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = mySpec,
      chunkPlan = ReconT.ByDay,
      settlementBuffer = 2 * nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      fetchSourcesByIds = fetchSourcesById,
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = defaultClassify,
      syncSourceStatus = Just syncUtrStatus
    }
  where
    mySpec = ReconT.ReconciliationSpec ReconT.ONDC_RSF ReconT.RSF_CLAIM ReconT.BANK_DEPOSIT

fetchSources ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope range = do
  rsoRows <- QRSOExtra.findByMerchantIdAndReceivedAtRange scope.merchantId range.from range.to
  rsoRowsToSourceRecords rsoRows

fetchSourcesById ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  [Text] ->
  m [ReconT.SourceRecord]
fetchSourcesById _scope utrIds = do
  rsoRows <- QRSOExtra.findByUtrSettlementIds utrIds
  rsoRowsToSourceRecords rsoRows

rsoRowsToSourceRecords ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  [RSO.ReconSettlementOrder] ->
  m [ReconT.SourceRecord]
rsoRowsToSourceRecords rsoRows = do
  let grouped :: Map.Map (Maybe (Id RUS.ReconUtrSettlement)) [RSO.ReconSettlementOrder]
      grouped = Map.fromListWith (<>) [(r.utrSettlementId, [r]) | r <- rsoRows]

  pure . catMaybes $
    flip map (Map.toList grouped) $ \(mbUtrId, rows) ->
      case mbUtrId of
        Nothing -> Nothing
        Just utrId ->
          Just $
            let totalClaimed = sum [r.claimedSettlementAmount | r <- rows]
                sortedOrders = L.sortOn (.orderSequence) rows
                orderIds = map (\r -> getId r.id) sortedOrders
                firstRow = head rows
                meta =
                  A.object
                    [ "totalClaimed" .= totalClaimed,
                      "utrId" .= getId utrId,
                      "orderIds" .= orderIds
                    ]
             in ReconT.SourceRecord
                  { srcId = getId utrId,
                    srcEntityId = Just (getId utrId),
                    srcPartyId = Nothing,
                    srcAmount = totalClaimed,
                    srcMatchKey = Just (getId utrId),
                    srcComponent = Nothing,
                    srcMeta = Just meta,
                    srcTimestamp = firstRow.receivedAt,
                    srcLifecycle = ReconT.Settled
                  }

fetchTargets ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope utrIds = do
  let ids = map Id (HS.toList utrIds)
  utrs <- QRUSExtra.findByIds ids
  pure
    [ ReconT.TargetRecord
        { tgtId = getId utr.id,
          tgtMatchKey = getId utr.id,
          tgtAmount = fromMaybe 0 utr.bankVerifiedAmount,
          tgtMeta = Nothing,
          tgtSettlementId = Nothing,
          tgtSettlementDate = Just utr.createdAt,
          tgtSettlementMode = Nothing,
          tgtRrn = Just utr.utr,
          tgtTransactionDate = Just utr.createdAt
        }
      | utr <- utrs
    ]

syncUtrStatus ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.SourceRecord ->
  ReconT.ReconciliationStatus ->
  m ()
syncUtrStatus src _status = do
  let meta = src.srcMeta
      utrIdText = meta >>= extractText "utrId"

  case utrIdText of
    Nothing -> pure ()
    Just uid -> do
      orders <- QRSO.findByUtrSettlementId (Just (Id uid))
      utr <- QRUS.findById (Id uid)
      case utr of
        Nothing -> pure ()
        Just u -> do
          let bankVerifiedAmount = fromMaybe 0 u.bankVerifiedAmount
              isLocked rso = rso.reconciliationStatus == Just "SENT" || isJust rso.manuallyConfirmedAt
              (locked, open) = L.partition isLocked orders
              consumed rso = rso.claimedSettlementAmount - fromMaybe 0 rso.diffAmount
              consumedByLocked = sum (map consumed locked)
              remaining = bankVerifiedAmount - consumedByLocked
              sortedOpen = L.sortOn (.orderSequence) open
              openClaimedTotal = sum (map (.claimedSettlementAmount) sortedOpen)
              diff = openClaimedTotal - remaining

          -- Only an anomaly when locked rows are the ones that can no longer be covered up
          when (consumedByLocked > 0 && remaining < 0) $
            logWarning $
              "RSF bank-verify: UTR " <> u.utr
                <> " locked rows already consumed "
                <> show consumedByLocked
                <> " against bankVerifiedAmount="
                <> show bankVerifiedAmount
                <> " (remaining="
                <> show remaining
                <> ") -- every open order will show full shortfall"

          if diff == 0
            then forM_ sortedOpen $ \rso -> QRSOExtra.updateReconVerdict rso.id RSO.PAID (Just 0)
            else
              if diff > 0
                then applyUnderpaidWaterfall (reverse sortedOpen) diff
                else case sortedOpen of
                  (headOrder : _) -> QRSOExtra.updateReconVerdict headOrder.id RSO.OVERPAID (Just diff)
                  [] -> pure ()

applyUnderpaidWaterfall :: (BeamFlow m r) => [RSO.ReconSettlementOrder] -> KTC.HighPrecMoney -> m ()
applyUnderpaidWaterfall [] _ = pure ()
applyUnderpaidWaterfall _ remaining | remaining <= 0 = pure ()
applyUnderpaidWaterfall (order : rest) remaining = do
  let orderAmt = order.claimedSettlementAmount
  if orderAmt <= remaining
    then do
      QRSOExtra.updateReconVerdict order.id RSO.UNDERPAID (Just orderAmt)
      applyUnderpaidWaterfall rest (remaining - orderAmt)
    else QRSOExtra.updateReconVerdict order.id RSO.UNDERPAID (Just remaining)

extractText :: Text -> A.Value -> Maybe Text
extractText key (A.Object o) = case A.parseMaybe (\_ -> o A..:? AK.fromText key) () of
  Just v -> v
  Nothing -> Nothing
extractText _ _ = Nothing
