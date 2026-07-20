{-
  Prepaid Subscription :: PG_PAYMENT_SETTLEMENT ↔ SUBSCRIPTION_PURCHASE.

  For every PG payment settlement report in the chunk:
    * expected = subscription.planFee (target)
    * actual   = report.txnAmount     (source)

  The join key is the subscription id. Subscription status is mapped to
  'Lifecycle' on the source: ACTIVE/EXPIRED/EXHAUSTED → Settled, PENDING
  → InFlight, FAILED → Cancelled. In-flight rows classify to
  AWAITING_SETTLEMENT and are revisited by the B2 sweep instead of being
  reported as MISSING_IN_TARGET while the subscription is still resolving.

  Ports doReconciliationPgPaymentVsSubscription.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PrepaidPgPaymentVsSubscription
  ( recipe,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (nub)
import Data.Time (nominalDay)
import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.PgPaymentSettlementReport (PgPaymentSettlementReport)
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra as QPgPaymentSettlement
import qualified SharedLogic.Finance.Reconciliation.EntitySync as EntitySync
import SharedLogic.Finance.Reconciliation.SubscriptionMeta (subscriptionMetaPairs)
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubPurchaseExtra

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
      chunkPlan = ReconT.ByHour,
      -- External PG settlement file: reports arrive T-1 to T-2. Two-day
      -- buffer keeps us well clear of files still being received.
      settlementBuffer = 2 * nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- Re-fetches PG payment reports by primary key and rebuilds the
      -- same SourceRecord shape 'fetchSources' produces. PENDING subs
      -- flip to ACTIVE/EXPIRED once the purchase settles, and the sweep
      -- is the mechanism that catches the transition without waiting
      -- for a new chunk.
      fetchSourcesByIds = fetchSourcesByIdsImpl,
      sweepInterval = nominalDay,
      maxOpenAge = 60 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = defaultClassify,
      syncSourceStatus = Just (EntitySync.syncSubscriptionStatus mySpec)
    }
  where
    mySpec = ReconT.ReconciliationSpec ReconT.PREPAID_SUBSCRIPTION ReconT.PG_PAYMENT_SETTLEMENT ReconT.SUBSCRIPTION_PURCHASE

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
  reports <-
    QPgPaymentSettlement.findByTxnDateRangeAndStatus
      scope.merchantId
      scope.merchantOperatingCityId
      range.from
      range.to
  buildSourceRecords reports

-- | Shared builder for both the chunk pass ('fetchSources') and the
--   sweep re-fetch ('fetchSourcesByIdsImpl'). Both need the same source-
--   record shape and the same status → lifecycle mapping; extracting
--   this keeps them from drifting.
buildSourceRecords ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  [PgPaymentSettlementReport] ->
  m [ReconT.SourceRecord]
buildSourceRecords reports = do
  -- Bulk-fetch every referenced subscription in one indexed query.
  -- Subscription status → source Lifecycle (no separate gate below).
  let subIds = mapMaybe (.referenceId) reports & nub
  subs <- QSubPurchaseExtra.findByIds subIds
  let subsById :: HM.HashMap Text DSP.SubscriptionPurchase
      subsById = HM.fromList [(s.id.getId, s) | s <- subs]
  pure $
    flip map reports $ \report ->
      let rid = report.id.getId
          mbSub = report.referenceId >>= (`HM.lookup` subsById)
          srcMatchKeyVal = (.id.getId) <$> mbSub
          lifecycle = maybe ReconT.Settled (statusToLifecycle . (.status)) mbSub
       in ReconT.SourceRecord
            { srcId = rid,
              srcEntityId = report.referenceId,
              srcPartyId = Nothing,
              srcAmount = report.txnAmount,
              srcMatchKey = srcMatchKeyVal,
              srcComponent = Just "PG_PAYMENT_SETTLEMENT",
              srcMeta =
                Just $
                  A.object
                    [ "settlementId" .= report.settlementId,
                      "txnDate" .= report.txnDate,
                      "pgTransactionDate" .= report.txnDate,
                      "rrn" .= report.rrn,
                      "utr" .= report.utr,
                      "pgOrderId" .= report.orderId,
                      "pgTxnId" .= report.txnId,
                      "paymentOrderId" .= report.subscriptionPurchaseId,
                      "paymentMethod" .= (fmap show report.paymentMethod :: Maybe Text)
                    ],
              srcTimestamp = fromMaybe report.createdAt (report.txnDate <|> report.settlementDate),
              srcLifecycle = lifecycle
            }

-- | Sweep re-fetch: bulk-lookup PG payment reports by primary key and
--   rebuild source records via the shared builder. Subscription status
--   may have flipped from PENDING → ACTIVE since the chunk pass, so the
--   lifecycle re-derivation is the whole point of the sweep for this
--   recipe.
fetchSourcesByIdsImpl ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  [Text] ->
  m [ReconT.SourceRecord]
fetchSourcesByIdsImpl _scope srcIds = do
  reports <- QPgPaymentSettlement.findByIds srcIds
  buildSourceRecords reports

fetchTargets ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope subIds = do
  -- Single bulk fetch replaces the per-id findByPrimaryKey loop.
  subs <- QSubPurchaseExtra.findByIds (HS.toList subIds)
  forM subs $ \sub -> do
    extraPairs <- subscriptionMetaPairs sub
    pure $
      ReconT.TargetRecord
        { tgtId = sub.id.getId,
          tgtMatchKey = sub.id.getId,
          tgtAmount = sub.planFee,
          tgtMeta =
            Just . A.object $
              [ "status" .= (show sub.status :: Text),
                "purchaseTimestamp" .= sub.purchaseTimestamp,
                "planRideCredit" .= sub.planRideCredit
              ]
                <> extraPairs,
          tgtSettlementId = Nothing,
          tgtSettlementDate = Nothing,
          tgtSettlementMode = Nothing,
          tgtRrn = Nothing,
          tgtTransactionDate = Just sub.purchaseTimestamp
        }

-- | ACTIVE/EXPIRED/EXHAUSTED mean the subscription has been paid for and
--   accepted; PENDING means the purchase is still resolving (PG report may
--   arrive before we've flipped the row); FAILED means the subscription
--   was declined and any PG report on it is a real MISMATCH.
statusToLifecycle :: DSP.SubscriptionPurchaseStatus -> ReconT.Lifecycle
statusToLifecycle = \case
  DSP.ACTIVE -> ReconT.Settled
  DSP.EXPIRED -> ReconT.Settled
  DSP.EXHAUSTED -> ReconT.Settled
  DSP.PENDING -> ReconT.InFlight
  DSP.FAILED -> ReconT.Cancelled
