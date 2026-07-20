{-
  Prepaid Subscription :: PG_PAYOUT_SETTLEMENT ↔ PAYOUT_REQUEST.

  For every PG payout settlement report in the chunk:
    * expected = payout_request.amount (target)
    * actual   = report.txnAmount      (source)

  The join key is the payout_request id. Target status is mapped to
  'Lifecycle' on the source: CREDITED → Settled, INITIATED/PROCESSING/
  RETRYING/AUTO_PAY_FAILED → InFlight, FAILED/CANCELLED/CASH_* → Cancelled.
  In-flight rows classify to AWAITING_SETTLEMENT and are revisited by the
  B2 sweep instead of counted as breaks.

  If payout_request isn't found by ID we fall back to scheduled_payout,
  matching the old runner's behaviour via 'scheduledPayoutToPayoutRequest'.

  Ports doReconciliationPgPayoutVsPayoutRequest.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PrepaidPgPayoutVsPayoutRequest
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
import qualified Domain.Types.ScheduledPayout as ScheduledPayout
import Kernel.Prelude
import Kernel.Types.Id (Id (..), cast)
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.PgPayoutSettlementReport (PgPayoutSettlementReport)
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.PgPayoutSettlementReportExtra as QPgPayoutSettlement
import qualified Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import Storage.Beam.Payment ()
import qualified Storage.Queries.ScheduledPayoutExtra as QScheduledPayoutExtra

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = ReconT.ReconciliationSpec ReconT.PREPAID_SUBSCRIPTION ReconT.PG_PAYOUT_SETTLEMENT ReconT.PAYOUT_REQUEST,
      chunkPlan = ReconT.ByHour,
      -- External PG payout file: reports arrive T-1 to T-2.
      settlementBuffer = 2 * nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- Re-fetches PG payout reports by primary key and rebuilds the
      -- same SourceRecord shape 'fetchSources' produces. In-flight
      -- INITIATED / PROCESSING / RETRYING payouts flip to Settled once
      -- the PR reaches CREDITED, and the sweep is the mechanism that
      -- catches the transition.
      fetchSourcesByIds = fetchSourcesByIdsImpl,
      sweepInterval = nominalDay,
      maxOpenAge = 60 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = defaultClassify,
      syncSourceStatus = Nothing
    }

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
    QPgPayoutSettlement.findByTxnDateRangeAndStatus
      scope.merchantId
      scope.merchantOperatingCityId
      range.from
      range.to
  buildSourceRecords reports

-- | Shared builder used by both chunk ('fetchSources') and sweep
--   ('fetchSourcesByIds' below) paths — the sweep needs to rebuild the
--   exact same 'SourceRecord' shape from a re-fetch by primary key.
buildSourceRecords ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  [PgPayoutSettlementReport] ->
  m [ReconT.SourceRecord]
buildSourceRecords reports = do
  -- Bulk resolve payout requests (with scheduled_payout fallback) up-front.
  -- Two queries total instead of two per report.
  let prIds = mapMaybe (.payoutRequestId) reports & nub
  resolvedById <- resolvePayoutRequestsBulk prIds
  pure $
    flip map reports $ \report ->
      let mbValidPr = report.payoutRequestId >>= (`HM.lookup` resolvedById)
          -- Match on PR id whenever the PR was found (or resolved via
          -- scheduled_payout fallback). The old CREDITED + same-day guards
          -- collapsed in-flight and stale-reference cases into a single
          -- MISSING_IN_TARGET; splitting them out means we can revisit
          -- in-flight rows on the B2 sweep without polluting break counts.
          srcMatchKeyVal = (.id.getId) <$> mbValidPr
          lifecycle = maybe ReconT.Settled (statusToLifecycle . (.status)) mbValidPr
       in ReconT.SourceRecord
            { srcId = report.id.getId,
              srcEntityId = report.payoutRequestId,
              srcPartyId = Nothing,
              srcAmount = report.txnAmount,
              srcMatchKey = srcMatchKeyVal,
              srcComponent = Just "PG_PAYOUT_SETTLEMENT",
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
                      "paymentOrderId" .= report.payoutRequestId,
                      "settlementMode" .= (fmap show report.settlementMode :: Maybe Text)
                    ],
              srcTimestamp = fromMaybe report.createdAt (report.txnDate <|> report.settlementDate),
              srcLifecycle = lifecycle
            }

-- | Sweep re-fetch: bulk-lookup PG payout reports by their primary key
--   (== 'srcId' as populated by 'buildSourceRecords') and rebuild
--   'SourceRecord's. The payout_request status may have flipped from
--   PROCESSING → CREDITED since the chunk pass, so the lifecycle
--   re-derivation is the whole point of the sweep for this recipe.
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
  reports <- QPgPayoutSettlement.findByIds srcIds
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
fetchTargets _scope prIds = do
  resolvedById <- resolvePayoutRequestsBulk (HS.toList prIds)
  pure
    [ ReconT.TargetRecord
        { tgtId = pr.id.getId,
          tgtMatchKey = pr.id.getId,
          tgtAmount = fromMaybe 0 pr.amount,
          tgtMeta =
            Just $
              A.object
                [ "status" .= (show pr.status :: Text),
                  "createdAt" .= pr.createdAt,
                  "beneficiaryId" .= pr.beneficiaryId
                ],
          tgtSettlementId = Nothing,
          tgtSettlementDate = Nothing,
          tgtSettlementMode = Nothing,
          tgtRrn = Nothing,
          tgtTransactionDate = Just pr.createdAt
        }
      | pr <- HM.elems resolvedById
    ]

-- ─── Bulk resolver: payout_request → scheduled_payout ─────────────────
--
-- Two queries total per chunk: first fetch every PayoutRequest by id, then
-- for the ids that weren't found there, fetch ScheduledPayout and cast.
-- Both queries are single indexed WHERE id = ANY(?).

resolvePayoutRequestsBulk ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  [Text] ->
  m (HM.HashMap Text PayoutRequest.PayoutRequest)
resolvePayoutRequestsBulk [] = pure HM.empty
resolvePayoutRequestsBulk prIds = do
  prs <- QPayoutRequestExtra.findByIds prIds
  let prHits :: HM.HashMap Text PayoutRequest.PayoutRequest
      prHits = HM.fromList [(pr.id.getId, pr) | pr <- prs]
      missingIds = filter (not . (`HM.member` prHits)) prIds
  spFallbacks <-
    if null missingIds
      then pure []
      else QScheduledPayoutExtra.findByIds (map Id missingIds)
  let spHits :: HM.HashMap Text PayoutRequest.PayoutRequest
      spHits = HM.fromList [(sp.id.getId, scheduledPayoutToPayoutRequest sp) | sp <- spFallbacks]
  pure (prHits <> spHits)

scheduledPayoutToPayoutRequest :: ScheduledPayout.ScheduledPayout -> PayoutRequest.PayoutRequest
scheduledPayoutToPayoutRequest sp =
  PayoutRequest.PayoutRequest
    { id = cast sp.id,
      entityName = Nothing,
      entityId = sp.rideId,
      entityRefId = Just sp.bookingId,
      beneficiaryId = sp.driverId,
      amount = sp.amount,
      status = castScheduledStatus sp.status,
      retryCount = sp.retryCount,
      failureReason = sp.failureReason,
      payoutTransactionId = sp.payoutTransactionId,
      cashMarkedById = (.getId) <$> sp.markCashPaidBy,
      cashMarkedByName = Nothing,
      cashMarkedAt = Nothing,
      expectedCreditTime = sp.expectedCreditTime,
      scheduledAt = sp.expectedCreditTime,
      customerVpa = Nothing,
      customerPhone = Nothing,
      customerEmail = Nothing,
      customerName = Nothing,
      remark = Nothing,
      orderType = Nothing,
      city = Nothing,
      merchantId = maybe "" (.getId) sp.merchantId,
      merchantOperatingCityId = maybe "" (.getId) sp.merchantOperatingCityId,
      payoutFee = Nothing,
      payoutType = Nothing,
      coverageFrom = Nothing,
      coverageTo = Nothing,
      createdAt = sp.createdAt,
      updatedAt = sp.updatedAt,
      ledgerEntryIds = Nothing
    }

castScheduledStatus :: ScheduledPayout.ScheduledPayoutStatus -> PayoutRequest.PayoutRequestStatus
castScheduledStatus = \case
  ScheduledPayout.INITIATED -> PayoutRequest.INITIATED
  ScheduledPayout.PROCESSING -> PayoutRequest.PROCESSING
  ScheduledPayout.CREDITED -> PayoutRequest.CREDITED
  ScheduledPayout.AUTO_PAY_FAILED -> PayoutRequest.AUTO_PAY_FAILED
  ScheduledPayout.RETRYING -> PayoutRequest.RETRYING
  ScheduledPayout.FAILED -> PayoutRequest.FAILED
  ScheduledPayout.CANCELLED -> PayoutRequest.CANCELLED
  ScheduledPayout.CASH_PAID -> PayoutRequest.CASH_PAID
  ScheduledPayout.CASH_PENDING -> PayoutRequest.CASH_PENDING

-- | Terminal on the PG side means CREDITED; anything still working its way
--   through retry/auto-pay is in-flight and should not be reported as
--   MISSING_IN_TARGET. CASH_* moves the payout off the PG rail entirely, so
--   a PG report against such a PR is a MISMATCH — Cancelled treats it as
--   terminal for classify purposes but leaves the sum check to notice.
statusToLifecycle :: PayoutRequest.PayoutRequestStatus -> ReconT.Lifecycle
statusToLifecycle = \case
  PayoutRequest.CREDITED -> ReconT.Settled
  PayoutRequest.INITIATED -> ReconT.InFlight
  PayoutRequest.PROCESSING -> ReconT.InFlight
  PayoutRequest.RETRYING -> ReconT.InFlight
  PayoutRequest.AUTO_PAY_FAILED -> ReconT.Cancelled
  PayoutRequest.FAILED -> ReconT.Cancelled
  PayoutRequest.CANCELLED -> ReconT.Cancelled
  PayoutRequest.CASH_PAID -> ReconT.Cancelled
  PayoutRequest.CASH_PENDING -> ReconT.Cancelled
