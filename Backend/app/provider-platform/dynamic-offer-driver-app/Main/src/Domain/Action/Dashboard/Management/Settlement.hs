{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Settlement
  ( getSettlementSummary,
    listSettlements,
    getSettlementDetails,
    listChargebacks,
    respondToChargeback,
    getSettlementTrend,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.Settlement as API
import qualified Dashboard.Common
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, diffUTCTime, nominalDay)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (UTCTime, identity)
import qualified Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), ShortId (..))
import Kernel.Utils.Common (MonadFlow, fromMaybeM, generateGUID, getCurrentTime, logInfo)
import qualified Lib.Finance.Domain.Types.Chargeback as Chargeback
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as PgSettlement
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconEntry
import qualified Lib.Finance.Storage.Queries.Chargeback as QChargeback
import qualified Lib.Finance.Storage.Queries.ChargebackExtra as QChargebackExtra
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReport as QPgSettlement
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra as QPgSettlementExtra
import qualified Lib.Finance.Storage.Queries.ReconciliationEntryExtra as QReconEntryExtra
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

-- ---------------------------------------------------------------------------
-- Pagination helpers
-- ---------------------------------------------------------------------------

defaultPageLimit :: Int
defaultPageLimit = 20

maxPageLimit :: Int
maxPageLimit = 100

mkPageLimit :: Maybe Int -> Int
mkPageLimit = min maxPageLimit . max 0 . fromMaybe defaultPageLimit

mkPageOffset :: Maybe Int -> Int
mkPageOffset = max 0 . fromMaybe 0

-- ---------------------------------------------------------------------------
-- 1. getSettlementSummary
-- ---------------------------------------------------------------------------

getSettlementSummary ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Flow API.SettlementSummaryRes
getSettlementSummary merchantShortId opCity mbDateFrom mbDateTo mbGateway = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let dateTo = fromMaybe now mbDateTo
      dateFrom = fromMaybe (addUTCTime (negate $ 30 * nominalDay) now) mbDateFrom

  logInfo $ "Settlement summary request: " <> show dateFrom <> " to " <> show dateTo

  -- Fetch all settlement reports for the date range
  reports <- QPgSettlementExtra.findAllByMerchantOpCityIdWithFilters
    merchant.id.getId
    merchantOpCityId.getId
    (Just dateFrom)
    (Just dateTo)
    Nothing -- subscriptionPurchaseId
    Nothing -- orderId
    Nothing -- settlementId
    Nothing -- txnType
    Nothing -- pgApprovalCode
    Nothing -- utr
    Nothing -- settlementFrom
    Nothing -- settlementTo
    (mbGateway) -- pgName
    Nothing -- settlementAmountMin
    Nothing -- settlementAmountMax
    Nothing -- txnAmountMin
    Nothing -- txnAmountMax
    Nothing -- limit (all)
    Nothing -- offset

  -- Aggregate totals
  let settledReports = filter (\r -> r.reconStatus == PgSettlement.MATCHED || r.reconStatus == PgSettlement.SETTLED) reports
      pendingReports = filter (\r -> r.reconStatus == PgSettlement.PENDING) reports
      failedReports = filter (\r -> r.txnStatus == PgSettlement.FAILED) reports
      disputedReports = filter (\r -> isJust r.disputeId) reports

      totalSettledAmount = sum $ map (.settlementAmount) settledReports
      totalPendingAmount = sum $ map (.settlementAmount) pendingReports
      totalFailedAmount = sum $ map (.txnAmount) failedReports
      totalDisputedAmount = sum $ mapMaybe (.chargebackAmount) disputedReports

      totalCount = length settledReports
      pendingCount = length pendingReports
      failedCount = length failedReports
      disputedCount = length disputedReports

      totalAll = totalCount + pendingCount + failedCount
      rate = if totalAll > 0 then T.pack $ show (round @Double @Int $ (fromIntegral totalCount / fromIntegral totalAll) * 100) <> "%" else "N/A"

  -- Gateway breakdown
  let gatewayMap = foldl' groupByGateway Map.empty reports
      gatewayBreakdown = map mkGatewaySummary (Map.toList gatewayMap)

  pure $
    API.SettlementSummaryRes
      { totalSettledAmount = totalSettledAmount,
        totalPendingAmount = totalPendingAmount,
        totalFailedAmount = totalFailedAmount,
        totalDisputedAmount = totalDisputedAmount,
        totalSettlementCount = totalCount,
        pendingCount = pendingCount,
        failedCount = failedCount,
        disputedCount = disputedCount,
        settlementRate = rate,
        gatewayBreakdown = gatewayBreakdown
      }
  where
    groupByGateway :: Map.Map Text [PgSettlement.PgPaymentSettlementReport] -> PgSettlement.PgPaymentSettlementReport -> Map.Map Text [PgSettlement.PgPaymentSettlementReport]
    groupByGateway acc report =
      let gw = fromMaybe "UNKNOWN" report.paymentGateway
       in Map.insertWith (<>) gw [report] acc

    mkGatewaySummary :: (Text, [PgSettlement.PgPaymentSettlementReport]) -> API.GatewaySettlementSummary
    mkGatewaySummary (gw, gwReports) =
      let settled = filter (\r -> r.reconStatus == PgSettlement.MATCHED || r.reconStatus == PgSettlement.SETTLED) gwReports
          pending = filter (\r -> r.reconStatus == PgSettlement.PENDING) gwReports
          failed = filter (\r -> r.txnStatus == PgSettlement.FAILED) gwReports
       in API.GatewaySettlementSummary
            { gateway = gw,
              settledAmount = sum $ map (.settlementAmount) settled,
              pendingAmount = sum $ map (.settlementAmount) pending,
              failedAmount = sum $ map (.txnAmount) failed,
              transactionCount = length gwReports
            }

-- ---------------------------------------------------------------------------
-- 2. listSettlements
-- ---------------------------------------------------------------------------

listSettlements ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe API.SettlementStatusFilter ->
  Flow API.SettlementListRes
listSettlements merchantShortId opCity mbAmountMax mbAmountMin mbDateFrom mbDateTo mbGateway mbLimit mbOffset mbSearchQuery _mbStatus = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Use the searchQuery to look up by orderId, txnId, rrn, or settlementId
  let (mbOrderId, mbSettlementId, mbUtr) = case mbSearchQuery of
        Just q -> (Just q, Just q, Nothing) -- simplified: search by orderId first
        Nothing -> (Nothing, Nothing, Nothing)

  reports <- QPgSettlementExtra.findAllByMerchantOpCityIdWithFilters
    merchant.id.getId
    merchantOpCityId.getId
    mbDateFrom
    mbDateTo
    Nothing -- subscriptionPurchaseId
    mbOrderId
    mbSettlementId
    Nothing -- txnType
    Nothing -- pgApprovalCode
    mbUtr
    Nothing -- settlementFrom
    Nothing -- settlementTo
    mbGateway
    mbAmountMin
    mbAmountMax
    Nothing -- txnAmountMin
    Nothing -- txnAmountMax
    (Just limit)
    (Just offset)

  items <- mapM mkSettlementListItem reports

  let totalItems = length items
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  pure $
    API.SettlementListRes
      { totalItems = totalItems,
        summary = summary,
        settlements = items
      }

mkSettlementListItem :: PgSettlement.PgPaymentSettlementReport -> Flow API.SettlementListItem
mkSettlementListItem report = do
  let hasChargeback = isJust report.chargebackId
  pure $
    API.SettlementListItem
      { settlementReportId = report.id.getId,
        orderId = report.orderId,
        txnId = report.txnId,
        rrn = report.rrn,
        txnType = show report.txnType,
        txnStatus = show report.txnStatus,
        txnAmount = report.txnAmount,
        settlementAmount = report.settlementAmount,
        pgBaseFee = report.pgBaseFee,
        pgTax = report.pgTax,
        paymentGateway = report.paymentGateway,
        paymentMethod = show <$> report.paymentMethod,
        settlementId = report.settlementId,
        settlementDate = report.settlementDate,
        settlementStatus = show <$> report.settlementType,
        reconStatus = show report.reconStatus,
        hasChargeback = hasChargeback,
        chargebackAmount = report.chargebackAmount,
        txnDate = report.txnDate,
        createdAt = report.createdAt
      }

-- ---------------------------------------------------------------------------
-- 3. getSettlementDetails
-- ---------------------------------------------------------------------------

getSettlementDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Dashboard.Common.PGPaymentSettlementReport ->
  Flow API.SettlementDetailsRes
getSettlementDetails merchantShortId opCity settlementId = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)

  report <- QPgSettlement.findById (Id settlementId.getId) >>= fromMaybeM (InvalidRequest "Settlement report not found")

  -- Build detailed report
  let reportDetail = mkSettlementReportDetail report

  -- Get linked chargebacks
  chargebacks <- QChargeback.findBySettlementReportId (Id settlementId.getId)
  let chargebackItems = map mkChargebackItem chargebacks

  -- Get linked reconciliation entries
  let mbSettId = report.settlementId
  reconEntries <- case mbSettId of
    Just settId -> QReconEntryExtra.findBySettlementId settId
    Nothing -> pure []
  let reconItems = map mkReconEntry reconEntries

  pure $
    API.SettlementDetailsRes
      { settlementReport = reportDetail,
        linkedChargebacks = chargebackItems,
        reconEntries = reconItems
      }

mkSettlementReportDetail :: PgSettlement.PgPaymentSettlementReport -> API.SettlementReportDetail
mkSettlementReportDetail report =
  API.SettlementReportDetail
    { id = report.id.getId,
      orderId = report.orderId,
      txnId = report.txnId,
      rrn = report.rrn,
      utr = report.utr,
      txnType = show report.txnType,
      txnStatus = show report.txnStatus,
      txnDate = report.txnDate,
      txnAmount = report.txnAmount,
      pgBaseFee = report.pgBaseFee,
      pgTax = report.pgTax,
      settlementAmount = report.settlementAmount,
      paymentGateway = report.paymentGateway,
      paymentMethod = show <$> report.paymentMethod,
      paymentMethodSubType = report.paymentMethodSubType,
      settlementType = show <$> report.settlementType,
      settlementMode = show <$> report.settlementMode,
      settlementId = report.settlementId,
      settlementDate = report.settlementDate,
      referenceId = report.referenceId,
      referenceType = report.referenceType,
      disputeId = report.disputeId,
      disputeType = show <$> report.disputeType,
      reconStatus = show report.reconStatus,
      reconMessage = report.reconMessage,
      merchantId = report.merchantId,
      createdAt = report.createdAt,
      updatedAt = report.updatedAt
    }

mkReconEntry :: ReconEntry.ReconciliationEntry -> API.SettlementReconEntry
mkReconEntry entry =
  API.SettlementReconEntry
    { reconEntryId = entry.id.getId,
      bookingId = entry.bookingId,
      expectedValue = entry.expectedDsrValue,
      actualValue = entry.actualLedgerValue,
      variance = entry.variance,
      reconStatus = show entry.reconStatus,
      mismatchReason = entry.mismatchReason
    }

-- ---------------------------------------------------------------------------
-- 4. listChargebacks
-- ---------------------------------------------------------------------------

listChargebacks ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe API.ChargebackStatusFilter ->
  Flow API.ChargebackListRes
listChargebacks merchantShortId opCity mbDateFrom mbDateTo mbLimit mbOffset mbStatusFilter = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset
      mbStatus = mbStatusFilter >>= chargebackStatusFilterToStatus

  chargebacks <- QChargebackExtra.findAllByMerchantWithFilters
    merchant.id.getId
    merchantOpCityId.getId
    mbStatus
    mbDateFrom
    mbDateTo
    (Just limit)
    (Just offset)

  let items = map mkChargebackItem chargebacks
      totalItems = length items
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  -- Count by status
  openCount <- QChargebackExtra.countByMerchantAndStatus merchant.id.getId merchantOpCityId.getId Chargeback.OPEN
  evidenceSubmittedCount <- QChargebackExtra.countByMerchantAndStatus merchant.id.getId merchantOpCityId.getId Chargeback.EVIDENCE_SUBMITTED
  wonCount <- QChargebackExtra.countByMerchantAndStatus merchant.id.getId merchantOpCityId.getId Chargeback.WON
  lostCount <- QChargebackExtra.countByMerchantAndStatus merchant.id.getId merchantOpCityId.getId Chargeback.LOST
  expiredCount <- QChargebackExtra.countByMerchantAndStatus merchant.id.getId merchantOpCityId.getId Chargeback.EXPIRED

  pure $
    API.ChargebackListRes
      { totalItems = totalItems,
        summary = summary,
        chargebacks = items,
        statusCounts =
          API.ChargebackStatusCounts
            { openCount = openCount,
              evidenceSubmittedCount = evidenceSubmittedCount,
              wonCount = wonCount,
              lostCount = lostCount,
              expiredCount = expiredCount
            }
      }

chargebackStatusFilterToStatus :: API.ChargebackStatusFilter -> Maybe Chargeback.ChargebackStatus
chargebackStatusFilterToStatus = \case
  API.AllChargebacks -> Nothing
  API.ChargebackOpen -> Just Chargeback.OPEN
  API.ChargebackEvidenceSubmitted -> Just Chargeback.EVIDENCE_SUBMITTED
  API.ChargebackWon -> Just Chargeback.WON
  API.ChargebackLost -> Just Chargeback.LOST
  API.ChargebackExpired -> Just Chargeback.EXPIRED

mkChargebackItem :: Chargeback.Chargeback -> API.ChargebackItem
mkChargebackItem cb =
  API.ChargebackItem
    { chargebackId = cb.id.getId,
      settlementReportId = cb.settlementReportId.getId,
      transactionId = cb.transactionId,
      chargebackReasonCode = cb.chargebackReasonCode,
      chargebackAmount = cb.chargebackAmount,
      chargebackStatus = show cb.chargebackStatus,
      responseDeadline = cb.responseDeadline,
      evidenceUrl = cb.evidenceUrl,
      adminNotes = cb.adminNotes,
      merchantId = cb.merchantId,
      createdAt = cb.createdAt,
      updatedAt = cb.updatedAt
    }

-- ---------------------------------------------------------------------------
-- 5. respondToChargeback
-- ---------------------------------------------------------------------------

respondToChargeback ::
  ShortId DM.Merchant ->
  Context.City ->
  Text -> -- chargebackId
  API.ChargebackRespondReq ->
  Flow API.ChargebackRespondRes
respondToChargeback merchantShortId opCity chargebackId req = do
  _merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing _merchant (Just opCity)

  chargeback <- QChargeback.findById (Id chargebackId) >>= fromMaybeM (InvalidRequest "Chargeback not found")

  -- Validate state transition
  let currentStatus = chargeback.chargebackStatus
  newStatus <- case req.action of
    API.AcceptChargeback -> do
      unless (currentStatus == Chargeback.OPEN || currentStatus == Chargeback.EVIDENCE_SUBMITTED) $
        throwError $ InvalidRequest "Cannot accept chargeback in current status"
      pure Chargeback.LOST
    API.RejectChargeback -> do
      unless (currentStatus == Chargeback.OPEN || currentStatus == Chargeback.EVIDENCE_SUBMITTED) $
        throwError $ InvalidRequest "Cannot reject chargeback in current status"
      pure Chargeback.WON
    API.SubmitEvidence -> do
      unless (currentStatus == Chargeback.OPEN) $
        throwError $ InvalidRequest "Can only submit evidence for OPEN chargebacks"
      when (isNothing req.evidenceUrl) $
        throwError $ InvalidRequest "Evidence URL is required when submitting evidence"
      pure Chargeback.EVIDENCE_SUBMITTED

  logInfo $ "Updating chargeback " <> chargebackId <> " from " <> show currentStatus <> " to " <> show newStatus

  QChargeback.updateChargebackStatus newStatus req.evidenceUrl req.adminNotes (Id chargebackId)

  pure $
    API.ChargebackRespondRes
      { success = True,
        message = "Chargeback updated successfully",
        updatedStatus = show newStatus
      }

-- ---------------------------------------------------------------------------
-- 6. getSettlementTrend
-- ---------------------------------------------------------------------------

getSettlementTrend ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe API.TrendGranularity ->
  Flow API.SettlementTrendRes
getSettlementTrend merchantShortId opCity mbDateFrom mbDateTo mbGateway _mbGranularity = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let dateTo = fromMaybe now mbDateTo
      dateFrom = fromMaybe (addUTCTime (negate $ 30 * nominalDay) now) mbDateFrom

  -- Fetch all settlement reports for the date range
  reports <- QPgSettlementExtra.findAllByMerchantOpCityIdWithFilters
    merchant.id.getId
    merchantOpCityId.getId
    (Just dateFrom)
    (Just dateTo)
    Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing
    mbGateway
    Nothing Nothing Nothing Nothing
    Nothing Nothing

  -- Group by day and aggregate
  let dayMap = foldl' groupByDay Map.empty reports
      trendData = map (uncurry mkTrendPoint) (Map.toAscList dayMap)

      totalSettled = sum $ map (.settledAmount) trendData
      totalPending = sum $ map (.pendingAmount) trendData

  pure $
    API.SettlementTrendRes
      { trendData = trendData,
        totalSettled = totalSettled,
        totalPending = totalPending,
        periodStart = dateFrom,
        periodEnd = dateTo
      }
  where
    groupByDay :: Map.Map UTCTime [PgSettlement.PgPaymentSettlementReport] -> PgSettlement.PgPaymentSettlementReport -> Map.Map UTCTime [PgSettlement.PgPaymentSettlementReport]
    groupByDay acc report =
      let dayKey = UTCTime (utctDay report.createdAt) 0
       in Map.insertWith (<>) dayKey [report] acc

    mkTrendPoint :: UTCTime -> [PgSettlement.PgPaymentSettlementReport] -> API.SettlementTrendPoint
    mkTrendPoint day dayReports =
      let settled = filter (\r -> r.reconStatus == PgSettlement.MATCHED || r.reconStatus == PgSettlement.SETTLED) dayReports
          pending = filter (\r -> r.reconStatus == PgSettlement.PENDING) dayReports
          failed = filter (\r -> r.txnStatus == PgSettlement.FAILED) dayReports
          disputed = filter (\r -> isJust r.disputeId) dayReports
       in API.SettlementTrendPoint
            { date = day,
              settledAmount = sum $ map (.settlementAmount) settled,
              pendingAmount = sum $ map (.settlementAmount) pending,
              failedAmount = sum $ map (.txnAmount) failed,
              disputedAmount = sum $ mapMaybe (.chargebackAmount) disputed,
              transactionCount = length dayReports,
              settlementCount = length settled
            }
