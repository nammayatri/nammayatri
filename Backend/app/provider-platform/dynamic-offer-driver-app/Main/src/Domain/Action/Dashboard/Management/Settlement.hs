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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, nominalDay, toGregorian, fromGregorian, toModifiedJulianDay, ModifiedJulianDay (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import EulerHS.Prelude hiding (id)
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
-- Settlement status filter helpers
-- ---------------------------------------------------------------------------

-- | [M1 fix] Apply SettlementStatusFilter to a list of reports.
applySettlementStatusFilter :: Maybe API.SettlementStatusFilter -> [PgSettlement.PgPaymentSettlementReport] -> [PgSettlement.PgPaymentSettlementReport]
applySettlementStatusFilter mbStatus reports = case mbStatus of
  Nothing -> reports
  Just API.AllSettlements -> reports
  Just API.Settled -> filter (\r -> r.reconStatus == PgSettlement.MATCHED || r.reconStatus == PgSettlement.SETTLED) reports
  Just API.Pending -> filter (\r -> r.reconStatus == PgSettlement.PENDING) reports
  Just API.Failed -> filter (\r -> r.txnStatus == PgSettlement.FAILED) reports
  Just API.Disputed -> filter (\r -> isJust r.disputeId) reports

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

  -- Fetch summary aggregates via dedicated SQL queries
  settledAgg <- QPgSettlementExtra.aggregateByReconStatus
    merchant.id.getId merchantOpCityId.getId (Just dateFrom) (Just dateTo) mbGateway PgSettlement.SETTLED
  matchedAgg <- QPgSettlementExtra.aggregateByReconStatus
    merchant.id.getId merchantOpCityId.getId (Just dateFrom) (Just dateTo) mbGateway PgSettlement.MATCHED
  pendingAgg <- QPgSettlementExtra.aggregateByReconStatus
    merchant.id.getId merchantOpCityId.getId (Just dateFrom) (Just dateTo) mbGateway PgSettlement.PENDING
  failedAgg <- QPgSettlementExtra.aggregateByTxnStatus
    merchant.id.getId merchantOpCityId.getId (Just dateFrom) (Just dateTo) mbGateway PgSettlement.FAILED
  disputedAgg <- QPgSettlementExtra.aggregateDisputed
    merchant.id.getId merchantOpCityId.getId (Just dateFrom) (Just dateTo) mbGateway

  let totalSettledAmount = settledAgg.totalAmount + matchedAgg.totalAmount
      totalPendingAmount = pendingAgg.totalAmount
      totalFailedAmount = failedAgg.totalAmount
      totalDisputedAmount = disputedAgg.totalAmount

      totalCount = settledAgg.count + matchedAgg.count
      pendingCount = pendingAgg.count
      failedCount = failedAgg.count
      disputedCount = disputedAgg.count

      totalAll = totalCount + pendingCount + failedCount
      rate = if totalAll > 0 then T.pack $ show (round @Double @Int $ (fromIntegral totalCount / fromIntegral totalAll) * 100) <> "%" else "N/A"

  -- Gateway breakdown (still needs per-gateway data)
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
listSettlements merchantShortId opCity mbAmountMax mbAmountMin mbDateFrom mbDateTo mbGateway mbLimit mbOffset mbSearchQuery mbStatus = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset
      -- [M6 fix] Servant orders query params alphabetically, so the first HighPrecMoney
      -- param is amountMax and the second is amountMin. Fix the swap:
      correctedAmountMin = mbAmountMin
      correctedAmountMax = mbAmountMax

  -- Use the searchQuery to look up by orderId, txnId, rrn, or settlementId
  let (mbOrderId, mbSettlementId, mbUtr) = case mbSearchQuery of
        Just q -> (Just q, Just q, Nothing) -- simplified: search by orderId first
        Nothing -> (Nothing, Nothing, Nothing)

  -- Get all matching reports (without limit/offset) for total count and status filtering
  allMatchingReports <- QPgSettlementExtra.findAllByMerchantOpCityIdWithFilters
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
    correctedAmountMin  -- [M6 fix] correctly pass min
    correctedAmountMax  -- [M6 fix] correctly pass max
    Nothing -- txnAmountMin
    Nothing -- txnAmountMax
    Nothing -- no limit for count
    Nothing -- no offset for count

  -- [M1 fix] Apply SettlementStatusFilter in-memory
  let filteredReports = applySettlementStatusFilter mbStatus allMatchingReports

  -- [C4 fix] totalItems is the true total count, not page size
  let trueTotal = length filteredReports

  -- Apply pagination manually since we need the total count
  let paginatedReports = take limit $ drop offset filteredReports

  items <- mapM mkSettlementListItem paginatedReports

  let summary = Dashboard.Common.Summary {totalCount = trueTotal, count = length items}

  pure $
    API.SettlementListRes
      { totalItems = trueTotal,
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
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  report <- QPgSettlement.findById (Id settlementId.getId) >>= fromMaybeM (InvalidRequest "Settlement report not found")

  -- [C2 fix] Verify merchant ownership
  unless (report.merchantId == merchant.id.getId) $
    throwError $ InvalidRequest "Settlement report does not belong to this merchant"

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

  -- [C4 fix] Get true total count from DB (without pagination)
  totalCount <- QChargebackExtra.countByMerchantWithFilters
    merchant.id.getId
    merchantOpCityId.getId
    mbStatus
    mbDateFrom
    mbDateTo

  let items = map mkChargebackItem chargebacks
      summary = Dashboard.Common.Summary {totalCount = totalCount, count = length items}

  -- [M4 fix] Count by status using a single grouped query
  statusCountMap <- QChargebackExtra.countGroupedByStatus merchant.id.getId merchantOpCityId.getId

  pure $
    API.ChargebackListRes
      { totalItems = totalCount,
        summary = summary,
        chargebacks = items,
        statusCounts =
          API.ChargebackStatusCounts
            { openCount = fromMaybe 0 $ Map.lookup Chargeback.OPEN statusCountMap,
              evidenceSubmittedCount = fromMaybe 0 $ Map.lookup Chargeback.EVIDENCE_SUBMITTED statusCountMap,
              wonCount = fromMaybe 0 $ Map.lookup Chargeback.WON statusCountMap,
              lostCount = fromMaybe 0 $ Map.lookup Chargeback.LOST statusCountMap,
              expiredCount = fromMaybe 0 $ Map.lookup Chargeback.EXPIRED statusCountMap
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
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  chargeback <- QChargeback.findById (Id chargebackId) >>= fromMaybeM (InvalidRequest "Chargeback not found")

  -- [C1 fix] Verify merchant ownership
  unless (chargeback.merchantId == merchant.id.getId) $
    throwError $ InvalidRequest "Chargeback does not belong to this merchant"

  -- [C3 fix] Enforce response deadline
  now <- getCurrentTime
  when (now > chargeback.responseDeadline) $
    throwError $ InvalidRequest "Response deadline has passed"

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

  -- [M5 fix] Log chargeback state change for penalty system integration.
  -- When a chargeback reaches a terminal state (WON/LOST), linked penalty records
  -- should be updated accordingly. The settlement report's disputeId links to
  -- the penalty system.
  when (newStatus == Chargeback.WON || newStatus == Chargeback.LOST) $ do
    report <- QPgSettlement.findById chargeback.settlementReportId
    case report >>= (.disputeId) of
      Just dispId -> logInfo $ "Chargeback " <> chargebackId <> " resolved to " <> show newStatus
        <> ", linked penalty disputeId=" <> dispId <> " should be updated"
      Nothing -> pure ()

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
getSettlementTrend merchantShortId opCity mbDateFrom mbDateTo mbGateway mbGranularity = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let dateTo = fromMaybe now mbDateTo
      dateFrom = fromMaybe (addUTCTime (negate $ 30 * nominalDay) now) mbDateFrom
      granularity = fromMaybe API.Daily mbGranularity

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

  -- [M2 fix] Group by the requested granularity, not just daily
  let periodMap = foldl' (groupByPeriod granularity) Map.empty reports
      trendData = map (uncurry mkTrendPoint) (Map.toAscList periodMap)

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
    -- [M2 fix] Group by day, week, or month depending on granularity
    groupByPeriod :: API.TrendGranularity -> Map.Map UTCTime [PgSettlement.PgPaymentSettlementReport] -> PgSettlement.PgPaymentSettlementReport -> Map.Map UTCTime [PgSettlement.PgPaymentSettlementReport]
    groupByPeriod gran acc report =
      let periodKey = case gran of
            API.Daily -> UTCTime (utctDay report.createdAt) 0
            API.Weekly ->
              -- Round down to start of ISO week (Monday)
              let dayNum = toModifiedJulianDay (utctDay report.createdAt)
                  weekStart = ModifiedJulianDay (dayNum - ((dayNum + 3) `mod` 7))
               in UTCTime weekStart 0
            API.Monthly ->
              let (yr, mo, _) = toGregorian (utctDay report.createdAt)
               in UTCTime (fromGregorian yr mo 1) 0
       in Map.insertWith (<>) periodKey [report] acc

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
