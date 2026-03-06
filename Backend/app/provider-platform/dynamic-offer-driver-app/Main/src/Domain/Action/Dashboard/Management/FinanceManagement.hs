{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.FinanceManagement
  ( getFinanceManagementSubscriptionPurchaseList,
    getFinanceManagementInvoiceList,
    getFinanceManagementFinanceInvoiceList,
    getReconciliation,
    getFinanceManagementReconciliation,
    getFinanceManagementFinanceReconciliation,
    postFinanceManagementReconciliationTrigger,
    getFinanceManagementFinancePayoutList,
    getFinanceManagementFinancePaymentSettlementList,
    getFinanceManagementFinancePaymentGatewayTransactionList,
    getFinanceManagementFinanceWalletLedger,
    getFinanceManagementFinanceEarningSummary,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement as API
import Control.Monad (forM)
import qualified Dashboard.Common
import Data.List (nub)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import Domain.Action.UI.Plan (getPlanAmount)
import qualified Domain.Types.FleetOwnerInformation as FleetOwnerInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SubscriptionPurchase as DSP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude (UTCTime, identity, listToMaybe)
import qualified Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), ShortId (..), cast)
import Kernel.Utils.Common (getCurrentTime, secondsToNominalDiffTime)
import Kernel.Utils.Error (fromMaybeM, throwError)
import Lib.Finance.Domain.Types.Account (AccountType (..), CounterpartyType (..))
import qualified Lib.Finance.Domain.Types.Account as Account
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as PgPaymentSettlementReport
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconciliationEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as ReconSummary
import qualified Lib.Finance.Ledger.Service as LedgerService
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QInvoiceLedgerLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntryExtra
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra as QPgPaymentSettlementReport
import qualified Lib.Finance.Storage.Queries.ReconciliationEntry as QReconEntry
import qualified Lib.Finance.Storage.Queries.ReconciliationEntryExtra as QReconEntryExtra
import qualified Lib.Finance.Storage.Queries.ReconciliationSummary as QReconSummary
import qualified Lib.Payment.Domain.Types.PaymentOrder as PaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as PaymentTransaction
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Domain.Types.Refunds as PaymentRefund
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import qualified Lib.Scheduler.JobStorageType.DB.Table as SchedulerJobT
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QSchedulerJob
import SharedLogic.Allocator (AllocatorJobType (..), ReconciliationJobData (..))
import qualified SharedLogic.Finance.Wallet as WalletService
import qualified SharedLogic.Merchant as SMerchant
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQPlan
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.FleetOwnerInformation as QFleetOwnerInfo
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import Tools.Encryption (decryptWithDefault)
import Tools.Error

defaultPageLimit :: Int
defaultPageLimit = 20

maxPageLimit :: Int
maxPageLimit = 100

mkPageLimit :: Maybe Int -> Int
mkPageLimit = min maxPageLimit . max 0 . fromMaybe defaultPageLimit

mkPageOffset :: Maybe Int -> Int
mkPageOffset = max 0 . fromMaybe 0

-- | Get subscription purchase list with filters
getFinanceManagementSubscriptionPurchaseList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe API.SubscriptionPurchaseStatus ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.SubscriptionPurchaseListRes
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity _amountMax _amountMin mbDriverId mbFleetOperatorId mbFrom mbLimit mbOffset mbServiceName mbStatus mbSubscriptionId mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  let mbParsedServiceName = mbServiceName >>= parseServiceName
      mbParsedStatus = fmap castSubscriptionPurchaseStatus mbStatus

  -- Get subscription purchases based on filters (subscriptionId is optional)
  subscriptions <- case mbSubscriptionId of
    Just subscriptionId -> do
      QSubscriptionPurchase.findByPrimaryKey (Id subscriptionId) >>= \case
        Just sub -> pure [sub]
        Nothing -> pure []
    Nothing -> do
      case (mbDriverId, mbFleetOperatorId) of
        (Just driverId, _) ->
          fetchSubscriptionsForOwner driverId DSP.DRIVER (fromMaybe DPlan.YATRI_SUBSCRIPTION mbParsedServiceName) mbParsedStatus limit offset merchantOpCityId
        (_, Just fleetOwnerId) ->
          fetchSubscriptionsForOwner fleetOwnerId DSP.FLEET_OWNER (fromMaybe DPlan.YATRI_SUBSCRIPTION mbParsedServiceName) mbParsedStatus limit offset merchantOpCityId
        _ -> do
          QSubscriptionPurchase.findAllByMerchantOpCityIdWithFilters
            merchantOpCityId
            mbParsedServiceName
            mbParsedStatus
            mbFrom
            mbTo
            (Just limit)
            (Just offset)

  -- Build response items (use each subscription's serviceName for plan lookup)
  items <- mapM (\sub -> buildSubscriptionPurchaseItem sub limit offset) subscriptions

  let totalItems = length items
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  pure $
    API.SubscriptionPurchaseListRes
      { totalItems,
        summary,
        subscriptions = items
      }
  where
    parseServiceName :: Text -> Maybe DPlan.ServiceNames
    parseServiceName "YATRI_SUBSCRIPTION" = Just DPlan.YATRI_SUBSCRIPTION
    parseServiceName "YATRI_RENTAL" = Just DPlan.YATRI_RENTAL
    parseServiceName "PREPAID_SUBSCRIPTION" = Just DPlan.PREPAID_SUBSCRIPTION
    parseServiceName "DASHCAM_RENTAL_CAUTIO" = Just $ DPlan.DASHCAM_RENTAL DPlan.CAUTIO
    parseServiceName "DASHCAM_RENTAL_OWNED" = Just $ DPlan.DASHCAM_RENTAL DPlan.OWNED
    parseServiceName _ = Nothing

    castSubscriptionPurchaseStatus :: API.SubscriptionPurchaseStatus -> DSP.SubscriptionPurchaseStatus
    castSubscriptionPurchaseStatus = \case
      API.SubscriptionPending -> DSP.PENDING
      API.SubscriptionActive -> DSP.ACTIVE
      API.SubscriptionExpired -> DSP.EXPIRED
      API.SubscriptionFailed -> DSP.FAILED
      API.SubscriptionExhausted -> DSP.EXHAUSTED

    fetchSubscriptionsForOwner :: Text -> DSP.SubscriptionOwnerType -> DPlan.ServiceNames -> Maybe DSP.SubscriptionPurchaseStatus -> Int -> Int -> (Id DMOC.MerchantOperatingCity) -> Flow [DSP.SubscriptionPurchase]
    fetchSubscriptionsForOwner ownerId ownerType serviceName mbStatusVal limit offset merchantOpCityId = do
      QSubscriptionPurchase.findAllByOwnerAndServiceNameWithPagination'
        merchantOpCityId
        ownerId
        ownerType
        serviceName
        mbStatusVal
        (Just limit)
        (Just offset)

    buildSubscriptionPurchaseItem :: DSP.SubscriptionPurchase -> Int -> Int -> Flow API.SubscriptionPurchaseListItem
    buildSubscriptionPurchaseItem subscription _limit _offset = do
      let serviceName = subscription.serviceName
      -- Get plan details
      plan <-
        CQPlan.findByIdAndPaymentModeWithServiceName subscription.planId DPlan.MANUAL serviceName >>= \case
          Just p -> pure p
          Nothing ->
            QPlan.findByIdAndPaymentModeWithServiceName subscription.planId DPlan.MANUAL serviceName >>= \case
              Just p -> pure p
              Nothing ->
                QPlan.findByPrimaryKey subscription.planId >>= \case
                  Just p -> pure p
                  Nothing -> throwError $ InvalidRequest "Plan not found"

      -- Get owner (driver/fleet) details
      (driverId, driverName, driverPhone, fleetOwnerId, fleetOwnerName, fleetOwnerPhone) <-
        case subscription.ownerType of
          DSP.DRIVER -> do
            person <- QPerson.findById (Id subscription.ownerId) >>= fromMaybeM (PersonDoesNotExist subscription.ownerId)
            let dName = T.intercalate " " $ catMaybes [Just person.firstName, person.middleName, person.lastName]
            dPhone <- decryptWithDefault person.mobileNumber Nothing
            -- Get fleet owner via FleetDriverAssociation (driver -> fleet owner)
            (fId, fName, fPhone) <- do
              mbFleetAssoc <- QFleetDriver.findByDriverId person.id True
              case mbFleetAssoc of
                Just fleetAssoc -> do
                  fPerson <- QPerson.findById (Id fleetAssoc.fleetOwnerId) >>= fromMaybeM (PersonDoesNotExist fleetAssoc.fleetOwnerId)
                  let fName' = T.intercalate " " $ catMaybes [Just fPerson.firstName, fPerson.middleName, fPerson.lastName]
                  fPhone' <- decryptWithDefault fPerson.mobileNumber Nothing
                  pure (Just fleetAssoc.fleetOwnerId, Just fName', fPhone')
                Nothing -> pure (Nothing, Nothing, Nothing)
            pure (Just person.id.getId, Just dName, dPhone, fId, fName, fPhone)
          DSP.FLEET_OWNER -> do
            fPerson <- QPerson.findById (Id subscription.ownerId) >>= fromMaybeM (PersonDoesNotExist subscription.ownerId)
            let fName = T.intercalate " " $ catMaybes [Just fPerson.firstName, fPerson.middleName, fPerson.lastName]
            fPhone <- decryptWithDefault fPerson.mobileNumber Nothing
            pure (Nothing, Nothing, Nothing, Just subscription.ownerId, Just fName, fPhone)

      -- Get merchant operating city for geography
      merchantOpCity <- CQMOC.findById subscription.merchantOperatingCityId
      let planGeography = merchantOpCity <&> (.city) <&> show

      -- Get GST details from indirect tax transaction
      indirectTaxTxns <- QIndirectTax.findByReferenceId subscription.id.getId
      let mbSubscriptionTxn = listToMaybe $ filter (\txn -> txn.transactionType == IndirectTax.Subscription) indirectTaxTxns

      -- Financials from Plan table (as per Excel)
      let baseAmount = Just $ getPlanAmount plan.planBaseAmount
      let discountAmount = Just 0 -- Hardcoded for now, could come from plan if available
      let totalAmount = Just $ getPlanAmount plan.planBaseAmount

      -- GST details from IndirectTaxTransaction
      let gstRate = mbSubscriptionTxn <&> (.gstRate)
      let gstAmount = mbSubscriptionTxn <&> (.totalGstAmount)

      -- Gross and total subscription amounts
      let grossSubscriptionAmount = baseAmount
      let totalSubscriptionAmount = (\b g -> b + g) <$> baseAmount <*> gstAmount

      -- Calculate plan status
      now <- getCurrentTime
      let planStatus =
            Just $
              if subscription.status == DSP.ACTIVE && maybe True (> now) subscription.expiryDate
                then "ACTIVE"
                else "INACTIVE"

      -- Get plan validity days
      let planValidityDays = fmap (calculateValidityDays subscription.purchaseTimestamp) subscription.expiryDate

      -- Calculate usage values
      utilizedValue <- calculateUtilizedValue subscription.id
      let entitledValue = subscription.planRideCredit
          remainingValue = entitledValue - utilizedValue

      -- Get revenue recognized
      revenueRecognized <- calculateRevenueRecognized subscription.id

      -- Get linked rides with rideId, bookingId, rideCreatedAt, rideSubscriptionDebitAmount
      rides <- QRide.findAllBySubscriptionPurchaseId subscription.id
      linkedRides <- mapM buildLinkedRideItem rides

      pure $
        API.SubscriptionPurchaseListItem
          { subscriptionPurchaseId = Just subscription.id.getId,
            purchasedAt = Just subscription.purchaseTimestamp,
            driverName = driverName,
            fleetOwnerName = fleetOwnerName,
            driverPhoneNumber = driverPhone,
            fleetOwnerPhoneNumber = fleetOwnerPhone,
            driverId = driverId,
            fleetOwnerId = fleetOwnerId,
            status = Just $ show subscription.status,
            amount = Just subscription.planFee,
            planName = Just plan.name,
            planType = Just $ show plan.frequency,
            planAmount = baseAmount,
            planRideCredits = Just subscription.planRideCredit,
            planStatus = planStatus,
            planValidityDays = planValidityDays,
            planGeography = planGeography,
            planVehicleCategory = Just $ show plan.vehicleCategory,
            subscriptionStartDate = Just subscription.purchaseTimestamp,
            subscriptionEndDate = subscription.expiryDate,
            subscriptionStatus = Just $ show subscription.status,
            baseAmount = baseAmount,
            discountAmount = discountAmount,
            totalAmount = totalAmount,
            grossSubscriptionAmount = grossSubscriptionAmount,
            gstRate = gstRate,
            gstAmount = gstAmount,
            totalSubscriptionAmount = totalSubscriptionAmount,
            invoiceId = subscription.financeInvoiceId <&> (.getId),
            totalEntitledValue = Just entitledValue,
            utilizedValue = Just utilizedValue,
            remainingValue = Just remainingValue,
            revenueRecognized = Just revenueRecognized,
            linkedRides = linkedRides,
            createdAt = Just subscription.createdAt,
            updatedAt = Just subscription.updatedAt
          }

    -- Calculate utilized value from RideSubscriptionDebit ledger entries
    calculateUtilizedValue :: Id DSP.SubscriptionPurchase -> Flow HighPrecMoney
    calculateUtilizedValue subId = do
      -- Get all rides linked to this subscription
      rides <- QRide.findAllBySubscriptionPurchaseId subId
      let bookingIds = map (\r -> r.bookingId.getId) rides

      -- Sum all RideSubscriptionDebit entries for these booking IDs
      utilized <- fmap sum $
        forM bookingIds $ \bId -> do
          entries <- QLedgerEntry.findByReference "RideSubscriptionDebit" bId
          pure $ sum $ map (.amount) entries

      pure utilized

    calculateValidityDays :: UTCTime -> UTCTime -> Int
    calculateValidityDays start end =
      let diff = diffUTCTime end start
          days = diff / (24 * 3600)
       in round days

    calculateRevenueRecognized :: Id DSP.SubscriptionPurchase -> Flow HighPrecMoney
    calculateRevenueRecognized subId = do
      -- Get all rides linked to this subscription
      rides <- QRide.findAllBySubscriptionPurchaseId subId
      let bookingIds = map (\r -> r.bookingId.getId) rides

      -- Sum ledger entries for these booking IDs with revenue recognition types
      revenue <- fmap sum $
        forM bookingIds $ \bId -> do
          entries <- QLedgerEntryExtra.findByReferenceIn ["RideRevenueRecognition", "ExpiryRevenueRecognition"] bId
          pure $ sum $ map (.amount) entries

      pure revenue

    buildLinkedRideItem :: DRide.Ride -> Flow API.LinkedRideItem
    buildLinkedRideItem ride = do
      -- Fetch RideSubscriptionDebit entries from finance_ledger_entry: rideId -> bookingId -> reference_type=RideSubscriptionDebit, reference_id=bookingId
      entries <- QLedgerEntry.findByReference "RideSubscriptionDebit" ride.bookingId.getId
      let rideSubscriptionDebitAmount = sum $ map (.amount) entries

      pure $
        API.LinkedRideItem
          { rideId = ride.id.getId,
            bookingId = ride.bookingId.getId,
            rideCreatedAt = ride.createdAt,
            rideSubscriptionDebitAmount = rideSubscriptionDebitAmount
          }

-- | Get invoice list with filters
getFinanceManagementInvoiceList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe FinanceInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe FinanceInvoice.InvoiceStatus ->
  Maybe UTCTime ->
  Flow API.InvoiceListRes
getFinanceManagementInvoiceList merchantShortId opCity mbFrom mbInvoiceId mbInvoiceType mbLimit mbOffset mbStatus mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Get invoices based on filters
  invoices <- case mbInvoiceId of
    Just invoiceId -> do
      QFinanceInvoice.findById (Id invoiceId) >>= \case
        Just inv -> pure [inv]
        Nothing -> pure []
    Nothing -> do
      QFinanceInvoiceExtra.findByMerchantOpCityIdAndDateRange
        merchantOpCityId.getId
        mbFrom
        mbTo
        mbInvoiceType
        mbStatus
        (Just limit)
        (Just offset)

  -- Build response items
  items <- mapM buildInvoiceItem invoices

  let totalItems = length items
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  pure $
    API.InvoiceListRes
      { totalItems,
        summary,
        invoices = items
      }
  where
    buildInvoiceItem :: FinanceInvoice.Invoice -> Flow API.InvoiceListItem
    buildInvoiceItem invoice = do
      -- Get GST details from indirect tax transaction
      indirectTaxTxns <- QIndirectTax.findByInvoiceNumber invoice.invoiceNumber

      let (taxableValue, gstRate, gstAmount) = case indirectTaxTxns of
            (txn : _) -> (Just txn.taxableValue, Just txn.gstRate, Just txn.totalGstAmount)
            _ -> (Nothing, Nothing, Nothing)

      -- Get linked ride/subscription IDs from ledger links
      ledgerLinks <- QInvoiceLedgerLink.findByInvoice invoice.id
      ledgerEntries <- mapM (QLedgerEntry.findById . (.ledgerEntryId)) ledgerLinks

      -- Extract ride_id and subscription_id from ledger entries
      let (rideIds, subscriptionIds) = foldr extractIds ([], []) (catMaybes ledgerEntries)

      pure $
        API.InvoiceListItem
          { invoiceId = Just invoice.id.getId,
            invoiceNumber = Just invoice.invoiceNumber,
            invoiceType = Just $ show invoice.invoiceType,
            invoiceDate = Just invoice.issuedAt,
            invoiceStatus = Just $ show invoice.status,
            counterpartyType = Nothing, -- TODO: Get from finance_account via issuedToType
            counterpartyId = Just invoice.issuedToId,
            taxableValue = taxableValue,
            gstRate = gstRate,
            gstAmount = gstAmount,
            totalInvoiceValue = Just invoice.totalAmount,
            tdsReference = Nothing,
            irn = Nothing,
            qrCode = Nothing,
            rideId = listToMaybe rideIds,
            subscriptionId = listToMaybe subscriptionIds,
            generatedAt = Just invoice.createdAt
          }

    extractIds :: LedgerEntry.LedgerEntry -> ([Text], [Text]) -> ([Text], [Text])
    extractIds entry (rides, subs) =
      case entry.referenceType of
        "Ride" -> (entry.referenceId : rides, subs)
        "SubscriptionPurchase" -> (rides, entry.referenceId : subs)
        _ -> (rides, subs)

-- | Get reconciliation data - just fetch from tables
getReconciliation ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.ReconciliationRes
getReconciliation merchantShortId opCity mbFromDate mbLimit mbOffset mbReconciliationType mbToDate = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Fetch summaries - use single date or merchant filter (generated API has findByDateAndType date type, findByMerchantId)
  summaries <- case (mbReconciliationType, mbFromDate <|> mbToDate) of
    (Just rType, Just d) -> QReconSummary.findByDateAndType d (caseToReconciliationType rType)
    (Just rType, Nothing) -> do
      now <- getCurrentTime
      QReconSummary.findByDateAndType now (caseToReconciliationType rType)
    (Nothing, _) -> do
      allSummaries <- QReconSummary.findByMerchantId merchant.id.getId
      pure $ filter (\s -> maybe True (s.reconciliationDate >=) mbFromDate && maybe True (s.reconciliationDate <=) mbToDate) allSummaries

  let latestSummary = listToMaybe summaries

  -- Build summary from latest summary (domain has totalDiscrepancies, matchedRecords, matchRate)
  let summaryRes = case latestSummary of
        Just s ->
          API.ReconciliationSummary
            { totalDiscrepancies = s.totalDiscrepancies,
              matchedRecords = s.matchedRecords,
              matchRate = s.matchRate,
              sourceTotal = s.sourceTotal,
              targetTotal = s.targetTotal,
              varianceAmount = s.varianceAmount
            }
        Nothing ->
          API.ReconciliationSummary
            { totalDiscrepancies = 0,
              matchedRecords = 0,
              matchRate = "0%",
              sourceTotal = 0,
              targetTotal = 0,
              varianceAmount = 0
            }

  -- Fetch entries from latest summary; paginate in memory (findBySummaryId returns list)
  entries <- case latestSummary of
    Just summary -> do
      allEntries <- QReconEntry.findBySummaryId summary.id
      pure $ take limit $ drop offset allEntries
    Nothing -> pure []

  entriesItems <- mapM buildReconciliationEntry entries
  pure $
    API.ReconciliationRes
      { summary = summaryRes,
        exceptions = entriesItems, -- Return all entries
        completed = [] -- Already included in entries
      }
  where
    buildReconciliationEntry :: ReconciliationEntry.ReconciliationEntry -> Flow API.ReconciliationEntry
    buildReconciliationEntry entry =
      let subscriptionPurchaseIdValue =
            case entry.reconciliationType of
              ReconciliationEntry.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST -> Nothing
              _ -> entry.sourceId
       in pure
            API.ReconciliationEntry
              { bookingId = entry.bookingId,
                dcoId = entry.dcoId,
                status = fmap show entry.status,
                mode = fmap show entry.mode,
                expectedDsrValue = Just entry.expectedDsrValue,
                actualLedgerValue = Just entry.actualLedgerValue,
                variance = Just entry.variance,
                reconStatus = Just (show entry.reconStatus),
                mismatchReason = entry.mismatchReason,
                timestamp = Just entry.timestamp,
                settlementId = entry.settlementId,
                subscriptionPurchaseId = subscriptionPurchaseIdValue,
                sourceId = entry.sourceId,
                targetId = entry.targetId,
                settlementDate = entry.settlementDate,
                transactionDate = entry.transactionDate,
                transactionAmount = Just entry.actualLedgerValue,
                subscriptionPurchaseAmount = Just entry.expectedDsrValue,
                rrn = entry.rrn,
                settlementMode = entry.settlementMode,
                financeComponent = fmap show entry.financeComponent
              }
    caseToReconciliationType :: Text -> ReconSummary.ReconciliationType
    caseToReconciliationType "DSR_VS_LEDGER" = ReconSummary.DSR_VS_LEDGER
    caseToReconciliationType "DSR_VS_SUBSCRIPTION" = ReconSummary.DSR_VS_SUBSCRIPTION
    caseToReconciliationType "DSSR_VS_SUBSCRIPTION" = ReconSummary.DSSR_VS_SUBSCRIPTION
    caseToReconciliationType "PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION" = ReconSummary.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION
    caseToReconciliationType "PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST" = ReconSummary.PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST
    caseToReconciliationType _ = error "Invalid reconciliation type"

getFinanceManagementReconciliation :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Flow API.ReconciliationRes
getFinanceManagementReconciliation = getReconciliation

-- Aliases for generated API (Dashboard Management uses FinanceInvoiceList / FinanceReconciliation naming)
getFinanceManagementFinanceInvoiceList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe FinanceInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe FinanceInvoice.InvoiceStatus ->
  Maybe UTCTime ->
  Flow API.InvoiceListRes
getFinanceManagementFinanceInvoiceList = getFinanceManagementInvoiceList

getFinanceManagementFinanceReconciliation :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Flow API.ReconciliationRes
getFinanceManagementFinanceReconciliation = getFinanceManagementReconciliation

-- | Trigger a reconciliation job on-demand
postFinanceManagementReconciliationTrigger ::
  ShortId DM.Merchant ->
  Context.City ->
  API.ReconciliationTriggerReq ->
  Flow API.ReconciliationTriggerRes
postFinanceManagementReconciliationTrigger merchantShortId opCity req = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  -- Validate the reconciliation type
  let reconciliationType = fromMaybe "DSR_VS_LEDGER" req.reconciliationType
  case reconciliationType of
    "DSR_VS_LEDGER" -> pure ()
    "DSR_VS_SUBSCRIPTION" -> pure ()
    "DSSR_VS_SUBSCRIPTION" -> pure ()
    "PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION" -> pure ()
    "PG_PAYOUT_SETTLEMENT_VS_PAYOUT_REQUEST" -> pure ()
    _ -> throwError $ InvalidRequest $ "Invalid reconciliation type: " <> reconciliationType

  -- Create the reconciliation job data
  let jobData =
        SharedLogic.Allocator.ReconciliationJobData
          { reconciliationType = reconciliationType,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            startTime = req.fromDate,
            endTime = req.toDate
          }

  -- Create the job immediately
  QSchedulerJob.createJobIn @_ @'Reconciliation (Just merchant.id) (Just merchantOpCityId) 0 jobData

  pure $
    API.ReconciliationTriggerRes
      { success = True,
        message = "Reconciliation job scheduled successfully for " <> reconciliationType <> " from " <> show req.fromDate <> " to " <> show req.toDate
      }

-- | Get payout list with filters
getFinanceManagementFinancePayoutList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Flow API.PayoutListRes
getFinanceManagementFinancePayoutList merchantShortId opCity mbDriverId mbFleetOperatorId mbFrom mbLimit mbOffset mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Resolve customerId from driverId or fleetOperatorId
  let mbCustomerId = mbDriverId <|> mbFleetOperatorId

  payouts <- case mbCustomerId of
    Just customerId ->
      QPayoutOrder.findAllWithOptions
        limit
        offset
        (Just customerId)
        Nothing
        mbFrom
        mbTo
        False
        opCity
    Nothing -> pure []

  -- Map to response items
  let payoutItems = map buildPayoutItem payouts
      totalItems = length payoutItems

  pure $
    API.PayoutListRes
      { totalItems = totalItems,
        payouts = payoutItems
      }
  where
    buildPayoutItem :: PayoutOrder.PayoutOrder -> API.PayoutListItem
    buildPayoutItem po =
      API.PayoutListItem
        { payoutReference = Just po.orderId,
          payoutAmount = Just po.amount.amount,
          payoutDate = Just po.createdAt,
          payoutStatus = Just (show po.status)
        }

getFinanceManagementFinancePaymentSettlementList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.PaymentSettlementListRes
getFinanceManagementFinancePaymentSettlementList merchantShortId opCity mbDriverId mbFleetOwnerId mbFrom mbLimit mbOffset mbOrderId mbSettlementId mbSubscriptionPurchaseId mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  reports <- case (mbDriverId, mbFleetOwnerId) of
    (Nothing, Nothing) ->
      QPgPaymentSettlementReport.findAllByMerchantOpCityIdWithFilters
        merchant.id.getId
        merchantOpCityId.getId
        mbFrom
        mbTo
        mbSubscriptionPurchaseId
        mbOrderId
        mbSettlementId
        Nothing
        (Just limit)
        (Just offset)
    (Just driverId, Nothing) -> fetchReportsForOwner merchantOpCityId driverId DSP.DRIVER
    (Nothing, Just fleetOwnerId) -> fetchReportsForOwner merchantOpCityId fleetOwnerId DSP.FLEET_OWNER
    (Just _, Just _) -> throwError $ InvalidRequest "Provide either driverId or fleetOwnerId, not both"

  let selectedSubscriptionIds = reports <&> (.referenceId) & catMaybes & nub
  latestReconEntries <- getLatestReconEntries merchant.id.getId selectedSubscriptionIds
  settlements <- catMaybes <$> mapM (buildPaymentSettlementItem latestReconEntries mbDriverId mbFleetOwnerId) reports

  let totalItems = length settlements
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  pure API.PaymentSettlementListRes {totalItems, summary, settlements}
  where
    getLatestReconEntries :: Text -> [Text] -> Flow [(Text, ReconciliationEntry.ReconciliationEntry)]
    getLatestReconEntries _ [] = pure []
    getLatestReconEntries _ subscriptionIds = do
      entries <- QReconEntryExtra.findBySourceIdsAndType subscriptionIds ReconciliationEntry.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION
      pure $ foldl' upsertLatest [] entries

    pageLimit :: Int
    pageLimit = mkPageLimit mbLimit

    pageOffset :: Int
    pageOffset = mkPageOffset mbOffset

    fetchReportsForOwner :: Id DMOC.MerchantOperatingCity -> Text -> DSP.SubscriptionOwnerType -> Flow [PgPaymentSettlementReport.PgPaymentSettlementReport]
    fetchReportsForOwner merchantOpCityId ownerId ownerType = do
      subscriptions <- QSubscriptionPurchaseExtra.findAllByOwnerWithFilters merchantOpCityId ownerId ownerType Nothing mbFrom mbTo Nothing Nothing
      let subscriptionIds =
            subscriptions
              & filter (\subscription -> maybe True (== subscription.id.getId) mbSubscriptionPurchaseId)
              & map (.id.getId)

      if null subscriptionIds
        then pure []
        else do
          reportsBySubscription <- QPgPaymentSettlementReport.findByReferenceIds subscriptionIds
          pure $
            reportsBySubscription
              & filter (\report -> maybe True (== report.orderId) mbOrderId)
              & filter (\report -> maybe True (\settlementId -> report.settlementId == Just settlementId) mbSettlementId)
              & sortOn (Down . (.createdAt))
              & drop pageOffset
              & take pageLimit

    upsertLatest ::
      [(Text, ReconciliationEntry.ReconciliationEntry)] ->
      ReconciliationEntry.ReconciliationEntry ->
      [(Text, ReconciliationEntry.ReconciliationEntry)]
    upsertLatest acc entry = case entry.sourceId of
      Nothing -> acc
      Just sourceId ->
        let shouldReplace existing =
              entry.reconciliationDate > existing.reconciliationDate
                || ( entry.reconciliationDate == existing.reconciliationDate
                       && entry.updatedAt > existing.updatedAt
                   )
            merge [] = [(sourceId, entry)]
            merge ((key, existing) : rest)
              | key /= sourceId = (key, existing) : merge rest
              | shouldReplace existing = (sourceId, entry) : rest
              | otherwise = (key, existing) : rest
         in merge acc

    buildPaymentSettlementItem ::
      [(Text, ReconciliationEntry.ReconciliationEntry)] ->
      Maybe Text ->
      Maybe Text ->
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Flow (Maybe API.PaymentSettlementListItem)
    buildPaymentSettlementItem latestReconEntries mbDriverID mbFleetOwnerID report = do
      mbSubscription <- case report.referenceId of
        Just subscriptionPurchaseId -> QSubscriptionPurchase.findByPrimaryKey (Id subscriptionPurchaseId)
        Nothing -> pure Nothing

      let ownerMatches = case (mbDriverID, mbFleetOwnerID, mbSubscription) of
            (Nothing, Nothing, _) -> True
            (Just driverId, _, Just subscription) -> subscription.ownerType == DSP.DRIVER && subscription.ownerId == driverId
            (_, Just fleetOwnerId, Just subscription) -> subscription.ownerType == DSP.FLEET_OWNER && subscription.ownerId == fleetOwnerId
            _ -> False

      mbDriver <- case mbSubscription of
        Just subscription -> QPerson.findById (Id subscription.ownerId)
        Nothing -> pure Nothing

      driverMobileNo <- case mbDriver of
        Just driver -> decryptWithDefault driver.mobileNumber Nothing
        Nothing -> pure Nothing

      let driverName = mkFullName <$> mbDriver
          driverEmailId = mbDriver >>= (.email)
          netAmount = Just $ report.txnAmount - report.pgBaseFee - report.pgTax
          reconciliationEntry = report.referenceId >>= (`Kernel.Prelude.lookup` latestReconEntries)

      pure $
        if ownerMatches
          then
            Just
              API.PaymentSettlementListItem
                { subscriptionPurchaseId = report.referenceId,
                  merchantRefNo = Just report.merchantId,
                  merchantOperatingCityId = Just report.merchantOperatingCityId,
                  pgApprovalCode = report.pgApprovalCode,
                  pgOrderId = Just report.orderId,
                  transactionDateAndTime = report.txnDate,
                  transactionType =
                    Just
                      ( case report.txnType of
                          PgPaymentSettlementReport.ORDER -> "Payment"
                          PgPaymentSettlementReport.REFUND -> "Refund"
                      ),
                  chargedAmount = Just report.txnAmount,
                  paymentStatus = Just $ show report.txnStatus,
                  pgFees = Just report.pgBaseFee,
                  gstOnPgFees = Just report.pgTax,
                  netAmount = netAmount,
                  merchantId = Just report.merchantId,
                  paymentMode = show <$> report.paymentMethod,
                  driverName = driverName,
                  driverMobileNo = driverMobileNo,
                  driverEmailId = driverEmailId,
                  pgName = report.paymentGateway,
                  chargebackAmount = Nothing,
                  chargebackId = report.chargebackId,
                  chargebackReasonCode = report.chargebackReasonCode,
                  chargebackStatus = report.chargebackStatus,
                  representmentStatus = Nothing,
                  settlementId = report.settlementId,
                  settlementDate = report.settlementDate,
                  settlementCycle = Nothing,
                  settlementAmount = Just report.settlementAmount,
                  utr = report.utr,
                  settlementStatus = Nothing,
                  rrnNo = report.rrn,
                  reconciliationStatus = show . (.reconStatus) <$> reconciliationEntry,
                  reconciliationDate = (.reconciliationDate) <$> reconciliationEntry,
                  differenceAmount = (.variance) <$> reconciliationEntry
                }
          else Nothing

    mkFullName :: DP.Person -> Text
    mkFullName person = T.intercalate " " $ catMaybes [Just person.firstName, person.middleName, person.lastName]

getFinanceManagementFinancePaymentGatewayTransactionList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Text ->
  Flow API.PaymentTransactionReportListRes
getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity mbFrom mbGatewayTransactionId mbLimit mbOffset mbOrderId mbPaymentMode mbPaymentStatus mbPgGateway mbSubscriptionId mbTo mbTransactionType = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset
      mbTxnTypeForDb = mbTransactionType >>= parseTxnTypeFilter
      requiresInMemoryPagination =
        any
          isJust
          [ mbGatewayTransactionId,
            mbPaymentMode,
            mbPaymentStatus,
            mbPgGateway
          ]

  resolveBaseReportFilters mbOrderId mbSubscriptionId >>= \case
    Nothing -> pure $ emptyPaymentTransactionReportListRes 0 0
    Just (mbBaseSubscriptionId, mbBaseOrderId) -> do
      reports <-
        QPgPaymentSettlementReport.findAllByMerchantOpCityIdWithFilters
          merchant.id.getId
          merchantOpCityId.getId
          mbFrom
          mbTo
          mbBaseSubscriptionId
          mbBaseOrderId
          Nothing
          mbTxnTypeForDb
          (if requiresInMemoryPagination then Nothing else Just limit)
          (if requiresInMemoryPagination then Nothing else Just offset)

      transactions <- mapM buildPaymentGatewayTransactionItem reports

      let filteredTransactions = filter matchesPaymentTransactionFilters transactions
          paginatedTransactions =
            if requiresInMemoryPagination
              then take limit $ drop offset filteredTransactions
              else filteredTransactions
          totalItems = length paginatedTransactions
          summaryTotalCount =
            if requiresInMemoryPagination
              then length filteredTransactions
              else totalItems

      pure $
        API.PaymentTransactionReportListRes
          { totalItems = totalItems,
            summary = Dashboard.Common.Summary {totalCount = summaryTotalCount, count = totalItems},
            transactions = paginatedTransactions
          }
  where
    emptyPaymentTransactionReportListRes :: Int -> Int -> API.PaymentTransactionReportListRes
    emptyPaymentTransactionReportListRes summaryTotalCount totalItems =
      API.PaymentTransactionReportListRes
        { totalItems = totalItems,
          summary = Dashboard.Common.Summary {totalCount = summaryTotalCount, count = totalItems},
          transactions = []
        }

    resolveBaseReportFilters ::
      Maybe Text ->
      Maybe Text ->
      Flow (Maybe (Maybe Text, Maybe Text))
    resolveBaseReportFilters explicitOrderId Nothing =
      pure $ Just (Nothing, explicitOrderId)
    resolveBaseReportFilters explicitOrderId (Just subscriptionId) = do
      QSubscriptionPurchase.findByPrimaryKey (Id subscriptionId) >>= \case
        Nothing -> pure Nothing
        Just subscription -> do
          mbOrder <- QPaymentOrder.findById subscription.paymentOrderId
          let resolvedOrderId = explicitOrderId <|> (mbOrder <&> (.shortId.getShortId))
              resolvedSubscriptionId =
                if isNothing resolvedOrderId
                  then Just subscriptionId
                  else Nothing
          pure $ Just (resolvedSubscriptionId, resolvedOrderId)

    buildPaymentGatewayTransactionItem ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Flow API.PaymentTransactionReportItem
    buildPaymentGatewayTransactionItem report = do
      mbOrderFromShortId <- QPaymentOrder.findByShortId (ShortId report.orderId)

      mbSubscription <-
        case report.referenceId of
          Just subscriptionId ->
            QSubscriptionPurchase.findByPrimaryKey (Id subscriptionId) >>= \case
              Just subscription -> pure $ Just subscription
              Nothing -> resolveSubscriptionFromOrder mbOrderFromShortId
          Nothing -> resolveSubscriptionFromOrder mbOrderFromShortId

      mbOrder <- case mbOrderFromShortId of
        Just order -> pure $ Just order
        Nothing -> maybe (pure Nothing) (QPaymentOrder.findById . (.paymentOrderId)) mbSubscription

      mbPaymentTransaction <- case mbOrder of
        Just order -> selectRelevantPaymentTransaction <$> QPaymentTransaction.findAllByOrderId order.id
        Nothing -> pure Nothing

      mbRefund <- case mbOrder of
        -- TODO(finance): Match refund using report.refundId to avoid picking wrong refund
        -- when an order has multiple refunds; keep latest fallback only if refundId is missing.  @dhruv-1010
        Just order -> QRefunds.findLatestByOrderId order.shortId
        Nothing -> pure Nothing

      (payerTypeValue, payerNameValue, payerIdValue, payerMobileValue, payerEmailValue, fleetOwnerIdValue) <-
        maybe (pure (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)) resolvePayerContext mbSubscription

      mbMerchantOpCity <- case mbSubscription of
        Just subscription -> CQMOC.findById subscription.merchantOperatingCityId
        Nothing -> CQMOC.findById (Id report.merchantOperatingCityId)

      let gatewayTransactionIdValue = (mbPaymentTransaction >>= (.epgTxnId)) <|> report.txnId
          merchantOrderIdValue = Just report.orderId <|> (mbOrder <&> (.shortId.getShortId))
          orderCreatedAt = (.createdAt) <$> mbOrder
          transactionTypeValue = Just $ show report.txnType
          transactionStatusValue = resolveTransactionStatus report mbOrder mbRefund
          failureReasonValue = resolveFailureReason report mbOrder mbPaymentTransaction mbRefund
          errorCodeValue = resolveErrorCode report mbOrder mbPaymentTransaction mbRefund
          refundAmountValue = (mbRefund <&> (.refundAmount)) <|> report.refundAmount
          gatewayChargesValue = resolveGatewayCharges report mbPaymentTransaction
          gstOnChargesValue = resolveGstOnCharges report mbPaymentTransaction
          paymentModeValue = resolvePaymentMode report mbPaymentTransaction
          paymentSubModeValue = resolvePaymentSubMode report mbPaymentTransaction
          walletProviderValue =
            case canonicalizeText <$> paymentModeValue of
              Just "wallet" -> paymentSubModeValue
              _ -> Nothing
          cityValue = mbMerchantOpCity <&> (.city) <&> show

      pure $
        API.PaymentTransactionReportItem
          { gatewayTransactionId = gatewayTransactionIdValue,
            merchantOrderId = merchantOrderIdValue,
            referenceId = Nothing,
            correlationId = Nothing,
            parentTransactionId = Nothing,
            transactionType = transactionTypeValue,
            transactionStatus = transactionStatusValue,
            failureReason = failureReasonValue,
            errorCode = errorCodeValue,
            transactionInitiationDate = orderCreatedAt,
            paymentPageOpenDate = orderCreatedAt,
            paymentGatewayOpenDate = mbPaymentTransaction >>= (.authorizationDateTime),
            refundInitiationDate = mbRefund <&> (.createdAt),
            refundCompletionDate = report.refundDate <|> (mbRefund >>= (.completedAt)),
            transactionAmountGross = resolveTransactionAmountGross report mbPaymentTransaction refundAmountValue,
            netAmount = resolveNetAmount report mbPaymentTransaction refundAmountValue,
            gatewayCharges = gatewayChargesValue,
            gstOnCharges = gstOnChargesValue,
            tdsAmount = Nothing,
            withholdingAmount = Nothing,
            refundAmount = refundAmountValue,
            currency = ((show <$> (mbOrder <&> (.currency))) <|> Just (show report.currency)),
            exchangeRate = Nothing,
            paymentMode = paymentModeValue,
            paymentSubMode = paymentSubModeValue,
            upiId = mbOrder >>= (.vpa),
            upiAppName = Nothing,
            cardType = mbPaymentTransaction >>= (.cardType),
            cardNetwork = mbPaymentTransaction >>= (.cardBrand),
            maskedCardNumber = mkMaskedCardNumber (mbPaymentTransaction >>= (.cardIsin)) (mbPaymentTransaction >>= (.cardLastFourDigits)),
            issuerBank = mbPaymentTransaction >>= (.cardIssuer),
            walletProvider = walletProviderValue,
            payerType = payerTypeValue,
            payerName = payerNameValue,
            payerId = payerIdValue,
            payerMobile = payerMobileValue,
            payerEmail = payerEmailValue,
            subscriptionId = (mbSubscription <&> (.id.getId)) <|> report.referenceId,
            vehicleId = Nothing,
            fleetOwnerId = fleetOwnerIdValue,
            city = cityValue,
            operatingLocation = cityValue,
            serviceType = if isJust mbSubscription then Just "Ride based subscription" else Nothing,
            refundId = (mbRefund >>= (.idAssignedByServiceProvider)) <|> report.refundId,
            refundReason = Nothing,
            refundMode = show <$> report.refundMethod,
            refundStatus = ((show . (.status)) <$> mbRefund) <|> guardReportRefundStatus report,
            ipAddress = Nothing,
            pgName = (mbPaymentTransaction >>= (.gatewayName)) <|> report.paymentGateway,
            pgMerchantId = mbOrder >>= (.paymentMerchantId)
          }

    resolveSubscriptionFromOrder ::
      Maybe PaymentOrder.PaymentOrder ->
      Flow (Maybe DSP.SubscriptionPurchase)
    resolveSubscriptionFromOrder =
      maybe (pure Nothing) (QSubscriptionPurchase.findByPaymentOrderId . (.id))

    selectRelevantPaymentTransaction ::
      [PaymentTransaction.PaymentTransaction] ->
      Maybe PaymentTransaction.PaymentTransaction
    selectRelevantPaymentTransaction transactions =
      listToMaybe (filter (\txn -> canonicalizeText (show txn.status) == "charged") transactions)
        <|> listToMaybe transactions

    resolvePayerContext ::
      DSP.SubscriptionPurchase ->
      Flow (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
    resolvePayerContext subscription =
      case subscription.ownerType of
        DSP.DRIVER -> do
          mbDriver <- QPerson.findById (Id subscription.ownerId)
          driverMobile <- maybe (pure Nothing) (\driver -> decryptWithDefault driver.mobileNumber Nothing) mbDriver
          mbFleetAssoc <- case mbDriver of
            Just driver -> QFleetDriver.findByDriverId driver.id True
            Nothing -> pure Nothing
          pure
            ( Just "DCO",
              mkFullName <$> mbDriver,
              Just subscription.ownerId,
              driverMobile,
              mbDriver >>= (.email),
              mbFleetAssoc <&> (.fleetOwnerId)
            )
        DSP.FLEET_OWNER -> do
          mbFleetOwner <- QPerson.findById (Id subscription.ownerId)
          fleetOwnerMobile <- maybe (pure Nothing) (\fleetOwner -> decryptWithDefault fleetOwner.mobileNumber Nothing) mbFleetOwner
          pure
            ( Just "FO",
              mkFullName <$> mbFleetOwner,
              Just subscription.ownerId,
              fleetOwnerMobile,
              mbFleetOwner >>= (.email),
              Just subscription.ownerId
            )

    resolveTransactionStatus ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentOrder.PaymentOrder ->
      Maybe PaymentRefund.Refunds ->
      Maybe Text
    resolveTransactionStatus report mbOrder mbRefund =
      case report.txnType of
        PgPaymentSettlementReport.ORDER -> (show <$> (mbOrder <&> (.status))) <|> Just (show report.txnStatus)
        PgPaymentSettlementReport.REFUND -> ((show . (.status)) <$> mbRefund) <|> Just (show report.txnStatus)

    resolveFailureReason ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentOrder.PaymentOrder ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe PaymentRefund.Refunds ->
      Maybe Text
    resolveFailureReason report mbOrder mbPaymentTransaction mbRefund =
      case report.txnType of
        PgPaymentSettlementReport.ORDER ->
          (mbOrder >>= (.bankErrorMessage)) <|> (mbPaymentTransaction >>= (.bankErrorMessage))
        PgPaymentSettlementReport.REFUND ->
          (mbRefund >>= (.errorMessage))
            <|> (mbOrder >>= (.bankErrorMessage))
            <|> (mbPaymentTransaction >>= (.bankErrorMessage))

    resolveErrorCode ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentOrder.PaymentOrder ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe PaymentRefund.Refunds ->
      Maybe Text
    resolveErrorCode report mbOrder mbPaymentTransaction mbRefund =
      case report.txnType of
        PgPaymentSettlementReport.ORDER ->
          (mbOrder >>= (.bankErrorCode)) <|> (mbPaymentTransaction >>= (.bankErrorCode))
        PgPaymentSettlementReport.REFUND ->
          (mbRefund >>= (.errorCode))
            <|> (mbOrder >>= (.bankErrorCode))
            <|> (mbPaymentTransaction >>= (.bankErrorCode))

    resolveGatewayCharges ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe HighPrecMoney
    resolveGatewayCharges report mbPaymentTransaction =
      case report.txnType of
        PgPaymentSettlementReport.ORDER ->
          (mbPaymentTransaction >>= (.surchargeAmount)) <|> Just report.pgBaseFee
        PgPaymentSettlementReport.REFUND ->
          report.refundBaseFee <|> Just report.pgBaseFee

    resolveGstOnCharges ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe HighPrecMoney
    resolveGstOnCharges report mbPaymentTransaction =
      case report.txnType of
        PgPaymentSettlementReport.ORDER ->
          (mbPaymentTransaction >>= (.taxAmount)) <|> Just report.pgTax
        PgPaymentSettlementReport.REFUND ->
          report.refundTax <|> Just report.pgTax

    resolveTransactionAmountGross ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe HighPrecMoney ->
      Maybe HighPrecMoney
    resolveTransactionAmountGross report mbPaymentTransaction refundAmountValue =
      case report.txnType of
        PgPaymentSettlementReport.ORDER ->
          (mbPaymentTransaction <&> (.amount)) <|> Just report.txnAmount
        PgPaymentSettlementReport.REFUND ->
          refundAmountValue <|> Just report.txnAmount

    resolveNetAmount ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe HighPrecMoney ->
      Maybe HighPrecMoney
    resolveNetAmount report mbPaymentTransaction refundAmountValue =
      case report.txnType of
        PgPaymentSettlementReport.ORDER ->
          (mbPaymentTransaction >>= (.netAmount))
            <|> Just (report.txnAmount - report.pgBaseFee - report.pgTax)
        PgPaymentSettlementReport.REFUND ->
          case (refundAmountValue, report.refundBaseFee, report.refundTax) of
            (Just refundAmount, Just refundBaseFee, Just refundTax) -> Just (refundAmount - refundBaseFee - refundTax)
            (Just refundAmount, _, _) -> Just refundAmount
            _ -> Nothing

    resolvePaymentMode ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe Text
    resolvePaymentMode report mbPaymentTransaction =
      (show <$> report.paymentMethod)
        <|> (mbPaymentTransaction >>= (.paymentMethod))
        <|> (mbPaymentTransaction >>= (.paymentMethodType))

    resolvePaymentSubMode ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe PaymentTransaction.PaymentTransaction ->
      Maybe Text
    resolvePaymentSubMode report mbPaymentTransaction =
      report.paymentMethodSubType
        <|> (mbPaymentTransaction >>= (.paymentMethodType))
        <|> (mbPaymentTransaction >>= (.paymentMethod))

    guardReportRefundStatus ::
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Maybe Text
    guardReportRefundStatus report =
      case report.txnType of
        PgPaymentSettlementReport.REFUND -> Just (show report.txnStatus)
        PgPaymentSettlementReport.ORDER -> Nothing

    matchesPaymentTransactionFilters :: API.PaymentTransactionReportItem -> Bool
    matchesPaymentTransactionFilters item =
      and
        [ matchesMaybeTextFilter mbGatewayTransactionId item.gatewayTransactionId,
          matchesMaybeTextFilter mbOrderId item.merchantOrderId,
          matchesPaymentModeFilter mbPaymentMode item.paymentMode,
          matchesMaybeTextFilter mbPaymentStatus item.transactionStatus,
          matchesMaybeTextFilter mbPgGateway item.pgName,
          matchesMaybeTextFilter mbSubscriptionId item.subscriptionId,
          matchesMaybeTextFilter mbTransactionType item.transactionType
        ]

    matchesMaybeTextFilter :: Maybe Text -> Maybe Text -> Bool
    matchesMaybeTextFilter Nothing _ = True
    matchesMaybeTextFilter (Just expectedValue) mbActualValue =
      maybe False ((== canonicalizeText expectedValue) . canonicalizeText) mbActualValue

    matchesPaymentModeFilter :: Maybe Text -> Maybe Text -> Bool
    matchesPaymentModeFilter Nothing _ = True
    matchesPaymentModeFilter (Just expectedValue) mbActualValue =
      case canonicalizeText expectedValue of
        "card" ->
          maybe
            False
            (\actualValue -> canonicalizeText actualValue `elem` ["card", "creditcard", "debitcard"])
            mbActualValue
        normalizedExpectedValue ->
          maybe False ((== normalizedExpectedValue) . canonicalizeText) mbActualValue

    canonicalizeText :: Text -> Text
    canonicalizeText =
      T.filter (\char -> char /= ' ' && char /= '_' && char /= '-')
        . T.toLower
        . T.strip

    parseTxnTypeFilter :: Text -> Maybe PgPaymentSettlementReport.TxnType
    parseTxnTypeFilter txnTypeText =
      case canonicalizeText txnTypeText of
        "order" -> Just PgPaymentSettlementReport.ORDER
        "refund" -> Just PgPaymentSettlementReport.REFUND
        _ -> Nothing

    mkMaskedCardNumber :: Maybe Text -> Maybe Text -> Maybe Text
    mkMaskedCardNumber mbCardIsin mbLastFourDigits =
      case (mbCardIsin, mbLastFourDigits) of
        (Just cardIsin, Just lastFourDigits) -> Just $ cardIsin <> "XXXXXX" <> lastFourDigits
        _ -> Nothing

    mkFullName :: DP.Person -> Text
    mkFullName person = T.intercalate " " $ catMaybes [Just person.firstName, person.middleName, person.lastName]

-- | Get wallet ledger with filters.
-- API/spec order is: limit, offset, driverId, fleetOperatorId, from, to, sourceType.
getFinanceManagementFinanceWalletLedger ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Flow API.WalletLedgerRes
getFinanceManagementFinanceWalletLedger merchantShortId opCity mbLimit mbOffset mbDriverId mbFleetOperatorId mbFrom mbTo mbSourceType =
  getFinanceManagementFinanceWalletLedgerImpl merchantShortId opCity mbDriverId mbFleetOperatorId mbFrom mbLimit mbOffset mbSourceType mbTo

getFinanceManagementFinanceWalletLedgerImpl ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.WalletLedgerRes
getFinanceManagementFinanceWalletLedgerImpl merchantShortId opCity mbDriverId mbFleetOperatorId mbFrom mbLimit mbOffset mbSourceType mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Resolve counterparty type and owner ID from driverId or fleetOperatorId
  let mbOwnerInfo = case (mbDriverId, mbFleetOperatorId) of
        (Just driverId, _) -> Just (DRIVER, driverId)
        (_, Just fleetOwnerId) -> Just (FLEET_OWNER, fleetOwnerId)
        _ -> Nothing

  case mbOwnerInfo of
    Nothing ->
      pure $
        API.WalletLedgerRes
          { availableWalletBalance = Nothing,
            lockedWalletBalance = Nothing,
            lastWalletUpdatedAt = Nothing,
            totalItems = 0,
            ledgerEntries = []
          }
    Just (counterpartyType, ownerId) -> do
      mbAccount <- WalletService.getWalletAccountByOwner counterpartyType ownerId

      case mbAccount of
        Nothing ->
          pure $
            API.WalletLedgerRes
              { availableWalletBalance = Nothing,
                lockedWalletBalance = Nothing,
                lastWalletUpdatedAt = Nothing,
                totalItems = 0,
                ledgerEntries = []
              }
        Just account -> do
          let availableBalance = account.balance

          let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
              cutOffDays = transporterConfig.driverWalletConfig.payoutCutOffDays
          lockedBalance <- WalletService.getNonRedeemableBalance account.id timeDiff cutOffDays =<< getCurrentTime

          -- Get last wallet update time from latest ledger entry
          allEntries <- LedgerService.getEntriesByAccount account.id
          let lastUpdated = listToMaybe $ sortOn (Down . (.createdAt)) allEntries <&> (.createdAt)

          -- Query ledger entries with filters
          let mbReferenceTypes = (\st -> [st]) <$> mbSourceType
          filteredEntries <- LedgerService.findByAccountWithFilters account.id mbFrom mbTo Nothing Nothing Nothing mbReferenceTypes

          -- Apply pagination
          let paginatedEntries = take limit $ drop offset filteredEntries

          -- Map entries to response items
          ledgerItems <- mapM (buildLedgerItem account.id) paginatedEntries

          pure $
            API.WalletLedgerRes
              { availableWalletBalance = Just availableBalance,
                lockedWalletBalance = Just lockedBalance,
                lastWalletUpdatedAt = lastUpdated,
                totalItems = length ledgerItems,
                ledgerEntries = ledgerItems
              }
  where
    buildLedgerItem :: Id Account.Account -> LedgerEntry.LedgerEntry -> Flow API.WalletLedgerItem
    buildLedgerItem walletAccountId entry = do
      let isCredit = entry.toAccountId == walletAccountId
          creditAmount = if isCredit then Just entry.amount else Just 0
          debitAmount = if not isCredit then Just entry.amount else Just 0
          openingBalance = if isCredit then entry.toStartingBalance else entry.fromStartingBalance
          closingBalance = if isCredit then entry.toEndingBalance else entry.fromEndingBalance

      pure $
        API.WalletLedgerItem
          { walletTxnId = Just entry.id.getId,
            walletTxnDate = Just entry.createdAt,
            sourceType = Just entry.referenceType,
            sourceReferenceId = Just entry.referenceId,
            creditAmount = creditAmount,
            debitAmount = debitAmount,
            openingBalance = openingBalance,
            closingBalance = closingBalance
          }

getFinanceManagementFinanceEarningSummary ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow API.EarningsSummaryRes
getFinanceManagementFinanceEarningSummary _merchantShortId _opCity mbDriverId mbFleetOwnerId mbFrom mbTo = do
  (ownerId, counterpartyType) <- case (mbDriverId, mbFleetOwnerId) of
    (Just driverId, Nothing) -> pure (driverId, DRIVER)
    (Nothing, Just fleetOwnerId) -> pure (fleetOwnerId, FLEET_OWNER)
    (Just _, Just _) -> throwError $ InvalidRequest "Provide either driverId or fleetOwnerId, not both"
    (Nothing, Nothing) -> throwError $ InvalidRequest "Provide either driverId or fleetOwnerId"

  mbAccount <- WalletService.getWalletAccountByOwner counterpartyType ownerId

  case mbAccount of
    Nothing -> pure $ emptyEarningsSummary
    Just account -> do
      allEntries <-
        LedgerService.findByAccountWithFilters
          account.id
          mbFrom
          mbTo
          Nothing
          Nothing
          (Just LedgerEntry.SETTLED)
          Nothing

      let rideEarningEntries = filter (\e -> e.toAccountId == account.id && e.referenceType `elem` rideEarningTypes) allEntries
      let baseRideEarnings = sum $ map (.amount) rideEarningEntries

      let rideEarningBookingIds = map (.referenceId) rideEarningEntries

      gstOnlineEntriesForRideBookings <- fmap concat $
        forM rideEarningBookingIds $ \bookingId ->
          LedgerService.getEntriesByReference WalletService.walletReferenceGSTOnline bookingId

      let gstOnlineCreditAmount = sum $ map (.amount) $ filter (\e -> e.toAccountId == account.id) gstOnlineEntriesForRideBookings
          totalRideEarnings = baseRideEarnings + gstOnlineCreditAmount
          penaltyEntries = filter (\e -> e.fromAccountId == account.id && e.referenceType == "DriverCancellation") allEntries
          totalPenalties = sum $ map (.amount) penaltyEntries
          totalIncentives = 0
          grossEarnings = totalRideEarnings + totalIncentives
          tdsEntries = filter (\e -> e.fromAccountId == account.id && e.referenceType `elem` tdsTypes) allEntries
          totalTdsDeducted = sum $ map (.amount) tdsEntries
          gstCashEntries = filter (\e -> e.fromAccountId == account.id && e.referenceType == WalletService.walletReferenceGSTCash) allEntries
          gstCashAmount = sum $ map (.amount) gstCashEntries
          gstOnlineDebitAmount = sum $ map (.amount) $ filter (\e -> e.fromAccountId == account.id) gstOnlineEntriesForRideBookings
          totalGstDeducted = gstCashAmount + gstOnlineDebitAmount
          netEarnings = grossEarnings - totalTdsDeducted - totalGstDeducted - totalPenalties

      pure $
        API.EarningsSummaryRes
          { totalRideEarnings,
            totalIncentives,
            totalPenalties,
            grossEarnings,
            totalTdsDeducted,
            totalGstDeducted,
            netEarnings
          }
  where
    rideEarningTypes :: [Text]
    rideEarningTypes = [WalletService.walletReferenceBaseRide, "UserCancellation", "BuyerDiscount"]

    tdsTypes :: [Text]
    tdsTypes = [WalletService.walletReferenceTDSDeductionOnline, WalletService.walletReferenceTDSDeductionCash]

    emptyEarningsSummary :: API.EarningsSummaryRes
    emptyEarningsSummary =
      API.EarningsSummaryRes
        { totalRideEarnings = 0,
          totalIncentives = 0,
          totalPenalties = 0,
          grossEarnings = 0,
          totalTdsDeducted = 0,
          totalGstDeducted = 0,
          netEarnings = 0
        }
