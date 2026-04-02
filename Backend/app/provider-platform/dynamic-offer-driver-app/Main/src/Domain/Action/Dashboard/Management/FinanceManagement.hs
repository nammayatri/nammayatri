module Domain.Action.Dashboard.Management.FinanceManagement
  ( getFinanceManagementSubscriptionPurchaseList,
    getFinanceManagementInvoiceList,
    getFinanceManagementFinanceInvoiceList,
    getFinanceManagementFinanceReconciliation,
    postFinanceManagementReconciliationTrigger,
    getFinanceManagementFinancePaymentSettlementList,
    getFinanceManagementFinancePaymentGatewayTransactionList,
    getFinanceManagementFinanceWalletLedger,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement as API
import qualified Dashboard.Common
import Data.List (nub, partition)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Domain.Action.UI.Plan (getPlanAmount)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SubscriptionPurchase as DSP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (UTCTime, listToMaybe)
import qualified Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), ShortId (..), cast)
import Kernel.Utils.Common (secondsToNominalDiffTime)
import Kernel.Utils.Error (fromMaybeM, throwError)
import Lib.Finance.Domain.Types.Account (CounterpartyType (..))
import qualified Lib.Finance.Domain.Types.Account as Account
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as PgPaymentSettlementReport
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconciliationEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as ReconSummary
import qualified Lib.Finance.Ledger.Service as LedgerService
import qualified Lib.Finance.Storage.Queries.DirectTaxTransactionExtra as QDirectTax
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QInvoiceLedgerLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerEntryExtra
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra as QPgPaymentSettlementReport
import qualified Lib.Finance.Storage.Queries.ReconciliationEntryExtra as QReconEntryExtra
import qualified Lib.Finance.Storage.Queries.ReconciliationSummaryExtra as QReconSummaryExtra
import qualified Lib.Payment.Domain.Types.PaymentOrder as PaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as PaymentTransaction
import qualified Lib.Payment.Domain.Types.Refunds as PaymentRefund
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified Lib.Payment.Storage.HistoryQueries.Refunds as HQRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QSchedulerJob
import SharedLogic.Allocator (AllocatorJobType (..), ReconciliationJobData (..))
import qualified SharedLogic.Finance.Wallet as WalletService
import qualified SharedLogic.Merchant as SMerchant
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQPlan
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase
import Tools.Encryption (decryptWithDefault)

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
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe (Id Dashboard.Common.Driver) ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe API.SubscriptionPurchaseStatus ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.SubscriptionPurchaseListRes
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity mbAmountMax mbAmountMin mbDriverId mbFleetOperatorId mbFrom mbLimit mbOffset mbServiceName mbStatus mbSubscriptionId mbTo = do
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
      case (getId <$> mbDriverId, mbFleetOperatorId) of
        (Just driverId, _) ->
          fetchSubscriptionsForOwner driverId DSP.DRIVER (fromMaybe DPlan.PREPAID_SUBSCRIPTION mbParsedServiceName) mbParsedStatus limit offset merchantOpCityId
        (_, Just fleetOwnerId) ->
          fetchSubscriptionsForOwner fleetOwnerId DSP.FLEET_OWNER (fromMaybe DPlan.PREPAID_SUBSCRIPTION mbParsedServiceName) mbParsedStatus limit offset merchantOpCityId
        _ -> do
          QSubscriptionPurchase.findAllByMerchantOpCityIdWithFilters
            merchantOpCityId
            mbParsedServiceName
            mbParsedStatus
            mbFrom
            mbTo
            mbAmountMin
            mbAmountMax
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
        mbAmountMin
        mbAmountMax
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
      let discountAmount = case plan.originalRegistrationAmount of
            Just origAmt -> Just (origAmt - plan.registrationAmount)
            Nothing -> Just 0
      let totalAmount = (\b d -> b - d) <$> baseAmount <*> discountAmount

      -- GST details from IndirectTaxTransaction
      let gstRate = mbSubscriptionTxn <&> (.gstRate)
      let gstAmount = mbSubscriptionTxn <&> (.totalGstAmount)

      -- Gross and total subscription amounts
      let grossSubscriptionAmount = baseAmount
      let totalSubscriptionAmount = (\b g -> b + g) <$> baseAmount <*> gstAmount

      -- Calculate plan status from Plan's deprecated flag
      let planStatus = Just $ if plan.isDeprecated then "INACTIVE" else "ACTIVE"

      -- Get plan validity days directly from plan table
      let planValidityDays = plan.validityInDays

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
            subscriptionStartDate = subscription.startDate <|> Just subscription.purchaseTimestamp,
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
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe FinanceInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe FinanceInvoice.InvoiceStatus ->
  Maybe UTCTime ->
  Flow API.InvoiceListRes
getFinanceManagementInvoiceList merchantShortId opCity mbFleetOwnerOrDriverId mbFrom mbInvoiceId mbInvoiceNumber mbInvoiceType mbLimit mbOffset mbStatus mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Get invoices based on filters (invoiceId takes precedence, then invoiceNumber, then date range)
  invoices <- case mbInvoiceId of
    Just invoiceId -> do
      QFinanceInvoice.findById (Id invoiceId) >>= \case
        Just inv -> pure [inv]
        Nothing -> pure []
    Nothing -> case mbInvoiceNumber of
      Just invoiceNumber -> do
        QFinanceInvoice.findByNumber invoiceNumber >>= \case
          Just inv -> pure [inv]
          Nothing -> pure []
      Nothing -> do
        let mbIssuedToId =
              case mbInvoiceType of
                Just FinanceInvoice.Ride -> Nothing
                _ -> mbFleetOwnerOrDriverId
            mbSupplierId =
              case mbInvoiceType of
                Just FinanceInvoice.Ride -> mbFleetOwnerOrDriverId
                _ -> Nothing
         in QFinanceInvoiceExtra.findByMerchantOpCityIdAndDateRange
              merchantOpCityId.getId
              mbFrom
              mbTo
              mbInvoiceType
              mbStatus
              mbIssuedToId
              mbSupplierId
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

      let (taxableValue, gstRate, gstAmount, cgstAmount, sgstAmount, gstinOfParty, sacCode, mbTaxRate, mbIssuedToTaxNo, mbIssuedByTaxNo) = case indirectTaxTxns of
            (txn : _) ->
              ( Just txn.taxableValue,
                Just txn.gstRate,
                Just txn.totalGstAmount,
                Just txn.cgstAmount,
                Just txn.sgstAmount,
                txn.gstinOfParty,
                txn.sacCode,
                txn.taxRate,
                txn.issuedToTaxNo,
                txn.issuedByTaxNo
              )
            _ -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

      -- Get TDS details from direct tax transaction by invoiceNumber
      directTaxTxns <- QDirectTax.findByInvoiceNumber invoice.invoiceNumber
      let tdsRef = case directTaxTxns of
            (txn : _) -> Just $ fromMaybe "" txn.tdsSection <> " (TDS: " <> show txn.tdsAmount <> ")"
            _ -> Nothing

      -- Get linked ride/subscription IDs from ledger links
      ledgerLinks <- QInvoiceLedgerLink.findByInvoice invoice.id
      ledgerEntries <- mapM (QLedgerEntry.findById . (.ledgerEntryId)) ledgerLinks

      -- Extract ride_id and subscription_id from ledger entries
      let (rideIds, subscriptionIds) = foldr extractIds ([], []) (catMaybes ledgerEntries)

      -- Get payment method from payment_transaction via invoice.paymentOrderId
      mbPaymentMethod <- case invoice.paymentOrderId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing

      pure $
        API.InvoiceListItem
          { invoiceId = invoice.id.getId,
            invoiceNumber = invoice.invoiceNumber,
            invoiceType = invoice.invoiceType,
            invoiceDate = invoice.issuedAt,
            invoiceStatus = invoice.status,
            counterpartyType = invoice.issuedToType,
            counterpartyId = invoice.issuedToId,
            taxableValue = taxableValue,
            gstRate = gstRate,
            gstAmount = gstAmount,
            cgstAmount = cgstAmount,
            sgstAmount = sgstAmount,
            totalInvoiceValue = invoice.totalAmount,
            tdsReference = tdsRef,
            irn = Nothing,
            qrCode = Nothing,
            rideId = listToMaybe rideIds,
            subscriptionId = listToMaybe subscriptionIds,
            supplierName = invoice.supplierName,
            supplierAddress = invoice.supplierAddress,
            supplierGstin = invoice.supplierGSTIN,
            supplierTaxNo = invoice.supplierTaxNo,
            issuedToName = invoice.issuedToName,
            issuedToAddress = invoice.issuedToAddress,
            issuedByName = invoice.issuedByName,
            issuedByAddress = invoice.issuedByAddress,
            gstinOfParty = gstinOfParty,
            sacCode = sacCode,
            paymentMethod = mbPaymentMethod,
            taxableValueOfServiceSupplied = Just invoice.subtotal,
            lineItems = invoice.lineItems,
            generatedAt = invoice.createdAt,
            taxRate = mbTaxRate,
            issuedToTaxNo = mbIssuedToTaxNo,
            issuedByTaxNo = mbIssuedByTaxNo
          }

    extractIds :: LedgerEntry.LedgerEntry -> ([Text], [Text]) -> ([Text], [Text])
    extractIds entry (rides, subs) =
      case entry.referenceType of
        "Ride" -> (entry.referenceId : rides, subs)
        "SubscriptionPurchase" -> (rides, entry.referenceId : subs)
        _ -> (rides, subs)

-- Alias for generated API
getFinanceManagementFinanceInvoiceList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text -> -- fleetOwnerOrDriverId
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe FinanceInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe FinanceInvoice.InvoiceStatus ->
  Maybe UTCTime ->
  Flow API.InvoiceListRes
getFinanceManagementFinanceInvoiceList = getFinanceManagementInvoiceList

-- | Get reconciliation data
getFinanceManagementFinanceReconciliation ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  ReconSummary.ReconciliationType ->
  Flow API.ReconciliationRes
getFinanceManagementFinanceReconciliation merchantShortId opCity mbFromDate mbLimit mbOffset mbToDate reconciliationType = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset
      -- Subtract IST offset (5h 30m) from incoming dates before querying (DB stores UTC)
      istOffset = secondsToNominalDiffTime 19800
      adjustedFromDate = addUTCTime (- istOffset) <$> mbFromDate
      adjustedToDate = addUTCTime (- istOffset) <$> mbToDate

  summaries <- QReconSummaryExtra.findByDateRangeAndType adjustedFromDate adjustedToDate reconciliationType

  let latestSummary = listToMaybe summaries
      summaryRes = maybe defaultSummary toSummary latestSummary

  entries <- case latestSummary of
    Just summary -> QReconEntryExtra.findBySummaryIdWithPagination summary.id limit offset
    Nothing -> pure []

  let (matched, mismatched) = partition (\e -> e.reconStatus == ReconciliationEntry.MATCHED) entries
  pure
    API.ReconciliationRes
      { summary = summaryRes,
        exceptions = map toReconEntry mismatched,
        completed = map toReconEntry matched
      }
  where
    defaultSummary :: API.ReconciliationSummary
    defaultSummary = API.ReconciliationSummary 0 0 "0%" 0 0 0

    toSummary :: ReconSummary.ReconciliationSummary -> API.ReconciliationSummary
    toSummary s =
      API.ReconciliationSummary
        { totalDiscrepancies = s.totalDiscrepancies,
          matchedRecords = s.matchedRecords,
          matchRate = s.matchRate,
          sourceTotal = s.sourceTotal,
          targetTotal = s.targetTotal,
          varianceAmount = s.varianceAmount
        }

    toReconEntry :: ReconciliationEntry.ReconciliationEntry -> API.ReconciliationEntry
    toReconEntry entry =
      API.ReconciliationEntry
        { bookingId = entry.bookingId,
          dcoId = entry.dcoId,
          status = show <$> entry.status,
          mode = show <$> entry.mode,
          expectedValue = Just entry.expectedDsrValue,
          actualValue = Just entry.actualLedgerValue,
          variance = Just entry.variance,
          reconStatus = Just (show entry.reconStatus),
          mismatchReason = entry.mismatchReason,
          timestamp = Just entry.timestamp,
          financeComponent = show <$> entry.financeComponent,
          sourceId = entry.sourceId,
          targetId = entry.targetId,
          settlementId = entry.settlementId,
          settlementDate = entry.settlementDate,
          settlementMode = entry.settlementMode,
          transactionDate = entry.transactionDate,
          rrn = entry.rrn
        }

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

getFinanceManagementFinancePaymentSettlementList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime -> -- from
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Maybe (ShortId Dashboard.Common.PaymentOrder) -> -- paymentOrderShortId
  Maybe Text -> -- pgApprovalCode
  Maybe API.PgGateway -> -- pgGateway
  Maybe HighPrecMoney -> -- settlementAmountMax
  Maybe HighPrecMoney -> -- settlementAmountMin
  Maybe UTCTime -> -- settlementFrom
  Maybe (Id Dashboard.Common.PGPaymentSettlementReport) -> -- settlementId
  Maybe UTCTime -> -- settlementTo
  Maybe Text -> -- settlementUtr
  Maybe (Id Dashboard.Common.SubscriptionPurchase) -> -- subscriptionPurchaseId
  Maybe UTCTime -> -- to
  Flow API.PaymentSettlementListRes
getFinanceManagementFinancePaymentSettlementList merchantShortId opCity mbFrom mbLimit mbOffset mbPaymentOrderShortId mbPgApprovalCode mbPgGateway mbSettlementAmountMax mbSettlementAmountMin mbSettlementFrom mbSettlementId mbSettlementTo mbSettlementUtr mbSubscriptionPurchaseId mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  reports <-
    QPgPaymentSettlementReport.findAllByMerchantOpCityIdWithFilters
      merchant.id.getId
      merchantOpCityId.getId
      mbFrom
      mbTo
      (mbSubscriptionPurchaseId <&> (.getId))
      (mbPaymentOrderShortId <&> (.getShortId))
      (mbSettlementId <&> (.getId))
      Nothing
      mbPgApprovalCode
      mbSettlementUtr
      mbSettlementFrom
      mbSettlementTo
      (mbPgGateway <&> show)
      mbSettlementAmountMin
      mbSettlementAmountMax
      Nothing
      Nothing
      (Just limit)
      (Just offset)

  let selectedSubscriptionIds = reports <&> (.referenceId) & catMaybes & nub
  latestReconEntries <- getLatestReconEntries merchant.id.getId selectedSubscriptionIds
  settlements <- mapM (buildPaymentSettlementItem latestReconEntries) reports

  let totalItems = length settlements
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  pure API.PaymentSettlementListRes {totalItems, summary, settlements}
  where
    getLatestReconEntries :: Text -> [Text] -> Flow [(Text, ReconciliationEntry.ReconciliationEntry)]
    getLatestReconEntries _ [] = pure []
    getLatestReconEntries _ subscriptionIds = do
      entries <- QReconEntryExtra.findBySourceIdsAndType subscriptionIds ReconciliationEntry.PG_PAYMENT_SETTLEMENT_VS_SUBSCRIPTION
      pure $ foldl' upsertLatest [] entries

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
      PgPaymentSettlementReport.PgPaymentSettlementReport ->
      Flow API.PaymentSettlementListItem
    buildPaymentSettlementItem latestReconEntries report = do
      mbSubscription <- case report.referenceId of
        Just subscriptionPurchaseId -> QSubscriptionPurchase.findByPrimaryKey (Id subscriptionPurchaseId)
        Nothing -> pure Nothing

      mbDriver <- case mbSubscription of
        Just subscription -> QPerson.findById (Id subscription.ownerId)
        Nothing -> pure Nothing

      driverMobileNo <- case mbDriver of
        Just driver -> decryptWithDefault driver.mobileNumber Nothing
        Nothing -> pure Nothing

      let driverName = mkFullName <$> mbDriver
          driverEmailId = mbDriver >>= (.email)
          reconciliationEntry = report.referenceId >>= (`Kernel.Prelude.lookup` latestReconEntries)

      pure
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
                    PgPaymentSettlementReport.ORDER -> API.Order
                    PgPaymentSettlementReport.REFUND -> API.Refund
                    PgPaymentSettlementReport.CHARGEBACK -> API.Chargeback
                ),
            chargedAmount = Just report.txnAmount,
            paymentStatus = Just $ show report.txnStatus,
            pgFees = Just report.pgBaseFee,
            gstOnPgFees = Just report.pgTax,
            netAmount = Just report.settlementAmount,
            merchantId = Just report.merchantId,
            paymentMode = show <$> report.paymentMethod,
            driverName = driverName,
            driverMobileNo = driverMobileNo,
            driverEmailId = driverEmailId,
            pgName = report.paymentGateway,
            chargebackAmount = report.chargebackAmount,
            chargebackId = report.chargebackId,
            chargebackReasonCode = report.chargebackReasonCode,
            chargebackStatus = report.chargebackStatus,
            representmentStatus = Nothing,
            settlementId = report.settlementId,
            settlementDate = report.settlementDate,
            settlementCycle = Nothing,
            settlementAmount = Just report.settlementAmount,
            utr = report.utr,
            settlementStatus = Just "Success",
            rrnNo = report.rrn,
            reconciliationStatus = show . (.reconStatus) <$> reconciliationEntry,
            reconciliationDate = (.reconciliationDate) <$> reconciliationEntry,
            differenceAmount = (.variance) <$> reconciliationEntry
          }

    mkFullName :: DP.Person -> Text
    mkFullName person = T.intercalate " " $ catMaybes [Just person.firstName, person.middleName, person.lastName]

getFinanceManagementFinancePaymentGatewayTransactionList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe API.PaymentModeFilter ->
  Maybe (ShortId Dashboard.Common.PaymentOrder) ->
  Maybe API.PaymentStatusFilter ->
  Maybe API.PgGateway ->
  Maybe (Id Dashboard.Common.SubscriptionPurchase) ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Flow API.PaymentTransactionReportListRes
getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity mbFrom mbLimit mbOffset mbPaymentMode mbPaymentOrderId mbPaymentStatus mbPgGateway mbSubscriptionId mbTo mbTxnAmountMax mbTxnAmountMin = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = mkPageLimit mbLimit
      offset = mkPageOffset mbOffset

  -- Resolve subscription purchases based on filters
  subscriptions <- resolveSubscriptions merchantOpCityId mbSubscriptionId mbPaymentOrderId mbFrom mbTo mbTxnAmountMin mbTxnAmountMax (Just limit) (Just offset)

  -- For each subscription, look up PaymentOrder, then build ORDER + REFUND entries
  allEntries <- fmap concat $
    forM subscriptions $ \subscription -> do
      mbOrder <- QPaymentOrder.findById subscription.paymentOrderId

      case mbOrder of
        Nothing -> pure []
        Just order -> do
          -- Get all PaymentTransactions for this order → ORDER entries
          paymentTransactions <- HQPaymentTransaction.findAllByOrderId order.id
          -- Get all Refunds for this order → REFUND entries
          refunds <- HQRefunds.findAllByOrderId order.shortId

          -- Resolve payer context (shared across all entries for this subscription)
          payerContext <- resolvePayerContext subscription

          -- Resolve TDS amount from DirectTaxTransaction for this subscription purchase
          directTaxTxns <- QDirectTax.findByReferenceId subscription.id.getId
          let mbTdsAmount = case filter (\txn -> txn.transactionType == DirectTax.Subscription) directTaxTxns of
                (txn : _) -> Just txn.tdsAmount
                _ -> Nothing

          -- Resolve city/operating location
          mbMerchantOpCity <- CQMOC.findById subscription.merchantOperatingCityId
          let cityValue = mbMerchantOpCity <&> (.city) <&> show

          -- Build ORDER entries (one per PaymentTransaction)
          orderEntries <- forM paymentTransactions $ \txn ->
            buildOrderEntry order txn subscription payerContext mbTdsAmount cityValue

          -- Build REFUND entries (one per Refund)
          -- For refund entries, we also need a PaymentTransaction for card/payment info
          let mbRelevantTxn = selectRelevantPaymentTransaction paymentTransactions
          refundEntries <- forM refunds $ \refund ->
            buildRefundEntry order refund mbRelevantTxn subscription payerContext mbTdsAmount cityValue

          pure $ orderEntries <> refundEntries

  -- Apply in-memory filters (only paymentMode, paymentStatus, pgGateway)
  let filteredEntries = filter (matchesFilters mbPaymentMode mbPaymentStatus mbPgGateway) allEntries
      totalItems = length filteredEntries

  pure $
    API.PaymentTransactionReportListRes
      { totalItems = totalItems,
        summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems},
        transactions = filteredEntries
      }
  where
    resolveSubscriptions ::
      Id DMOC.MerchantOperatingCity ->
      Maybe (Id Dashboard.Common.SubscriptionPurchase) ->
      Maybe (ShortId Dashboard.Common.PaymentOrder) ->
      Maybe UTCTime ->
      Maybe UTCTime ->
      Maybe HighPrecMoney ->
      Maybe HighPrecMoney ->
      Maybe Int ->
      Maybe Int ->
      Flow [DSP.SubscriptionPurchase]
    resolveSubscriptions merchantOpCityId mbSubId mbOrderId mbFromDate mbToDate mbAmountMin mbAmountMax dbLimit dbOffset =
      case mbSubId of
        Just subscriptionId ->
          QSubscriptionPurchase.findByPrimaryKey (cast subscriptionId) >>= \case
            Just sub -> pure [sub]
            Nothing -> pure []
        Nothing ->
          case mbOrderId of
            Just paymentOrderId -> do
              mbOrder <- QPaymentOrder.findByShortId (ShortId paymentOrderId.getShortId)
              case mbOrder of
                Just order ->
                  QSubscriptionPurchase.findByPaymentOrderId order.id >>= \case
                    Just sub -> pure [sub]
                    Nothing -> pure []
                Nothing -> pure []
            Nothing -> do
              -- from and to are mandatory when no specific ID filter is provided
              fromDate <- mbFromDate & fromMaybeM (InvalidRequest "'from' date is required when subscriptionId or paymentOrderId is not provided")
              toDate <- mbToDate & fromMaybeM (InvalidRequest "'to' date is required when subscriptionId or paymentOrderId is not provided")
              QSubscriptionPurchase.findAllByMerchantOpCityIdWithFilters
                merchantOpCityId
                (Just DPlan.PREPAID_SUBSCRIPTION)
                Nothing
                (Just fromDate)
                (Just toDate)
                mbAmountMin
                mbAmountMax
                dbLimit
                dbOffset

    buildOrderEntry ::
      PaymentOrder.PaymentOrder ->
      PaymentTransaction.PaymentTransaction ->
      DSP.SubscriptionPurchase ->
      (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text) ->
      Maybe HighPrecMoney ->
      Maybe Text ->
      Flow API.PaymentTransactionReportItem
    buildOrderEntry order txn subscription (payerTypeValue, payerNameValue, payerIdValue, payerMobileValue, payerEmailValue, fleetOwnerIdValue) mbTdsAmount cityValue = do
      let paymentModeValue = txn.paymentMethodType <|> txn.paymentMethod
          paymentSubModeValue = txn.paymentMethodType <|> txn.paymentMethod
          walletProviderValue =
            case canonicalizeText <$> paymentModeValue of
              Just "wallet" -> paymentSubModeValue
              _ -> Nothing

      pure $
        API.PaymentTransactionReportItem
          { gatewayTransactionId = txn.epgTxnId,
            merchantOrderId = Just order.shortId.getShortId,
            referenceId = Nothing,
            correlationId = Just subscription.id.getId,
            parentTransactionId = Nothing,
            transactionType = Just API.Order,
            transactionStatus = Just $ show txn.status,
            failureReason = txn.bankErrorMessage,
            errorCode = txn.bankErrorCode,
            transactionInitiationDate = Just txn.createdAt,
            paymentPageOpenDate = Just txn.createdAt,
            paymentGatewayOpenDate = txn.authorizationDateTime,
            refundInitiationDate = Nothing,
            refundCompletionDate = Nothing,
            transactionAmountGross = Just txn.amount,
            netAmount = txn.netAmount,
            gatewayCharges = txn.surchargeAmount,
            gstOnCharges = txn.taxAmount,
            tdsAmount = mbTdsAmount,
            withholdingAmount = Nothing,
            refundAmount = Nothing,
            currency = Just $ show order.currency,
            exchangeRate = Nothing,
            paymentMode = paymentModeValue,
            paymentSubMode = paymentSubModeValue,
            upiId = order.vpa,
            upiAppName = Nothing,
            cardType = txn.cardType,
            cardNetwork = txn.cardBrand,
            maskedCardNumber = mkMaskedCardNumber txn.cardIsin txn.cardLastFourDigits,
            issuerBank = txn.cardIssuer,
            walletProvider = walletProviderValue,
            payerType = payerTypeValue,
            payerName = payerNameValue,
            payerId = payerIdValue,
            payerMobile = payerMobileValue,
            payerEmail = payerEmailValue,
            subscriptionId = Just subscription.id.getId,
            vehicleId = Nothing,
            fleetOwnerId = fleetOwnerIdValue,
            city = cityValue,
            operatingLocation = cityValue,
            serviceType = Just "Ride based subscription",
            refundId = Nothing,
            refundReason = Nothing,
            refundMode = Nothing,
            refundStatus = Nothing,
            ipAddress = Nothing,
            pgName = txn.gatewayName,
            pgMerchantId = order.paymentMerchantId
          }

    buildRefundEntry ::
      PaymentOrder.PaymentOrder ->
      PaymentRefund.Refunds ->
      Maybe PaymentTransaction.PaymentTransaction ->
      DSP.SubscriptionPurchase ->
      (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text) ->
      Maybe HighPrecMoney ->
      Maybe Text ->
      Flow API.PaymentTransactionReportItem
    buildRefundEntry order refund mbTxn subscription (payerTypeValue, payerNameValue, payerIdValue, payerMobileValue, payerEmailValue, fleetOwnerIdValue) mbTdsAmount cityValue = do
      let paymentModeValue = (mbTxn >>= (.paymentMethodType)) <|> (mbTxn >>= (.paymentMethod))
          paymentSubModeValue = paymentModeValue
          walletProviderValue =
            case canonicalizeText <$> paymentModeValue of
              Just "wallet" -> paymentSubModeValue
              _ -> Nothing

      pure $
        API.PaymentTransactionReportItem
          { gatewayTransactionId = refund.idAssignedByServiceProvider,
            merchantOrderId = Just order.shortId.getShortId,
            referenceId = Nothing,
            correlationId = Just subscription.id.getId,
            parentTransactionId = Nothing,
            transactionType = Just API.Refund,
            transactionStatus = Just $ show refund.status,
            failureReason = refund.errorMessage,
            errorCode = refund.errorCode,
            transactionInitiationDate = Nothing,
            paymentPageOpenDate = Nothing,
            paymentGatewayOpenDate = Nothing,
            refundInitiationDate = Just refund.createdAt,
            refundCompletionDate = refund.completedAt,
            transactionAmountGross = Just refund.refundAmount,
            netAmount = Just refund.refundAmount,
            gatewayCharges = Nothing,
            gstOnCharges = Just 0,
            tdsAmount = mbTdsAmount,
            withholdingAmount = Nothing,
            refundAmount = Just refund.refundAmount,
            currency = Just $ show order.currency,
            exchangeRate = Nothing,
            paymentMode = paymentModeValue,
            paymentSubMode = paymentSubModeValue,
            upiId = order.vpa,
            upiAppName = Nothing,
            cardType = mbTxn >>= (.cardType),
            cardNetwork = mbTxn >>= (.cardBrand),
            maskedCardNumber = mkMaskedCardNumber (mbTxn >>= (.cardIsin)) (mbTxn >>= (.cardLastFourDigits)),
            issuerBank = mbTxn >>= (.cardIssuer),
            walletProvider = walletProviderValue,
            payerType = payerTypeValue,
            payerName = payerNameValue,
            payerId = payerIdValue,
            payerMobile = payerMobileValue,
            payerEmail = payerEmailValue,
            subscriptionId = Just subscription.id.getId,
            vehicleId = Nothing,
            fleetOwnerId = fleetOwnerIdValue,
            city = cityValue,
            operatingLocation = cityValue,
            serviceType = Just "Ride based subscription",
            refundId = refund.idAssignedByServiceProvider,
            refundReason = Nothing,
            refundMode = paymentModeValue,
            refundStatus = Just $ show refund.status,
            ipAddress = Nothing,
            pgName = mbTxn >>= (.gatewayName),
            pgMerchantId = order.paymentMerchantId
          }

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

    matchesFilters ::
      Maybe API.PaymentModeFilter ->
      Maybe API.PaymentStatusFilter ->
      Maybe API.PgGateway ->
      API.PaymentTransactionReportItem ->
      Bool
    matchesFilters mbModeFilter mbStatusFilter mbGwFilter item =
      and
        [ matchesPaymentModeFilter mbModeFilter item.paymentMode,
          matchesPaymentStatusFilter mbStatusFilter item.transactionStatus,
          matchesPgGatewayFilter mbGwFilter item.pgName
        ]

    matchesPaymentModeFilter :: Maybe API.PaymentModeFilter -> Maybe Text -> Bool
    matchesPaymentModeFilter Nothing _ = True
    matchesPaymentModeFilter (Just modeFilter) mbActualValue =
      let expectedValues = case modeFilter of
            API.UPI -> ["upi"]
            API.Card -> ["card", "creditcard", "debitcard", "credit_card", "debit_card"]
            API.NetBanking -> ["netbanking", "nb"]
            API.Wallet -> ["wallet"]
       in maybe False (\actual -> canonicalizeText actual `elem` expectedValues) mbActualValue

    matchesPaymentStatusFilter :: Maybe API.PaymentStatusFilter -> Maybe Text -> Bool
    matchesPaymentStatusFilter Nothing _ = True
    matchesPaymentStatusFilter (Just statusFilter) mbActualValue =
      let expectedValue = case statusFilter of
            API.Success -> "charged"
            API.Failed -> "authentication_failed"
            API.Pending -> "new"
       in maybe False (\actual -> canonicalizeText actual == expectedValue || canonicalizeText actual == canonicalizeText (show statusFilter)) mbActualValue

    matchesPgGatewayFilter :: Maybe API.PgGateway -> Maybe Text -> Bool
    matchesPgGatewayFilter Nothing _ = True
    matchesPgGatewayFilter (Just gwFilter) mbActualValue =
      let expectedValue = case gwFilter of
            API.Juspay -> "juspay"
            API.BillDesk -> "billdesk"
       in maybe False ((== expectedValue) . canonicalizeText) mbActualValue

    canonicalizeText :: Text -> Text
    canonicalizeText =
      T.filter (\char -> char /= ' ' && char /= '_' && char /= '-')
        . T.toLower
        . T.strip

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
