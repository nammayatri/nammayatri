{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.FinanceManagement
  ( getFinanceManagementSubscriptionPurchaseList,
    getFinanceManagementFleetOperatorFinanceList,
    getFinanceManagementInvoiceList,
    getReconciliation,
    getFinanceManagementReconciliation,
    postFinanceManagementReconciliationTrigger,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement as API
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.FleetOwnerInformation as FleetOwnerInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.SubscriptionPurchase as DSP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (UTCTime, identity, listToMaybe)
import qualified Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id (..), ShortId, cast)
import Kernel.Utils.Common (getCurrentTime)
import Kernel.Utils.Error (fromMaybeM, throwError)
import qualified Lib.Finance.Domain.Types.Account as Account
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Domain.Types.ReconciliationEntry as ReconciliationEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary as ReconSummary
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QInvoiceLedgerLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Finance.Storage.Queries.ReconciliationEntry as QReconEntry
import qualified Lib.Finance.Storage.Queries.ReconciliationSummary as QReconSummary
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import qualified Lib.Scheduler.JobStorageType.DB.Table as SchedulerJobT
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QSchedulerJob
import Storage.Beam.SchedulerJob ()
import SharedLogic.Allocator (AllocatorJobType (..), ReconciliationJobData (..))
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQPlan
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.FleetOwnerInformation as QFleetOwnerInfo
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase
import Tools.Error

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
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.SubscriptionPurchaseListRes
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity _amountMax _amountMin mbDriverId mbFleetOperatorId _mbFrom mbLimit mbOffset _mbStatus mbServiceName mbSubscriptionId _mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset

  -- Parse service name from input or default to YATRI_SUBSCRIPTION
  let serviceName = fromMaybe DPlan.YATRI_SUBSCRIPTION $ mbServiceName >>= parseServiceName

  -- Get subscription purchases based on filters
  subscriptions <- case mbSubscriptionId of
    Just subscriptionId -> do
      QSubscriptionPurchase.findByPrimaryKey (Id subscriptionId) >>= \case
        Just sub -> pure [sub]
        Nothing -> pure []
    Nothing -> do
      case (mbDriverId, mbFleetOperatorId) of
        (Just driverId, _) ->
          fetchSubscriptionsForOwner driverId DSP.DRIVER serviceName limit offset merchantOpCityId
        (_, Just fleetOwnerId) ->
          fetchSubscriptionsForOwner fleetOwnerId DSP.FLEET_OWNER serviceName limit offset merchantOpCityId
        _ -> do
          QSubscriptionPurchase.findAllByMerchantOpCityIdAndServiceNameWithPagination
            merchantOpCityId
            serviceName
            (Just limit)
            (Just offset)

  -- Build response items
  items <- mapM (buildSubscriptionPurchaseItem serviceName limit offset) subscriptions

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

    fetchSubscriptionsForOwner :: Text -> DSP.SubscriptionOwnerType -> DPlan.ServiceNames -> Int -> Int -> (Id DMOC.MerchantOperatingCity) -> Flow [DSP.SubscriptionPurchase]
    fetchSubscriptionsForOwner ownerId ownerType serviceName limit offset merchantOpCityId = do
      let _mbStatus = Nothing -- Convert Text to SubscriptionPurchaseStatus if needed
      -- Use merchantOpCityId filter for proper KV lookup
      QSubscriptionPurchase.findAllByOwnerAndServiceNameWithPagination'
        merchantOpCityId
        ownerId
        ownerType
        serviceName
        _mbStatus
        (Just limit)
        (Just offset)

    buildSubscriptionPurchaseItem :: DPlan.ServiceNames -> Int -> Int -> DSP.SubscriptionPurchase -> Flow API.SubscriptionPurchaseListItem
    buildSubscriptionPurchaseItem serviceName _limit _offset subscription = do
      -- Get plan details (cache first, then DB by id+mode+serviceName, then by primary key for seed/test data)
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

      -- Get owner details
      (driverId, fleetOperatorId) <- case subscription.ownerType of
        DSP.DRIVER -> do
          person <- QPerson.findById (Id subscription.ownerId) >>= fromMaybeM (PersonDoesNotExist subscription.ownerId)
          fleetAssoc <- QFleetDriver.findByDriverId (Id subscription.ownerId) True
          let fleetOpId = case fleetAssoc of
                Just assoc -> Just assoc.fleetOwnerId
                _ -> Nothing
          pure (Just person.id.getId, fleetOpId)
        DSP.FLEET_OWNER -> do
          _person <- QPerson.findById (Id subscription.ownerId) >>= fromMaybeM (PersonDoesNotExist subscription.ownerId)
          pure (Just subscription.ownerId, Just subscription.ownerId)

      -- Get GST details from indirect tax transaction using subscription purchase ID as reference
      indirectTaxTxns <- QIndirectTax.findByReferenceId subscription.id.getId
      let mbSubscriptionTxn = listToMaybe $ filter (\txn -> txn.transactionType == IndirectTax.Subscription) indirectTaxTxns
      let gstRate = mbSubscriptionTxn <&> (.gstRate)
      let gstAmount = mbSubscriptionTxn <&> (.totalGstAmount)
      let subAmount = Just subscription.planFee
      let totalValue = Just subscription.planRideCredit

      -- Calculate usage values from ledger entries
      -- Get Ride Credit account and calculate utilized amount
      utilizedValue <- calculateUtilizedValue subscription.ownerId
      let entitledValue = subscription.planRideCredit
          remainingValue = entitledValue - utilizedValue

      -- Get linked ride IDs from ledger entries
      linkedRideIds <- getLinkedRideIds subscription.id

      pure $
        API.SubscriptionPurchaseListItem
          { subscriptionPurchaseId = Just subscription.id.getId,
            planName = Just plan.name,
            planType = Just $ show plan.frequency,
            subscriptionStartDate = Just subscription.purchaseTimestamp,
            subscriptionEndDate = subscription.expiryDate,
            subscriptionStatus = Just $ show subscription.status,
            driverId = driverId,
            fleetOperatorId = fleetOperatorId,
            subscriptionAmount = subAmount,
            gstRate = gstRate,
            gstAmount = gstAmount,
            totalSubscriptionValue = totalValue,
            totalEntitledValue = Just entitledValue,
            utilizedValue = Just utilizedValue,
            remainingValue = Just remainingValue,
            linkedRideIds = linkedRideIds,
            invoiceIds = maybe [] (\invId -> [invId.getId]) subscription.financeInvoiceId,
            createdAt = Just subscription.purchaseTimestamp,
            updatedAt = Just subscription.purchaseTimestamp
          }

    -- Calculate utilized value from Ride Credit account ledger entries
    calculateUtilizedValue :: Text -> Flow HighPrecMoney
    calculateUtilizedValue ownerId = do
      -- Find Ride Credit account for owner
      let currency = INR -- Default currency, should come from context
      mbRideCreditAccount <- QAccount.findByCounterpartyAndType Nothing (Just ownerId) Account.RideCredit currency
      case mbRideCreditAccount of
        Just account -> do
          -- Get all ledger entries where fromAccountId = ride credit account (debits)
          ledgerEntries <- QLedgerEntry.findByFromAccount account.id
          -- Sum all amounts (these represent credits used for rides)
          pure $ sum $ map (.amount) ledgerEntries
        Nothing -> pure 0

    -- Get linked ride IDs from ledger entries
    getLinkedRideIds :: Id DSP.SubscriptionPurchase -> Flow [Text]
    getLinkedRideIds subId = do
      -- Find ledger entries where reference_id = subscription_purchase.id
      -- and reference_type indicates a ride transaction
      entries <- QLedgerEntry.findByReference "RideSubscriptionDebit" subId.getId
      -- Extract ride IDs from metadata or reference
      pure $ mapMaybe extractRideId entries

    extractRideId :: LedgerEntry.LedgerEntry -> Maybe Text
    extractRideId entry = do
      -- Try to get ride ID from metadata if available
      _metadata <- entry.metadata
      -- Parse JSON metadata to extract ride_id
      -- For now, return Nothing as this requires JSON parsing
      Nothing

-- | Get fleet operator list with filters
getFinanceManagementFleetOperatorFinanceList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.FleetOperatorListRes
getFinanceManagementFleetOperatorFinanceList merchantShortId opCity mbFleetOperatorId _mbFrom mbLimit mbOffset _mbSettlementStatus _mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let _limit = fromMaybe 20 mbLimit
      _offset = fromMaybe 0 mbOffset

  -- Get fleet operators based on filters
  fleetOperators <- case mbFleetOperatorId of
    Just fleetOwnerId -> do
      QPerson.findById (Id fleetOwnerId) >>= \case
        Just person | person.role == DP.FLEET_OWNER -> do
          fleetInfo <- QFleetOwnerInfo.findByPrimaryKey (Id fleetOwnerId)
          pure [(person, fleetInfo)]
        _ -> pure []
    Nothing -> do
      -- TODO: Need query to fetch ALL FLEET_OWNERs
      pure []

  -- Build response items
  items <- mapM buildFleetOperatorItem fleetOperators

  let totalItems = length items
      summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems}

  pure $
    API.FleetOperatorListRes
      { totalItems,
        summary,
        fleetOperators = items
      }
  where
    buildFleetOperatorItem :: (DP.Person, Maybe FleetOwnerInfo.FleetOwnerInformation) -> Flow API.FleetOperatorListItem
    buildFleetOperatorItem (person, mbFleetInfo) = do
      -- Get linked drivers
      linkedDrivers <- QFleetDriver.findAllActiveByFleetOwnerId person.id.getId

      -- Build full legal name
      let legalName = T.intercalate " " $ catMaybes [Just person.firstName, person.middleName, person.lastName]

      -- Get and decrypt PAN and GSTIN if available
      let (pan, gstin) = case mbFleetInfo of
            Just fleetInfo ->
              ( fleetInfo.panNumberDec,
                fleetInfo.gstNumberDec
              )
            Nothing -> (Nothing, Nothing)

      pure $
        API.FleetOperatorListItem
          { fleetOperatorId = Just person.id.getId,
            legalName = Just legalName,
            pan = pan,
            gstin = gstin,
            settlementPreference = Nothing,
            linkedDriverIds = map (\d -> d.driverId.getId) linkedDrivers,
            totalEarnings = Nothing,
            totalDeductions = Nothing,
            outstandingBalance = Nothing,
            payoutReference = Nothing,
            payoutAmount = Nothing,
            payoutStatus = Nothing,
            createdAt = Just person.createdAt,
            updatedAt = Just person.updatedAt
          }

-- | Get invoice list with filters
getFinanceManagementInvoiceList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.InvoiceListRes
getFinanceManagementInvoiceList merchantShortId opCity _mbFrom mbInvoiceId _mbInvoiceType mbLimit mbOffset _mbStatus _mbTo = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let _limit = fromMaybe 20 mbLimit
      _offset = fromMaybe 0 mbOffset

  -- Get invoices based on filters
  invoices <- case mbInvoiceId of
    Just invoiceId -> do
      QFinanceInvoice.findById (Id invoiceId) >>= \case
        Just inv -> pure [inv]
        Nothing -> pure []
    Nothing -> do
      -- TODO: Need query to fetch all invoices with filters
      pure []

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

  let limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset

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
      pure
        API.ReconciliationEntry
          { bookingId = Just entry.bookingId,
            dcoId = Just entry.dcoId,
            status = Just (show entry.status),
            mode = fmap show entry.mode,
            expectedDsrValue = Just entry.expectedDsrValue,
            actualLedgerValue = Just entry.actualLedgerValue,
            variance = Just entry.variance,
            reconStatus = Just (show entry.reconStatus),
            mismatchReason = entry.mismatchReason,
            timestamp = Just entry.timestamp
          }
    caseToReconciliationType :: Text -> ReconSummary.ReconciliationType
    caseToReconciliationType "DSR_VS_LEDGER" = ReconSummary.DSR_VS_LEDGER
    caseToReconciliationType "DSR_VS_SUBSCRIPTION" = ReconSummary.DSR_VS_SUBSCRIPTION
    caseToReconciliationType "DSSR_VS_SUBSCRIPTION" = ReconSummary.DSSR_VS_SUBSCRIPTION
    caseToReconciliationType _ = error "Invalid reconciliation type"

getFinanceManagementReconciliation :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Flow API.ReconciliationRes
getFinanceManagementReconciliation = getReconciliation

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
