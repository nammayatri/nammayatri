{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.FinanceManagement
  ( getFinanceManagementSubscriptionPurchaseList,
    getFinanceManagementFleetOperatorList,
    getFinanceManagementInvoiceList,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement as API
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.FleetOwnerInformation as FleetOwnerInfo
import qualified Domain.Types.Merchant as DM
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
import Kernel.Utils.Error (fromMaybeM, throwError)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QInvoiceLedgerLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified SharedLogic.Merchant as SMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQPlan
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.FleetOwnerInformation as QFleetOwnerInfo
import qualified Storage.Queries.Person as QPerson
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
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

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
          fetchSubscriptionsForOwner driverId DSP.DRIVER serviceName limit offset
        (_, Just fleetOwnerId) ->
          fetchSubscriptionsForOwner fleetOwnerId DSP.FLEET_OWNER serviceName limit offset
        _ -> do
          -- TODO: Need query to fetch ALL subscriptions for merchant
          pure []

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

    fetchSubscriptionsForOwner :: Text -> DSP.SubscriptionOwnerType -> DPlan.ServiceNames -> Int -> Int -> Flow [DSP.SubscriptionPurchase]
    fetchSubscriptionsForOwner ownerId ownerType serviceName limit offset = do
      let _mbStatus = Nothing -- Convert Text to SubscriptionPurchaseStatus if needed
      QSubscriptionPurchase.findAllByOwnerAndServiceNameWithPagination
        ownerId
        ownerType
        serviceName
        _mbStatus
        (Just limit)
        (Just offset)

    buildSubscriptionPurchaseItem :: DPlan.ServiceNames -> Int -> Int -> DSP.SubscriptionPurchase -> Flow API.SubscriptionPurchaseListItem
    buildSubscriptionPurchaseItem serviceName _limit _offset subscription = do
      -- Get plan details
      plan <-
        CQPlan.findByIdAndPaymentModeWithServiceName subscription.planId DPlan.MANUAL serviceName >>= \case
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

      -- Get finance invoice details
      (subAmount, gstRate, gstAmount, totalValue) <- case subscription.financeInvoiceId of
        Just invId -> do
          QFinanceInvoice.findById invId >>= \case
            Just invoice -> do
              -- Get GST details from indirect tax transaction
              indirectTaxTxns <- QIndirectTax.findByInvoiceNumber invoice.invoiceNumber
              let (_taxableVal, gstRt, gstAmt) = case indirectTaxTxns of
                    (txn : _) -> (Just txn.taxableValue, Just txn.gstRate, Just txn.totalGstAmount)
                    _ -> (Nothing, Nothing, Nothing)
                  subAmt = invoice.subtotal
                  totalAmt = invoice.totalAmount
              pure (Just subAmt, gstRt, gstAmt, Just totalAmt)
            Nothing -> pure (Nothing, Nothing, Nothing, Nothing)
        Nothing -> pure (Nothing, Nothing, Nothing, Nothing)

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
    calculateUtilizedValue _ownerId = do
      -- This is a placeholder - actual implementation would:
      -- 1. Find Ride Credit account for owner (finance_account where account_type = 'RideCredit' and counterparty_id = ownerId)
      -- 2. Sum all debit amounts from ledger entries where reference_type = 'RideSubscriptionDebit'
      pure 0 -- TODO: Implement actual calculation

    -- Get linked ride IDs from ledger entries
    getLinkedRideIds :: Id DSP.SubscriptionPurchase -> Flow [Text]
    getLinkedRideIds _subId = do
      -- This is a placeholder - actual implementation would:
      -- 1. Find ledger entries where reference_id = subscription_purchase.id
      -- 2. Get ride_id from metadata or reference_id
      pure [] -- TODO: Implement actual logic

-- | Get fleet operator list with filters
getFinanceManagementFleetOperatorList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe UTCTime ->
  Flow API.FleetOperatorListRes
getFinanceManagementFleetOperatorList merchantShortId opCity mbFleetOperatorId _mbFrom mbLimit mbOffset _mbSettlementStatus _mbTo = do
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
      let linkedDriverIds = map (\d -> d.driverId.getId) linkedDrivers

      -- Get fleet owner info
      let (legalName, pan, gstin) = case mbFleetInfo of
            Just _fleetInfo -> (Just person.firstName, Nothing, Nothing) -- TODO: Get actual pan/gstin
            Nothing -> (Just person.firstName, Nothing, Nothing)

      -- Calculate financials from ledger entries
      (totalEarnings, totalDeductions) <- calculateFleetFinancials person.id.getId
      outstandingBalance <- getOutstandingBalance person.id.getId

      pure $
        API.FleetOperatorListItem
          { fleetOperatorId = Just person.id.getId,
            legalName = legalName,
            pan = pan,
            gstin = gstin,
            settlementPreference = Nothing,
            linkedDriverIds = linkedDriverIds,
            totalEarnings = Just totalEarnings,
            totalDeductions = Just totalDeductions,
            outstandingBalance = Just outstandingBalance,
            payoutReference = Nothing, -- TODO: Get from payout table
            payoutAmount = Nothing,
            payoutStatus = Nothing,
            createdAt = Just person.createdAt,
            updatedAt = Just person.updatedAt
          }

    -- Calculate total earnings and deductions from ledger entries
    calculateFleetFinancials :: Text -> Flow (HighPrecMoney, HighPrecMoney)
    calculateFleetFinancials _fleetOwnerId = do
      -- TODO: Implement based on SQL query:
      -- Earnings: Sum of ledger entries where to_account is DRIVER_PAYABLE
      -- Deductions: Sum of ledger entries where from_account is DRIVER_PAYABLE
      pure (0, 0)

    -- Get outstanding balance from finance_account
    getOutstandingBalance :: Text -> Flow HighPrecMoney
    getOutstandingBalance _fleetOwnerId = do
      -- TODO: Find finance_account where counterparty_id = fleetOwnerId and account_type = 'Liability'
      pure 0

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
