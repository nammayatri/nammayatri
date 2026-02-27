{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Prepaid
  ( counterpartyDriver,
    counterpartyFleetOwner,
    prepaidRideDebitReferenceType,
    subscriptionCreditReferenceType,
    subscriptionPurchaseReferenceType,
    subscriptionRideReferenceType,
    expiryRevenueRecognitionReferenceType,
    expiryCreditTransferReferenceType,
    getPrepaidAccountByOwner,
    getPrepaidBalanceByOwner,
    getPrepaidPendingHoldByOwner,
    getPrepaidAvailableBalanceByOwner,
    createPrepaidHold,
    voidPrepaidHold,
    creditPrepaidBalance,
    debitPrepaidBalance,
    handleSubscriptionExpiry,
    checkAndMarkExhaustedSubscriptions,
    activateNextQueuedPurchaseExpiry,
    InvoiceCreationParams (..),
  )
where

import Data.Aeson (Value)
import qualified Data.List as DL
import Domain.Types.Extra.Plan (ServiceNames (..))
import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, getCurrentTime, logInfo)
import Lib.Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Queries.Plan as QPlan
import qualified Storage.Queries.SubscriptionPurchase as QSP
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSPE

-- | Optional parameters for creating a finance invoice during prepaid balance credit
data InvoiceCreationParams = InvoiceCreationParams
  { paymentOrderId :: Text,
    issuedToType :: Text,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text,
    issuedByType :: Text,
    issuedById :: Text,
    issuedByName :: Maybe Text,
    issuedByAddress :: Maybe Text,
    gstinOfParty :: Maybe Text,
    merchantShortId :: Text
  }
  deriving (Show)

counterpartyDriver :: CounterpartyType
counterpartyDriver = DRIVER

counterpartyFleetOwner :: CounterpartyType
counterpartyFleetOwner = FLEET_OWNER

subscriptionPurchaseReferenceType :: Text
subscriptionPurchaseReferenceType = "SubscriptionPurchase"

subscriptionCreditReferenceType :: Text
subscriptionCreditReferenceType = "SubscriptionCredit"

subscriptionRideReferenceType :: Text
subscriptionRideReferenceType = "RideRevenueRecognition"

prepaidRideDebitReferenceType :: Text
prepaidRideDebitReferenceType = "RideSubscriptionDebit"

expiryRevenueRecognitionReferenceType :: Text
expiryRevenueRecognitionReferenceType = "ExpiryRevenueRecognition"

expiryCreditTransferReferenceType :: Text
expiryCreditTransferReferenceType = "ExpiryCreditTransfer"

tdsReimbursementReferenceType :: Text
tdsReimbursementReferenceType = "TDSReimbursement"

getPrepaidAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getPrepaidAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == RideCredit) accounts

getPrepaidBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getPrepaidBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getPrepaidAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

getPrepaidPendingHoldByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m HighPrecMoney
getPrepaidPendingHoldByOwner counterpartyType ownerId = do
  mbAcc <- getPrepaidAccountByOwner counterpartyType ownerId
  case mbAcc of
    Nothing -> pure 0
    Just acc -> do
      entries <- getEntriesByAccount acc.id
      pure $ sum $ map (.amount) $ filter (\e -> e.fromAccountId == acc.id && e.status == PENDING && e.referenceType == prepaidRideDebitReferenceType) entries

getPrepaidAvailableBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getPrepaidAvailableBalanceByOwner counterpartyType ownerId = do
  mbBalance <- getPrepaidBalanceByOwner counterpartyType ownerId
  pendingHold <- getPrepaidPendingHoldByOwner counterpartyType ownerId
  pure $ (\balance -> balance - pendingHold) <$> mbBalance

getOrCreatePrepaidAccount ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreatePrepaidAccount counterpartyType ownerId currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = RideCredit,
            counterpartyType = Just counterpartyType,
            counterpartyId = Just ownerId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateSellerAssetAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateSellerAssetAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            counterpartyType = Just SELLER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateSellerLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateSellerLiabilityAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just SELLER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateGovernmentLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateGovernmentLiabilityAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just GOVERNMENT_INDIRECT,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateGovtDirectAssetAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateGovtDirectAssetAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            counterpartyType = Just GOVERNMENT_DIRECT,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateGovtDirectExpenseAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateGovtDirectExpenseAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Expense,
            counterpartyType = Just GOVERNMENT_DIRECT,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateSellerRideCreditAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateSellerRideCreditAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = RideCredit,
            counterpartyType = Just SELLER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateSellerRevenueAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateSellerRevenueAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Revenue,
            counterpartyType = Just SELLER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

createPrepaidHold ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney ->
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID
  Maybe Value ->
  m (Either FinanceError ())
createPrepaidHold counterpartyType ownerId amount currency merchantId merchantOperatingCityId referenceId metadata = do
  mbOwnerAccount <- getOrCreatePrepaidAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
  mbSellerRideCredit <- getOrCreateSellerRideCreditAccount currency merchantId merchantOperatingCityId
  case (mbOwnerAccount, mbSellerRideCredit) of
    (Right ownerAccount, Right sellerRideCredit) -> do
      mbExistingHold <- findPendingPrepaidHoldByReference ownerAccount.id prepaidRideDebitReferenceType referenceId
      case mbExistingHold of
        Just _ -> pure $ Right ()
        Nothing -> do
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = ownerAccount.id,
                    toAccountId = sellerRideCredit.id,
                    amount = amount,
                    currency = currency,
                    entryType = Lib.Finance.Domain.Types.LedgerEntry.Revenue,
                    status = PENDING,
                    referenceType = prepaidRideDebitReferenceType,
                    referenceId = referenceId,
                    metadata = metadata,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId
                  }
          entryRes <- createEntry entryInput
          case entryRes of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

findPendingPrepaidHoldByReference ::
  (BeamFlow m r) =>
  Id Account ->
  Text ->
  Text ->
  m (Maybe LedgerEntry)
findPendingPrepaidHoldByReference ownerAccountId referenceType referenceId = do
  entries <- getEntriesByReference referenceType referenceId
  pure $ find (\entry -> entry.fromAccountId == ownerAccountId && entry.status == PENDING) entries

voidPrepaidHold ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  Text -> -- Reference ID
  Text -> -- Reason
  m ()
voidPrepaidHold counterpartyType ownerId referenceId reason = do
  mbOwnerAccount <- getPrepaidAccountByOwner counterpartyType ownerId
  case mbOwnerAccount of
    Nothing -> pure ()
    Just ownerAccount -> do
      entries <- getEntriesByReference prepaidRideDebitReferenceType referenceId
      let pendingEntries =
            filter
              (\entry -> entry.fromAccountId == ownerAccount.id && entry.status == PENDING)
              entries
      forM_ pendingEntries $ \entry -> voidEntry entry.id reason

settlePrepaidHoldByReference ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  Text ->
  HighPrecMoney -> -- Final amount to settle at (may differ from hold amount)
  m (Either FinanceError ())
settlePrepaidHoldByReference counterpartyType ownerId referenceId finalAmount = do
  mbOwnerAccount <- getPrepaidAccountByOwner counterpartyType ownerId
  case mbOwnerAccount of
    Nothing -> pure $ Right ()
    Just ownerAccount -> do
      entries <- getEntriesByReference prepaidRideDebitReferenceType referenceId
      let pendingEntries =
            filter
              (\entry -> entry.fromAccountId == ownerAccount.id && entry.status == PENDING)
              entries
      foldM
        ( \_ entry -> do
            let settleAmount = finalAmount
            -- Read starting balances before applying deltas
            fromStartBal <- fromMaybe 0 <$> getBalance entry.fromAccountId
            toStartBal <- fromMaybe 0 <$> getBalance entry.toAccountId
            resFrom <- updateBalanceByDelta entry.fromAccountId (-1 * settleAmount)
            case resFrom of
              Left err -> pure $ Left err
              Right fromEndBal -> do
                resTo <- updateBalanceByDelta entry.toAccountId settleAmount
                case resTo of
                  Left err -> pure $ Left err
                  Right toEndBal -> do
                    settleEntryWithBalances entry.id fromStartBal fromEndBal toStartBal toEndBal
                    pure $ Right ()
        )
        (Right ())
        pendingEntries

creditPrepaidBalance ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney -> -- Ride credit amount
  HighPrecMoney -> -- Paid amount
  Maybe Double -> -- TDS rate (%)
  Maybe GstAmountBreakdown -> -- Explicit CGST/SGST/IGST breakdown (gstAmount computed from this)
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID
  Maybe Value ->
  Maybe InvoiceCreationParams -> -- Optional invoice creation params
  m (Either FinanceError (HighPrecMoney, Maybe (Id FInvoice.Invoice)))
creditPrepaidBalance counterpartyType ownerId creditAmount paidAmount mbTdsRate mbGstBreakdown currency merchantId merchantOperatingCityId referenceId metadata mbInvoiceParams = do
  let gstAmount = case mbGstBreakdown of
        Just breakdown -> fromMaybe 0 breakdown.cgstAmount + fromMaybe 0 breakdown.sgstAmount + fromMaybe 0 breakdown.igstAmount
        Nothing -> 0
  mbOwnerAccount <- getOrCreatePrepaidAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
  mbSellerAsset <- getOrCreateSellerAssetAccount currency merchantId merchantOperatingCityId
  mbSellerLiability <- getOrCreateSellerLiabilityAccount currency merchantId merchantOperatingCityId
  mbGovernmentLiability <- getOrCreateGovernmentLiabilityAccount currency merchantId merchantOperatingCityId
  mbSellerRideCredit <- getOrCreateSellerRideCreditAccount currency merchantId merchantOperatingCityId
  mbGovtDirectAsset <- getOrCreateGovtDirectAssetAccount currency merchantId merchantOperatingCityId
  mbGovtDirectExpense <- getOrCreateGovtDirectExpenseAccount currency merchantId merchantOperatingCityId
  case (mbOwnerAccount, mbSellerAsset, mbSellerLiability, mbGovernmentLiability, mbSellerRideCredit, mbGovtDirectAsset, mbGovtDirectExpense) of
    (Right ownerAccount, Right sellerAsset, Right sellerLiability, Right governmentLiability, Right sellerRideCredit, Right govtDirectAsset, Right govtDirectExpense) -> do
      existingPurchaseEntries <- getEntriesByReference subscriptionPurchaseReferenceType referenceId
      existingCreditEntries <- getEntriesByReference subscriptionCreditReferenceType referenceId
      existingTdsEntries <- getEntriesByReference tdsReimbursementReferenceType referenceId
      let gstAmount' = max 0 gstAmount
          netAmount = max 0 (paidAmount - gstAmount')
      let purchaseEntryExists fromId toId amount =
            any
              ( \entry ->
                  entry.fromAccountId == fromId
                    && entry.toAccountId == toId
                    && entry.amount == amount
                    && entry.status == SETTLED
              )
              existingPurchaseEntries
          creditEntryExists =
            any
              ( \entry ->
                  entry.fromAccountId == sellerRideCredit.id
                    && entry.toAccountId == ownerAccount.id
                    && entry.amount == creditAmount
                    && entry.status == SETTLED
              )
              existingCreditEntries
          tdsEntryExists fromId toId amount =
            any
              ( \entry ->
                  entry.fromAccountId == fromId
                    && entry.toAccountId == toId
                    && entry.amount == amount
                    && entry.status == SETTLED
              )
              existingTdsEntries
      -- Create GST entry
      gstEntryId <-
        if paidAmount > 0 && gstAmount' > 0
          then do
            if purchaseEntryExists sellerAsset.id governmentLiability.id gstAmount'
              then pure Nothing
              else do
                let gstEntry =
                      LedgerEntryInput
                        { fromAccountId = sellerAsset.id,
                          toAccountId = governmentLiability.id,
                          amount = gstAmount',
                          currency = currency,
                          entryType = LiabilityCreated,
                          status = SETTLED,
                          referenceType = subscriptionPurchaseReferenceType,
                          referenceId = referenceId,
                          metadata = Nothing,
                          merchantId = merchantId,
                          merchantOperatingCityId = merchantOperatingCityId
                        }
                result <- createEntryWithBalanceUpdate gstEntry
                pure $ either (const Nothing) (Just . (.id)) result
          else pure Nothing
      -- Create liability entry
      liabilityEntryId <-
        if paidAmount > 0
          then do
            if netAmount > 0
              then do
                if purchaseEntryExists sellerAsset.id sellerLiability.id netAmount
                  then pure Nothing
                  else do
                    let liabilityEntry =
                          LedgerEntryInput
                            { fromAccountId = sellerAsset.id,
                              toAccountId = sellerLiability.id,
                              amount = netAmount,
                              currency = currency,
                              entryType = LiabilityCreated,
                              status = SETTLED,
                              referenceType = subscriptionPurchaseReferenceType,
                              referenceId = referenceId,
                              metadata = Nothing,
                              merchantId = merchantId,
                              merchantOperatingCityId = merchantOperatingCityId
                            }
                    result <- createEntryWithBalanceUpdate liabilityEntry
                    pure $ either (const Nothing) (Just . (.id)) result
              else pure Nothing
          else pure Nothing
      -- Create TDS reimbursement entry
      tdsEntryId <-
        case mbTdsRate of
          Just rate | rate > 0 && netAmount > 0 -> do
            let tdsAmount = netAmount * realToFrac rate
            if tdsAmount > 0
              then
                if tdsEntryExists govtDirectAsset.id govtDirectExpense.id tdsAmount
                  then pure Nothing
                  else do
                    let tdsEntry =
                          LedgerEntryInput
                            { fromAccountId = govtDirectAsset.id,
                              toAccountId = govtDirectExpense.id,
                              amount = tdsAmount,
                              currency = currency,
                              entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                              status = SETTLED,
                              referenceType = tdsReimbursementReferenceType,
                              referenceId = referenceId,
                              metadata = Nothing,
                              merchantId = merchantId,
                              merchantOperatingCityId = merchantOperatingCityId
                            }
                    result <- createEntryWithBalanceUpdate tdsEntry
                    pure $ either (const Nothing) (Just . (.id)) result
              else pure Nothing
          _ -> pure Nothing
      -- Create credit entry
      _ <-
        if creditAmount > 0
          then do
            if creditEntryExists
              then pure Nothing
              else do
                let creditEntry =
                      LedgerEntryInput
                        { fromAccountId = sellerRideCredit.id,
                          toAccountId = ownerAccount.id,
                          amount = creditAmount,
                          currency = currency,
                          entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                          status = SETTLED,
                          referenceType = subscriptionCreditReferenceType,
                          referenceId = referenceId,
                          metadata = metadata,
                          merchantId = merchantId,
                          merchantOperatingCityId = merchantOperatingCityId
                        }
                result <- createEntryWithBalanceUpdate creditEntry
                pure $ either (const Nothing) (Just . (.id)) result
          else pure Nothing
      -- Collect all entry IDs
      let entryIds = catMaybes [gstEntryId, liabilityEntryId, tdsEntryId]
      -- Create invoice for subscription purchases
      mbInvoiceId <- case mbInvoiceParams of
        Just invoiceParams | not (null entryIds) -> do
          let invoiceInput =
                InvoiceInput
                  { invoiceType = SubscriptionPurchase,
                    paymentOrderId = Just invoiceParams.paymentOrderId,
                    issuedToType = invoiceParams.issuedToType,
                    issuedToId = ownerId,
                    issuedToName = invoiceParams.issuedToName,
                    issuedToAddress = invoiceParams.issuedToAddress,
                    issuedByType = invoiceParams.issuedByType,
                    issuedById = invoiceParams.issuedById,
                    issuedByName = invoiceParams.issuedByName,
                    issuedByAddress = invoiceParams.issuedByAddress,
                    supplierName = Nothing,
                    supplierAddress = Nothing,
                    supplierGSTIN = Nothing,
                    supplierId = Nothing,
                    gstinOfParty = invoiceParams.gstinOfParty,
                    panOfParty = Nothing,
                    tanOfDeductee = Nothing,
                    lineItems =
                      let lineItemGstAmount = max 0 gstAmount
                          lineItemNetAmount = max 0 (paidAmount - lineItemGstAmount)
                       in catMaybes
                            [ if lineItemNetAmount > 0
                                then
                                  Just
                                    InvoiceLineItem
                                      { description = "Subscription Plan Fee",
                                        quantity = 1,
                                        unitPrice = lineItemNetAmount,
                                        lineTotal = lineItemNetAmount,
                                        isExternalCharge = False
                                      }
                                else Nothing,
                              if lineItemGstAmount > 0
                                then
                                  Just
                                    InvoiceLineItem
                                      { description = "GST",
                                        quantity = 1,
                                        unitPrice = lineItemGstAmount,
                                        lineTotal = lineItemGstAmount,
                                        isExternalCharge = False
                                      }
                                else Nothing
                            ],
                    gstBreakdown = mbGstBreakdown, -- explicit CGST/SGST/IGST from caller, falls back to 50/50 in Service.hs if Nothing
                    currency = currency,
                    dueAt = Nothing,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId,
                    merchantShortId = invoiceParams.merchantShortId
                  }
          invoiceResult <- createInvoice invoiceInput entryIds
          case invoiceResult of
            Right invoice -> pure (Just invoice.id)
            Left _err -> pure Nothing
        _ -> pure Nothing
      mbBal <- getBalance ownerAccount.id
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") (\bal -> Right (bal, mbInvoiceId)) mbBal
    (Left err, _, _, _, _, _, _) -> pure $ Left err
    (_, Left err, _, _, _, _, _) -> pure $ Left err
    (_, _, Left err, _, _, _, _) -> pure $ Left err
    (_, _, _, Left err, _, _, _) -> pure $ Left err
    (_, _, _, _, Left err, _, _) -> pure $ Left err
    (_, _, _, _, _, Left err, _) -> pure $ Left err
    (_, _, _, _, _, _, Left err) -> pure $ Left err

debitPrepaidBalance ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney -> -- Final ride fare (base fare for settlement)
  HighPrecMoney -> -- Revenue recognition amount
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID (booking ID)
  Maybe Value ->
  m (Either FinanceError HighPrecMoney)
debitPrepaidBalance counterpartyType ownerId finalFare revenueAmount currency merchantId merchantOperatingCityId referenceId _metadata = do
  mbOwnerAccount <- getOrCreatePrepaidAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
  mbSellerLiability <- getOrCreateSellerLiabilityAccount currency merchantId merchantOperatingCityId
  mbSellerRevenue <- getOrCreateSellerRevenueAccount currency merchantId merchantOperatingCityId
  case (mbOwnerAccount, mbSellerLiability, mbSellerRevenue) of
    (Right ownerAccount, Right sellerLiability, Right sellerRevenue) -> do
      holdRes <- settlePrepaidHoldByReference counterpartyType ownerId referenceId finalFare
      case holdRes of
        Left err -> pure $ Left err
        Right _ -> do
          when (revenueAmount > 0) $ do
            let revenueEntry =
                  LedgerEntryInput
                    { fromAccountId = sellerLiability.id,
                      toAccountId = sellerRevenue.id,
                      amount = revenueAmount,
                      currency = currency,
                      entryType = Lib.Finance.Domain.Types.LedgerEntry.Revenue,
                      status = SETTLED,
                      referenceType = subscriptionRideReferenceType,
                      referenceId = referenceId,
                      metadata = Nothing,
                      merchantId = merchantId,
                      merchantOperatingCityId = merchantOperatingCityId
                    }
            _ <- createEntryWithBalanceUpdate revenueEntry
            pure ()
          mbBal <- getBalance ownerAccount.id
          pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
    (Left err, _, _) -> pure $ Left err
    (_, Left err, _) -> pure $ Left err
    (_, _, Left err) -> pure $ Left err

-- | Handle subscription expiry: compute expired credits, create revenue recognition
-- and credit transfer entries, then mark the subscription as EXPIRED.
-- NOTE: Does NOT activate the next queued purchase's expiry timer.
-- The caller is responsible for calling activateNextQueuedPurchaseExpiry if needed.
handleSubscriptionExpiry ::
  (BeamFlow m r) =>
  DSP.SubscriptionPurchase ->
  m ()
handleSubscriptionExpiry purchase = do
  when (purchase.status == DSP.ACTIVE) $ do
    let counterpartyType = case purchase.ownerType of
          DSP.FLEET_OWNER -> counterpartyFleetOwner
          DSP.DRIVER -> counterpartyDriver
        ownerId = purchase.ownerId
        currency = INR -- TODO: derive from merchant config if needed
        merchantId = purchase.merchantId.getId
        merchantOperatingCityId = purchase.merchantOperatingCityId.getId
        referenceId = purchase.id.getId

    -- Fetch all other ACTIVE subscriptions (excluding the one being expired)
    allActive <- QSPE.findAllActiveByOwnerAndServiceName ownerId purchase.ownerType PREPAID_SUBSCRIPTION
    let otherActive = filter (\p -> p.id /= purchase.id) allActive
        otherActiveCredits = sum $ map (.planRideCredit) otherActive

    -- Get current unified balance
    mbBalance <- getPrepaidBalanceByOwner counterpartyType ownerId
    let currentBalance = fromMaybe 0 mbBalance
        -- Credits attributable to the expiring subscription
        expiredCredits = max 0 (currentBalance - otherActiveCredits)

    when (expiredCredits > 0) $ do
      -- Calculate proportional revenue amount from planFee
      let totalPlanCredit = purchase.planRideCredit
          revenueAmount =
            if totalPlanCredit > 0
              then (expiredCredits / totalPlanCredit) * purchase.planFee
              else 0

      mbOwnerAccount <- getOrCreatePrepaidAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
      mbSellerLiability <- getOrCreateSellerLiabilityAccount currency merchantId merchantOperatingCityId
      mbSellerRevenue <- getOrCreateSellerRevenueAccount currency merchantId merchantOperatingCityId
      mbSellerRideCredit <- getOrCreateSellerRideCreditAccount currency merchantId merchantOperatingCityId

      case (mbOwnerAccount, mbSellerLiability, mbSellerRevenue, mbSellerRideCredit) of
        (Right ownerAccount, Right sellerLiability, Right sellerRevenue, Right sellerRideCredit) -> do
          -- 1. Revenue Recognition: Seller Liability -> Seller Revenue
          when (revenueAmount > 0) $ do
            let revenueEntry =
                  LedgerEntryInput
                    { fromAccountId = sellerLiability.id,
                      toAccountId = sellerRevenue.id,
                      amount = revenueAmount,
                      currency = currency,
                      entryType = Lib.Finance.Domain.Types.LedgerEntry.Revenue,
                      status = SETTLED,
                      referenceType = expiryRevenueRecognitionReferenceType,
                      referenceId = referenceId,
                      metadata = Nothing,
                      merchantId = merchantId,
                      merchantOperatingCityId = merchantOperatingCityId
                    }
            _ <- createEntryWithBalanceUpdate revenueEntry
            pure ()

          -- 2. Credit Transfer: Owner RideCredit -> Seller RideCredit
          let creditTransferEntry =
                LedgerEntryInput
                  { fromAccountId = ownerAccount.id,
                    toAccountId = sellerRideCredit.id,
                    amount = expiredCredits,
                    currency = currency,
                    entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                    status = SETTLED,
                    referenceType = expiryCreditTransferReferenceType,
                    referenceId = referenceId,
                    metadata = Nothing,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId
                  }
          _ <- createEntryWithBalanceUpdate creditTransferEntry
          pure ()
        _ -> do
          logInfo $ "Failed to get accounts for subscription expiry: " <> referenceId
          pure ()

    QSP.updateStatusById DSP.EXPIRED purchase.id
    logInfo $ "Subscription " <> purchase.id.getId <> " expired. Expired credits: " <> show expiredCredits

-- | After a ride debit, check if the oldest ACTIVE subscription should be marked EXHAUSTED.
-- FIFO logic: if the current balance is at or below the sum of newer subscriptions' credits,
-- the oldest subscription's credits are fully used up.
-- Returns (contributing purchase IDs, whether any were exhausted).
-- If any were exhausted, the caller should call activateNextQueuedPurchaseExpiry
-- and schedule follow-up jobs.
checkAndMarkExhaustedSubscriptions ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  DSP.SubscriptionOwnerType ->
  m ([Id DSP.SubscriptionPurchase], Bool)
checkAndMarkExhaustedSubscriptions counterpartyType ownerId ownerType = do
  allActive <- QSPE.findAllActiveByOwnerAndServiceName ownerId ownerType PREPAID_SUBSCRIPTION
  mbBalance <- getPrepaidBalanceByOwner counterpartyType ownerId
  let currentBalance = fromMaybe 0 mbBalance
      -- Sort by purchaseTimestamp ASC (already from query), process FIFO
      sorted = DL.sortOn (.purchaseTimestamp) allActive
  go sorted currentBalance [] False
  where
    go [] _ acc exhausted = pure (acc, exhausted)
    go [single] _ acc exhausted = pure (acc <> [single.id], exhausted) -- Last one is always contributing
    go (oldest : rest) balance acc exhausted = do
      let restCredits = sum $ map (.planRideCredit) rest
      if balance <= restCredits
        then do
          -- Oldest subscription's credits are fully consumed
          QSP.updateStatusById DSP.EXHAUSTED oldest.id
          logInfo $ "Subscription " <> oldest.id.getId <> " marked EXHAUSTED"
          -- Continue checking (there might be more to exhaust)
          go rest balance (acc <> [oldest.id]) True
        else pure (acc <> [oldest.id], exhausted) -- Oldest is partially consumed

-- | Activate the expiry timer for the next queued subscription purchase.
-- A queued purchase is ACTIVE but has expiryDate = Nothing (its timer hasn't started).
-- Called after an exhaustion or expiry event to cascade to the next FIFO purchase.
-- Returns Just (purchaseId, expiryDate) if a purchase was activated, Nothing otherwise.
-- The caller is responsible for scheduling the ExpireSubscriptionPurchase job.
activateNextQueuedPurchaseExpiry ::
  (BeamFlow m r) =>
  Text -> -- Owner ID
  DSP.SubscriptionOwnerType ->
  m (Maybe (Id DSP.SubscriptionPurchase, UTCTime))
activateNextQueuedPurchaseExpiry ownerId ownerType = do
  allActive <- QSPE.findAllActiveByOwnerAndServiceName ownerId ownerType PREPAID_SUBSCRIPTION
  let sorted = DL.sortOn (.purchaseTimestamp) allActive
      -- Find first ACTIVE purchase with no expiryDate (queued)
      queued = filter (\p -> isNothing p.expiryDate) sorted
  case queued of
    (nextPurchase : _) -> do
      mbPlan <- QPlan.findByPrimaryKey nextPurchase.planId
      case mbPlan of
        Just plan -> do
          now <- getCurrentTime
          let expiryDate = fmap (\days -> addUTCTime (fromIntegral (days * 60 * 60 * 24)) now) plan.validityInDays
          QSPE.updateExpiryDateById expiryDate nextPurchase.id
          logInfo $ "Activated expiry for queued subscription " <> nextPurchase.id.getId <> " with expiryDate: " <> show expiryDate
          pure $ (nextPurchase.id,) <$> expiryDate
        Nothing -> do
          logInfo $ "Plan not found for queued subscription: " <> nextPurchase.planId.getId
          pure Nothing
    [] -> pure Nothing
