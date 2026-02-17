{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Prepaid
  ( counterpartyDriver,
    counterpartyFleetOwner,
    prepaidRideDebitReferenceType,
    subscriptionCreditReferenceType,
    subscriptionPurchaseReferenceType,
    subscriptionRideReferenceType,
    getPrepaidAccountByOwner,
    getPrepaidBalanceByOwner,
    getPrepaidPendingHoldByOwner,
    getPrepaidAvailableBalanceByOwner,
    createPrepaidHold,
    voidPrepaidHold,
    creditPrepaidBalance,
    debitPrepaidBalance,
    InvoiceCreationParams (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id
import Lib.Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)

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
            resFrom <- updateBalanceByDelta entry.fromAccountId (-1 * settleAmount)
            case resFrom of
              Left err -> pure $ Left err
              Right _ -> do
                resTo <- updateBalanceByDelta entry.toAccountId settleAmount
                case resTo of
                  Left err -> pure $ Left err
                  Right _ -> do
                    settleEntry entry.id
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
  HighPrecMoney -> -- GST amount
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID
  Maybe Value ->
  Maybe InvoiceCreationParams -> -- Optional invoice creation params
  m (Either FinanceError (HighPrecMoney, Maybe (Id FInvoice.Invoice)))
creditPrepaidBalance counterpartyType ownerId creditAmount paidAmount gstAmount currency merchantId merchantOperatingCityId referenceId metadata mbInvoiceParams = do
  mbOwnerAccount <- getOrCreatePrepaidAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
  mbSellerAsset <- getOrCreateSellerAssetAccount currency merchantId merchantOperatingCityId
  mbSellerLiability <- getOrCreateSellerLiabilityAccount currency merchantId merchantOperatingCityId
  mbGovernmentLiability <- getOrCreateGovernmentLiabilityAccount currency merchantId merchantOperatingCityId
  mbSellerRideCredit <- getOrCreateSellerRideCreditAccount currency merchantId merchantOperatingCityId
  case (mbOwnerAccount, mbSellerAsset, mbSellerLiability, mbGovernmentLiability, mbSellerRideCredit) of
    (Right ownerAccount, Right sellerAsset, Right sellerLiability, Right governmentLiability, Right sellerRideCredit) -> do
      -- Create GST entry
      gstEntryId <-
        if paidAmount > 0 && max 0 gstAmount > 0
          then do
            let gstAmount' = max 0 gstAmount
                gstEntry =
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
            let gstAmount' = max 0 gstAmount
                netAmount = max 0 (paidAmount - gstAmount')
            if netAmount > 0
              then do
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
      -- Create credit entry
      _ <-
        if creditAmount > 0
          then do
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
      let entryIds = catMaybes [gstEntryId, liabilityEntryId]
      -- Create invoice for subscription purchases
      mbInvoiceId <- case mbInvoiceParams of
        Just invoiceParams -> do
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
                    lineItems =
                      let gstAmount' = max 0 gstAmount
                          netAmount = max 0 (paidAmount - gstAmount')
                       in catMaybes
                            [ if netAmount > 0
                                then
                                  Just
                                    InvoiceLineItem
                                      { description = "Subscription Plan Fee",
                                        quantity = 1,
                                        unitPrice = netAmount,
                                        lineTotal = netAmount
                                      }
                                else Nothing,
                              if gstAmount' > 0
                                then
                                  Just
                                    InvoiceLineItem
                                      { description = "GST",
                                        quantity = 1,
                                        unitPrice = gstAmount',
                                        lineTotal = gstAmount'
                                      }
                                else Nothing
                            ],
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
        Nothing -> pure Nothing
      mbBal <- getBalance ownerAccount.id
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") (\bal -> Right (bal, mbInvoiceId)) mbBal
    (Left err, _, _, _, _) -> pure $ Left err
    (_, Left err, _, _, _) -> pure $ Left err
    (_, _, Left err, _, _) -> pure $ Left err
    (_, _, _, Left err, _) -> pure $ Left err
    (_, _, _, _, Left err) -> pure $ Left err

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
