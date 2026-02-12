{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Prepaid
  ( ownerTypeDriver,
    ownerTypeFleetOwner,
    ownerTypePlatform,
    getPrepaidAccountByOwner,
    getPrepaidBalanceByOwner,
    getPrepaidPendingHoldByOwner,
    getPrepaidAvailableBalanceByOwner,
    createPrepaidHold,
    voidPrepaidHold,
    creditPrepaidBalance,
    debitPrepaidBalance,
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)

ownerTypeDriver :: Text
ownerTypeDriver = "DRIVER"

ownerTypeFleetOwner :: Text
ownerTypeFleetOwner = "FLEET_OWNER"

ownerTypePlatform :: Text
ownerTypePlatform = "PLATFORM"

prepaidDeferredRevenueReferenceType :: Text
prepaidDeferredRevenueReferenceType = "PREPAID_DEFERRED_REVENUE"

prepaidRevenueRecognitionReferenceType :: Text
prepaidRevenueRecognitionReferenceType = "PREPAID_REVENUE_RECOGNITION"

prepaidPaymentReferenceType :: Text
prepaidPaymentReferenceType = "PREPAID_PAYMENT"

prepaidCreditIssuedReferenceType :: Text
prepaidCreditIssuedReferenceType = "PREPAID_CREDIT_ISSUED"

prepaidRideConsumeReferenceType :: Text
prepaidRideConsumeReferenceType = "PREPAID_RIDE_CONSUME"

ownerTypeSubscriptionGST :: Text
ownerTypeSubscriptionGST = "SubscriptionGST"

getPrepaidAccountByOwner ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  m (Maybe Account)
getPrepaidAccountByOwner ownerType ownerId = do
  accounts <- findAccountsByCounterparty (Just ownerType) (Just ownerId)
  pure $
    find
      (\acc -> acc.accountType == Asset && acc.accountCategory `elem` [Driver, Fleet])
      accounts

getPrepaidBalanceByOwner ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m (Maybe HighPrecMoney)
getPrepaidBalanceByOwner ownerType ownerId = do
  mbAcc <- getPrepaidAccountByOwner ownerType ownerId
  pure $ mbAcc <&> (.balance)

getPrepaidPendingHoldByOwner ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m HighPrecMoney
getPrepaidPendingHoldByOwner ownerType ownerId =
  sumByOwnerAndStatus ownerType ownerId PENDING

getPrepaidAvailableBalanceByOwner ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m (Maybe HighPrecMoney)
getPrepaidAvailableBalanceByOwner ownerType ownerId = do
  mbBalance <- getPrepaidBalanceByOwner ownerType ownerId
  pendingHold <- getPrepaidPendingHoldByOwner ownerType ownerId
  pure $ (\balance -> balance - pendingHold) <$> mbBalance

getOrCreatePrepaidAccount ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  AccountCategory ->
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreatePrepaidAccount ownerType ownerId category currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            accountCategory = category,
            counterpartyType = Just ownerType,
            counterpartyId = Just ownerId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreatePlatformSuspenseAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreatePlatformSuspenseAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            accountCategory = Suspense,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreatePlatformDeferredRevenueAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreatePlatformDeferredRevenueAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = DeferredRevenue,
            accountCategory = Platform,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreatePlatformRevenueAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreatePlatformRevenueAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Revenue,
            accountCategory = Platform,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreatePlatformCreditIssuerAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreatePlatformCreditIssuerAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Equity,
            accountCategory = Platform,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateExternalBankAccount ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateExternalBankAccount ownerType ownerId currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = External,
            accountCategory = Suspense,
            counterpartyType = Just ownerType,
            counterpartyId = Just ownerId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateSubscriptionGSTAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateSubscriptionGSTAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Expense,
            accountCategory = Settlement,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

createPrepaidHold ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  AccountCategory ->
  HighPrecMoney ->
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference type
  Text -> -- Reference ID
  Maybe Value ->
  m (Either FinanceError ())
createPrepaidHold ownerType ownerId category amount currency merchantId merchantOperatingCityId referenceType referenceId metadata = do
  mbExistingHold <- findPendingPrepaidHoldByReference ownerType ownerId referenceType referenceId
  case mbExistingHold of
    Just _ -> pure $ Right ()
    Nothing -> do
      mbOwnerAccount <- getOrCreatePrepaidAccount ownerType ownerId category currency merchantId merchantOperatingCityId
      mbDeferredRevenueAccount <- getOrCreatePlatformDeferredRevenueAccount currency merchantId merchantOperatingCityId
      case (mbOwnerAccount, mbDeferredRevenueAccount) of
        (Right ownerAccount, Right deferredRevenueAccount) -> do
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = ownerAccount.id,
                    toAccountId = deferredRevenueAccount.id,
                    amount = amount,
                    currency = currency,
                    entryType = LiabilityCreated,
                    status = PENDING,
                    ownerType = ownerType,
                    ownerId = ownerId,
                    referenceType = referenceType,
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
  Text ->
  Text ->
  Text ->
  Text ->
  m (Maybe LedgerEntry)
findPendingPrepaidHoldByReference ownerType ownerId referenceType referenceId = do
  entries <- getEntriesByReference referenceType referenceId
  pure $ find (\entry -> entry.ownerType == ownerType && entry.ownerId == ownerId && entry.status == PENDING) entries

voidPrepaidHold ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  Text -> -- Reference type
  Text -> -- Reference ID
  Text -> -- Reason
  m ()
voidPrepaidHold ownerType ownerId referenceType referenceId reason = do
  entries <- getEntriesByReference referenceType referenceId
  let pendingEntries =
        filter
          (\entry -> entry.ownerType == ownerType && entry.ownerId == ownerId && entry.status == PENDING)
          entries
  forM_ pendingEntries $ \entry -> voidEntry entry.id reason

creditPrepaidBalance ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  AccountCategory ->
  HighPrecMoney -> -- Ride credit amount
  HighPrecMoney -> -- Paid amount
  HighPrecMoney -> -- GST amount
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID
  Maybe Value ->
  m (Either FinanceError HighPrecMoney)
creditPrepaidBalance ownerType ownerId category creditAmount paidAmount gstAmount currency merchantId merchantOperatingCityId referenceId metadata = do
  mbOwnerAccount <- getOrCreatePrepaidAccount ownerType ownerId category currency merchantId merchantOperatingCityId
  mbPlatformSuspense <- getOrCreatePlatformSuspenseAccount currency merchantId merchantOperatingCityId
  mbDeferredRevenueAccount <- getOrCreatePlatformDeferredRevenueAccount currency merchantId merchantOperatingCityId
  mbCreditIssuer <- getOrCreatePlatformCreditIssuerAccount currency merchantId merchantOperatingCityId
  mbSubscriptionGST <- getOrCreateSubscriptionGSTAccount currency merchantId merchantOperatingCityId
  mbExternalBank <- getOrCreateExternalBankAccount ownerType ownerId currency merchantId merchantOperatingCityId
  case (mbOwnerAccount, mbPlatformSuspense, mbDeferredRevenueAccount, mbCreditIssuer) of
    (Right ownerAccount, Right platformSuspense, Right deferredRevenueAccount, Right creditIssuerAccount) -> do
      when (paidAmount > 0) $ do
        case mbExternalBank of
          Right externalBank -> do
            let paymentEntry =
                  LedgerEntryInput
                    { fromAccountId = externalBank.id,
                      toAccountId = platformSuspense.id,
                      amount = paidAmount,
                      currency = currency,
                      entryType = Transfer,
                      status = SETTLED,
                      ownerType = ownerType,
                      ownerId = ownerId,
                      referenceType = prepaidPaymentReferenceType,
                      referenceId = referenceId,
                      metadata = Nothing,
                      merchantId = merchantId,
                      merchantOperatingCityId = merchantOperatingCityId
                    }
            _ <- createEntryWithBalanceUpdate paymentEntry
            pure ()
          _ -> pure ()
        when (gstAmount > 0) $ do
          case mbSubscriptionGST of
            Right subscriptionGST -> do
              let gstEntry =
                    LedgerEntryInput
                      { fromAccountId = platformSuspense.id,
                        toAccountId = subscriptionGST.id,
                        amount = gstAmount,
                        currency = currency,
                        entryType = Transfer,
                        status = SETTLED,
                        ownerType = ownerTypePlatform,
                        ownerId = merchantId,
                        referenceType = "SUBSCRIPTION_GST",
                        referenceId = referenceId,
                        metadata = Nothing,
                        merchantId = merchantId,
                        merchantOperatingCityId = merchantOperatingCityId
                      }
              _ <- createEntryWithBalanceUpdate gstEntry
              pure ()
            _ -> pure ()
        let deferredAmount = max 0 (paidAmount - gstAmount)
        when (deferredAmount > 0) $ do
          let deferredRevenueEntry =
                LedgerEntryInput
                  { fromAccountId = platformSuspense.id,
                    toAccountId = deferredRevenueAccount.id,
                    amount = deferredAmount,
                    currency = currency,
                    entryType = Transfer,
                    status = SETTLED,
                    ownerType = ownerTypePlatform,
                    ownerId = merchantId,
                    referenceType = prepaidDeferredRevenueReferenceType,
                    referenceId = referenceId,
                    metadata = Nothing,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId
                  }
          _ <- createEntryWithBalanceUpdate deferredRevenueEntry
          pure ()
      when (creditAmount > 0) $ do
        let creditEntry =
              LedgerEntryInput
                { fromAccountId = creditIssuerAccount.id,
                  toAccountId = ownerAccount.id,
                  amount = creditAmount,
                  currency = currency,
                  entryType = Transfer,
                  status = SETTLED,
                  ownerType = ownerType,
                  ownerId = ownerId,
                  referenceType = prepaidCreditIssuedReferenceType,
                  referenceId = referenceId,
                  metadata = metadata,
                  merchantId = merchantId,
                  merchantOperatingCityId = merchantOperatingCityId
                }
        _ <- createEntryWithBalanceUpdate creditEntry
        pure ()
      mbBal <- getBalance ownerAccount.id
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
    (Left err, _, _, _) -> pure $ Left err
    (_, Left err, _, _) -> pure $ Left err
    (_, _, Left err, _) -> pure $ Left err
    (_, _, _, Left err) -> pure $ Left err

debitPrepaidBalance ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  AccountCategory ->
  HighPrecMoney -> -- Ride amount
  HighPrecMoney -> -- Revenue recognition amount
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID
  Maybe Value ->
  m (Either FinanceError HighPrecMoney)
debitPrepaidBalance ownerType ownerId category rideAmount revenueAmount currency merchantId merchantOperatingCityId referenceId metadata = do
  mbOwnerAccount <- getOrCreatePrepaidAccount ownerType ownerId category currency merchantId merchantOperatingCityId
  mbCreditIssuer <- getOrCreatePlatformCreditIssuerAccount currency merchantId merchantOperatingCityId
  mbDeferredRevenueAccount <- getOrCreatePlatformDeferredRevenueAccount currency merchantId merchantOperatingCityId
  mbRevenueAccount <- getOrCreatePlatformRevenueAccount currency merchantId merchantOperatingCityId
  case (mbOwnerAccount, mbCreditIssuer) of
    (Right ownerAccount, Right creditIssuerAccount) -> do
      when (rideAmount > 0) $ do
        let consumeEntry =
              LedgerEntryInput
                { fromAccountId = ownerAccount.id,
                  toAccountId = creditIssuerAccount.id,
                  amount = rideAmount,
                  currency = currency,
                  entryType = Transfer,
                  status = SETTLED,
                  ownerType = ownerType,
                  ownerId = ownerId,
                  referenceType = prepaidRideConsumeReferenceType,
                  referenceId = referenceId,
                  metadata = metadata,
                  merchantId = merchantId,
                  merchantOperatingCityId = merchantOperatingCityId
                }
        _ <- createEntryWithBalanceUpdate consumeEntry
        pure ()
      when (revenueAmount > 0) $ do
        case (mbDeferredRevenueAccount, mbRevenueAccount) of
          (Right deferredRevenueAccount, Right revenueAccount) -> do
            let revenueEntry =
                  LedgerEntryInput
                    { fromAccountId = deferredRevenueAccount.id,
                      toAccountId = revenueAccount.id,
                      amount = revenueAmount,
                      currency = currency,
                      entryType = Transfer,
                      status = SETTLED,
                      ownerType = ownerTypePlatform,
                      ownerId = merchantId,
                      referenceType = prepaidRevenueRecognitionReferenceType,
                      referenceId = referenceId,
                      metadata = Nothing,
                      merchantId = merchantId,
                      merchantOperatingCityId = merchantOperatingCityId
                    }
            _ <- createEntryWithBalanceUpdate revenueEntry
            pure ()
          _ -> pure ()
      mbBal <- getBalance ownerAccount.id
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err
