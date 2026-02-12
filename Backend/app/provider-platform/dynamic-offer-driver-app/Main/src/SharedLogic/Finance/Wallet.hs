{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Wallet
  ( walletReferenceRideEarning,
    walletReferenceRideGstDeduction,
    walletReferenceRidePaymentOnline,
    walletReferenceRidePaymentCash,
    walletReferenceRidePlatformRevenue,
    walletReferenceTopup,
    walletReferencePayout,
    getWalletAccountByOwner,
    getWalletBalanceByOwner,
    getOrCreateWalletAccount,
    createWalletEntryDelta,
    reverseWalletEntryByReference,
    getOrCreatePlatformSuspenseAccount,
    getOrCreatePlatformRevenueAccount,
    getOrCreateRideGSTExpenseAccount,
    getOrCreateExternalCustomerBankAccount,
    getOrCreateExternalDriverBankAccount,
    createLedgerTransfer,
  )
where

import Data.Aeson (Value)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.Finance.Prepaid (ownerTypePlatform)

walletReferenceRideEarning :: Text
walletReferenceRideEarning = "WALLET_RIDE_EARNING"

walletReferenceRideGstDeduction :: Text
walletReferenceRideGstDeduction = "WALLET_RIDE_GST_DEDUCTION"

walletReferenceRidePaymentOnline :: Text
walletReferenceRidePaymentOnline = "RIDE_PAYMENT_ONLINE"

walletReferenceRidePaymentCash :: Text
walletReferenceRidePaymentCash = "RIDE_PAYMENT_CASH"

walletReferenceRidePlatformRevenue :: Text
walletReferenceRidePlatformRevenue = "RIDE_PLATFORM_REVENUE"

walletReferenceTopup :: Text
walletReferenceTopup = "WALLET_TOPUP"

walletReferencePayout :: Text
walletReferencePayout = "WALLET_PAYOUT"

getWalletAccountByOwner ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  m (Maybe Account)
getWalletAccountByOwner ownerType ownerId = do
  accounts <- findAccountsByCounterparty (Just ownerType) (Just ownerId)
  pure $
    find
      (\acc -> acc.accountType == Asset && acc.accountCategory == Settlement)
      accounts

getWalletBalanceByOwner ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletBalanceByOwner ownerType ownerId = do
  mbAcc <- getWalletAccountByOwner ownerType ownerId
  pure $ mbAcc <&> (.balance)

getOrCreateWalletAccount ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateWalletAccount ownerType ownerId currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            accountCategory = Settlement,
            counterpartyType = Just ownerType,
            counterpartyId = Just ownerId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreatePlatformAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreatePlatformAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            accountCategory = Settlement,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
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

getOrCreateRideGSTExpenseAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateRideGSTExpenseAccount currency merchantId merchantOperatingCityId = do
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

getOrCreateExternalCustomerBankAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateExternalCustomerBankAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = External,
            accountCategory = Suspense,
            counterpartyType = Nothing,
            counterpartyId = Nothing,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateExternalDriverBankAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreateExternalDriverBankAccount currency merchantId merchantOperatingCityId driverId = do
  let input =
        AccountInput
          { accountType = External,
            accountCategory = Suspense,
            counterpartyType = Nothing,
            counterpartyId = Just driverId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

createLedgerTransfer ::
  (BeamFlow m r) =>
  Account ->
  Account ->
  HighPrecMoney ->
  Text ->
  Text ->
  Text ->
  Text ->
  m (Either FinanceError ())
createLedgerTransfer fromAccount toAccount amount ownerType ownerId referenceType referenceId = do
  if amount <= 0
    then pure $ Right ()
    else do
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAccount.id,
                toAccountId = toAccount.id,
                amount = amount,
                currency = fromAccount.currency,
                entryType = Transfer,
                status = SETTLED,
                ownerType = ownerType,
                ownerId = ownerId,
                referenceType = referenceType,
                referenceId = referenceId,
                metadata = Nothing,
                merchantId = fromAccount.merchantId,
                merchantOperatingCityId = fromAccount.merchantOperatingCityId
              }
      entryRes <- createEntryWithBalanceUpdate entryInput
      case entryRes of
        Left err -> pure $ Left err
        Right _ -> pure $ Right ()

createWalletEntryDelta ::
  (BeamFlow m r) =>
  Text -> -- Owner type
  Text -> -- Owner ID
  HighPrecMoney -> -- Delta (positive credit, negative debit)
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference type
  Text -> -- Reference ID
  Maybe Value ->
  m (Either FinanceError HighPrecMoney)
createWalletEntryDelta ownerType ownerId delta currency merchantId merchantOperatingCityId referenceType referenceId metadata = do
  if delta == 0
    then do
      mbBalance <- getWalletBalanceByOwner ownerType ownerId
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBalance
    else do
      mbOwnerAccount <- getOrCreateWalletAccount ownerType ownerId currency merchantId merchantOperatingCityId
      mbPlatformAccount <- getOrCreatePlatformAccount currency merchantId merchantOperatingCityId
      case (mbOwnerAccount, mbPlatformAccount) of
        (Right ownerAccount, Right platformAccount) -> do
          let (fromAcc, toAcc, amount) =
                if delta > 0
                  then (platformAccount.id, ownerAccount.id, delta)
                  else (ownerAccount.id, platformAccount.id, abs delta)
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = fromAcc,
                    toAccountId = toAcc,
                    amount = amount,
                    currency = currency,
                    entryType = Transfer,
                    status = SETTLED,
                    ownerType = ownerType,
                    ownerId = ownerId,
                    referenceType = referenceType,
                    referenceId = referenceId,
                    metadata = metadata,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId
                  }
          entryRes <- createEntryWithBalanceUpdate entryInput
          case entryRes of
            Left err -> pure $ Left err
            Right _ -> do
              mbBal <- getBalance ownerAccount.id
              pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err

reverseWalletEntryByReference ::
  (BeamFlow m r) =>
  Text -> -- Reference type
  Text -> -- Reference ID
  Text -> -- Reason
  m ()
reverseWalletEntryByReference referenceType referenceId reason = do
  entries <- getEntriesByReference referenceType referenceId
  let hasReversal = any (\e -> e.entryType == Reversal) entries
  unless hasReversal $
    case sortOn (Down . (.timestamp)) entries of
      (latest : _) -> do
        _ <- createReversal latest.id reason
        pure ()
      [] -> pure ()
