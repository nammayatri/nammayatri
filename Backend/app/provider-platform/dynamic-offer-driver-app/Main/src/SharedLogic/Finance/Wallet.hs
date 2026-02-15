{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Wallet
  ( walletReferenceBaseRide,
    walletReferenceGSTOnline,
    walletReferenceTollCharges,
    walletReferenceParkingCharges,
    walletReferenceTDSDeductionOnline,
    walletReferenceGSTCash,
    walletReferenceTDSDeductionCash,
    walletReferenceTopup,
    walletReferencePayout,
    getWalletAccountByOwner,
    getWalletBalanceByOwner,
    getOrCreateWalletAccount,
    createWalletEntryDelta,
    reverseWalletEntryByReference,
    getOrCreateBuyerAssetAccount,
    getOrCreateBuyerExternalAccount,
    getOrCreateDriverLiabilityAccount,
    getOrCreateFleetOwnerLiabilityAccount,
    getOrCreateGovtIndirectLiabilityAccount,
    getOrCreateGovtDirectLiabilityAccount,
    createLedgerTransfer,
    createLedgerTransferAllowZero,
  )
where

import Data.Aeson (Value)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Common (Currency, HighPrecMoney)
import Lib.Finance
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)

-- Reference type constants (PascalCase, abbreviations in all caps)

walletReferenceBaseRide :: Text
walletReferenceBaseRide = "BaseRide"

walletReferenceGSTOnline :: Text
walletReferenceGSTOnline = "GSTOnline"

walletReferenceTollCharges :: Text
walletReferenceTollCharges = "TollCharges"

walletReferenceParkingCharges :: Text
walletReferenceParkingCharges = "ParkingCharges"

walletReferenceTDSDeductionOnline :: Text
walletReferenceTDSDeductionOnline = "TDSDeductionOnline"

walletReferenceGSTCash :: Text
walletReferenceGSTCash = "GSTCash"

walletReferenceTDSDeductionCash :: Text
walletReferenceTDSDeductionCash = "TDSDeductionCash"

walletReferenceTopup :: Text
walletReferenceTopup = "WALLET_TOPUP"

walletReferencePayout :: Text
walletReferencePayout = "WALLET_PAYOUT"

-- Account helpers

getWalletAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getWalletAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == Liability) accounts

getWalletBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

getOrCreateWalletAccount ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateWalletAccount counterpartyType ownerId currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just counterpartyType,
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
          { accountType = Asset,
            counterpartyType = Just SELLER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

-- New account helpers for the refactored wallet flow

getOrCreateBuyerAssetAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateBuyerAssetAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            counterpartyType = Just BUYER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateBuyerExternalAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateBuyerExternalAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = External,
            counterpartyType = Just BUYER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateDriverLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Driver ID
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateDriverLiabilityAccount currency driverId =
  getOrCreateWalletAccount DRIVER driverId currency

getOrCreateFleetOwnerLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Fleet owner ID
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateFleetOwnerLiabilityAccount currency fleetOwnerId =
  getOrCreateWalletAccount FLEET_OWNER fleetOwnerId currency

getOrCreateGovtIndirectLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateGovtIndirectLiabilityAccount currency merchantId merchantOperatingCityId = do
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

getOrCreateGovtDirectLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateGovtDirectLiabilityAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just GOVERNMENT_DIRECT,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

-- Ledger transfer functions

createLedgerTransfer ::
  (BeamFlow m r) =>
  Account ->
  Account ->
  HighPrecMoney ->
  Text ->
  Text ->
  m (Either FinanceError (Maybe (Id LedgerEntry)))
createLedgerTransfer fromAccount toAccount amount referenceType referenceId = do
  if amount <= 0
    then pure $ Right Nothing
    else do
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAccount.id,
                toAccountId = toAccount.id,
                amount = amount,
                currency = fromAccount.currency,
                entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                status = SETTLED,
                referenceType = referenceType,
                referenceId = referenceId,
                metadata = Nothing,
                merchantId = fromAccount.merchantId,
                merchantOperatingCityId = fromAccount.merchantOperatingCityId
              }
      entryRes <- createEntryWithBalanceUpdate entryInput
      case entryRes of
        Left err -> pure $ Left err
        Right entry -> pure $ Right (Just entry.id)

-- | Like createLedgerTransfer but allows zero-amount entries (for placeholder entries like TDS)
createLedgerTransferAllowZero ::
  (BeamFlow m r) =>
  Account ->
  Account ->
  HighPrecMoney ->
  Text ->
  Text ->
  m (Either FinanceError (Maybe (Id LedgerEntry)))
createLedgerTransferAllowZero fromAccount toAccount amount referenceType referenceId = do
  if amount < 0
    then pure $ Right Nothing
    else do
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAccount.id,
                toAccountId = toAccount.id,
                amount = amount,
                currency = fromAccount.currency,
                entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                status = SETTLED,
                referenceType = referenceType,
                referenceId = referenceId,
                metadata = Nothing,
                merchantId = fromAccount.merchantId,
                merchantOperatingCityId = fromAccount.merchantOperatingCityId
              }
      entryRes <- createEntryWithBalanceUpdate entryInput
      case entryRes of
        Left err -> pure $ Left err
        Right entry -> pure $ Right (Just entry.id)

-- Wallet entry delta (for topup/payout)

createWalletEntryDelta ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney -> -- Delta (positive credit, negative debit)
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference type
  Text -> -- Reference ID
  Maybe Value ->
  m (Either FinanceError HighPrecMoney)
createWalletEntryDelta counterpartyType ownerId delta currency merchantId merchantOperatingCityId referenceType referenceId metadata = do
  if delta == 0
    then do
      mbBalance <- getWalletBalanceByOwner counterpartyType ownerId
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBalance
    else do
      mbOwnerAccount <- getOrCreateWalletAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
      mbPlatformAccount <- getOrCreatePlatformAccount currency merchantId merchantOperatingCityId
      case (mbOwnerAccount, mbPlatformAccount) of
        (Right ownerAccount, Right platformAccount) -> do
          let (fromAcc, toAcc, amount, eType) =
                if delta > 0
                  then (platformAccount.id, ownerAccount.id, delta, Lib.Finance.Domain.Types.LedgerEntry.Expense)
                  else (ownerAccount.id, platformAccount.id, abs delta, Lib.Finance.Domain.Types.LedgerEntry.Revenue)
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = fromAcc,
                    toAccountId = toAcc,
                    amount = amount,
                    currency = currency,
                    entryType = eType,
                    status = SETTLED,
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
