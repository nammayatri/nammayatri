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
    utcToLocalDay,
    payoutCutoffTimeUTC,
    todayRangeUTC,
    getNonRedeemableBalance,
  )
where

import Data.Aeson (Value)
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id
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
walletReferenceTopup = "WalletTopup"

walletReferencePayout :: Text
walletReferencePayout = "WalletPayout"

-- Time helpers (shared across getWalletTransactions, postWalletPayout, postWalletTopup)

-- | Convert a UTC time to a local Day given a timezone offset (seconds from UTC)
utcToLocalDay :: NominalDiffTime -> UTCTime -> Time.Day
utcToLocalDay timeDiff utcTime = Time.utctDay (Time.addUTCTime timeDiff utcTime)

-- | Compute the payout cutoff time in UTC.
--   Entries after this time are considered non-redeemable (recent ride earnings).
payoutCutoffTimeUTC :: NominalDiffTime -> Int -> UTCTime -> UTCTime
payoutCutoffTimeUTC timeDiff cutOffDays now =
  let localDay = utcToLocalDay timeDiff now
      cutOffDay = Time.addDays (negate (fromIntegral cutOffDays)) localDay
   in Time.addUTCTime (negate timeDiff) (Time.UTCTime cutOffDay 0)

-- | Get the UTC time range for "today" given a timezone offset.
--   Returns (startOfDayUTC, endOfDayUTC).
todayRangeUTC :: NominalDiffTime -> UTCTime -> (UTCTime, UTCTime)
todayRangeUTC timeDiff now =
  let localDay = utcToLocalDay timeDiff now
      start = Time.addUTCTime (negate timeDiff) (Time.UTCTime localDay 0)
      end = Time.addUTCTime (negate timeDiff) (Time.UTCTime localDay 86399)
   in (start, end)

-- | Calculate non-redeemable balance: sum of recent ride earnings after payout cutoff.
getNonRedeemableBalance ::
  (BeamFlow m r) =>
  Id Account ->
  NominalDiffTime -> -- timezone offset
  Int -> -- payoutCutOffDays
  UTCTime -> -- current time
  m HighPrecMoney
getNonRedeemableBalance accountId timeDiff cutOffDays now = do
  let cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  entries <- findByAccountWithFilters accountId (Just cutoff) (Just now) Nothing Nothing Nothing (Just [walletReferenceBaseRide])
  pure $ sum $ map (.amount) entries

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
